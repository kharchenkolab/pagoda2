library(pagoda2)

## The threaded ANN kNN backend (.pagoda2_knn_sparse): RcppHNSW (hnswlib) preferred, N2R fallback.
## These guard output shape and *neighbor recall*. The swap from N2R was for threading (N2R 1.0.5's query
## loop is serial — pragma commented out); RcppHNSW also recalls a touch higher (benchmark/KNN_BACKEND.md).

test_that(".pagoda2_knn_sparse returns an n x n sparse distance matrix with ~k neighbors per row", {
  skip_if_not_installed("RcppHNSW")
  set.seed(3)
  n <- 1200L
  X <- matrix(rnorm(n * 12L), n, 12L)
  k <- 15L
  M <- pagoda2:::.pagoda2_knn_sparse(X, k, n.cores = 2L, distance = "L2")
  expect_true(inherits(M, "CsparseMatrix"))
  expect_identical(dim(M), c(n, n))
  Matrix::diag(M) <- 0
  M <- Matrix::drop0(M)
  deg <- Matrix::rowSums(M != 0)
  expect_true(all(deg >= k - 2L & deg <= k)) # k incl self -> ~k-1 after self removed
})

test_that(".pagoda2_knn_sparse has high recall vs exact kNN (L2 and cosine)", {
  skip_if_not_installed("RcppHNSW")
  set.seed(5)
  n <- 1500L
  k <- 15L
  X <- matrix(rnorm(n * 18L), n, 18L)
  # exact L2 kNN ground truth in base R (col 1 = self); cosine kNN == L2 kNN on L2-normalized rows.
  exact_knnx <- function(Xref, query.rows, k) {
    t(vapply(query.rows, function(i) {
      d <- colSums((t(Xref) - Xref[i, ])^2)
      order(d)[seq_len(k)]
    }, integer(k)))
  }
  recall_of <- function(distance, Xexact) {
    M <- pagoda2:::.pagoda2_knn_sparse(X, k, n.cores = 2L, distance = distance)
    Matrix::diag(M) <- 0
    M <- Matrix::drop0(M)
    idx <- sort(sample(n, 300L))
    exact <- exact_knnx(Xexact, idx, k) # col 1 = self
    mean(vapply(seq_along(idx), function(t) {
      nb <- which(M[idx[t], ] != 0)
      mean(exact[t, -1L] %in% nb)
    }, numeric(1)))
  }
  expect_gt(recall_of("L2", X), 0.9) # high-recall ANN (RcppHNSW ~0.98-1.0)
  Xn <- X / sqrt(rowSums(X^2)) # unit rows -> L2 ranking == cosine ranking
  expect_gt(recall_of("cosine", Xn), 0.85)
})

test_that("makeKnnGraph runs on the threaded backend and yields a connected-ish graph", {
  skip_if_not_installed("RcppHNSW")
  set.seed(7)
  ng <- 200L
  nc <- 600L
  grp <- rep(1:3, length.out = nc)
  m <- matrix(rpois(ng * nc, 1), ng, nc)
  for (gg in 1:3) m[((gg - 1) * 30 + 1):(gg * 30), grp == gg] <- m[((gg - 1) * 30 + 1):(gg * 30), grp == gg] + rpois(30 * sum(grp == gg), 6)
  dimnames(m) <- list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc)))
  p2 <- Pagoda2$new(as(Matrix::Matrix(m, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 2,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  suppressWarnings({
    p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(nPcs = 10, var.scale = FALSE, verbose = FALSE)
    p2$runGraph(reduction = "PCA", k = 15, n.cores = 2, verbose = FALSE)
  })
  g <- p2$graphs[["PCA"]]
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), nc) # vcount() returns a double
  expect_gt(igraph::ecount(g), nc) # each cell contributes several neighbors
})
