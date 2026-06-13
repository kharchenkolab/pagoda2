library(pagoda2)

# TF-IDF reference (§6.2): tf = x/(depth/depthScale); * idf_peak; then log1p. Sparse-preserving.
tfidf_reference <- function(counts, depthScale = 1000) {
  depth <- rowSums(counts)
  idf <- log(1 + nrow(counts) / pmax(colSums(counts > 0), 1))
  tf <- counts / (depth / depthScale)
  out <- log1p(sweep(tf, 2, idf, "*"))
  out[counts == 0] <- 0 # zeros stay structural zeros
  out
}

tfidf_p2 <- function() {
  cm <- Matrix::Matrix(
    matrix(c(5, 0, 2, 1, 0, 4, 1, 3, 3, 0, 6, 2, 1, 2, 0, 5), nrow = 4, byrow = TRUE),
    sparse = TRUE, dimnames = list(paste0("g", 1:4), paste0("c", 1:4))
  )
  p2 <- Pagoda2$new(as(cm, "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  atac <- matrix(c(3, 0, 1, 0, 2, 4, 5, 1, 0, 2, 0, 3), nrow = 4, byrow = TRUE,
    dimnames = list(paste0("c", 1:4), c("p1", "p2", "p3")))
  p2$addFacet("ATAC", as(Matrix::Matrix(atac, sparse = TRUE), "dgCMatrix"),
    modelType = "tfidf", featureType = "peak", defaultReduction = "LSI")
  list(p2 = p2, atac = atac)
}

test_that("TF-IDF view model materializes to the reference (float64)", {
  o <- tfidf_p2()
  mat <- as.matrix(o$p2$getExpressionBlock(facet = "ATAC"))
  ref <- tfidf_reference(o$atac)
  expect_equal(mat, ref, tolerance = 1e-12)
})

test_that("TF-IDF per-peak mean/var match reference; thread-count invariant; facet records LSI default", {
  o <- tfidf_p2()
  ref <- tfidf_reference(o$atac)
  v1 <- o$p2$viewColMeanVar(facet = "ATAC", n.cores = 1)
  expect_identical(v1, o$p2$viewColMeanVar(facet = "ATAC", n.cores = 4)) # bit-identical across threads
  popvar <- function(x) mean((x - mean(x))^2) # kernel uses population variance (/n)
  expect_equal(v1$m, as.numeric(colMeans(ref)), tolerance = 1e-10)
  expect_equal(v1$v, as.numeric(apply(ref, 2, popvar)), tolerance = 1e-10)
  expect_identical(o$p2$getFacet("ATAC")$defaultReduction, "LSI")
})
