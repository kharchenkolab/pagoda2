library(pagoda2)

make_thread_p2 <- function(n.cores = 2, threads = NULL) {
  values <- matrix(rpois(8 * 10, lambda = 3), nrow = 8, ncol = 10)
  values[values < 1] <- 0
  cm <- Matrix::Matrix(values, sparse = TRUE)
  rownames(cm) <- paste0("g", seq_len(nrow(cm)))
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  args <- list(cm, verbose = FALSE)
  if (!is.null(threads)) {
    args$threads <- threads
  } else {
    args$n.cores <- n.cores
  }
  do.call(Pagoda2$new, args)
}

test_that("thread policy resolves object, method, and task defaults", {
  p2 <- make_thread_p2(n.cores = 6)

  policy <- p2$getThreads()
  expect_equal(policy$total, 6)
  expect_equal(policy$native, 6)
  expect_equal(policy$sgd, 4)
  expect_equal(policy$r.workers, 6)

  expect_equal(p2$getThreads(method = "papply", n.cores = 20, tasks = 3)$r.workers, 3)
  expect_equal(p2$getThreads(method = "runEmbedding", threads = list(total = 6, sgd = 1))$sgd, 1)
  expect_error(p2$getThreads(method = "runGraph", threads = list(sgd = 1)), "not supported")
})

test_that("setCores and setThreads update object-level policy", {
  p2 <- make_thread_p2(n.cores = 2)

  p2$setCores(5)
  expect_equal(p2$n.cores, 5)
  expect_equal(p2$getThreads(method = "runGraph")$native, 5)

  p2$setThreads(native = 2, sgd = 1)
  expect_equal(p2$n.cores, 5)
  expect_equal(p2$getThreads(method = "runGraph")$native, 2)
  expect_equal(p2$getThreads(method = "runEmbedding")$sgd, 1)
})

test_that("run merges top-level budget with step-specific thread roles", {
  p2 <- make_thread_p2(n.cores = 1)

  suppressWarnings(
    p2$run(
      steps = "pca",
      profile = "pipeline",
      plots = "none",
      n.cores = 4,
      qc = list(min.molecules = 0, max.molecules = Inf),
      variance = list(plot = FALSE, gam.k = 3),
      pca = list(nPcs = 2, use.odgenes = FALSE, threads = list(blas = 1))
    )
  )

  last.run <- p2$history$runs[[length(p2$history$runs)]]
  expect_equal(last.run$steps$variance$params$threads$total, 4)
  expect_equal(last.run$steps$variance$params$threads$native, 4)
  expect_equal(last.run$steps$pca$params$threads$total, 4)
  expect_equal(last.run$steps$pca$params$threads$blas, 1)
})

test_that("new embedding API uses threads role instead of n.sgd.cores", {
  p2 <- make_thread_p2(n.cores = 1)

  expect_equal(formals(p2$runEmbedding)$method, "UMAP")
  expect_null(formals(p2$runEmbedding)$distance)
  expect_false("runUMAP" %in% names(p2))

  expect_error(
    p2$runEmbedding(n.sgd.cores = 1),
    "threads = list\\(sgd = \\.\\.\\.\\)"
  )
  expect_equal(p2$getThreads(method = "runEmbedding", threads = list(total = 3, sgd = 1))$sgd, 1)
})

test_that("runLeiden reports that pagoda2 thread controls do not apply", {
  p2 <- make_thread_p2(n.cores = 1)

  expect_error(
    p2$runLeiden(n.cores = 2),
    "does not currently use pagoda2 thread controls"
  )
})

test_that("smatColVecCorr honors explicit thread counts", {
  set.seed(11)
  x <- Matrix::rsparsematrix(20, 8, density = 0.25)
  v <- rnorm(20)

  expect_equal(
    pagoda2:::smatColVecCorr(x, v, ncores = 1),
    pagoda2:::smatColVecCorr(x, v, ncores = 2),
    tolerance = 1e-12
  )
})
