library(pagoda2)

make_workflow_p2 <- function() {
  set.seed(1)
  values <- matrix(rpois(30 * 8, lambda = 2), nrow = 30, ncol = 8)
  values[values < 1] <- 0
  cm <- Matrix::Matrix(values, sparse = TRUE)
  rownames(cm) <- paste0("g", seq_len(nrow(cm)))
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0
  )
}

workflow_args <- function() {
  list(
    profile = "pipeline",
    pca = list(nPcs = 3, use.odgenes = FALSE),
    graph = list(k = 3, nrand = 0),
    umap = list(n_neighbors = 3, n_epochs = 20),
    leiden = list(resolution = 1),
    markers = list(append.specificity.metrics = FALSE)
  )
}

test_that("run skip markers creates canonical workflow state without markers", {
  testthat::skip_if_not_installed("uwot")
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_workflow_p2()
  args <- c(list(skip = "markers"), workflow_args())

  expect_silent(do.call(p2$run, args))

  expect_true(all(c("n_molecules", "n_genes", "leiden") %in% colnames(p2$cellMeta)))
  expect_true("PCA" %in% names(p2$reductions))
  expect_true("PCA" %in% names(p2$graphs))
  expect_true("UMAP" %in% names(p2$embeddings$PCA))
  expect_identical(p2$getDefaultGrouping(), "leiden")
  expect_equal(length(p2$diffgenes), 0)
})

test_that("run auto dependencies can create marker result from fresh object", {
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_workflow_p2()
  args <- c(list(steps = "markers", dependencies = "auto"), workflow_args())
  args$umap <- NULL

  expect_silent(do.call(p2$run, args))

  expect_true("leiden" %in% colnames(p2$cellMeta))
  expect_true("leiden" %in% names(p2$diffgenes$counts))
  expect_identical(attr(p2$diffgenes$counts$leiden, "pagoda2.marker")$grouping, "leiden")
})

test_that("run validates step selection and dependency policy", {
  p2 <- make_workflow_p2()

  expect_error(p2$run(steps = "pca", skip = "markers"), "only one")
  expect_error(p2$run(steps = "unknown"), "Unknown workflow")
  expect_error(p2$run(steps = "markers", dependencies = "error", profile = "pipeline"), "defaultGrouping")
})

test_that("run skips existing results when overwrite is false", {
  testthat::skip_if_not_installed("uwot")
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_workflow_p2()
  args <- c(list(skip = "markers"), workflow_args())
  expect_silent(do.call(p2$run, args))
  expect_silent(do.call(p2$run, args))

  last.run <- p2$history$runs[[length(p2$history$runs)]]
  expect_identical(last.run$steps$pca$status, "skipped")
  expect_identical(last.run$steps$graph$status, "skipped")
  expect_identical(last.run$steps$umap$status, "skipped")
  expect_identical(last.run$steps$leiden$status, "skipped")
})
