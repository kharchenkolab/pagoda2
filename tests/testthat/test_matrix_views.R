library(pagoda2)

make_view_matrix <- function() {
  cm <- Matrix::Matrix(
    c(
      10, 0, 2, 5, 100,
      0, 12, 3, 6, 80,
      4, 0, 8, 7, 60,
      1, 2, 0, 9, 40
    ),
    nrow = 4,
    ncol = 5,
    sparse = TRUE,
    dimnames = list(paste0("gene", 1:4), paste0("cell", 1:5))
  )
  as(cm, "dgCMatrix")
}

expect_view_matches_counts <- function(p2) {
  view <- p2$materializeView("analysis")
  expect_identical(rownames(view), rownames(p2$counts))
  expect_identical(colnames(view), colnames(p2$counts))
  expect_equal(as.matrix(view), as.matrix(p2$counts), tolerance = 1e-10)
  expect_true(p2$validateMatrices())
}

test_that("analysis view materializes plain normalization", {
  p2 <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = FALSE,
    trim = 0
  )

  expect_view_matches_counts(p2)
  expect_identical(p2$getMatrixView("analysis")$model, "plain")
})

test_that("analysis view materializes log-scaled normalization", {
  p2 <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 0
  )

  expect_view_matches_counts(p2)
  block <- p2$getExpressionBlock(cells = c("cell1", "cell3"), genes = c("gene1", "gene4"))
  expect_equal(as.matrix(block), as.matrix(p2$counts[c("cell1", "cell3"), c("gene1", "gene4")]), tolerance = 1e-10)
  block.t <- p2$getExpressionBlock(cells = c("cell1", "cell3"), genes = c("gene1", "gene4"), orientation = "gene_by_cell")
  expect_equal(as.matrix(block.t), as.matrix(Matrix::t(block)), tolerance = 1e-10)
})

test_that("analysis view materializes batch-adjusted normalization", {
  batch <- factor(c(cell1 = "a", cell2 = "a", cell3 = "b", cell4 = "b", cell5 = "b"))
  p2 <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    batch = batch,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 0
  )

  expect_view_matches_counts(p2)
  expect_false(is.null(p2$getMatrixView("analysis")$batchFactors))
})

test_that("analysis view materializes winsorized normalization", {
  p2 <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 1
  )

  expect_view_matches_counts(p2)
  expect_false(is.null(p2$getMatrixView("analysis")$winsorCaps))
})

test_that("analysis view materializes raw model", {
  p2 <- Pagoda2$new(
    make_view_matrix(),
    modelType = "raw",
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 0
  )

  expect_view_matches_counts(p2)
  expect_equal(as.matrix(p2$getRawCounts(orientation = "gene_by_cell")), as.matrix(make_view_matrix()))
})
