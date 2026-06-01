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

reference_analysis_matrix <- function(p2) {
  raw <- as.matrix(p2$getRawCounts())
  view <- p2$getMatrixView("analysis")
  x <- raw
  if (identical(view$model, "raw")) {
    if (isTRUE(view$log.scale)) {
      x <- log(x + 1)
    }
    return(Matrix::Matrix(x, sparse = TRUE))
  }
  stopifnot(identical(view$model, "plain"))
  if (!is.null(view$batchFactors)) {
    for (cell in rownames(x)) {
      x[cell, ] <- x[cell, ] / view$batchFactors[colnames(x), as.character(view$batch[cell])]
    }
  }
  depth <- view$depth[rownames(x)]
  if (!is.null(view$winsorCaps)) {
    pre.depth <- view$preWinsorDepth[rownames(x)]
    x <- x / as.numeric(pre.depth)
    x <- pmin(x, matrix(view$winsorCaps[colnames(x)], nrow = nrow(x), ncol = ncol(x), byrow = TRUE))
    x <- x * as.numeric(pre.depth)
    depth <- view$postWinsorDepth[rownames(x)]
  }
  x <- x / as.numeric(depth / view$depthScale)
  if (isTRUE(view$log.scale)) {
    x <- log(x + 1)
  }
  Matrix::Matrix(x, sparse = TRUE)
}

expect_view_matches_reference <- function(p2) {
  view <- p2$materializeView("analysis")
  ref <- reference_analysis_matrix(p2)
  expect_identical(rownames(view), rownames(ref))
  expect_identical(colnames(view), colnames(ref))
  expect_equal(as.matrix(view), as.matrix(ref), tolerance = 1e-10)
  expect_true(p2$validateMatrices())
}

expect_view_summaries_match_materialized <- function(p2) {
  view <- p2$materializeView("analysis")
  groups <- factor(c(cell1 = "A", cell2 = "B", cell3 = NA, cell4 = "A", cell5 = "B"))
  selected.cells <- c("cell1", "cell3", "cell5")
  row.sel <- rownames(view) %in% selected.cells

  expect_equal(
    as.data.frame(p2$viewColMeanVar(n.cores = 1)),
    as.data.frame(pagoda2:::colMeanVarS(view, NULL, 1)),
    tolerance = 1e-10
  )
  expect_equal(
    as.data.frame(p2$viewColMeanVar(cells = selected.cells, n.cores = 1)),
    as.data.frame(pagoda2:::colMeanVarS(view, row.sel, 1)),
    tolerance = 1e-10
  )

  p2$setGrouping("test_groups", groups, setDefault = TRUE, overwrite = TRUE)
  expect_equal(
    unname(p2$viewColSumByFac()),
    unname(pagoda2:::colSumByFac(view, as.integer(groups[rownames(view)]))),
    tolerance = 1e-10
  )
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

  expect_view_matches_reference(p2)
  expect_view_summaries_match_materialized(p2)
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

  expect_view_matches_reference(p2)
  expect_view_summaries_match_materialized(p2)
  block <- p2$getExpressionBlock(cells = c("cell1", "cell3"), genes = c("gene1", "gene4"))
  expect_equal(as.matrix(block), as.matrix(reference_analysis_matrix(p2)[c("cell1", "cell3"), c("gene1", "gene4")]), tolerance = 1e-10)
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

  expect_view_matches_reference(p2)
  expect_view_summaries_match_materialized(p2)
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

  expect_view_matches_reference(p2)
  expect_view_summaries_match_materialized(p2)
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

  expect_view_matches_reference(p2)
  expect_view_summaries_match_materialized(p2)
  expect_equal(as.matrix(p2$getRawCounts(orientation = "gene_by_cell")), as.matrix(make_view_matrix()))
})

test_that("legacy counts slot is not materialized by default", {
  p2 <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 0
  )

  expect_error(p2$counts, "no longer a stored Pagoda2 matrix")
  expect_error(p2$counts <- p2$getExpressionBlock(), "cannot be assigned")
})

test_that("view variance path accepts logical cell selections", {
  p2 <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 0
  )

  expect_no_error(
    p2$runVariance(
      cells = c(TRUE, FALSE, TRUE, FALSE, TRUE),
      plot = FALSE,
      verbose = FALSE,
      gam.k = 1,
      persist = FALSE,
      min.gene.cells = 0
    )
  )
})

test_that("workflow-facing methods use matrix views without a legacy counts slot", {
  groups <- factor(c(cell1 = "A", cell2 = "A", cell3 = "B", cell4 = "B", cell5 = "B"))

  p2.ref <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 0
  )
  p2.ref$runVariance(plot = FALSE, verbose = FALSE, gam.k = 1, min.gene.cells = 0)
  ref.expr <- p2.ref$getNormalizedExpressionMatrix(genes = c("gene1", "gene3"))
  ref.de <- p2.ref$getDifferentialGenes(
    groups = groups,
    z.threshold = 0,
    append.specificity.metrics = FALSE,
    .legacy.warn = FALSE
  )

  p2 <- Pagoda2$new(
    make_view_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = TRUE,
    trim = 0
  )
  p2$runVariance(plot = FALSE, verbose = FALSE, gam.k = 1, min.gene.cells = 0)

  p2$setGrouping("test_groups", groups, setDefault = TRUE)
  expect_equal(p2$getGrouping(), groups)
  expect_equal(
    as.matrix(p2$getNormalizedExpressionMatrix(genes = c("gene1", "gene3"))),
    as.matrix(ref.expr),
    tolerance = 1e-10
  )
  expect_no_error(
    suppressWarnings(
      p2$calculatePcaReduction(
        nPcs = 2,
        use.odgenes = FALSE,
        odgenes = c("gene1", "gene2", "gene3"),
        verbose = FALSE,
        .legacy.warn = FALSE
      )
    )
  )
  expect_equal(
    p2$getDifferentialGenes(
      groups = groups,
      z.threshold = 0,
      append.specificity.metrics = FALSE,
      .legacy.warn = FALSE
    ),
    ref.de,
    tolerance = 1e-10
  )
})
