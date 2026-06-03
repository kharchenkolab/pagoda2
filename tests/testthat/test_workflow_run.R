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
    verbose = FALSE
  )
}

workflow_args <- function() {
  list(
    profile = "pipeline",
    qc = list(min.molecules = 0, max.molecules = Inf),
    pca = list(nPcs = 3, use.odgenes = FALSE),
    graph = list(k = 3, nrand = 0),
    embedding = list(n_neighbors = 3, n_epochs = 20),
    leiden = list(resolution = 1),
    markers = list(append.specificity.metrics = FALSE)
  )
}

make_qc_p2 <- function() {
  values <- matrix(120, nrow = 5, ncol = 6)
  values[, 6] <- 2
  cm <- Matrix::Matrix(values, sparse = TRUE)
  rownames(cm) <- paste0("g", seq_len(nrow(cm)))
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE
  )
}

make_qc_composition_p2 <- function() {
  values <- matrix(
    c(
      10, 0, 5, 1,
      5, 10, 0, 1,
      5, 10, 5, 0,
      0, 0, 0, 8
    ),
    nrow = 4,
    ncol = 4,
    byrow = TRUE
  )
  cm <- Matrix::Matrix(values, sparse = TRUE)
  rownames(cm) <- c("MT-ND1", "RPS3", "GeneA", "GeneB")
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE
  )
}

make_gene_filter_p2 <- function() {
  values <- matrix(
    c(
      5, 5, 5, 5, 5,
      1, 0, 0, 0, 0,
      0, 2, 0, 0, 0,
      3, 3, 3, 0, 0
    ),
    nrow = 4,
    ncol = 5,
    byrow = TRUE
  )
  cm <- Matrix::Matrix(values, sparse = TRUE)
  rownames(cm) <- paste0("g", seq_len(nrow(cm)))
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE
  )
}

test_that("filterCells auto-runs QC and filters by qc_pass", {
  p2 <- make_qc_p2()

  expect_silent(out <- capture.output(qc <- p2$runQC(min.molecules = 100)))
  expect_equal(out, character(0))
  expect_s3_class(qc, "data.frame")
  expect_message(p2$runQC(overwrite = TRUE, min.molecules = 100, verbose = TRUE), "QC: 6 cells")

  p2 <- make_qc_p2()
  expect_silent(p2$filterCells(min.molecules = 100))

  expect_equal(nrow(p2$getRawCounts()), 5)
  expect_true("qc_pass" %in% colnames(p2$cellMeta))
  expect_equal(sum(!p2$cellMeta$qc_pass), 1)
  expect_s3_class(p2$plotQC(), "ggplot")
})

test_that("runQC calculates optional mitochondrial and ribosomal percentages", {
  p2 <- make_qc_composition_p2()

  qc <- p2$runQC(min.molecules = 0, max.molecules = Inf)

  expect_true(all(c("percent_mito", "percent_ribo") %in% colnames(qc)))
  expect_equal(qc["c1", "percent_mito"], 50)
  expect_equal(qc["c1", "percent_ribo"], 25)
  expect_equal(qc["c2", "percent_mito"], 0)
  expect_equal(qc["c2", "percent_ribo"], 50)
  expect_equal(p2$history$qc$composition$mitochondrial$column, "percent_mito")
  expect_s3_class(p2$plotQCViolin(), "ggplot")
  expect_s3_class(p2$plotQCViolin(thresholds = c(percent_mito = 25)), "ggplot")
})

test_that("runVariance stores diagnostics and plotVarianceQC renders them", {
  p2 <- make_workflow_p2()

  expect_error(p2$plotVarianceQC(), "runVariance")
  expect_s3_class(p2$plotVarianceQC(run.variance = TRUE, verbose = FALSE, gam.k = 1, min.gene.cells = 0), "ggplot")
  expect_s3_class(p2$plotVarianceQC(), "ggplot")
  expect_true(is.data.frame(p2$misc$varinfo))
  expect_true(is.list(p2$history$variance))
  expect_identical(p2$history$variance$method, "lm")
  expect_true(is.data.frame(p2$history$variance$fit_curve))
  expect_true(all(c("log10_magnitude", "log10_variance") %in% colnames(p2$history$variance$fit_curve)))
})

test_that("pagoda2 plot theme resolver supports object, option, and call overrides", {
  p2 <- make_qc_p2()
  p2$history$pca <- list(
    PCA = list(
      variance = data.frame(
        component = 1:3,
        percent_variance = c(20, 10, 5),
        cumulative_percent_variance = c(20, 30, 35)
      )
    )
  )

  p <- p2$plotPCAElbow()
  expect_s3_class(p, "ggplot")
  expect_equal(p$theme$legend.background$fill, ggplot2::alpha("white", 0.75))

  old.theme <- getOption("pagoda2.plot.theme")
  on.exit(options(pagoda2.plot.theme = old.theme), add = TRUE)
  options(pagoda2.plot.theme = ggplot2::theme(axis.title = ggplot2::element_text(face = "bold")))
  p <- p2$plotPCAElbow()
  expect_equal(p$theme$axis.title$face, "bold")

  p2$setPlotTheme(ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")))
  p <- p2$plotPCAElbow()
  expect_equal(p$theme$plot.title$face, "bold")

  p <- p2$plotPCAElbow(plot.theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "italic")))
  expect_equal(p$theme$plot.title$face, "italic")

  expect_identical(p2$setPlotTheme(), p2)
  expect_null(p2$defaults$plot.theme)
})

test_that("runQC composition metrics are optional and explicit misses warn", {
  p2 <- make_qc_p2()
  qc <- p2$runQC(min.molecules = 0, max.molecules = Inf)

  expect_false("percent_mito" %in% colnames(qc))
  expect_false("percent_ribo" %in% colnames(qc))
  expect_error(p2$plotQCViolin(), "No requested QC composition metrics")

  p2 <- make_qc_composition_p2()
  expect_warning(
    p2$runQC(overwrite = TRUE, mt.pattern = "^NO_MATCH", infer.qc.genes = FALSE),
    "No mitochondrial genes matched"
  )
})

test_that("run warns about QC failures without filtering by default", {
  p2 <- make_qc_p2()

  expect_warning(
    p2$run(steps = "qc", profile = "pipeline", qc = list(min.molecules = 100)),
    "did not pass QC"
  )

  expect_equal(nrow(p2$getRawCounts()), 6)
})

test_that("run is quiet by default and verbose on request", {
  p2 <- make_workflow_p2()

  expect_silent(out <- capture.output(
    p2$run(steps = "qc", qc = list(min.molecules = 0, max.molecules = Inf))
  ))
  expect_equal(out, character(0))

  p2 <- make_workflow_p2()
  expect_message(
    p2$run(steps = "qc", qc = list(min.molecules = 0, max.molecules = Inf), verbose = TRUE),
    "QC: 8 cells"
  )
})

test_that("run can explicitly filter after QC", {
  p2 <- make_qc_p2()

  expect_silent(
    p2$run(steps = "qc", profile = "pipeline", qc = list(min.molecules = 100, filter = TRUE))
  )

  expect_equal(nrow(p2$getRawCounts()), 5)
  last.run <- p2$history$runs[[length(p2$history$runs)]]
  expect_identical(last.run$steps$filter$status, "completed")
})

test_that("filterData sets analysis gene mask without dropping raw genes", {
  p2 <- make_gene_filter_p2()

  expect_silent(
    p2$filterData(cells = FALSE, genes = TRUE, min.cells.per.gene = 2)
  )

  expect_equal(ncol(p2$getRawCounts()), 4)
  expect_true(all(c("n_cells_detected", "n_molecules", "analysis_pass") %in% colnames(p2$geneMeta)))
  expect_identical(
    as.logical(p2$resolveGeneMeta("analysis_pass")$analysis_pass),
    c(TRUE, FALSE, FALSE, TRUE)
  )
  expect_equal(.subset2(p2$history$filterData[[1]]$genes, "analysis.pass"), 2L)
})

test_that("legacy constructor filter arguments are deferred to QC and filtering", {
  values <- matrix(
    c(
      120, 120, 120, 120, 2,
      1, 0, 0, 0, 0,
      1, 1, 0, 0, 0,
      1, 1, 1, 0, 0
    ),
    nrow = 4,
    ncol = 5,
    byrow = TRUE
  )
  cm <- Matrix::Matrix(values, sparse = TRUE)
  rownames(cm) <- paste0("g", seq_len(nrow(cm)))
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))

  p2 <- Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 3,
    min.transcripts.per.cell = 100,
    keep.genes = "g2"
  )

  expect_equal(unname(dim(p2$getRawCounts())), c(5, 4))
  expect_equal(p2$defaults$filter$min.molecules, 100)
  expect_equal(p2$defaults$filter$min.cells.per.gene, 3)
  expect_equal(p2$defaults$filter$keep.genes, "g2")

  p2$runQC()
  expect_identical(as.logical(p2$resolveCellMeta("qc_pass")$qc_pass), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  p2$runQC(overwrite = TRUE, min.molecules = 0)
  expect_true(all(as.logical(p2$resolveCellMeta("qc_pass")$qc_pass)))

  p2$filterData(cells = FALSE)
  expect_identical(
    as.logical(p2$resolveGeneMeta("analysis_pass")$analysis_pass),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(unname(dim(p2$getRawCounts())), c(5, 4))
})

test_that("filterCells refuses to invalidate downstream results unless forced", {
  p2 <- make_qc_p2()
  p2$reductions$PCA <- matrix(0, nrow = 6, ncol = 2, dimnames = list(rownames(p2$getRawCounts()), c("PC1", "PC2")))

  expect_error(p2$filterCells(min.molecules = 100), "invalidate existing")
  expect_silent(p2$filterCells(min.molecules = 100, force = TRUE))
  expect_equal(nrow(p2$getRawCounts()), 5)
  expect_equal(length(p2$reductions), 0)
})

test_that("run skip markers creates canonical workflow state without markers", {
  testthat::skip_if_not_installed("uwot")
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_workflow_p2()
  args <- c(list(skip = "markers"), workflow_args())

  expect_silent(do.call(p2$run, args))

  expect_true(all(c("n_molecules", "n_genes", "leiden") %in% colnames(p2$cellMeta)))
  expect_true("analysis_pass" %in% colnames(p2$geneMeta))
  expect_true("filter" %in% names(p2$history$runs[[length(p2$history$runs)]]$steps))
  expect_true("PCA" %in% names(p2$reductions))
  expect_true("PCA" %in% names(p2$history$pca))
  expect_equal(nrow(p2$history$pca$PCA$variance), 3)
  expect_s3_class(p2$plotPCAElbow(), "ggplot")
  expect_true("PCA" %in% names(p2$graphs))
  expect_true("UMAP" %in% names(p2$embeddings$PCA))
  expect_equal(p2$history$runs[[length(p2$history$runs)]]$steps$embedding$params$distance, "cosine")
  expect_identical(p2$getDefaultGrouping(), "leiden")
  expect_equal(length(p2$diffgenes), 0)
})

test_that("runEmbedding resolves method-specific distance defaults", {
  p2 <- make_workflow_p2()
  set.seed(10)
  p2$reductions$PCA <- matrix(
    rnorm(nrow(p2$getRawCounts()) * 3),
    nrow = nrow(p2$getRawCounts()),
    ncol = 3,
    dimnames = list(rownames(p2$getRawCounts()), paste0("PC", 1:3))
  )

  expect_equal(formals(p2$runEmbedding)$method, "UMAP")
  expect_null(formals(p2$runEmbedding)$distance)

  expect_equal(pagoda2:::.pagoda2_embedding_default_distance("UMAP"), "cosine")
  expect_equal(pagoda2:::.pagoda2_embedding_default_distance("largeVis"), "cosine")
  expect_equal(pagoda2:::.pagoda2_embedding_default_distance("tSNE"), "L2")

  expect_silent(
    p2$runEmbedding(
      reduction = "PCA",
      method = "tSNE",
      name = "tSNE_default",
      perplexity = 2,
      max_iter = 250,
      n.cores = 1,
      verbose = FALSE
    )
  )
  expect_equal(dim(p2$embeddings$PCA$tSNE_default), c(nrow(p2$getRawCounts()), 2))

  expect_warning(
    p2$runEmbedding(
      reduction = "PCA",
      method = "tSNE",
      name = "tSNE_cosine",
      distance = "cosine",
      perplexity = 2,
      max_iter = 250,
      n.cores = 1,
      verbose = FALSE
    ),
    "dense cell-cell distance"
  )

  expect_equal(dim(p2$embeddings$PCA$tSNE_cosine), c(nrow(p2$getRawCounts()), 2))
})

test_that("run auto dependencies can create marker result from fresh object", {
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_workflow_p2()
  args <- c(list(steps = "markers", dependencies = "auto"), workflow_args())
  args$embedding <- NULL

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

test_that("canonical analysis defaults match pipeline defaults", {
  p2 <- make_workflow_p2()

  pca.formals <- formals(p2$calculatePcaReduction)
  graph.formals <- formals(p2$makeKnnGraph)

  expect_equal(eval(pca.formals$nPcs), 50)
  expect_equal(eval(pca.formals$n.odgenes), 3000)
  expect_equal(eval(graph.formals$weight.type), "1m")
  expect_equal(eval(graph.formals$distance), "cosine")
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
  expect_identical(last.run$steps$embedding$status, "skipped")
  expect_identical(last.run$steps$leiden$status, "skipped")
})
