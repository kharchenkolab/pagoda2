#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(Matrix)
  library(pagoda2)
})

data_dir <- Sys.getenv(
  "P21_GSM5746259_DIR",
  file.path("..", "tests", "data", "GSE192391", "GSM5746259_MGI0369_1_SLAB-145-0")
)
if (!dir.exists(data_dir)) {
  stop("Cannot find GSM5746259 data directory: ", data_dir)
}

n_cores <- as.integer(Sys.getenv("P21_BENCH_CORES", "8"))
n_cores <- max(1L, min(n_cores, parallel::detectCores(logical = FALSE)))

bench <- data.frame(
  step = character(),
  elapsed_sec = numeric(),
  object_mb = numeric(),
  stringsAsFactors = FALSE
)

timed <- function(label, expr, size.object = NULL) {
  gc()
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  target <- if (is.null(size.object)) value else size.object(value)
  size.mb <- as.numeric(utils::object.size(target)) / 1024^2
  bench[nrow(bench) + 1, ] <<- list(label, elapsed, size.mb)
  message(sprintf("%-28s %8.2f sec %8.1f MB", label, elapsed, size.mb))
  value
}

cm <- timed("read10xMatrix", {
  pagoda2::read10xMatrix(data_dir, version = "V3", transcript.id = "SYMBOL", verbose = FALSE)
})
if (anyDuplicated(rownames(cm)) > 0) {
  rownames(cm) <- make.unique(rownames(cm))
}

cm_filt <- timed("gene.vs.molecule.cell.filter", {
  pagoda2::gene.vs.molecule.cell.filter(
    cm,
    min.cell.size = 500,
    max.cell.size = 5e4,
    plot = FALSE,
    .legacy.warn = FALSE
  )
})

p2 <- timed("Pagoda2$new", {
  pagoda2::Pagoda2$new(
    cm_filt,
    n.cores = n_cores,
    log.scale = TRUE,
    trim = 10,
    min.cells.per.gene = 5,
    min.transcripts.per.cell = 500,
    verbose = FALSE
  )
}, size.object = function(x) list(rawCounts = x$rawCounts, matrixViews = x$matrixViews))

invisible(timed("p2 matrix storage", {
  p2$describeMatrices()
}, size.object = function(x) list(
  rawCounts = p2$rawCounts,
  matrixViews = p2$matrixViews
)))

invisible(timed("p2$run(skip='markers')", {
  p2$run(
    skip = "markers",
    profile = "pipeline",
    plots = "none",
    n.cores = n_cores,
    variance = list(plot = FALSE, gam.k = 10),
    pca = list(nPcs = 50, n.odgenes = 3000, maxit = 1000),
    graph = list(
      k = 30,
      center = TRUE,
      weight.type = "none",
      distance = "cosine"
    ),
    embedding = list(
      threads = list(sgd = 1),
      min_dist = 0.25,
      n_neighbors = 30,
      n_epochs = 500
    ),
    leiden = list(resolution = 1.0)
  )
}, size.object = function(x) list(
  reductions = p2$reductions,
  graphs = p2$graphs,
  embeddings = p2$embeddings,
  cellMeta = p2$cellMeta
)))

invisible(timed("p2$runMarkers", {
  p2$runMarkers(
    grouping = "leiden",
    name = "leiden",
    z.threshold = 3,
    upregulated.only = TRUE,
    append.specificity.metrics = TRUE,
    append.auc = TRUE,
    verbose = FALSE
  )
}, size.object = function(x) x))

invisible(timed("p2$plotMarkerDotPlot", {
  p2$plotMarkerDotPlot(
    markers = "leiden",
    n.genes.per.group = 5,
    z.threshold = 3,
    highest.only = TRUE
  )
}, size.object = function(x) ggplot2::ggplot_build(x)$data))

if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
  invisible(timed("p2$plotMarkerHeatmap", {
    p2$plotMarkerHeatmap(
      markers = "leiden",
      n.genes.per.group = 5,
      z.threshold = 3,
      highest.only = TRUE,
      return.details = TRUE
    )
  }, size.object = function(x) x$matrix))
}

print(bench)
