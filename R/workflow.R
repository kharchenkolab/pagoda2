## Workflow implementation for Pagoda2


.pagoda2_deprecated_call <- function(old, new) {
  warning(
    "Legacy method `", old, "` is deprecated and will be removed in the next pagoda2 version. ",
    "Use `", new, "` instead.",
    call. = FALSE
  )
}

.pagoda2_workflow_steps <- c("qc", "filter", "variance", "pca", "graph", "embedding", "leiden", "markers")

.pagoda2_workflow_dependencies <- list(
  qc = character(),
  filter = "qc",
  variance = "filter",
  pca = "variance",
  graph = "pca",
  embedding = "pca",
  leiden = "graph",
  markers = "leiden"
)

.pagoda2_expand_workflow_steps <- function(steps) {
  out <- character()
  visit <- function(step) {
    if (!step %in% .pagoda2_workflow_steps) {
      stop("Unknown workflow step `", step, "`")
    }
    for (dep in .pagoda2_workflow_dependencies[[step]]) {
      visit(dep)
    }
    out <<- unique(c(out, step))
  }
  for (step in steps) {
    visit(step)
  }
  out
}

.pagoda2_step_args <- function(args, defaults = list()) {
  if (is.null(args)) {
    args <- list()
  }
  if (!is.list(args)) {
    stop("Step arguments must be supplied as a named list")
  }
  utils::modifyList(defaults, args)
}

.pagoda2_filter_default <- function(p2, name, value, explicit = FALSE) {
  if (isTRUE(explicit)) {
    return(value)
  }
  defaults <- p2$defaults$filter
  if (is.null(defaults) || !name %in% names(defaults)) {
    return(value)
  }
  defaults[[name]]
}


.pagoda2_r6_run <- function(p2, steps = NULL, skip = NULL, dependencies = c("auto", "error"), overwrite = FALSE,
                            profile = c("interactive", "pipeline", "report"), plots = NULL,
                            verbose = FALSE, n.cores = NULL, threads = NULL, qc = list(), filter = list(), variance = list(), pca = list(), graph = list(), embedding = list(),
                            leiden = list(), markers = list()) {
  dependencies <- match.arg(dependencies)
  profile <- match.arg(profile)
  run.threads <- NULL
  if (!is.null(n.cores) || !is.null(threads)) {
    run.threads <- .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = "run", validate = FALSE)
  }
  add_run_threads <- function(args, method) {
    if (is.null(run.threads) || !is.null(args$n.cores)) {
      return(args)
    }
    run.step.threads <- .pagoda2_thread_subset(run.threads, method)
    if (!is.null(args$threads)) {
      args$threads <- utils::modifyList(run.step.threads, .pagoda2_normalize_threads(args$threads))
      return(args)
    }
    args$threads <- run.step.threads
    args
  }
  if (!is.null(steps) && !is.null(skip)) {
    stop("Supply only one of `steps` or `skip`")
  }
  if (!is.null(steps)) {
    unknown <- setdiff(steps, .pagoda2_workflow_steps)
    if (length(unknown) > 0) {
      stop("Unknown workflow step(s): ", paste(unknown, collapse = ", "))
    }
    requested.steps <- steps
  } else {
    requested.steps <- .pagoda2_workflow_steps
  }
  if (!is.null(skip)) {
    unknown <- setdiff(skip, .pagoda2_workflow_steps)
    if (length(unknown) > 0) {
      stop("Unknown workflow step(s): ", paste(unknown, collapse = ", "))
    }
  }
  resolved.steps <- if (dependencies == "auto") {
    .pagoda2_expand_workflow_steps(requested.steps)
  } else {
    requested.steps
  }
  if (!is.null(skip)) {
    resolved.steps <- setdiff(resolved.steps, skip)
  }
  resolved.steps <- .pagoda2_workflow_steps[.pagoda2_workflow_steps %in% resolved.steps]

  if (is.null(plots)) {
    plots <- switch(profile,
      interactive = "show",
      pipeline = "none",
      report = "collect"
    )
  }
  if (!plots %in% c("show", "none", "collect")) {
    stop("`plots` must be one of show, none, or collect")
  }
  verbose.default <- isTRUE(verbose)

  if (is.null(p2$history$runs)) {
    p2$history$runs <- list()
  }
  run.id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", length(p2$history$runs) + 1)
  step.records <- list()
  record_step <- function(step, status, params = list(), elapsed = NA_real_, message = NULL) {
    step.records[[step]] <<- list(
      status = status,
      params = params,
      elapsed = elapsed,
      message = message
    )
  }
  run_step <- function(step, params, expr) {
    t0 <- proc.time()[["elapsed"]]
    value <- force(expr)
    elapsed <- proc.time()[["elapsed"]] - t0
    record_step(step, "completed", params = params, elapsed = elapsed)
    value
  }
  skip_step <- function(step, params, reason) {
    if (verbose.default) {
      message("Skipping ", step, ": ", reason)
    }
    record_step(step, "skipped", params = params, message = reason)
  }

  if ("qc" %in% resolved.steps) {
    args <- .pagoda2_step_args(qc, list(overwrite = overwrite, verbose = verbose.default))
    filter.after.qc <- isTRUE(args$filter)
    args$filter <- NULL
    expected.qc.cols <- if (!is.null(args$method) && args$method == "metrics") {
      c("n_molecules", "n_genes")
    } else {
      c("n_molecules", "n_genes", "qc_pass")
    }
    if (!overwrite && all(expected.qc.cols %in% colnames(p2$cellMeta))) {
      skip_step("qc", args, "QC metrics already exist")
    } else {
      run_step("qc", args, do.call(p2$runQC, args))
    }
    if ("qc_pass" %in% colnames(p2$cellMeta)) {
      qc.meta <- p2$resolveCellMeta("qc_pass")
      n.fail <- sum(!as.logical(qc.meta$qc_pass), na.rm = TRUE)
      if (filter.after.qc) {
        run_step("filter", list(pass.column = "qc_pass", verbose = verbose.default), p2$filterCells(pass.column = "qc_pass", verbose = verbose.default))
      } else if (n.fail > 0L && !"filter" %in% resolved.steps) {
        warning(
          n.fail, " cell(s) did not pass QC. ",
          "Call p2$plotQC() to inspect them and p2$filterData() before analysis.",
          call. = FALSE
        )
      }
    }
  }

  if ("filter" %in% resolved.steps) {
    args <- .pagoda2_step_args(filter, list(overwrite = overwrite, verbose = verbose.default))
    explicit.filter.args <- setdiff(names(args), c("overwrite", "verbose"))
    if (!overwrite && length(explicit.filter.args) == 0L && .pagoda2_filter_data_complete(p2)) {
      skip_step("filter", args, "current cells pass QC and analysis gene mask already exists")
    } else {
      run_step("filter", args, do.call(p2$filterData, args))
    }
  }

  if ("variance" %in% resolved.steps) {
    args <- .pagoda2_step_args(variance, list(plot = FALSE, verbose = verbose.default))
    args <- add_run_threads(args, "variance")
    if (!overwrite && !is.null(p2$misc[["varinfo"]])) {
      skip_step("variance", args, "variance model already exists")
    } else {
      run_step("variance", args, do.call(p2$runVariance, args))
    }
  }

  if ("pca" %in% resolved.steps) {
    pca.name <- if (!is.null(pca$name)) pca$name else p2$defaults$reduction
    args <- .pagoda2_step_args(pca, list(name = pca.name, verbose = verbose.default))
    args <- add_run_threads(args, "pca")
    if (!overwrite && !is.null(p2$reductions[[args$name]])) {
      skip_step("pca", args, paste0("reduction `", args$name, "` already exists"))
    } else {
      run_step("pca", args, do.call(p2$runPCA, args))
    }
  }

  if ("graph" %in% resolved.steps) {
    graph.reduction <- if (!is.null(graph$reduction)) graph$reduction else p2$defaults$reduction
    args <- .pagoda2_step_args(graph, list(reduction = graph.reduction, verbose = verbose.default))
    args <- add_run_threads(args, "graph")
    if (!overwrite && !is.null(p2$graphs[[args$reduction]])) {
      skip_step("graph", args, paste0("graph `", args$reduction, "` already exists"))
    } else {
      run_step("graph", args, do.call(p2$runGraph, args))
    }
  }

  if ("embedding" %in% resolved.steps) {
    embedding.reduction <- if (!is.null(embedding$reduction)) embedding$reduction else p2$defaults$reduction
    embedding.method <- if (!is.null(embedding$method)) embedding$method else p2$defaults$embedding
    embedding.name <- if (!is.null(embedding$name)) embedding$name else embedding.method
    args <- .pagoda2_step_args(embedding, list(reduction = embedding.reduction, method = embedding.method, name = embedding.name, verbose = verbose.default))
    if (is.null(args$distance)) {
      args$distance <- .pagoda2_embedding_default_distance(args$method)
    }
    args <- add_run_threads(args, "embedding")
    if (!overwrite && !is.null(p2$embeddings[[args$reduction]]) && !is.null(p2$embeddings[[args$reduction]][[args$name]])) {
      skip_step("embedding", args, paste0("embedding `", args$reduction, "/", args$name, "` already exists"))
    } else {
      run_step("embedding", args, do.call(p2$runEmbedding, args))
    }
  }

  if ("leiden" %in% resolved.steps) {
    leiden.name <- if (!is.null(leiden$name)) leiden$name else "leiden"
    args <- .pagoda2_step_args(leiden, list(name = leiden.name, setDefault = TRUE, overwrite = overwrite))
    if (!overwrite && leiden.name %in% colnames(p2$cellMeta)) {
      skip_step("leiden", args, paste0("grouping `", leiden.name, "` already exists"))
    } else {
      run_step("leiden", args, do.call(p2$runLeiden, args))
    }
  }

  if ("markers" %in% resolved.steps) {
    marker.name <- if (!is.null(markers$name)) markers$name else p2$defaultGrouping
    if (is.null(marker.name)) {
      stop("Cannot run markers without a defaultGrouping or markers$name")
    }
    args <- .pagoda2_step_args(
      markers,
      list(
        name = marker.name,
        verbose = verbose.default,
        z.threshold = 3,
        upregulated.only = TRUE,
        append.specificity.metrics = TRUE,
        append.auc = TRUE
      )
    )
    args <- add_run_threads(args, "markers")
    marker.type <- if (!is.null(args$type)) args$type else "counts"
    if (!overwrite && !is.null(p2$diffgenes[[marker.type]]) && !is.null(p2$diffgenes[[marker.type]][[args$name]])) {
      skip_step("markers", args, paste0("marker result `", args$name, "` already exists"))
    } else {
      run_step("markers", args, do.call(p2$runMarkers, args))
    }
  }

  p2$history$runs[[run.id]] <- list(
    started = Sys.time(),
    requested.steps = requested.steps,
    resolved.steps = resolved.steps,
    skip = skip,
    dependencies = dependencies,
    overwrite = overwrite,
    profile = profile,
    plots = plots,
    steps = step.records
  )
  invisible(p2)
}

.pagoda2_r6_run_variance <- function(p2, n.cores = NULL, threads = NULL, ...) {
  tp <- .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = "variance")
  p2$adjustVariance(..., n.cores = tp$native, .legacy.warn = FALSE)
}

.pagoda2_r6_run_graph <- function(p2, reduction = NULL, n.cores = NULL, threads = NULL, ...) {
  if (is.null(reduction)) {
    reduction <- p2$defaults$reduction
  }
  tp <- .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = "graph")
  p2$makeKnnGraph(type = reduction, ..., n.cores = tp$native, .legacy.warn = FALSE)
}

.pagoda2_r6_run_leiden <- function(p2, reduction = NULL, graph = NULL, name = "leiden", setDefault = TRUE, overwrite = FALSE, method = NULL, n.cores = NULL, threads = NULL, ...) {
  if (!is.null(n.cores) || !is.null(threads)) {
    stop("runLeiden() does not currently use pagoda2 thread controls; pass backend-specific method arguments through `...` only if the backend supports them")
  }
  if (is.null(graph)) {
    graph <- reduction
  }
  if (is.null(graph)) {
    graph <- p2$defaults$graph
  }
  if (is.null(graph)) {
    graph <- "PCA"
  }
  if (!overwrite && name %in% colnames(p2$cellMeta)) {
    stop("Grouping `", name, "` already exists; use overwrite=TRUE")
  }
  if (!overwrite && !is.null(p2$clusters[[graph]][[name]])) {
    stop("Clustering `", name, "` already exists for graph `", graph, "`; use overwrite=TRUE")
  }
  if (is.null(method)) {
    if (!requireNamespace("leidenAlg", quietly = TRUE)) {
      stop("Package `leidenAlg` is required for runLeiden()")
    }
    method <- leidenAlg::leiden.community
    method.name <- "leidenAlg::leiden.community"
  } else {
    method.name <- deparse(substitute(method))
  }
  cls <- p2$getKnnClusters(type = graph, method = method, name = name, persist = TRUE, .legacy.warn = FALSE, ...)
  groups <- p2$clusters[[graph]][[name]]
  p2$setGrouping(name, groups, source = list(method = "runLeiden", graph = graph), setDefault = setDefault, overwrite = TRUE)
  community <- NULL
  if (!is.null(p2$misc[["community"]]) && !is.null(p2$misc[["community"]][[graph]])) {
    community <- p2$misc[["community"]][[graph]][[name]]
  }
  p2$clusterings[[name]] <- list(
    grouping = name,
    reduction = reduction,
    graph = graph,
    method = method.name,
    community = community,
    created = Sys.time()
  )
  invisible(cls)
}

.pagoda2_r6_run_pca <- function(p2, n.cores = NULL, threads = NULL, ...) {
  tp <- .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = "pca")
  .pagoda2_with_blas_threads(tp$blas, p2$calculatePcaReduction(..., .legacy.warn = FALSE))
}

## Generic reduction step: method defaults to the facet's defaultReduction (PCA for RNA, LSI for ATAC).
## The algorithm is always a `method=`, mirroring runGraph/runClustering/runEmbedding (no runPCA/runLSI API).
.pagoda2_r6_run_reduction <- function(p2, facet = NULL, method = NULL, name = NULL, ...) {
  f <- p2$resolveFacet(facet)
  if (is.null(method)) {
    method <- f$defaultReduction
  }
  m <- tolower(method)
  if (is.null(name)) {
    name <- toupper(m)
  }
  if (identical(m, "pca")) {
    return(p2$runPCA(facet = facet, name = name, ...))
  }
  if (identical(m, "lsi")) {
    stop("LSI reduction is not implemented yet (Phase 2b); use method='pca' for now", call. = FALSE)
  }
  stop("unknown reduction method '", method, "'", call. = FALSE)
}

.pagoda2_embedding_default_distance <- function(method) {
  if (identical(method, "tSNE")) {
    return("L2")
  }
  "cosine"
}

.pagoda2_r6_run_embedding <- function(p2, reduction = NULL, method = "UMAP", name = NULL, distance = NULL, n.cores = NULL, threads = NULL, ...) {
  if (is.null(reduction)) {
    reduction <- p2$defaults$reduction
  }
  if (is.null(method)) {
    method <- p2$defaults$embedding
  }
  if (is.null(name)) {
    name <- method
  }
  if (is.null(distance)) {
    distance <- .pagoda2_embedding_default_distance(method)
  }
  args <- list(...)
  if ("n.sgd.cores" %in% names(args)) {
    stop("Use `threads = list(sgd = ...)` instead of `n.sgd.cores` in the pagoda2.1 embedding API")
  }
  is.umap <- method %in% c("UMAP", "UMAP_graph")
  tp <- .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = "embedding")
  args$n.cores <- tp$native
  args$distance <- distance
  if (is.umap) {
    args$n.sgd.cores <- tp$sgd
  }
  do.call(p2$getEmbedding, c(list(type = reduction, embeddingType = method, name = name, .legacy.warn = FALSE), args))
}
