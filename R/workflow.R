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
      run_step("pca", args, do.call(p2$runReduction, args))
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
    marker.key <- p2$resolveFacet(args$facet)$name
    if (!overwrite && !is.null(p2$diffgenes[[marker.key]]) && !is.null(p2$diffgenes[[marker.key]][[args$name]])) {
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

.pagoda2_r6_run_graph <- function(p2, reduction = NULL, method = NULL, facets = NULL, n.cores = NULL, threads = NULL, ...) {
  m <- if (is.null(method)) "" else tolower(method)
  ## facets whose default reduction is already computed (candidates for auto-integration)
  ready <- Filter(function(fn) {
    f <- p2$resolveFacet(fn)
    !is.null(p2$reductions[[.pagoda2_reduction_key(p2, fn, f$defaultReduction)]])
  }, p2$listFacets())
  ## Integrate facets by default (§0.2.6): explicit method="wnn", OR — when nothing single-facet was
  ## requested — auto-WNN whenever >= 2 facets are reduction-ready. A single-facet object (or a run()
  ## pipeline that has only computed the default facet's reduction) falls through to a plain kNN, so
  ## single-RNA behavior is unchanged.
  use.wnn <- identical(m, "wnn") || (is.null(method) && is.null(reduction) && length(ready) > 1L)
  if (use.wnn) {
    if (is.null(facets)) {
      facets <- if (identical(m, "wnn")) p2$listFacets() else ready
    }
    return(.pagoda2_r6_run_wnn(p2, facets = facets, n.cores = n.cores, threads = threads, ...))
  }
  if (is.null(reduction)) {
    reduction <- p2$defaults$reduction
  }
  tp <- .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = "graph")
  p2$makeKnnGraph(type = reduction, ..., n.cores = tp$native, .legacy.warn = FALSE)
}

## Approximate kNN as a sparse n x n distance matrix (self on the diagonal, like N2R::Knn's raw output;
## callers zero/drop the diagonal). Backend preference: RcppHNSW (hnswlib, *threaded* + high-recall) when
## available, else N2R. distance: "angular"/"cosine" (cosine) or "L2"/"euclidean". M/ef are the hnswlib
## graph degree / search width; ef defaults generous for recall (N2R 1.0.5 is non-threading and stuck at
## ~0.36 recall regardless of its ef — see benchmark/RESULTS). Note: multi-threaded HNSW build is not
## bit-reproducible across thread counts (inherent to parallel hnswlib, as with N2R); kNN is approximate.
.pagoda2_knn_sparse <- function(X, k, n.cores = 1L, distance = "angular",
                                M = 16L, ef = NULL, ef.construction = 200L, verbose = FALSE) {
  X <- as.matrix(X)
  n <- nrow(X)
  k <- min(k, n)
  ann <- if (distance %in% c("angular", "cosine")) "angular" else "L2"
  if (requireNamespace("RcppHNSW", quietly = TRUE)) {
    if (is.null(ef)) ef <- max(50L, 2L * as.integer(k))
    hd <- if (identical(ann, "angular")) "cosine" else "l2"
    r <- RcppHNSW::hnsw_knn(X, k = k, distance = hd, n_threads = max(1L, as.integer(n.cores)),
      M = as.integer(M), ef = as.integer(ef), ef_construction = as.integer(ef.construction), verbose = verbose)
    return(Matrix::sparseMatrix(i = rep(seq_len(n), times = k), j = as.vector(r$idx),
      x = as.vector(r$dist), dims = c(n, n)))
  }
  if (requireNamespace("N2R", quietly = TRUE)) {
    ## N2R returns the transpose: triplets are (neighbor, query), so a query's neighbors are a COLUMN.
    ## Transpose to the (query, neighbor) convention this function guarantees (row i = i's neighbors),
    ## matching the RcppHNSW path. (Harmless for symmetrized graphs, but WNN reads per-row.)
    return(methods::as(Matrix::t(methods::as(N2R::Knn(X, k, nThreads = max(1L, as.integer(n.cores)), verbose = verbose, indexType = ann), "CsparseMatrix")), "CsparseMatrix"))
  }
  stop("no approximate-kNN backend available (install RcppHNSW or N2R)", call. = FALSE)
}

## Per-facet kNN as a sparse distance matrix M (row i = i's k nearest, self excluded). Large n: the
## threaded ANN backend (.pagoda2_knn_sparse, RcppHNSW/N2R). Small n (<= 200): an exact base-R kNN — on
## tiny inputs the approximate index's neighbor noise distorts the WNN weights, and exact is cheap here.
## One representation drives both the vectorized weight computation and the weighted graph.
.pagoda2_facet_knn_dist <- function(X, k, n.cores = 1L, distance = c("L2", "angular")) {
  distance <- match.arg(distance)
  X <- as.matrix(X)
  n <- nrow(X)
  k <- min(k, n - 1L)
  if (n > 200L) {
    M <- .pagoda2_knn_sparse(X, k + 1L, n.cores = n.cores, distance = distance) # +1: self is dropped below
    Matrix::diag(M) <- 0
    return(Matrix::drop0(M))
  }
  ## exact kNN (no approximate-index noise on tiny inputs)
  D <- if (identical(distance, "angular")) {
    nr <- sqrt(rowSums(X^2)); nr[nr == 0] <- 1
    1 - tcrossprod(X / nr)
  } else {
    as.matrix(stats::dist(X))
  }
  diag(D) <- Inf
  ord <- t(apply(D, 1L, function(d) order(d)[seq_len(k)]))
  i <- rep(seq_len(n), times = k)
  j <- as.vector(ord)
  Matrix::sparseMatrix(i = i, j = j, x = D[cbind(i, j)], dims = c(n, n))
}

## Per-row (per-cell) min / mean of a sparse distance matrix's nonzeros (the 1st-NN distance and a local
## bandwidth), computed in one O(nnz) pass.
.pagoda2_knn_row_stats <- function(M) {
  Tt <- methods::as(M, "TsparseMatrix")
  ri <- Tt@i + 1L
  d0 <- rep(Inf, nrow(M))
  acc <- numeric(nrow(M))
  cnt <- integer(nrow(M))
  for (t in seq_along(ri)) {
    r <- ri[t]
    v <- Tt@x[t]
    if (v < d0[r]) d0[r] <- v
    acc[r] <- acc[r] + v
    cnt[r] <- cnt[r] + 1L
  }
  sigma <- ifelse(cnt > 0, acc / cnt, 1)
  d0[!is.finite(d0)] <- 0
  list(d0 = d0, sigma = sigma)
}

## WNN (weighted nearest neighbors), faithful to Hao et al., Integrated analysis of multimodal single-cell
## data, Cell 2021;184(13):3573-3587 (doi:10.1016/j.cell.2021.04.048): per-cell modality weights from within-
## vs cross-modality predictive affinity under a per-cell bandwidth kernel exp(-(d - d_nn1)/(sigma - d_nn1)).
## A modality whose OWN neighbors predict the cell's state much better than the other modalities' neighbors
## do is up-weighted for that cell; the bandwidth normalization makes this dimensionality-robust (a noise
## modality is down-weighted even with a different reduction dimension). Builds (A) per-cell weights, (B) a
## weighted SNN graph = sum_m w_m(i)*K_m(i,j), and a per-cell-weighted joint reduction. Operates on the
## cells common to the facets (the default, intersection).
.pagoda2_r6_run_wnn <- function(p2, facets, reductions = NULL, k = 20, name = "WNN",
                                n.cores = NULL, threads = NULL, verbose = TRUE, ...) {
  if (length(facets) < 2L) {
    stop("WNN needs >= 2 facets", call. = FALSE)
  }
  if (!requireNamespace("RcppHNSW", quietly = TRUE) && !requireNamespace("N2R", quietly = TRUE)) {
    stop("WNN requires a kNN backend (RcppHNSW or N2R)", call. = FALSE)
  }
  .pagoda2_validate_joint_name(p2, name)
  tp <- .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = "graph")
  red <- list()
  input.axes <- character()
  for (i in seq_along(facets)) {
    f <- p2$resolveFacet(facets[[i]])
    rname <- if (!is.null(reductions)) reductions[[i]] else f$defaultReduction
    key <- .pagoda2_reduction_key(p2, f$name, rname)
    sc <- p2$reductions[[key]]
    if (is.null(sc)) {
      stop("reduction `", key, "` not found; run runReduction(facet='", f$name, "') before WNN", call. = FALSE)
    }
    red[[as.character(facets[[i]])]] <- sc
    input.axes <- c(input.axes, .pagoda2_facet_feature_axis(f))
  }
  common <- Reduce(intersect, lapply(red, rownames)) # default: intersection (common cells)
  if (length(common) < 3L) {
    stop("WNN: facets share fewer than 3 cells", call. = FALSE)
  }
  red <- lapply(red, function(m) as.matrix(m[common, , drop = FALSE]))
  nmod <- length(red)
  ncell <- length(common)
  k <- min(k, ncell - 1L)
  eps <- 1e-9
  kdist <- lapply(red, function(X) .pagoda2_facet_knn_dist(X, k, n.cores = tp$native))
  stats <- lapply(kdist, .pagoda2_knn_row_stats)
  ## row-normalized incidence A_m (1/|nbrs| per neighbor) -> vectorized predicted state A_m %*% X
  incid <- lapply(kdist, function(M) {
    A <- M
    A@x <- rep(1, length(A@x))
    rs <- Matrix::rowSums(A)
    rs[rs == 0] <- 1
    Matrix::Diagonal(x = 1 / rs) %*% A
  })
  rownorm <- function(D) sqrt(rowSums(D * D))
  aff.of <- function(d, m) exp(-pmax(d - stats[[m]]$d0, 0) / pmax(stats[[m]]$sigma - stats[[m]]$d0, eps))
  ratio <- matrix(0, ncell, nmod)
  for (m in seq_len(nmod)) {
    X <- red[[m]]
    d.in <- rownorm(X - as.matrix(incid[[m]] %*% X)) # within-modality prediction error
    f.in <- aff.of(d.in, m)
    others <- setdiff(seq_len(nmod), m)
    f.cross <- rep(0, ncell)
    for (o in others) {
      d.cr <- rownorm(X - as.matrix(incid[[o]] %*% X)) # predict modality m from modality o's neighbors
      f.cross <- pmax(f.cross, aff.of(d.cr, m)) # strongest competing predictor
    }
    ratio[, m] <- f.in / pmax(f.cross, eps)
  }
  rs <- rowSums(ratio)
  rs[rs == 0] <- 1
  W <- ratio / rs # per-cell modality weights (normalized affinity ratios), sum to 1; overflow-free
  colnames(W) <- names(red)
  rownames(W) <- common
  for (m in seq_len(nmod)) { # store as shared cell measures, aligned to the canonical axis
    v <- stats::setNames(rep(NA_real_, length(p2$cells)), p2$cells)
    v[common] <- W[, m]
    p2$cellMeta[[paste0("wnn_weight_", names(red)[m])]] <- v[rownames(p2$cellMeta)]
  }
  ## (B) weighted SNN graph: combined affinity sum_m diag(w_m) %*% K_m, symmetrized (C + t(C)).
  C <- NULL
  for (m in seq_len(nmod)) {
    K <- kdist[[m]] # CSC distances; K@i = row index of each nonzero
    ri <- K@i + 1L
    d0 <- stats[[m]]$d0
    sg <- stats[[m]]$sigma
    K@x <- exp(-pmax(K@x - d0[ri], 0) / pmax(sg[ri] - d0[ri], eps)) # per-entry bandwidth kernel
    Km <- Matrix::Diagonal(x = W[, m]) %*% K # scale row i by cell i's weight for modality m
    C <- if (is.null(C)) Km else C + Km
  }
  C <- Matrix::drop0(methods::as(C + Matrix::t(C), "CsparseMatrix")) # symmetric, undirected weighted graph
  dimnames(C) <- list(common, common)
  g <- igraph::graph_from_adjacency_matrix(C, mode = "undirected", weighted = TRUE, diag = FALSE)
  igraph::V(g)$name <- common
  attr(g, "facets") <- as.character(facets)
  attr(g, "input_axes") <- input.axes
  attr(g, "method") <- "wnn"
  p2$graphs[[name]] <- g
  if (is.null(p2$misc[["edgeMat"]])) p2$misc[["edgeMat"]] <- list()
  p2$misc[["edgeMat"]][[name]] <- C
  ## per-cell-weighted joint reduction (usable for embedding); same provenance.
  scaled <- lapply(red, function(mm) {
    s <- sqrt(sum(mm^2) / nrow(mm))
    if (s > 0) mm / s else mm
  })
  Xj <- do.call(cbind, lapply(seq_len(nmod), function(m) scaled[[m]] * W[, m]))
  rownames(Xj) <- common
  colnames(Xj) <- paste0(name, seq_len(ncol(Xj)))
  attr(Xj, "facets") <- as.character(facets)
  attr(Xj, "input_axes") <- input.axes
  attr(Xj, "method") <- "wnn"
  ## Name-scoped per-cell modality weights (cells x facets): travel WITH the joint reduction so multiple
  ## WNN joints over different facet subsets coexist without clobbering each other (the cellMeta
  ## wnn_weight_<facet> columns above hold only the most-recent run). This is the accessor surface
  ## getModalityWeights() / conos Path-B fusion reads (§3.3 ask #1).
  attr(Xj, "weights") <- W
  .pagoda2_warn_joint_clobber(p2, name, facets, "wnn")
  p2$reductions[[name]] <- Xj
  if (verbose) {
    message("WNN over ", paste(facets, collapse = "+"), ": per-cell weights + graphs[['", name,
      "']] (WSNN) + reductions[['", name, "']]")
  }
  invisible(W)
}

.pagoda2_r6_run_leiden <- function(p2, reduction = NULL, graph = NULL, name = "leiden", setDefault = TRUE, overwrite = FALSE, method = NULL, n.cores = NULL, threads = NULL, verbose = FALSE, ...) {
  ## `verbose` is consumed here (used for pagoda2-level messaging) and deliberately NOT forwarded through
  ## `...` to the community-detection backend: leidenAlg::leiden.community and igraph's community methods
  ## do not accept a `verbose` argument, so forwarding it would error. This keeps `verbose=` working
  ## uniformly across the run* verbs.
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
  if (isTRUE(verbose)) message("Running Leiden clustering on graph `", graph, "` -> `", name, "`")
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

## Generic clustering step: method "leiden" (default) or a community-detection function. The algorithm is
## a `method=`, mirroring runReduction/runGraph/runEmbedding (no runLeiden/runWNN primary verb).
.pagoda2_r6_run_clustering <- function(p2, method = "leiden", name = NULL, ...) {
  fn <- NULL
  if (is.function(method)) {
    fn <- method
    if (is.null(name)) name <- "clustering"
  } else {
    if (!identical(tolower(method), "leiden")) {
      stop("runClustering: `method` must be \"leiden\" or a community-detection function; got '", method, "'", call. = FALSE)
    }
    fn <- NULL # .pagoda2_r6_run_leiden defaults to leidenAlg::leiden.community
    if (is.null(name)) name <- "leiden"
  }
  .pagoda2_r6_run_leiden(p2, name = name, method = fn, ...)
}

## Map a facet to its lstar feature-axis name (for joint-product provenance, §5/§7).
.pagoda2_facet_feature_axis <- function(facet) {
  switch(facet$featureType, gene = "genes", protein = "proteins", peak = "peaks", facet$featureType)
}

## Centered truncated SVD for reductions. Prefers RSpectra (C++ Spectra — faster + more accurate than
## irlba on these blocks, benchmark/README.md), implicit-centering via a matrix operator so the sparse
## block is never densified; falls back to irlba. Returns a list(d, u, v) compatible with the irlba
## object the reduction code consumes (uses $v, $d, and stores $center itself).
.pagoda2_truncated_svd <- function(x, nv, center = NULL, maxit = 100L, fastpath = TRUE, ...) {
  nv <- as.integer(min(nv, nrow(x) - 1L, ncol(x) - 1L))
  ## Tiny matrices: iterative solvers (RSpectra requires dims >= 3; irlba requires nv < min(dim))
  ## are not applicable -- compute the exact SVD densely. Never triggers on real PCA blocks.
  if (nrow(x) < 3L || ncol(x) < 3L || nv < 1L) {
    xm <- as.matrix(x)
    if (!is.null(center)) xm <- sweep(xm, 2L, as.numeric(center), "-")
    s <- svd(xm)
    k <- max(1L, nv)
    return(list(d = s$d[seq_len(k)], u = s$u[, seq_len(k), drop = FALSE], v = s$v[, seq_len(k), drop = FALSE]))
  }
  if (requireNamespace("RSpectra", quietly = TRUE)) {
    if (is.null(center)) {
      r <- RSpectra::svds(x, k = nv, nu = 0L, nv = nv)
    } else {
      cm <- as.numeric(center)
      A  <- function(v, args) as.numeric(x %*% v) - sum(cm * v)          # (X - 1 cm') v, no densify
      At <- function(u, args) as.numeric(Matrix::crossprod(x, u)) - cm * sum(u)
      r <- RSpectra::svds(A, k = nv, nu = 0L, nv = nv, Atrans = At, dim = dim(x))
    }
    return(list(d = r$d, u = r$u, v = r$v))
  }
  if (is.null(center)) {
    irlba::irlba(x, nv = nv, nu = 0, fastpath = fastpath, maxit = maxit, reorth = TRUE)
  } else {
    irlba::irlba(x, nv = nv, nu = 0, center = center, fastpath = fastpath, maxit = maxit, reorth = TRUE)
  }
}

## Generic reduction step. Single facet (facet=): method defaults to the facet's defaultReduction
## (PCA for RNA, LSI for ATAC). Multiple facets (facets=): a JOINT reduction (the §0.4.4 "one joint
## method"), a named cell-space product over the shared cells with the contributing feature axes in
## provenance. The algorithm is always a `method=`, mirroring runGraph/runClustering/runEmbedding.
.pagoda2_r6_run_reduction <- function(p2, facet = NULL, facets = NULL, method = NULL, name = NULL, ...) {
  if (!is.null(facets) && length(facets) >= 2L) {
    m <- if (is.null(method)) "concat" else tolower(method)
    if (m %in% c("cca", "scca", "sparse-cca", "sparsecca", "spcca")) {
      ## CCA family: dense (irlba) or sparse (PMA). `sparse=` in ... wins; else the alias implies it.
      dots <- list(...)
      sp <- if (!is.null(dots$sparse)) isTRUE(dots$sparse) else !identical(m, "cca")
      dots$sparse <- NULL
      return(do.call(.pagoda2_r6_run_cca, c(list(p2, facets = facets, name = name, sparse = sp), dots)))
    }
    return(.pagoda2_r6_run_joint_reduction(p2, facets = facets, method = m, name = name, ...))
  }
  f <- p2$resolveFacet(facet)
  if (is.null(method)) {
    method <- f$defaultReduction
  }
  m <- tolower(method)
  if (is.null(name)) {
    name <- toupper(m)
  }
  if (identical(m, "pca")) {
    return(.pagoda2_r6_run_pca(p2, facet = facet, name = name, ...))
  }
  if (identical(m, "lsi")) {
    return(.pagoda2_r6_run_lsi(p2, facet = facet, name = name, ...))
  }
  stop("unknown reduction method '", method, "'", call. = FALSE)
}

## LSI = TF-IDF view -> SVD -> drop the first component (it tracks sequencing depth), §6.3. The view stays
## a pure per-entry recipe; drop.first is a reduction post-step, so the §6.2 invariance argument is intact.
.pagoda2_r6_run_lsi <- function(p2, facet = NULL, name = "LSI", nPcs = 50, drop.first = TRUE,
                                genes = NULL, fastpath = TRUE, maxit = 100, verbose = TRUE, ...) {
  f <- p2$resolveFacet(facet)
  x <- p2$getExpressionBlock(facet = facet, genes = genes) # cells x peaks, TF-IDF view materialized
  k <- if (isTRUE(drop.first)) nPcs + 1L else nPcs
  k <- min(k, ncol(x) - 1L, nrow(x) - 1L)
  if (k < 1L) {
    stop("LSI: too few features/cells for the requested number of components", call. = FALSE)
  }
  sv <- .pagoda2_truncated_svd(x, nv = k, center = NULL, maxit = maxit, fastpath = fastpath)
  scores <- as.matrix(x %*% sv$v) # cells x k
  loadings <- sv$v
  if (isTRUE(drop.first)) { # drop the depth-correlated first component
    scores <- scores[, -1L, drop = FALSE]
    loadings <- loadings[, -1L, drop = FALSE]
  }
  rownames(scores) <- rownames(x)
  colnames(scores) <- paste0(name, seq_len(ncol(scores)))
  rownames(loadings) <- colnames(x)
  key <- .pagoda2_reduction_key(p2, f$name, name)
  p2$reductions[[key]] <- scores
  f$loadings[[name]] <- loadings
  if (verbose) message("LSI on facet `", f$name, "` -> ", key, " (", ncol(scores), " dims", if (drop.first) ", dropped comp 1" else "", ")")
  invisible(scores)
}

## Joint reduction (concat-PCA): the shipped "one joint method" (§0.4.4). Scales each facet's reduction
## scores to unit average norm, concatenates, and re-PCAs to a shared latent over the common cells. Stored
## as a name-keyed named product `reductions[[name]]` with provenance {facets, input_axes, method} — the
## §5 shape (scores top-level; per-facet loadings stay in the facet). WNN/MOFA can replace `method` later.
.pagoda2_r6_run_joint_reduction <- function(p2, facets, method = NULL, name = NULL, reductions = NULL,
                                            nPcs = 50, fastpath = TRUE, maxit = 100, verbose = TRUE, ...) {
  if (is.null(method)) {
    method <- "concat"
  }
  if (!identical(method, "concat")) {
    stop("joint reduction: unknown method '", method, "'; available joint methods are ",
         "'concat' (concat-PCA), 'cca', 'scca' (use runReduction(facets=, method=...))", call. = FALSE)
  }
  if (is.null(name)) {
    name <- "concatPCA" # product name (NOT "WNN" — that is reserved for runGraph(method="wnn"))
  }
  .pagoda2_validate_joint_name(p2, name) # no per-facet-method-name shadow (§4.5.1)
  parts <- list()
  input.axes <- character()
  for (i in seq_along(facets)) {
    f <- p2$resolveFacet(facets[[i]])
    red <- if (!is.null(reductions)) reductions[[i]] else f$defaultReduction
    key <- .pagoda2_reduction_key(p2, f$name, red)
    sc <- p2$reductions[[key]]
    if (is.null(sc)) {
      stop("reduction `", key, "` not found; run runReduction(facet='", f$name, "') before the joint step", call. = FALSE)
    }
    parts[[i]] <- sc
    input.axes <- c(input.axes, .pagoda2_facet_feature_axis(f))
  }
  common <- Reduce(intersect, lapply(parts, rownames))
  if (length(common) < 2L) {
    stop("joint reduction: facets share fewer than 2 cells", call. = FALSE)
  }
  scaled <- lapply(parts, function(m) {
    m <- m[common, , drop = FALSE]
    s <- sqrt(sum(m^2) / nrow(m))
    if (s > 0) m / s else m
  })
  X <- do.call(cbind, scaled)
  nPcs <- min(nPcs, ncol(X) - 1L, length(common) - 1L)
  cm <- Matrix::colMeans(X)
  pc <- .pagoda2_truncated_svd(X, nv = nPcs, center = cm, maxit = maxit, fastpath = fastpath)
  scores <- as.matrix(sweep(X %*% pc$v, 2, as.numeric(cm %*% pc$v)))
  rownames(scores) <- common
  colnames(scores) <- paste0(name, seq_len(ncol(scores)))
  attr(scores, "facets") <- as.character(facets)
  attr(scores, "input_axes") <- input.axes # lstar provenance: feature-axis names (S5)
  attr(scores, "method") <- paste0("joint:", method)
  .pagoda2_warn_joint_clobber(p2, name, facets, paste0("joint:", method))
  p2$reductions[[name]] <- scores
  if (verbose) message("joint reduction `", name, "` over facets ", paste(facets, collapse = "+"), " -> ", ncol(scores), " dims")
  invisible(scores)
}

## Warn before a joint write silently replaces an existing reduction of the SAME name but DIFFERENT
## provenance (different facet set or method) -- the safety net for "multiple joints coexist" (§3.3 ask
## #3): reusing a name across facet subsets is almost always an accident, so flag it and point at name=.
.pagoda2_warn_joint_clobber <- function(p2, name, facets, method) {
  prev <- p2$reductions[[name]]
  if (is.null(prev)) {
    return(invisible(NULL))
  }
  pf <- attr(prev, "facets")
  pm <- attr(prev, "method")
  if (!identical(as.character(pf), as.character(facets)) || !identical(pm, method)) {
    was <- if (is.null(pm)) "a per-facet reduction" else paste0(pm, " over ", paste(pf, collapse = "+"))
    warning("overwriting reduction '", name, "' (was ", was, ") with ", method, " over ",
            paste(facets, collapse = "+"), "; pass a distinct name= to keep both", call. = FALSE)
  }
  invisible(NULL)
}

## CCA / sparse-CCA — vertical (same-cell) canonical correlation between exactly two facets, §5.1.
## Operates on each facet's scaled feature block (the same od-gene/feature matrix runReduction reduces),
## restricted to the shared cells. The cross-covariance over cells, C = t(X1) %*% X2 (features1 x
## features2), is SVD'd: the singular vectors ARE the per-facet FEATURE loadings (genes x k, proteins x k),
## the canonical variates X1 U and X2 V are the per-facet cell scores, and the joint product is their
## mean (the maximally-correlated pair averaged). This mirrors conos::quickCCA (conos.R:327) transposed
## from its horizontal, shared-gene orientation to our vertical, shared-cell one. Dense path = irlba on a
## kept-sparse centered cross-product (scales in n); sparse path = PMA::CCA (L1-sparse loadings; gated dep).
## Stored as the §4.5 named product reductions[[name]] (default "CCA") + per-facet loadings[[name]].
.pagoda2_r6_run_cca <- function(p2, facets, name = NULL, sparse = FALSE, nPcs = 30,
                                reductions = NULL, genes = NULL, n.odgenes = NULL, var.scale = TRUE,
                                penalty = 0.3, penaltyx = penalty, penaltyz = penalty,
                                fastpath = TRUE, maxit = 100, verbose = TRUE, ...) {
  if (length(facets) != 2L) {
    stop("CCA is a two-block method; pass exactly two facets (use method='concat' for >2)", call. = FALSE)
  }
  if (is.null(name)) {
    name <- "CCA"
  }
  .pagoda2_validate_joint_name(p2, name) # no per-facet-method-name shadow (§4.5.1)
  blocks <- list()
  facetObjs <- list()
  input.axes <- character()
  for (i in seq_along(facets)) {
    f <- p2$resolveFacet(facets[[i]])
    od <- if (!is.null(genes)) genes else f$odgenes
    if (!is.null(od) && !is.null(n.odgenes)) {
      od <- od[seq_len(min(length(od), n.odgenes))]
    }
    x <- p2$getExpressionBlock(genes = od, facet = facets[[i]]) # cells x features, view materialized
    if (isTRUE(var.scale)) {
      x <- .pagoda2_apply_variance_scaling(x, f$varinfo) # column scaling keeps it sparse
    }
    blocks[[i]] <- x
    facetObjs[[i]] <- f
    input.axes <- c(input.axes, .pagoda2_facet_feature_axis(f))
  }
  common <- Reduce(intersect, lapply(blocks, rownames))
  if (length(common) < 3L) {
    stop("CCA: facets share fewer than 3 cells", call. = FALSE)
  }
  X1 <- blocks[[1]][common, , drop = FALSE]
  X2 <- blocks[[2]][common, , drop = FALSE]
  n <- length(common)
  k <- min(nPcs, ncol(X1), ncol(X2), n - 1L)
  if (k < 1L) {
    stop("CCA: too few features/cells for a canonical component", call. = FALSE)
  }
  mu1 <- Matrix::colMeans(X1)
  mu2 <- Matrix::colMeans(X2)
  if (isTRUE(sparse)) {
    if (!requireNamespace("PMA", quietly = TRUE)) {
      stop("sparse CCA needs the 'PMA' package: install.packages('PMA')", call. = FALSE)
    }
    ## PMA's L1-penalized CCA. Pre-standardize ourselves (center + unit sd) and pass standardize=FALSE,
    ## so the same standardized blocks produce both the (sparse) loadings and the cell scores below.
    Z1 <- scale(as.matrix(X1), center = TRUE, scale = TRUE)
    Z2 <- scale(as.matrix(X2), center = TRUE, scale = TRUE)
    Z1[, attr(Z1, "scaled:scale") == 0] <- 0
    Z2[, attr(Z2, "scaled:scale") == 0] <- 0
    res <- PMA::CCA(Z1, Z2, K = k, penaltyx = penaltyx, penaltyz = penaltyz,
                    standardize = FALSE, trace = FALSE)
    U <- res$u
    V <- res$v
    s1 <- Z1 %*% U
    s2 <- Z2 %*% V
    cancor <- if (!is.null(res$cors)) as.numeric(res$cors) else
      vapply(seq_len(ncol(U)), function(j) abs(stats::cor(s1[, j], s2[, j])), numeric(1))
  } else {
    ## Centered cross-covariance via the identity C = t(X1)X2 - n * mu1 mu2', so X1/X2 stay sparse.
    C <- as.matrix(Matrix::crossprod(X1, X2)) - n * outer(as.numeric(mu1), as.numeric(mu2))
    sv <- if (k < min(dim(C))) {
      irlba::irlba(C, nv = k, nu = k, fastpath = fastpath, maxit = maxit)
    } else {
      s <- svd(C, nu = k, nv = k)
      s$d <- s$d[seq_len(k)]
      s
    }
    U <- sv$u
    V <- sv$v
    ## per-facet cell scores = centered block projected (kept sparse: X U - 1 mu'U)
    s1 <- sweep(as.matrix(X1 %*% U), 2L, as.numeric(crossprod(as.numeric(mu1), U)))
    s2 <- sweep(as.matrix(X2 %*% V), 2L, as.numeric(crossprod(as.numeric(mu2), V)))
    cancor <- sv$d[seq_len(k)] / (n - 1L) # cross-covariance singular values -> per-cell scale
  }
  rownames(U) <- colnames(X1)
  rownames(V) <- colnames(X2)
  colnames(U) <- colnames(V) <- paste0(name, seq_len(ncol(U)))
  scores <- (s1 + s2) / 2 # the maximally-correlated canonical pair, averaged -> joint cells x k
  rownames(scores) <- common
  colnames(scores) <- paste0(name, seq_len(ncol(scores)))
  attr(scores, "facets") <- as.character(facets)
  attr(scores, "input_axes") <- input.axes # lstar S5 provenance: per-facet feature axes
  attr(scores, "method") <- if (sparse) "joint:scca" else "joint:cca"
  attr(scores, "cancor") <- cancor
  .pagoda2_warn_joint_clobber(p2, name, facets, attr(scores, "method"))
  p2$reductions[[name]] <- scores
  ## feature-space loadings live with each facet (lstar shared-factor-axis induction, §5)
  facetObjs[[1]]$loadings[[name]] <- U
  facetObjs[[2]]$loadings[[name]] <- V
  if (verbose) {
    message(if (sparse) "sparse-CCA" else "CCA", " over facets ", paste(facets, collapse = "+"),
            " -> ", ncol(scores), " components (top canonical assoc ", signif(cancor[[1]], 3), ")")
  }
  invisible(scores)
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
