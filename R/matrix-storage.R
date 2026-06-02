## Matrix storage and view implementation for Pagoda2


.pagoda2_matrix_sumsq <- function(x) {
  if (inherits(x, "sparseMatrix")) {
    return(sum(x@x^2))
  }
  sum(as.numeric(x)^2)
}

.pagoda2_has_explicit_rownames <- function(x) {
  if (is.data.frame(x)) {
    return(.row_names_info(x, type = 1L) >= 0L)
  }
  rn <- attr(x, "row.names")
  !(length(rn) == 2 && is.na(rn[1]) && rn[2] < 0)
}


.pagoda2_sparse_winsor_caps <- function(x, trim) {
  trim <- as.integer(trim)
  if (is.na(trim) || trim <= 0) {
    return(stats::setNames(rep(Inf, ncol(x)), colnames(x)))
  }
  caps <- rep(Inf, ncol(x))
  for (j in seq_len(ncol(x))) {
    p0 <- x@p[[j]] + 1L
    p1 <- x@p[[j + 1L]]
    if (p1 < p0) {
      next
    }
    values <- x@x[p0:p1]
    values <- values[is.finite(values)]
    if (length(values) > trim + 1L) {
      caps[[j]] <- sort(values, decreasing = TRUE)[[trim + 1L]]
    }
  }
  stats::setNames(caps, colnames(x))
}

.pagoda2_materialize_view <- function(raw, view) {
  x <- raw
  x@x <- as.numeric(x@x)
  if (identical(view$model, "raw")) {
    if (isTRUE(view$log.scale)) {
      x@x <- log(x@x + 1)
    }
    return(x)
  }
  if (!identical(view$model, "plain")) {
    stop("Matrix view model `", view$model, "` is not supported by the R materializer yet")
  }

  if (!is.null(view$batchFactors)) {
    batch <- view$batch[rownames(x)]
    batch.factors <- view$batchFactors[colnames(x), , drop = FALSE]
    gene.index <- rep(seq_len(ncol(x)), diff(x@p))
    batch.index <- as.integer(batch)[x@i + 1L]
    x@x <- as.numeric(x@x / batch.factors[cbind(gene.index, batch.index)])
  }

  depth <- view$depth[rownames(x)]
  if (!is.null(view$winsorCaps)) {
    pre.depth <- view$preWinsorDepth[rownames(x)]
    gene.index <- rep(seq_len(ncol(x)), diff(x@p))
    x@x <- as.numeric(x@x / pre.depth[x@i + 1L])
    caps <- view$winsorCaps[colnames(x)]
    x@x <- pmin(x@x, caps[gene.index])
    x@x <- as.numeric(x@x * pre.depth[x@i + 1L])
    depth <- view$postWinsorDepth[rownames(x)]
  }
  x@x <- as.numeric(x@x / (depth[x@i + 1L] / view$depthScale))
  if (isTRUE(view$log.scale)) {
    x@x <- log(x@x + 1)
  }
  x
}

.pagoda2_counts_removed_message <- function(action = c("access", "assign")) {
  action <- match.arg(action)
  if (identical(action, "assign")) {
    return(paste0(
      "`$counts` is no longer a stored Pagoda2 matrix and cannot be assigned. ",
      "Construct a new object from raw counts, then use `p2$getRawCounts()` for raw values ",
      "or `p2$getExpressionBlock()` to materialize the normalized analysis view."
    ))
  }
  paste0(
    "`$counts` is no longer a stored Pagoda2 matrix. ",
    "Use `p2$getRawCounts()` for raw values, `p2$getExpressionBlock()` for normalized analysis values, ",
    "or `p2$materializeView(\"analysis\")` for explicit view materialization."
  )
}

.pagoda2_view_kernel_args <- function(raw, view) {
  if (!view$model %in% c("plain", "raw")) {
    stop("Matrix view model `", view$model, "` is not supported by sparse view kernels yet")
  }
  normalize <- identical(view$model, "plain")
  depth <- numeric()
  if (normalize) {
    depth <- as.numeric(view$depth[rownames(raw)])
    if (anyNA(depth)) {
      stop("Matrix view depth is not available for all requested cells")
    }
  }

  batch <- integer()
  batch.factors <- matrix(numeric(), nrow = 0, ncol = 0)
  if (!is.null(view$batchFactors)) {
    batch <- as.integer(view$batch[rownames(raw)])
    if (anyNA(batch)) {
      stop("Matrix view batch is not available for all requested cells")
    }
    batch.factors <- as.matrix(view$batchFactors[colnames(raw), , drop = FALSE])
  }

  winsor.caps <- numeric()
  pre.winsor.depth <- numeric()
  post.winsor.depth <- numeric()
  if (!is.null(view$winsorCaps)) {
    winsor.caps <- as.numeric(view$winsorCaps[colnames(raw)])
    pre.winsor.depth <- as.numeric(view$preWinsorDepth[rownames(raw)])
    post.winsor.depth <- as.numeric(view$postWinsorDepth[rownames(raw)])
    if (anyNA(winsor.caps) || anyNA(pre.winsor.depth) || anyNA(post.winsor.depth)) {
      stop("Matrix view winsorization values are not available for all requested axes")
    }
  }

  list(
    depth = depth,
    depthScale = view$depthScale,
    normalize = normalize,
    log.scale = isTRUE(view$log.scale),
    batch = batch,
    batchFactors = batch.factors,
    winsorCaps = winsor.caps,
    preWinsorDepth = pre.winsor.depth,
    postWinsorDepth = post.winsor.depth
  )
}


.pagoda2_apply_variance_scaling <- function(x, varinfo) {
  if (is.null(varinfo)) {
    stop("Please run adjustVariance first")
  }
  missing <- setdiff(colnames(x), rownames(varinfo))
  if (length(missing) > 0) {
    stop("Variance information is missing for gene(s): ", paste(missing, collapse = ", "))
  }
  x@x <- x@x * rep(varinfo[colnames(x), "gsf"], diff(x@p))
  x
}

.pagoda2_align_vector <- function(x, target, what = "values") {
  if (is.null(target)) {
    stop("Cannot align ", what, " before count matrix names are available")
  }
  if (is.null(names(x)) || all(is.na(names(x))) || all(names(x) == "")) {
    if (length(x) != length(target)) {
      stop("Unnamed ", what, " must have length ", length(target))
    }
    names(x) <- target
    return(x)
  }
  if (any(duplicated(names(x)))) {
    stop("Duplicate names are not allowed in ", what)
  }
  idx <- match(target, names(x))
  out <- x[rep(NA_integer_, length(target))]
  matched <- !is.na(idx)
  out[matched] <- x[idx[matched]]
  names(out) <- target
  out
}

.pagoda2_is_integerish <- function(x) {
  is.numeric(x) && all(is.na(x) | abs(x - round(x)) < sqrt(.Machine$double.eps))
}


.pagoda2_r6_sync_metadata <- function(p2) {
  matrix <- p2$rawCounts
  if (is.null(matrix)) {
    matrix <- p2$misc[["rawCounts"]]
  }
  if (is.null(matrix)) {
    return(invisible(p2))
  }
  cells <- rownames(matrix)
  genes <- colnames(matrix)
  if (is.null(p2$cellMeta) || (nrow(p2$cellMeta) == 0 && ncol(p2$cellMeta) == 0)) {
    p2$cellMeta <- data.frame(row.names = cells)
  }
  if (is.null(p2$geneMeta) || (nrow(p2$geneMeta) == 0 && ncol(p2$geneMeta) == 0)) {
    p2$geneMeta <- data.frame(row.names = genes)
  }
  invisible(p2)
}

.pagoda2_r6_get_raw_counts <- function(p2, cells = NULL, genes = NULL, orientation = c("cell_by_gene", "gene_by_cell")) {
  orientation <- match.arg(orientation)
  raw <- p2$rawCounts
  if (is.null(raw)) {
    raw <- p2$misc[["rawCounts"]]
  }
  if (is.null(raw)) {
    stop("Raw counts are not available")
  }
  if (!is.null(cells)) {
    raw <- raw[.pagoda2_axis_selection_index(cells, rownames(raw), what = "cell(s)"), , drop = FALSE]
  }
  if (!is.null(genes)) {
    raw <- raw[, .pagoda2_axis_selection_index(genes, colnames(raw), what = "gene(s)"), drop = FALSE]
  }
  raw <- as(raw, "CsparseMatrix")
  if (orientation == "gene_by_cell") {
    return(Matrix::t(raw))
  }
  raw
}

.pagoda2_r6_get_matrix_view <- function(p2, name = "analysis") {
  view <- p2$matrixViews[[name]]
  if (is.null(view)) {
    stop("Unknown matrix view `", name, "`")
  }
  view
}

.pagoda2_r6_materialize_view <- function(p2, name = "analysis", cells = NULL, genes = NULL, orientation = c("cell_by_gene", "gene_by_cell")) {
  orientation <- match.arg(orientation)
  view <- p2$getMatrixView(name)
  raw <- p2$getRawCounts(cells = cells, genes = genes)
  x <- .pagoda2_materialize_view(raw, view)
  if (orientation == "gene_by_cell") {
    return(Matrix::t(x))
  }
  x
}

.pagoda2_r6_get_expression_block <- function(p2, layer = "analysis", cells = NULL, genes = NULL,
                                             orientation = c("cell_by_gene", "gene_by_cell"),
                                             scale.variance = FALSE) {
  orientation <- match.arg(orientation)
  x <- p2$materializeView(name = layer, cells = cells, genes = genes, orientation = "cell_by_gene")
  if (isTRUE(scale.variance)) {
    x <- .pagoda2_apply_variance_scaling(x, p2$misc[["varinfo"]])
  }
  if (orientation == "gene_by_cell") {
    return(Matrix::t(x))
  }
  x
}

.pagoda2_r6_view_col_mean_var <- function(p2, name = "analysis", cells = NULL, n.cores = p2$n.cores) {
  raw <- p2$getRawCounts()
  view <- p2$getMatrixView(name)
  rowSel <- .pagoda2_cell_selection_mask(cells, rownames(raw), what = "cells")
  args <- .pagoda2_view_kernel_args(raw, view)
  colMeanVarView(
    raw,
    rowSel,
    args$depth,
    args$depthScale,
    args$normalize,
    args$log.scale,
    args$batch,
    args$batchFactors,
    args$winsorCaps,
    args$preWinsorDepth,
    args$postWinsorDepth,
    n.cores
  )
}

.pagoda2_r6_view_col_sum_by_fac <- function(p2, grouping = NULL, groups = NULL, name = "analysis", cells = NULL) {
  raw <- p2$getRawCounts()
  selected <- .pagoda2_cell_selection_mask(cells, rownames(raw), what = "cells")
  if (!is.null(selected)) {
    raw <- raw[selected, , drop = FALSE]
  }
  view <- p2$getMatrixView(name)
  cols <- p2$resolveGrouping(
    grouping = grouping,
    groups = groups,
    cells = rownames(raw),
    allow.missing = TRUE
  )
  args <- .pagoda2_view_kernel_args(raw, view)
  out <- colSumByFacView(
    raw,
    as.integer(cols),
    args$depth,
    args$depthScale,
    args$normalize,
    args$log.scale,
    args$batch,
    args$batchFactors,
    args$winsorCaps,
    args$preWinsorDepth,
    args$postWinsorDepth
  )
  rownames(out) <- c("<NA>", levels(cols)[seq_len(nrow(out) - 1L)])
  colnames(out) <- colnames(raw)
  out
}

.pagoda2_r6_validate_matrices <- function(p2, stop.on.error = TRUE) {
  errors <- character()
  raw <- p2$rawCounts
  if (is.null(raw)) {
    raw <- p2$misc[["rawCounts"]]
  }
  if (is.null(raw)) {
    errors <- c(errors, "rawCounts is missing")
  } else {
    if (!inherits(raw, "dgCMatrix")) {
      errors <- c(errors, "rawCounts must be a dgCMatrix")
    }
    if (is.null(rownames(raw)) || is.null(colnames(raw))) {
      errors <- c(errors, "rawCounts must have cell and gene names")
    }
    if (!.pagoda2_is_integerish(raw@x)) {
      errors <- c(errors, "rawCounts contains non-integer values")
    }
  }
  if (!is.null(p2$depth) && !is.null(raw)) {
    if (length(p2$depth) != nrow(raw) || !identical(names(p2$depth), rownames(raw))) {
      errors <- c(errors, "depth is not named on the rawCounts cell axis")
    }
  }
  if (!is.null(p2$batch) && !is.null(raw)) {
    if (length(p2$batch) != nrow(raw) || !identical(names(p2$batch), rownames(raw))) {
      errors <- c(errors, "batch is not named on the rawCounts cell axis")
    }
  }
  view <- p2$matrixViews$analysis
  if (!is.null(view) && !is.null(raw)) {
    if (!is.null(view$depth) &&
      (length(view$depth) != nrow(raw) || !identical(names(view$depth), rownames(raw)))) {
      errors <- c(errors, "analysis view depth is not named on the rawCounts cell axis")
    }
    if (!is.null(view$batch) &&
      (length(view$batch) != nrow(raw) || !identical(names(view$batch), rownames(raw)))) {
      errors <- c(errors, "analysis view batch is not named on the rawCounts cell axis")
    }
    if (!is.null(view$batchFactors) &&
      !identical(rownames(view$batchFactors), colnames(raw))) {
      errors <- c(errors, "analysis view batch factors are not named on the rawCounts gene axis")
    }
    if (!is.null(view$winsorCaps) &&
      !identical(names(view$winsorCaps), colnames(raw))) {
      errors <- c(errors, "analysis view winsorization caps are not named on the rawCounts gene axis")
    }
    if (!is.null(view$preWinsorDepth) &&
      (length(view$preWinsorDepth) != nrow(raw) || !identical(names(view$preWinsorDepth), rownames(raw)))) {
      errors <- c(errors, "analysis view pre-winsor depth is not named on the rawCounts cell axis")
    }
    if (!is.null(view$postWinsorDepth) &&
      (length(view$postWinsorDepth) != nrow(raw) || !identical(names(view$postWinsorDepth), rownames(raw)))) {
      errors <- c(errors, "analysis view post-winsor depth is not named on the rawCounts cell axis")
    }
  }
  if (length(errors) > 0) {
    if (stop.on.error) {
      stop(paste(errors, collapse = "; "))
    }
    return(FALSE)
  }
  TRUE
}

.pagoda2_r6_describe_matrices <- function(p2) {
  entries <- list()
  add_entry <- function(name, role, matrix) {
    if (is.null(matrix)) {
      return(NULL)
    }
    data.frame(
      name = name,
      role = role,
      class = class(matrix)[1],
      n.cells = nrow(matrix),
      n.genes = ncol(matrix),
      nnz = length(matrix@x),
      integer.like = .pagoda2_is_integerish(matrix@x),
      stringsAsFactors = FALSE
    )
  }
  add_view_entry <- function(name, role, view, raw) {
    if (is.null(view) || is.null(raw)) {
      return(NULL)
    }
    data.frame(
      name = name,
      role = role,
      class = paste0(view$model, "_view"),
      n.cells = nrow(raw),
      n.genes = ncol(raw),
      nnz = NA_integer_,
      integer.like = NA,
      stringsAsFactors = FALSE
    )
  }
  entries[["raw"]] <- add_entry("raw", "raw_counts", p2$rawCounts)
  entries[["analysis"]] <- add_view_entry("analysis", "analysis_view", p2$matrixViews$analysis, p2$rawCounts)
  entries <- entries[!vapply(entries, is.null, logical(1))]
  if (length(entries) == 0) {
    return(data.frame(
      name = character(),
      role = character(),
      class = character(),
      n.cells = integer(),
      n.genes = integer(),
      nnz = integer(),
      integer.like = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, entries)
}

.pagoda2_r6_set_count_matrix <- function(p2, countMatrix, depthScale = 1e3, min.cells.per.gene = 0,
                                         trim = round(min.cells.per.gene / 2), min.transcripts.per.cell = 10,
                                         lib.sizes = NULL, log.scale = FALSE, keep.genes = NULL, verbose = TRUE) {
  # check names
  if (any(duplicated(rownames(countMatrix)))) {
    stop("Duplicate gene names are not allowed - please reduce")
  }
  if (any(duplicated(colnames(countMatrix)))) {
    stop("Duplicate cell names are not allowed - please reduce")
  }

  if (any(is.na(rownames(countMatrix)))) {
    stop("NA gene names are not allowed - please fix")
  }
  if (any(is.na(colnames(countMatrix)))) {
    stop("NA cell names are not allowed - please fix")
  }

  if (ncol(countMatrix) < 3) {
    stop("Too few cells remaining after min.count.per.cell filter applied - have you pre-filtered the count matrix to include only cells of a realistic size?")
  }

  counts <- t(countMatrix)

  p2$misc$depthScale <- depthScale
  colBatch <- NULL
  if (!is.null(p2$batch)) {
    if (!all(colnames(countMatrix) %in% names(p2$batch))) {
      stop("The supplied batch vector doesn't contain all the cells in its names attribute")
    }
    colBatch <- as.factor(p2$batch[colnames(countMatrix)])
  }

  if (!is.null(lib.sizes)) {
    if (!all(colnames(countMatrix) %in% names(lib.sizes))) {
      stop("The supplied lib.sizes vector doesn't contain all the cells in its names attribute")
    }
    lib.sizes <- lib.sizes[colnames(countMatrix)]
    depth <- lib.sizes / mean(lib.sizes) * mean(Matrix::colSums(countMatrix))
  } else {
    depth <- Matrix::colSums(countMatrix)
  }

  names(depth) <- rownames(counts)
  if (!is.null(colBatch)) {
    p2$batch <- droplevels(colBatch)
    names(p2$batch) <- rownames(counts)
  }

  p2$rawCounts <- counts
  p2$misc[["rawCounts"]] <- p2$rawCounts
  analysis.view <- list(
    name = "analysis",
    source = "raw",
    model = p2$modelType,
    depthScale = depthScale,
    depth = depth,
    log.scale = log.scale,
    trim = trim,
    batch = p2$batch,
    batchFactors = NULL,
    winsorCaps = NULL,
    preWinsorDepth = NULL,
    postWinsorDepth = NULL
  )

  if (any(depth == 0)) {
    stop("Cells with zero expression over all genes are not allowed")
  }

  if (p2$modelType == "raw") {
    p2$depth <- depth
    p2$matrixViews$analysis <- analysis.view
    p2$syncMetadata()
    invisible(p2)
    return()
  }

  if (p2$modelType == "linearObs") {
    stop("modelType `linearObs` is not supported without stored `$counts` yet")
  }

  counts <- p2$rawCounts
  counts@x <- as.numeric(counts@x)

  if (verbose) message(nrow(counts), " cells, ", ncol(counts), " genes; normalizing ... ")

  # get normalized matrix
  if (p2$modelType == "linearObs") { # this shouldn't work well, since the depth dependency is not completely normalized out

    # winsorize in normalized space first in hopes of getting a more stable depth estimate
    if (trim > 0) {
      counts <- counts / as.numeric(depth)
      inplaceWinsorizeSparseCols(counts, trim, p2$n.cores)
      counts <- counts * as.numeric(depth)
      if (is.null(lib.sizes)) {
        depth <- round(Matrix::rowSums(counts))
      }
    }

    ldepth <- log(depth)

    # rank cells, cut into n pieces
    n.depth.slices <- 20
    # depth.fac <- as.factor(floor(rank(depth)/(length(depth)+1)*n.depth.slices)+1); names(depth.fac) <- rownames(counts);
    depth.fac <- cut(cumsum(sort(depth)), breaks = seq(0, sum(depth), length.out = n.depth.slices))
    names(depth.fac) <- rownames(counts)
    depth.fac <- depth.fac[rank(depth)]
    # dataset-wide gene average
    gene.av <- (Matrix::colSums(counts) + n.depth.slices) / (sum(depth) + n.depth.slices)

    # pooled counts, df for all genes
    tc <- colSumByFac(counts, as.integer(depth.fac))[-1, , drop = FALSE]
    tc <- log(tc + 1) - log(as.numeric(tapply(depth, depth.fac, sum)) + 1)
    md <- log(as.numeric(tapply(depth, depth.fac, mean)))
    # combined lm
    cm <- stats::lm(tc ~ md)
    colnames(cm$coef) <- colnames(counts)
    # adjust counts
    # predict log(p) for each non-0 entry
    count.gene <- rep(1:counts@Dim[2], diff(counts@p))
    exp.x <- exp(log(gene.av)[count.gene] - cm$coef[1, count.gene] - ldepth[counts@i + 1] * cm$coef[2, count.gene])
    counts@x <- as.numeric(counts@x * exp.x / (depth[counts@i + 1] / depthScale)) # normalize by depth as well
    # perform a another round of trimming
    if (trim > 0) {
      inplaceWinsorizeSparseCols(counts, trim, p2$n.cores)
    }


    # regress out on non-0 observations of each gene
    # non0LogColLmS(counts,mx,ldepth)
  } else if (p2$modelType == "plain") {
    if (verbose) message("Using plain model ")

    if (!is.null(p2$batch)) {
      if (verbose) message("Batch ... ")

      # dataset-wide gene average
      gene.av <- (Matrix::colSums(counts) + length(levels(p2$batch))) / (sum(depth) + length(levels(p2$batch)))

      # pooled counts, df for all genes
      tc <- colSumByFac(counts, as.integer(p2$batch))[-1, , drop = FALSE]
      tc <- t(log(tc + 1) - log(as.numeric(tapply(depth, p2$batch, sum)) + 1))
      bc <- exp(tc - log(gene.av))
      rownames(bc) <- colnames(counts)
      colnames(bc) <- levels(p2$batch)
      analysis.view$batch <- p2$batch
      analysis.view$batchFactors <- bc

      # adjust every non-0 entry
      count.gene <- rep(1:counts@Dim[2], diff(counts@p))

      counts@x <- as.numeric(counts@x / bc[cbind(count.gene, as.integer(p2$batch)[counts@i + 1])])
    }

    if (trim > 0) {
      if (verbose) message("Winsorizing ... ")
      counts <- counts / as.numeric(depth)
      analysis.view$preWinsorDepth <- depth
      analysis.view$winsorCaps <- .pagoda2_sparse_winsor_caps(counts, trim)

      inplaceWinsorizeSparseCols(counts, trim, p2$n.cores)
      counts <- counts * as.numeric(depth)

      if (is.null(lib.sizes)) {
        depth <- round(Matrix::rowSums(counts))
      }
      names(depth) <- rownames(counts)
      analysis.view$postWinsorDepth <- depth
    }

    counts <- counts / as.numeric(depth / depthScale)
  } else {
    stop("modelType ", p2$modelType, " is not implemented")
  }
  if (log.scale) {
    if (verbose) message("log scale ... ")
    counts@x <- as.numeric(log(counts@x + 1))
  }
  p2$misc[["rescaled.mat"]] <- NULL
  if (verbose) message("done.\n")

  p2$depth <- depth
  analysis.view$depth <- depth
  p2$matrixViews$analysis <- analysis.view
  p2$syncMetadata()
  invisible(p2)
}
