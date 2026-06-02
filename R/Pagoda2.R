#' @useDynLib pagoda2
#' @import MASS
#' @import Matrix
#' @importFrom Rcpp evalCpp sourceCpp
#' @import Rook
#' @import igraph
#' @import sccore
#' @import R6
#' @import RMTstat 
#' @importFrom irlba irlba
#' @importFrom parallel mclapply
#' @importFrom magrittr %>%
#' @importFrom mgcv gam
#' @importFrom N2R Knn
#' @importFrom Rtsne Rtsne
#' @import drat
NULL

.pagoda2_deprecated_call <- function(old, new) {
  warning(
    "Legacy method `", old, "` is deprecated and will be removed in the next pagoda2 version. ",
    "Use `", new, "` instead.",
    call. = FALSE
  )
}

.pagoda2_workflow_steps <- c("qc", "variance", "pca", "graph", "umap", "leiden", "markers")

.pagoda2_workflow_dependencies <- list(
  qc = character(),
  variance = character(),
  pca = "variance",
  graph = "pca",
  umap = "pca",
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

.pagoda2_qc_gene_molecule <- function(matrix, min.molecules = 500, max.molecules = 5e4,
                                      p.level = NULL) {
  if (is.null(rownames(matrix))) {
    stop("QC matrix must have cell names as rownames")
  }
  if (is.null(p.level)) {
    p.level <- min(1e-3, 1 / nrow(matrix))
  }
  n.molecules <- as.numeric(Matrix::rowSums(matrix))
  n.genes <- as.numeric(Matrix::rowSums(matrix > 0))
  qc <- data.frame(
    n_molecules = n.molecules,
    n_genes = n.genes,
    qc_log_molecules = ifelse(n.molecules > 0, log10(n.molecules), NA_real_),
    qc_log_genes = ifelse(n.genes > 0, log10(n.genes), NA_real_),
    qc_gene_molecule_fitted = NA_real_,
    qc_gene_molecule_lower = NA_real_,
    qc_gene_molecule_upper = NA_real_,
    qc_gene_molecule_residual = NA_real_,
    qc_gene_molecule_z = NA_real_,
    qc_size_outlier = n.molecules < min.molecules | n.molecules > max.molecules,
    qc_gene_molecule_outlier = FALSE,
    qc_pass = TRUE,
    row.names = rownames(matrix)
  )

  fit.cells <- which(
    is.finite(qc$qc_log_molecules) &
      is.finite(qc$qc_log_genes) &
      qc$n_molecules >= min.molecules &
      qc$n_molecules <= max.molecules
  )
  fit <- NULL
  sigma <- NA_real_
  cutoff <- stats::qnorm(1 - p.level / 2)
  if (length(fit.cells) >= 4L && is.finite(cutoff)) {
    df <- qc[fit.cells, c("qc_log_molecules", "qc_log_genes"), drop = FALSE]
    fit <- tryCatch(
      MASS::rlm(qc_log_genes ~ qc_log_molecules, data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      pred <- as.numeric(stats::predict(fit, newdata = qc))
      qc$qc_gene_molecule_fitted <- pred
      residual <- qc$qc_log_genes - pred
      sigma <- stats::mad(residual[fit.cells], center = 0, constant = 1.4826, na.rm = TRUE)
      if (!is.finite(sigma) || sigma == 0) {
        sigma <- stats::sd(residual[fit.cells], na.rm = TRUE)
      }
      if (is.finite(sigma) && sigma > 0) {
        qc$qc_gene_molecule_residual <- residual
        qc$qc_gene_molecule_z <- residual / sigma
        qc$qc_gene_molecule_lower <- pred - cutoff * sigma
        qc$qc_gene_molecule_upper <- pred + cutoff * sigma
        qc$qc_gene_molecule_outlier <- abs(qc$qc_gene_molecule_z) > cutoff
        qc$qc_gene_molecule_outlier[!is.finite(qc$qc_gene_molecule_z)] <- FALSE
      }
    }
  }
  qc$qc_pass <- !qc$qc_size_outlier & !qc$qc_gene_molecule_outlier
  qc$qc_pass[is.na(qc$qc_pass)] <- FALSE
  attr(qc, "pagoda2.qc") <- list(
    method = "gene_molecule",
    min.molecules = min.molecules,
    max.molecules = max.molecules,
    p.level = p.level,
    fit.available = !is.null(fit) && is.finite(sigma) && sigma > 0
  )
  qc
}

.pagoda2_qc_summary <- function(qc) {
  n.cells <- nrow(qc)
  n.fail <- if ("qc_pass" %in% colnames(qc)) sum(!as.logical(qc$qc_pass), na.rm = TRUE) else NA_integer_
  molecule.q <- stats::quantile(qc$n_molecules, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  gene.q <- stats::quantile(qc$n_genes, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  msg <- paste0(
    "QC: ", n.cells, " cells; molecules median ", signif(molecule.q[2], 4),
    " [IQR ", signif(molecule.q[1], 4), "-", signif(molecule.q[3], 4), "]; genes median ",
    signif(gene.q[2], 4), " [IQR ", signif(gene.q[1], 4), "-", signif(gene.q[3], 4), "]"
  )
  if (is.finite(n.fail)) {
    msg <- paste0(msg, "; ", n.fail, " failed (", signif(100 * n.fail / n.cells, 3), "%)")
  }
  msg
}

.pagoda2_has_downstream_results <- function(p2) {
  length(p2$reductions) > 0L ||
    length(p2$graphs) > 0L ||
    length(p2$embeddings) > 0L ||
    length(p2$diffgenes) > 0L ||
    length(p2$markerResults) > 0L ||
    length(p2$clusterings) > 0L
}

.pagoda2_filter_analysis_view_cells <- function(view, cells) {
  for (nm in c("depth", "batch", "preWinsorDepth", "postWinsorDepth")) {
    if (!is.null(view[[nm]])) {
      view[[nm]] <- view[[nm]][cells]
      if (is.factor(view[[nm]])) {
        view[[nm]] <- droplevels(view[[nm]])
      }
    }
  }
  view
}

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

.pagoda2_align_metadata <- function(metadata, target, axis = "cell") {
  if (is.null(target)) {
    stop("Cannot set ", axis, " metadata before count matrix names are available")
  }
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  if (.pagoda2_has_explicit_rownames(metadata)) {
    if (any(duplicated(rownames(metadata)))) {
      stop("Duplicate ", axis, " names are not allowed in metadata")
    }
    metadata <- metadata[match(target, rownames(metadata)), , drop = FALSE]
    rownames(metadata) <- target
  } else {
    if (nrow(metadata) != length(target)) {
      stop("Unnamed ", axis, " metadata must have one row per ", axis)
    }
    rownames(metadata) <- target
  }
  metadata
}

.pagoda2_expand_metadata_rows <- function(metadata, rows) {
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE, optional = TRUE)
  rows <- unique(as.character(rows))
  if (length(rows) == 0) {
    return(metadata[FALSE, , drop = FALSE])
  }
  if (ncol(metadata) == 0) {
    return(data.frame(row.names = rows))
  }
  missing <- setdiff(rows, rownames(metadata))
  if (length(missing) > 0) {
    metadata[missing, colnames(metadata)] <- NA
  }
  metadata[rows, , drop = FALSE]
}

.pagoda2_prepare_metadata <- function(metadata, target, axis = "cell") {
  if (is.null(target)) {
    stop("Cannot set ", axis, " metadata before count matrix names are available")
  }
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE, optional = TRUE)
  if (.pagoda2_has_explicit_rownames(metadata)) {
    rn <- rownames(metadata)
    if (any(is.na(rn) | rn == "")) {
      stop("Missing ", axis, " names are not allowed in metadata")
    }
    if (any(duplicated(rn))) {
      stop("Duplicate ", axis, " names are not allowed in metadata")
    }
  } else {
    if (nrow(metadata) != length(target)) {
      stop("Unnamed ", axis, " metadata must have one row per ", axis)
    }
    rownames(metadata) <- target
  }
  metadata
}

.pagoda2_store_metadata <- function(existing, metadata, target, axis = "cell", overwrite = TRUE) {
  incoming <- .pagoda2_prepare_metadata(metadata, target = target, axis = axis)
  if (is.null(existing)) {
    existing <- data.frame(row.names = target)
  }
  existing <- as.data.frame(existing, stringsAsFactors = FALSE, optional = TRUE)
  overlap <- intersect(colnames(incoming), colnames(existing))
  if (!overwrite && length(overlap) > 0) {
    axis.label <- paste0(toupper(substr(axis, 1, 1)), substring(axis, 2))
    stop(axis.label, " metadata column(s) already exist: ", paste(overlap, collapse = ", "))
  }
  rows <- unique(c(rownames(existing), rownames(incoming)))
  existing <- .pagoda2_expand_metadata_rows(existing, rows)
  incoming <- .pagoda2_expand_metadata_rows(incoming, rows)
  for (n in colnames(incoming)) {
    existing[[n]] <- incoming[[n]]
  }
  existing
}

.pagoda2_vector_metadata <- function(name, value, target, axis = "cell") {
  if (is.null(target)) {
    stop("Cannot set ", axis, " metadata before count matrix names are available")
  }
  if (!is.character(name) || length(name) != 1 || is.na(name) || name == "") {
    stop("Metadata column name must be a single non-empty string")
  }
  if (is.null(names(value)) || all(is.na(names(value))) || all(names(value) == "")) {
    if (length(value) != length(target)) {
      stop("Unnamed ", axis, " metadata `", name, "` must have length ", length(target))
    }
    names(value) <- target
  } else if (any(duplicated(names(value)))) {
    stop("Duplicate names are not allowed in ", axis, " metadata `", name, "`")
  } else if (any(is.na(names(value)) | names(value) == "")) {
    stop("Missing names are not allowed in ", axis, " metadata `", name, "`")
  }
  out <- data.frame(value, row.names = names(value), stringsAsFactors = FALSE)
  colnames(out) <- name
  out
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

.pagoda2_cell_selection_mask <- function(cells, target, what = "cells") {
  if (is.null(cells)) {
    return(NULL)
  }
  if (is.logical(cells)) {
    if (length(cells) != length(target)) {
      stop("Logical ", what, " selection must have length ", length(target))
    }
    if (anyNA(cells)) {
      stop("Logical ", what, " selection cannot contain NA values")
    }
    return(cells)
  }
  mask <- rep(FALSE, length(target))
  names(mask) <- target
  if (is.character(cells)) {
    missing <- setdiff(cells, target)
    if (length(missing) > 0) {
      stop("Unknown ", what, ": ", paste(missing, collapse = ", "))
    }
    mask[cells] <- TRUE
    return(mask)
  }
  if (is.integer(cells) || (is.numeric(cells) && .pagoda2_is_integerish(cells))) {
    if (any(is.na(cells) | cells < 1 | cells > length(target))) {
      stop("Integer ", what, " selection is out of bounds")
    }
    mask[as.integer(cells)] <- TRUE
    return(mask)
  }
  stop("`", what, "` must be NULL, a logical vector, character names, or integer indices")
}

.pagoda2_axis_selection_index <- function(selection, target, what = "values") {
  if (is.null(selection)) {
    return(NULL)
  }
  if (is.logical(selection)) {
    if (length(selection) != length(target)) {
      stop("Logical ", what, " selection must have length ", length(target))
    }
    if (anyNA(selection)) {
      stop("Logical ", what, " selection cannot contain NA values")
    }
    return(which(selection))
  }
  if (is.character(selection)) {
    missing <- setdiff(selection, target)
    if (length(missing) > 0) {
      stop("Unknown ", what, ": ", paste(missing, collapse = ", "))
    }
    return(match(selection, target))
  }
  if (is.integer(selection) || (is.numeric(selection) && .pagoda2_is_integerish(selection))) {
    if (any(is.na(selection) | selection < 1 | selection > length(target))) {
      stop("Integer ", what, " selection is out of bounds")
    }
    return(as.integer(selection))
  }
  stop("`", what, "` must be NULL, a logical vector, character names, or integer indices")
}

.pagoda2_axis_names <- function(p2, axis = c("cell", "gene")) {
  axis <- match.arg(axis)
  matrix <- p2$rawCounts
  if (is.null(matrix)) {
    matrix <- p2$misc[['rawCounts']]
  }
  if (is.null(matrix)) {
    stop("Cannot determine ", axis, " names before counts are initialized")
  }
  if (axis == "cell") {
    rownames(matrix)
  } else {
    colnames(matrix)
  }
}

.pagoda2_apply_variance_scaling <- function(x, varinfo) {
  if (is.null(varinfo)) {
    stop("Please run adjustVariance first")
  }
  missing <- setdiff(colnames(x), rownames(varinfo))
  if (length(missing) > 0) {
    stop("Variance information is missing for gene(s): ", paste(missing, collapse = ", "))
  }
  x@x <- x@x * rep(varinfo[colnames(x), 'gsf'], diff(x@p))
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

.pagoda2_is_discrete_grouping <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return(TRUE)
  }
  if (is.integer(x) || .pagoda2_is_integerish(x)) {
    n <- sum(!is.na(x))
    n.levels <- length(unique(x[!is.na(x)]))
    max.levels <- max(20L, min(1000L, floor(n * 0.2)))
    return(n.levels > 0 && n.levels < n && n.levels <= max.levels)
  }
  FALSE
}

.pagoda2_as_grouping <- function(x, name = "grouping") {
  if (!.pagoda2_is_discrete_grouping(x)) {
    stop("`", name, "` does not look like a discrete cell grouping")
  }
  if (is.factor(x)) {
    return(droplevels(x))
  }
  stats::setNames(as.factor(x), names(x))
}

.pagoda2_marker_result <- function(name, type, grouping, groups, tables, params) {
  nonmissing.groups <- groups[!is.na(groups)]
  result <- list(
    schema = "pagoda2.marker.v1",
    name = name,
    type = type,
    grouping = grouping,
    levels = levels(groups),
    cells = names(nonmissing.groups),
    n.cells = as.integer(table(nonmissing.groups)),
    params = params,
    tables = tables,
    created.by = "runMarkers",
    created = Sys.time()
  )
  class(result) <- c("pagoda2_marker_result", "list")
  result
}

.pagoda2_marker_metadata <- function(result) {
  result$tables <- NULL
  result
}

.pagoda2_order_marker_table <- function(d, ordering = c("-AUC", "-Z", "-Precision", "-Specificity", "-M")) {
  if (nrow(d) == 0) {
    return(d)
  }
  for (ord in ordering) {
    decreasing <- startsWith(ord, "-")
    column <- sub("^-", "", ord)
    if (column %in% colnames(d)) {
      return(d[order(d[[column]], decreasing = decreasing, na.last = NA), , drop = FALSE])
    }
  }
  d
}

.pagoda2_select_marker_genes <- function(tables, n.genes.per.group = 5, genes = NULL,
                                         z.threshold = NULL, highest.only = TRUE,
                                         ordering = c("-AUC", "-Z", "-Precision", "-Specificity", "-M"),
                                         remove.duplicates = TRUE) {
  if (!is.null(genes)) {
    genes <- unique(as.character(genes))
    return(list(genes = genes, groups = stats::setNames(rep("selected", length(genes)), genes)))
  }
  if (is.null(tables) || length(tables) == 0) {
    stop("Marker result does not contain marker tables")
  }
  selected <- lapply(names(tables), function(group) {
    d <- tables[[group]]
    if (is.null(d) || nrow(d) == 0) {
      return(character())
    }
    if (!is.null(z.threshold) && "Z" %in% colnames(d)) {
      d <- d[d$Z >= z.threshold, , drop = FALSE]
    }
    if (highest.only && "highest" %in% colnames(d)) {
      d <- d[d$highest %in% TRUE, , drop = FALSE]
    }
    d <- .pagoda2_order_marker_table(d, ordering = ordering)
    if (!is.null(n.genes.per.group)) {
      d <- utils::head(d, n.genes.per.group)
    }
    if ("Gene" %in% colnames(d)) {
      as.character(d$Gene)
    } else {
      rownames(d)
    }
  })
  names(selected) <- names(tables)
  selected <- selected[lengths(selected) > 0]
  genes <- unlist(selected, use.names = FALSE)
  gene.groups <- rep(names(selected), lengths(selected))
  if (remove.duplicates && length(genes) > 0) {
    keep <- !duplicated(genes)
    genes <- genes[keep]
    gene.groups <- gene.groups[keep]
  }
  if (length(genes) == 0) {
    stop("No marker genes passed the requested filters")
  }
  list(genes = genes, groups = stats::setNames(gene.groups, genes))
}

.pagoda2_discrete_palette <- function(levels, s = 1, v = 1) {
  levels <- as.character(levels)
  if (length(levels) == 0L) {
    return(stats::setNames(character(), character()))
  }
  values <- stats::setNames(factor(levels, levels = levels), levels)
  sccore::fac2col(values, s = s, v = v, return.details = TRUE)$palette
}

.pagoda2_palette_axis_key <- function(axis) {
  axis <- match.arg(axis, c("cell", "gene", "cellMeta", "geneMeta"))
  if (axis == "cell") {
    return("cellMeta")
  }
  if (axis == "gene") {
    return("geneMeta")
  }
  axis
}

.pagoda2_normalize_palette_entry <- function(colors, name, axis, values = NULL,
                                             palette.id = name,
                                             generated.by = "user") {
  if (is.null(names(colors)) || any(names(colors) == "")) {
    if (is.null(values)) {
      stop("Palette colors for `", name, "` must be named by factor level")
    }
    levels <- levels(droplevels(as.factor(values)))
    if (length(colors) < length(levels)) {
      stop("Palette for `", name, "` has fewer colors than factor levels")
    }
    colors <- stats::setNames(colors[seq_along(levels)], levels)
  }
  color.names <- names(colors)
  colors <- as.character(colors)
  names(colors) <- color.names
  colors <- colors[!is.na(names(colors)) & names(colors) != ""]
  list(
    type = "discrete",
    axis = .pagoda2_palette_axis_key(axis),
    name = name,
    levels = names(colors),
    colors = colors,
    palette.id = palette.id,
    generated.by = generated.by,
    updated = Sys.time()
  )
}

.pagoda2_extend_palette_colors <- function(existing = NULL, values, s = 1, v = 1) {
  values <- droplevels(as.factor(values))
  levels <- levels(values)
  generated <- sccore::fac2col(stats::setNames(values, seq_along(values)),
                               s = s, v = v, return.details = TRUE)$palette
  if (!is.null(existing) && length(existing) > 0L) {
    generated[names(existing)] <- existing[names(existing)]
  }
  generated[levels]
}

.pagoda2_prepare_marker_heatmap <- function(p2, markers = NULL, type = "counts",
                                            genes = NULL, grouping = NULL, groups = NULL,
                                            n.genes.per.group = 5, additional.genes = NULL,
                                            exclude.genes = NULL, z.threshold = 2,
                                            highest.only = TRUE,
                                            ordering = c("-AUC", "-Z", "-Precision", "-Specificity", "-M"),
                                            remove.duplicates = TRUE, expression.quantile = 0.99,
                                            pal = grDevices::colorRampPalette(c("grey95", "firebrick3"), space = "Lab")(1024),
                                            column.metadata = NULL, column.metadata.colors = NULL,
                                            show.gene.groups = TRUE, show.group.legend = TRUE,
                                            show_heatmap_legend = FALSE, border = TRUE,
                                            row.label.font.size = 10, labeled.gene.subset = NULL,
                                            group.colors = NULL, gene.group.colors = NULL,
                                            order.groups = FALSE, split = FALSE, split.gap = 0,
                                            cell.order = NULL, averaging.window = 0, v = 1, s = 1,
                                            max.cells = Inf, max.dense.entries = 5e7,
                                            cluster.rows = FALSE, cluster.columns = FALSE,
                                            cluster.max.items = 2000, cluster.method = "complete",
                                            annotation.grobs = NULL, legend.max.levels = 18,
                                            legend.columns = NULL) {
  scale_heatmap_rows <- function(x, expression.quantile = 0.99) {
    out <- t(vapply(seq_len(nrow(x)), function(i) {
      xp <- x[i, ]
      finite <- is.finite(xp)
      if (!any(finite)) {
        return(rep(0, length(xp)))
      }
      if (expression.quantile < 1) {
        qs <- as.numeric(stats::quantile(xp[finite], c(1 - expression.quantile, expression.quantile), na.rm = TRUE))
        if (!all(is.finite(qs)) || diff(qs) == 0) {
          xps <- sort(unique(xp[finite]))
          if (length(xps) < 3) {
            qs <- range(xp[finite])
          } else {
            xpm <- stats::median(xp[finite])
            if (sum(xp < xpm, na.rm = TRUE) > sum(xp > xpm, na.rm = TRUE)) {
              lower <- xp[xp < xpm]
              if (length(lower) > 0) {
                qs[1] <- max(lower, na.rm = TRUE)
              }
            } else {
              higher <- xps[xps > xpm]
              if (length(higher) > 0) {
                qs[2] <- min(higher, na.rm = TRUE)
              }
            }
          }
        }
        if (!all(is.finite(qs)) || diff(qs) == 0) {
          qs <- range(xp[finite])
        }
        if (!all(is.finite(qs)) || diff(qs) == 0) {
          return(rep(0, length(xp)))
        }
        xp[xp < qs[1]] <- qs[1]
        xp[xp > qs[2]] <- qs[2]
      }
      xp <- xp - min(xp[finite], na.rm = TRUE)
      xmax <- max(xp[finite], na.rm = TRUE)
      if (is.finite(xmax) && xmax > 0) {
        xp <- xp / xmax
      } else {
        xp[] <- 0
      }
      xp
    }, numeric(ncol(x))))
    dimnames(out) <- dimnames(x)
    out
  }

  rollmean_matrix_cols <- function(x, width) {
    width <- as.integer(width)
    if (!is.finite(width) || width <= 1L || ncol(x) == 0L) {
      return(x)
    }
    out <- t(vapply(seq_len(nrow(x)), function(i) {
      v <- x[i, ]
      starts <- seq_along(v)
      ends <- pmin(length(v), starts + width - 1L)
      cs <- c(0, cumsum(v))
      (cs[ends + 1L] - cs[starts]) / (ends - starts + 1L)
    }, numeric(ncol(x))))
    dimnames(out) <- dimnames(x)
    out
  }

  order_within_groups <- function(x, groups, margin = c("row", "column"),
                                  max.items = Inf, method = "complete") {
    safe_hclust_order <- function(x, margin = c("row", "column"), method = "complete") {
      margin <- match.arg(margin)
      n <- if (margin == "row") nrow(x) else ncol(x)
      if (n <= 2L) {
        return(seq_len(n))
      }
      cmat <- suppressWarnings(if (margin == "row") stats::cor(t(x)) else stats::cor(x))
      d <- 1 - cmat
      d[!is.finite(d)] <- 1
      diag(d) <- 0
      stats::hclust(stats::as.dist(d), method = method)$order
    }

    margin <- match.arg(margin)
    groups <- droplevels(as.factor(groups))
    out <- lapply(levels(groups), function(level) {
      ii <- which(groups == level)
      if (length(ii) <= 2L) {
        return(ii)
      }
      if (length(ii) > max.items) {
        warning(
          "Skipping ", margin, " clustering for group `", level, "` with ", length(ii),
          " items; increase `cluster.max.items` to cluster it.",
          call. = FALSE
        )
        return(ii)
      }
      block <- if (margin == "row") x[ii, , drop = FALSE] else x[, ii, drop = FALSE]
      ii[safe_hclust_order(block, margin = margin, method = method)]
    })
    unlist(out, use.names = FALSE)
  }

  resolved <- p2$resolveMarkers(markers = markers, type = type)
  selected <- .pagoda2_select_marker_genes(
    resolved$tables,
    n.genes.per.group = n.genes.per.group,
    genes = genes,
    z.threshold = z.threshold,
    highest.only = highest.only,
    ordering = ordering,
    remove.duplicates = remove.duplicates
  )
  selected.genes <- selected$genes
  selected.groups <- unname(selected$groups)
  if (!is.null(additional.genes)) {
    additional.genes <- as.character(additional.genes)
    selected.genes <- c(selected.genes, additional.genes)
    selected.groups <- c(selected.groups, rep("additional", length(additional.genes)))
  }
  if (!is.null(exclude.genes)) {
    exclude.genes <- as.character(exclude.genes)
    keep <- !selected.genes %in% exclude.genes
    selected.genes <- selected.genes[keep]
    selected.groups <- selected.groups[keep]
  }
  available.genes <- .pagoda2_axis_names(p2, "gene")
  missing.genes <- setdiff(selected.genes, available.genes)
  if (length(missing.genes) > 0) {
    warning("Omitting marker genes absent from count matrix: ", paste(missing.genes, collapse = ", "))
  }
  keep <- selected.genes %in% available.genes
  selected.genes <- selected.genes[keep]
  selected.groups <- selected.groups[keep]
  if (length(selected.genes) == 0) {
    stop("No selected marker genes are present in count matrix")
  }

  if (is.null(grouping) && is.null(groups) && !is.null(resolved$result$grouping)) {
    grouping <- resolved$result$grouping
  }
  grouping.palette.name <- NULL
  if (!is.null(grouping) && is.character(grouping) && length(grouping) == 1L && grouping %in% colnames(p2$cellMeta)) {
    grouping.palette.name <- grouping
  } else if (is.character(groups) && length(groups) == 1L && is.null(names(groups)) && groups %in% colnames(p2$cellMeta)) {
    grouping.palette.name <- groups
  }
  resolved.groups <- p2$resolveGrouping(grouping = grouping, groups = groups, allow.missing = TRUE)
  cells <- intersect(names(resolved.groups)[!is.na(resolved.groups)], .pagoda2_axis_names(p2, "cell"))
  if (length(cells) == 0) {
    stop("No cells with non-missing groups are present in counts")
  }
  resolved.groups <- droplevels(resolved.groups[cells])
  if (is.finite(max.cells)) {
    sampled <- unlist(tapply(names(resolved.groups), resolved.groups, function(ii) {
      if (length(ii) > max.cells) sample(ii, max.cells) else ii
    }), use.names = FALSE)
    cells <- cells[cells %in% sampled]
    resolved.groups <- droplevels(resolved.groups[cells])
  }
  if (!is.null(cell.order)) {
    if (is.null(names(cell.order))) {
      cell.order <- as.character(cell.order)
    } else {
      cell.order <- names(cell.order)
    }
    cells <- cell.order[cell.order %in% cells]
    if (length(cells) == 0) {
      stop("`cell.order` does not contain any cells present in the heatmap")
    }
  } else {
    cells <- cells[order(resolved.groups[cells])]
  }
  resolved.groups <- droplevels(resolved.groups[cells])

  dense.entries <- length(selected.genes) * length(cells)
  if (is.finite(max.dense.entries) && dense.entries > max.dense.entries) {
    warning(
      "Marker heatmap will densify ", dense.entries, " expression values for plotting. ",
      "Consider reducing `n.genes.per.group`, supplying `genes`, or setting `max.cells`.",
      call. = FALSE
    )
  }

  x <- as.matrix(t(p2$getExpressionBlock(cells = cells, genes = selected.genes)))
  dimnames(x) <- list(selected.genes, cells)
  if (isTRUE(order.groups) && length(levels(resolved.groups)) > 1L) {
    xc <- do.call(cbind, tapply(seq_len(ncol(x)), resolved.groups[colnames(x)], function(ii) {
      rowMeans(x[, ii, drop = FALSE])
    }))
    group.order <- tryCatch({
      hc <- stats::hclust(stats::as.dist(2 - stats::cor(xc)), method = "ward.D2")
      hc$labels[hc$order]
    }, error = function(e) NULL)
    if (!is.null(group.order)) {
      resolved.groups <- factor(resolved.groups, levels = group.order)
      cells <- colnames(x)[order(resolved.groups[colnames(x)])]
      x <- x[, cells, drop = FALSE]
    }
  }
  if (averaging.window > 1) {
    x <- do.call(cbind, tapply(seq_len(ncol(x)), resolved.groups[colnames(x)], function(ii) {
      rollmean_matrix_cols(x[, ii, drop = FALSE], averaging.window)
    }))
  }
  x <- scale_heatmap_rows(x, expression.quantile = expression.quantile)
  x <- x[, cells[cells %in% colnames(x)], drop = FALSE]
  resolved.groups <- droplevels(resolved.groups[colnames(x)])

  row.groups <- factor(selected.groups, levels = unique(selected.groups))
  if (length(row.groups) != nrow(x)) {
    stop("Internal marker heatmap row grouping mismatch")
  }
  names(row.groups) <- rownames(x)

  if (isTRUE(cluster.rows)) {
    row.order <- order_within_groups(
      x,
      row.groups,
      margin = "row",
      max.items = cluster.max.items,
      method = cluster.method
    )
    x <- x[row.order, , drop = FALSE]
    row.groups <- row.groups[row.order]
  }
  if (isTRUE(cluster.columns)) {
    column.order <- order_within_groups(
      x,
      resolved.groups[colnames(x)],
      margin = "column",
      max.items = cluster.max.items,
      method = cluster.method
    )
    x <- x[, column.order, drop = FALSE]
    resolved.groups <- droplevels(resolved.groups[colnames(x)])
  }

  column.annotation <- data.frame(group = resolved.groups[colnames(x)], row.names = colnames(x))
  named.cell.metadata <- character()
  if (!is.null(column.metadata)) {
    if (is.character(column.metadata) && all(column.metadata %in% colnames(p2$cellMeta))) {
      metadata <- p2$resolveCellMeta(columns = column.metadata, cells = colnames(x), allow.missing = TRUE)
      named.cell.metadata <- column.metadata
    } else if (is.data.frame(column.metadata)) {
      metadata <- .pagoda2_align_metadata(column.metadata, colnames(x), axis = "cell")
    } else if (is.list(column.metadata)) {
      metadata <- data.frame(lapply(column.metadata, function(value) {
        .pagoda2_align_vector(value, colnames(x), what = "column metadata")
      }), check.names = FALSE)
      rownames(metadata) <- colnames(x)
    } else {
      stop("`column.metadata` must be cellMeta column names, a data.frame, or a named list")
    }
    column.annotation <- cbind(column.annotation, metadata)
  }
  if (is.null(column.metadata.colors)) {
    column.metadata.colors <- list()
  }
  if (!is.list(column.metadata.colors)) {
    stop("`column.metadata.colors` must be a list in annotation color format")
  }
  if (!is.null(group.colors)) {
    group.colors <- group.colors[levels(resolved.groups)]
    if (anyNA(group.colors) || is.null(names(group.colors))) {
      stop("`group.colors` must be a named color vector containing all displayed group levels")
    }
    column.metadata.colors$group <- group.colors
  }
  if (is.null(column.metadata.colors$group)) {
    if (!is.null(grouping.palette.name)) {
      column.metadata.colors$group <- p2$resolveFactorColors(
        axis = "cell",
        name = grouping.palette.name,
        values = resolved.groups,
        store = FALSE,
        s = s,
        v = v
      )
    } else {
      column.metadata.colors$group <- .pagoda2_discrete_palette(levels(resolved.groups), s = s, v = v)
    }
  } else {
    missing.colors <- setdiff(levels(resolved.groups), names(column.metadata.colors$group))
    if (length(missing.colors) > 0) {
      stop("`column.metadata.colors$group` is missing color(s) for: ", paste(missing.colors, collapse = ", "))
    }
    column.metadata.colors$group <- column.metadata.colors$group[levels(resolved.groups)]
  }
  for (nm in named.cell.metadata) {
    if (is.null(column.metadata.colors[[nm]]) && .pagoda2_is_discrete_grouping(column.annotation[[nm]])) {
      column.metadata.colors[[nm]] <- p2$resolveFactorColors(
        axis = "cell",
        name = nm,
        values = column.annotation[[nm]],
        store = FALSE,
        s = s,
        v = v
      )
    }
  }
  annotation.colors <- .pagoda2_heatmap_annotation_colors(
    column.annotation,
    color.list = column.metadata.colors,
    s = s,
    v = v
  )

  if (is.null(gene.group.colors)) {
    gene.group.colors <- annotation.colors$palettes$group[levels(row.groups)]
    if (anyNA(gene.group.colors) || length(gene.group.colors) != length(levels(row.groups))) {
      gene.group.colors <- .pagoda2_discrete_palette(levels(row.groups), s = s, v = v)
    }
  } else {
    gene.group.colors <- gene.group.colors[levels(row.groups)]
    if (anyNA(gene.group.colors) || is.null(names(gene.group.colors))) {
      stop("`gene.group.colors` must be a named color vector containing all displayed gene group levels")
    }
  }

  label.indices <- NULL
  if (!is.null(labeled.gene.subset)) {
    if (is.numeric(labeled.gene.subset)) {
      label.n <- as.integer(labeled.gene.subset[1])
      by.group <- split(seq_len(nrow(x)), row.groups)
      label.indices <- unique(unlist(lapply(by.group, utils::head, label.n), use.names = FALSE))
      labeled.gene.subset <- rownames(x)[label.indices]
    } else {
      labeled.gene.subset <- as.character(labeled.gene.subset)
      label.indices <- which(rownames(x) %in% labeled.gene.subset)
    }
  }

  .pagoda2_native_heatmap_spec(
    x,
    column.groups = resolved.groups,
    row.groups = row.groups,
    column.annotation = column.annotation,
    annotation.colors = annotation.colors,
    row.group.colors = gene.group.colors,
    expression.palette = pal,
    labeled.row.subset = labeled.gene.subset,
    label.indices = label.indices,
    show.row.groups = show.gene.groups,
    show.group.legend = show.group.legend,
    show_heatmap_legend = show_heatmap_legend,
    border = border,
    row.label.font.size = row.label.font.size,
    split = split,
    split.gap = split.gap,
    annotation.grobs = annotation.grobs,
    legend.max.levels = legend.max.levels,
    legend.columns = legend.columns,
    s = s,
    v = v,
    extra = list(
      type = type,
      marker.name = resolved$name,
      marker.result = resolved$result
    ),
    class = "pagoda2_marker_heatmap_spec"
  )
}

.pagoda2_render_marker_heatmap_complex <- function(spec, use.raster = TRUE, raster.by.magick = FALSE, ...) {
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE) || utils::packageVersion("ComplexHeatmap") < "2.4") {
    stop("ComplexHeatmap >= 2.4 is required for `engine = \"complex\"`; use `engine = \"native\"` or install ComplexHeatmap.")
  }
  if (sum(lengths(spec$annotation.grobs)) > 0L) {
    warning("`annotation.grobs` are currently rendered only by `engine = \"native\"`.", call. = FALSE)
  }
  x <- spec$matrix
  top.annotation <- ComplexHeatmap::HeatmapAnnotation(
    df = spec$column.annotation,
    col = spec$annotation.colors$palettes,
    border = spec$border,
    show_legend = spec$show.group.legend
  )
  row.annotation <- NULL
  if (isTRUE(spec$show.gene.groups) && !is.null(spec$gene.groups)) {
    row.annotation <- ComplexHeatmap::HeatmapAnnotation(
      marker_group = spec$gene.groups,
      which = "row",
      col = list(marker_group = spec$gene.group.colors),
      border = spec$border,
      show_annotation_name = FALSE,
      show_legend = FALSE
    )
  }
  heatmap.args <- list(
    matrix = x,
    name = "expression",
    col = spec$expression.palette,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_names = is.null(spec$labeled.gene.subset),
    show_column_names = FALSE,
    top_annotation = top.annotation,
    left_annotation = row.annotation,
    border = spec$border,
    show_heatmap_legend = spec$show_heatmap_legend,
    row_names_gp = grid::gpar(fontsize = spec$row.label.font.size),
    use_raster = use.raster,
    raster_by_magick = raster.by.magick
  )
  if (isTRUE(spec$split)) {
    heatmap.args$column_split <- spec$groups[colnames(x)]
    heatmap.args$row_split <- if (!is.null(spec$gene.groups)) spec$gene.groups else NULL
    heatmap.args$column_gap <- grid::unit(spec$split.gap, "mm")
    heatmap.args$row_gap <- grid::unit(spec$split.gap, "mm")
  }
  dots <- list(...)
  heatmap.args[names(dots)] <- dots
  ht <- do.call(ComplexHeatmap::Heatmap, heatmap.args)
  if (!is.null(spec$label.indices) && length(spec$label.indices) > 0L) {
    ht <- ht + ComplexHeatmap::rowAnnotation(
      link = ComplexHeatmap::anno_mark(
        at = spec$label.indices,
        labels = rownames(x)[spec$label.indices],
        labels_gp = grid::gpar(fontsize = spec$row.label.font.size)
      )
    )
  }
  ht
}

.pagoda2_marker_heatmap_details <- function(spec, heatmap = NULL, engine = NULL) {
  list(
    engine = engine,
    heatmap = heatmap,
    spec = spec,
    matrix = spec$matrix,
    groups = spec$groups,
    genes = rownames(spec$matrix),
    gene.groups = spec$gene.groups,
    column.annotation = spec$column.annotation,
    row.annotation = spec$gene.groups,
    labeled.gene.subset = spec$labeled.gene.subset,
    ha = heatmap,
    x = spec$matrix,
    annot = spec$column.annotation,
    rannot = spec$gene.groups
  )
}


#' @title Pagoda2 R6 class
#' @description The class encompasses gene count matrices, providing methods for normalization, calculating embeddings, and differential expression.
#' @param type string Data type (default='counts'). Currently only 'counts' supported.
#' @param n.cores numeric Number of cores to use (default=1)
#' @param n.odgenes integer Number of overdispersed genes to retrieve (default=NULL). If NULL, will return all.
#' @param verbose boolean Whether to give verbose output (default=TRUE)
#' @param batch fctor Batch factor for the dataset (default=NULL)
#' @param lib.sizes character vector of library sizes (default=NULL)
#' @param log.scale boolean If TRUE, scale counts by log() (default=TRUE)
#' @param min.cells.per.gene integer Minimum number of cells per gene, used to subset counts for coverage (default=0)
#' @param min.transcripts.per.cell integer Minimum number of transcripts per cells, used to subset counts for coverage (default=10)
#' @param keep.genes list of genes to keep in count matrix after filtering out by coverage but before normalization (default=NULL)
#' @param trim numeric Parameter used for winsorizing count data (default=round(min.cells.per.gene/2)). If value>0, will winsorize counts in normalized space in the hopes of getting a more stable depth estimates. If value<=0, ignored.
#' @param clusterType Optional cluster type to use as a group-defining factor (default=NULL)
#' @param groups factor named with cell names specifying the clusters of cells to be compared (one against all) (default=NULL). To compare two cell clusters against each other, simply pass a factor containing only two levels.
#' @param plot boolean Whether to output the plot (default=FALSE)
#'
#' @export Pagoda2
Pagoda2 <- R6::R6Class("Pagoda2", lock_objects=FALSE,
  public = list(
    #' @field rawCounts Raw count matrix on the current filtered axis, cell-by-gene.
    rawCounts = NULL,

    #' @field modelType string Model used to normalize count matrices. Supported values are 'raw' and 'plain'.
    #'     -- 'plain': Normalize by regressing out on the non-zero observations of each gene (default).
    #'     -- 'raw': Use the raw count matrices, without normalization. The expression matrix taken "as is" without normalization, although log.scale still applies. 
    #'     -- 'linearObs': Currently unavailable under matrix-view storage.
    modelType = NULL,

    #' @field clusters Results of clustering (default=list())
    clusters = list(),

    #' @field graphs Graph representations of the dataset (default=list())
    graphs = list(),

    #' @field reductions Results of reductions, e.g. PCA (default=list())
    reductions = list(),

    #' @field embeddings Results of visualization algorithms, t-SNE or largeVis (default=list())
    embeddings = list(),

    #' @field diffgenes Lists of differentially expressed genes (default=list())
    diffgenes = list(),

    #' @field markerResults Structured marker result registry keyed by matrix type and result name.
    markerResults = list(),

    #' @field matrixViews Lightweight expression matrix view recipes.
    matrixViews = list(),

    #' @field n.cores number of cores (default=1)
    n.cores = 1,

    #' @field misc list with additional info (default=list())
    misc = list(),
    
    #' @field batch Batch factor for the dataset (default=NULL)
    batch = NULL,

    #' @field genegraphs Slot to store graphical representations in gene space (i.e. gene kNN graphs) (default=list())
    genegraphs = list(),

	    #' @field depth Number of molecules measured per cell (default=NULL)
	    depth = NULL,

	    #' @field cellMeta Data frame with cell-axis metadata, rownames are cell IDs.
	    cellMeta = data.frame(row.names = character()),

	    #' @field geneMeta Data frame with gene-axis metadata, rownames are gene IDs.
	    geneMeta = data.frame(row.names = character()),

	    #' @field palettes Factor color maps keyed by metadata axis and column name.
	    palettes = list(cellMeta = list(), geneMeta = list()),

	    #' @field defaults Canonical names for default reductions, graphs, and embeddings.
	    defaults = list(reduction = "PCA", graph = "PCA", embedding = "UMAP"),

	    #' @field defaultGrouping Name of the default grouping column in cellMeta.
	    defaultGrouping = NULL,

	    #' @field clusterings Clustering provenance keyed by grouping name.
	    clusterings = list(),

	    #' @field history Workflow and result provenance.
	    history = list(),

	    #' @description Initialize Pagoda2 class
    #'
    #' @param x input count matrix
    #' @param modelType Model used to normalize count matrices (default='plain'). Supported values are 'raw' and 'plain'; 'linearObs' is currently unavailable under matrix-view storage.
    #' @examples
    #' \donttest{ 
    #' ## Load pre-generated a dataset of 50 bone marrow cells as matrix
    #' cm <- readRDS(system.file("extdata", "sample_BM1_50.rds", package="pagoda2"))
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts, log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' }
    #'
    #' @return new Pagoda2 object 
    initialize=function(x, modelType='plain', ## batchNorm='glm',
                        n.cores=parallel::detectCores(logical=FALSE), verbose=TRUE,
                        min.cells.per.gene=0, trim=round(min.cells.per.gene/2), 
                        min.transcripts.per.cell=10, batch=NULL,
                        lib.sizes=NULL, log.scale=TRUE, keep.genes=NULL) {

      if ('Pagoda2' %in% class(x)) { # copy constructor
        for (n in setdiff(ls(x), "counts")) {
          if (!is.function(get(n, x))) assign(n, get(n, x), self)
        }

        return()
      }
      
      self$n.cores <- n.cores
      self$batch <- batch
      self$misc <-list(lib.sizes=lib.sizes, log.scale=log.scale, model.type=modelType, trim=trim)
      self$modelType <- modelType

      ##if (!missing(x) && ('Pagoda2' %in% class(x))) { # copy constructor
      ##  super$initialize(x, ..., modelType=modelType, batchNorm=batchNorm, n.cores=n.cores)
      ##} else {
      ##  super$initialize(..., modelType=modelType, batchNorm=batchNorm, n.cores=n.cores,verbose=verbose)
      ##if (!missing(x) && is.null(counts)) { # interpret x as a countMatrix
      if ('matrix' %in% class(x)) {
        x <- as(Matrix(x, sparse=TRUE), "CsparseMatrix")
      }
      if (!('dgCMatrix' %in% class(x))) {
        stop("x is not of class dgCMatrix or matrix")
      }
      #if(any(x@x < 0)) {
      #  stop("x contains negative values")
      #}
	      self$setCountMatrix(x, min.cells.per.gene=min.cells.per.gene, trim=trim, 
	                     min.transcripts.per.cell=min.transcripts.per.cell, lib.sizes=lib.sizes,
	                     log.scale=log.scale, keep.genes=keep.genes, verbose=verbose)
	      ##}
	    },

	    #' @description Initialize cellMeta and geneMeta when missing.
	    #'
	    #' @return Invisibly returns self.
	    syncMetadata=function() {
	      matrix <- self$rawCounts
	      if (is.null(matrix)) {
	        matrix <- self$misc[['rawCounts']]
	      }
	      if (is.null(matrix)) {
	        return(invisible(self))
	      }
	      cells <- rownames(matrix)
	      genes <- colnames(matrix)
	      if (is.null(self$cellMeta) || (nrow(self$cellMeta) == 0 && ncol(self$cellMeta) == 0)) {
	        self$cellMeta <- data.frame(row.names = cells)
	      }
	      if (is.null(self$geneMeta) || (nrow(self$geneMeta) == 0 && ncol(self$geneMeta) == 0)) {
	        self$geneMeta <- data.frame(row.names = genes)
	      }
	      invisible(self)
	    },

	    #' @description Return the canonical raw count matrix.
	    #'
	    #' @param cells Optional cells to return.
	    #' @param genes Optional genes to return.
	    #' @param orientation Matrix orientation to return.
	    #' @return Sparse raw count matrix.
	    getRawCounts=function(cells=NULL, genes=NULL, orientation=c("cell_by_gene", "gene_by_cell")) {
	      orientation <- match.arg(orientation)
	      raw <- self$rawCounts
	      if (is.null(raw)) {
	        raw <- self$misc[['rawCounts']]
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
	    },

	    #' @description Return a matrix view recipe.
	    #'
	    #' @param name Matrix view name.
	    #' @return List describing the view recipe.
	    getMatrixView=function(name="analysis") {
	      view <- self$matrixViews[[name]]
	      if (is.null(view)) {
	        stop("Unknown matrix view `", name, "`")
	      }
	      view
	    },

	    #' @description Materialize a matrix view over selected cells and genes.
	    #'
	    #' @param name Matrix view name.
	    #' @param cells Optional cells to include.
	    #' @param genes Optional genes to include.
	    #' @param orientation Matrix orientation to return.
	    #' @return Sparse matrix for the requested view.
	    materializeView=function(name="analysis", cells=NULL, genes=NULL, orientation=c("cell_by_gene", "gene_by_cell")) {
	      orientation <- match.arg(orientation)
	      view <- self$getMatrixView(name)
	      raw <- self$getRawCounts(cells = cells, genes = genes)
	      x <- .pagoda2_materialize_view(raw, view)
	      if (orientation == "gene_by_cell") {
	        return(Matrix::t(x))
	      }
	      x
	    },

	    #' @description Alias for materializeView() using expression terminology.
	    #'
	    #' @param layer Matrix view name.
	    #' @param cells Optional cells to include.
	    #' @param genes Optional genes to include.
	    #' @param orientation Matrix orientation to return.
	    #' @param scale.variance Whether to apply stored gene variance scale factors.
	    #' @return Sparse matrix for the requested expression block.
	    getExpressionBlock=function(layer="analysis", cells=NULL, genes=NULL,
	                                orientation=c("cell_by_gene", "gene_by_cell"),
	                                scale.variance=FALSE) {
	      orientation <- match.arg(orientation)
	      x <- self$materializeView(name = layer, cells = cells, genes = genes, orientation = "cell_by_gene")
	      if (isTRUE(scale.variance)) {
	        x <- .pagoda2_apply_variance_scaling(x, self$misc[['varinfo']])
	      }
	      if (orientation == "gene_by_cell") {
	        return(Matrix::t(x))
	      }
	      x
	    },

	    #' @description Calculate column means and variances for a matrix view without materializing it.
	    #'
	    #' @param name Matrix view name.
	    #' @param cells Optional cells to include.
	    #' @param n.cores Number of threads for the sparse kernel.
	    #' @return data.frame with m, v, and nobs columns.
	    viewColMeanVar=function(name="analysis", cells=NULL, n.cores=self$n.cores) {
	      raw <- self$getRawCounts()
	      view <- self$getMatrixView(name)
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
	    },

	    #' @description Calculate grouping-stratified column sums for a matrix view without materializing it.
	    #'
	    #' @param grouping Name of a discrete cellMeta column. NULL uses defaultGrouping.
	    #' @param groups Direct vector of group labels. Mutually exclusive with grouping.
	    #' @param name Matrix view name.
	    #' @param cells Optional cells to include.
	    #' @return Matrix with one row for NA values followed by factor levels present in groups.
	    viewColSumByFac=function(grouping=NULL, groups=NULL, name="analysis", cells=NULL) {
	      raw <- self$getRawCounts()
	      selected <- .pagoda2_cell_selection_mask(cells, rownames(raw), what = "cells")
	      if (!is.null(selected)) {
	        raw <- raw[selected, , drop = FALSE]
	      }
	      view <- self$getMatrixView(name)
	      cols <- self$resolveGrouping(
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
	    },

	    #' @description Validate current matrix storage invariants.
	    #'
	    #' @param stop.on.error Whether to throw on invalid storage.
	    #' @return Logical TRUE when valid, otherwise FALSE if stop.on.error=FALSE.
	    validateMatrices=function(stop.on.error=TRUE) {
	      errors <- character()
	      raw <- self$rawCounts
	      if (is.null(raw)) {
	        raw <- self$misc[['rawCounts']]
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
	      if (!is.null(self$depth) && !is.null(raw)) {
	        if (length(self$depth) != nrow(raw) || !identical(names(self$depth), rownames(raw))) {
	          errors <- c(errors, "depth is not named on the rawCounts cell axis")
	        }
	      }
	      if (!is.null(self$batch) && !is.null(raw)) {
	        if (length(self$batch) != nrow(raw) || !identical(names(self$batch), rownames(raw))) {
	          errors <- c(errors, "batch is not named on the rawCounts cell axis")
	        }
	      }
	      view <- self$matrixViews$analysis
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
	    },

	    #' @description Describe stored expression matrices.
	    #'
	    #' @return data.frame with matrix metadata.
	    describeMatrices=function() {
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
	      entries[["raw"]] <- add_entry("raw", "raw_counts", self$rawCounts)
	      entries[["analysis"]] <- add_view_entry("analysis", "analysis_view", self$matrixViews$analysis, self$rawCounts)
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
	    },

	    #' @description Set cell-axis metadata.
	    #'
	    #' @param metadata data.frame-like metadata or a single column name.
	    #' @param value vector of values when metadata is a column name.
	    #' @param overwrite Whether to overwrite existing columns.
	    #' @return Invisibly returns self.
	    setCellMeta=function(metadata, value=NULL, overwrite=TRUE) {
	      cells <- .pagoda2_axis_names(self, "cell")
	      self$syncMetadata()
	      if (is.character(metadata) && length(metadata) == 1 && !is.null(value)) {
	        metadata <- .pagoda2_vector_metadata(metadata, value, target = cells, axis = "cell")
	      } else {
	        if (!is.null(value)) {
	          stop("`value` can only be supplied when `metadata` is a single column name")
	        }
	      }
	      self$cellMeta <- .pagoda2_store_metadata(self$cellMeta, metadata, target = cells, axis = "cell", overwrite = overwrite)
	      invisible(self)
	    },

	    #' @description Get cell-axis metadata.
	    #'
	    #' @param columns Optional metadata columns to return.
	    #' @param resolved Whether to resolve metadata onto the current cells.
	    #' @param cells Optional cell names to resolve onto when resolved=TRUE.
	    #' @param allow.missing Whether missing cells are allowed when resolved=TRUE.
	    #' @return data.frame of cell metadata.
	    getCellMeta=function(columns=NULL, resolved=FALSE, cells=NULL, allow.missing=TRUE) {
	      self$syncMetadata()
	      if (isTRUE(resolved)) {
	        return(self$resolveCellMeta(columns = columns, cells = cells, allow.missing = allow.missing))
	      }
	      if (!is.null(cells)) {
	        stop("`cells` can only be supplied when `resolved = TRUE`")
	      }
	      if (is.null(columns)) {
	        return(self$cellMeta)
	      }
	      missing <- setdiff(columns, colnames(self$cellMeta))
	      if (length(missing) > 0) {
	        stop("Unknown cell metadata column(s): ", paste(missing, collapse = ", "))
	      }
	      self$cellMeta[, columns, drop = FALSE]
	    },

	    #' @description Set gene-axis metadata.
	    #'
	    #' @param metadata data.frame-like metadata or a single column name.
	    #' @param value vector of values when metadata is a column name.
	    #' @param overwrite Whether to overwrite existing columns.
	    #' @return Invisibly returns self.
	    setGeneMeta=function(metadata, value=NULL, overwrite=TRUE) {
	      genes <- .pagoda2_axis_names(self, "gene")
	      self$syncMetadata()
	      if (is.character(metadata) && length(metadata) == 1 && !is.null(value)) {
	        metadata <- .pagoda2_vector_metadata(metadata, value, target = genes, axis = "gene")
	      } else {
	        if (!is.null(value)) {
	          stop("`value` can only be supplied when `metadata` is a single column name")
	        }
	      }
	      self$geneMeta <- .pagoda2_store_metadata(self$geneMeta, metadata, target = genes, axis = "gene", overwrite = overwrite)
	      invisible(self)
	    },

	    #' @description Get gene-axis metadata.
	    #'
	    #' @param columns Optional metadata columns to return.
	    #' @param resolved Whether to resolve metadata onto the current genes.
	    #' @param genes Optional gene names to resolve onto when resolved=TRUE.
	    #' @param allow.missing Whether missing genes are allowed when resolved=TRUE.
	    #' @return data.frame of gene metadata.
	    getGeneMeta=function(columns=NULL, resolved=FALSE, genes=NULL, allow.missing=TRUE) {
	      self$syncMetadata()
	      if (isTRUE(resolved)) {
	        return(self$resolveGeneMeta(columns = columns, genes = genes, allow.missing = allow.missing))
	      }
	      if (!is.null(genes)) {
	        stop("`genes` can only be supplied when `resolved = TRUE`")
	      }
	      if (is.null(columns)) {
	        return(self$geneMeta)
	      }
	      missing <- setdiff(columns, colnames(self$geneMeta))
	      if (length(missing) > 0) {
	        stop("Unknown gene metadata column(s): ", paste(missing, collapse = ", "))
	      }
	      self$geneMeta[, columns, drop = FALSE]
	    },

	    #' @description Set a discrete metadata palette.
	    #'
	    #' @param name Metadata column name.
	    #' @param colors Named color vector keyed by metadata level.
	    #' @param axis Metadata axis: cell/cellMeta or gene/geneMeta.
	    #' @param palette.id Optional semantic palette identifier.
	    #' @return Invisibly returns self.
	    setPalette=function(name, colors, axis=c("cell", "gene", "cellMeta", "geneMeta"), palette.id=name) {
	      axis <- .pagoda2_palette_axis_key(axis)
	      if (!is.character(name) || length(name) != 1L || is.na(name) || name == "") {
	        stop("`name` must be a single non-empty metadata column name")
	      }
	      entry <- .pagoda2_normalize_palette_entry(colors, name = name, axis = axis, palette.id = palette.id)
	      if (is.null(self$palettes) || !is.list(self$palettes)) {
	        self$palettes <- list(cellMeta = list(), geneMeta = list())
	      }
	      if (is.null(self$palettes[[axis]])) {
	        self$palettes[[axis]] <- list()
	      }
	      self$palettes[[axis]][[name]] <- entry
	      invisible(self)
	    },

	    #' @description Get a stored metadata palette.
	    #'
	    #' @param name Metadata column name.
	    #' @param axis Metadata axis: cell/cellMeta or gene/geneMeta.
	    #' @param colors.only Whether to return only the named color vector.
	    #' @return Palette entry or named color vector. NULL when unset.
	    getPalette=function(name, axis=c("cell", "gene", "cellMeta", "geneMeta"), colors.only=FALSE) {
	      axis <- .pagoda2_palette_axis_key(axis)
	      entry <- NULL
	      if (!is.null(self$palettes) && !is.null(self$palettes[[axis]])) {
	        entry <- self$palettes[[axis]][[name]]
	      }
	      if (isTRUE(colors.only)) {
	        return(if (is.null(entry)) NULL else entry$colors)
	      }
	      entry
	    },

	    #' @description Resolve factor colors using the stored metadata palette or sccore defaults.
	    #'
	    #' @param axis Metadata axis: cell/cellMeta or gene/geneMeta.
	    #' @param name Optional metadata column name for palette storage.
	    #' @param values Factor/vector values to color. If NULL, values are read from metadata column `name`.
	    #' @param colors Optional explicit named color vector. Overrides stored/generated colors for this resolution.
	    #' @param store Whether to store/update a generated palette for named metadata.
	    #' @param s Saturation passed to sccore::fac2col() for generated colors.
	    #' @param v Value passed to sccore::fac2col() for generated colors.
	    #' @return Named level-to-color vector.
	    resolveFactorColors=function(axis=c("cell", "gene", "cellMeta", "geneMeta"),
	                                 name=NULL, values=NULL, colors=NULL,
	                                 store=FALSE, s=1, v=1) {
	      axis <- .pagoda2_palette_axis_key(axis)
	      self$syncMetadata()
	      if (is.null(self$palettes) || !is.list(self$palettes)) {
	        self$palettes <- list(cellMeta = list(), geneMeta = list())
	      }
	      if (is.null(self$palettes[[axis]])) {
	        self$palettes[[axis]] <- list()
	      }
	      metadata.values <- NULL
	      if (!is.null(name)) {
	        metadata <- if (axis == "cellMeta") self$cellMeta else self$geneMeta
	        if (name %in% colnames(metadata)) {
	          metadata.values <- metadata[[name]]
	          names(metadata.values) <- rownames(metadata)
	        }
	      }
	      if (is.null(values)) {
	        if (is.null(name)) {
	          stop("Supply `values` or a metadata column `name`")
	        }
	        if (is.null(metadata.values)) {
	          stop("Unknown ", axis, " column `", name, "`")
	        }
	        values <- metadata.values
	      } else if (!is.null(metadata.values) && .pagoda2_is_discrete_grouping(metadata.values)) {
	        full.levels <- levels(.pagoda2_as_grouping(metadata.values, name = name))
	        values <- stats::setNames(factor(as.character(values), levels = full.levels), names(values))
	      }
	      values <- as.factor(values)
	      if (nlevels(values) == 0L) {
	        return(stats::setNames(character(), character()))
	      }

	      if (!is.null(colors)) {
	        entry <- .pagoda2_normalize_palette_entry(colors, name = if (is.null(name)) "groups" else name,
	                                                  axis = axis, values = values, generated.by = "user")
	        resolved <- entry$colors[levels(values)]
	        if (anyNA(resolved)) {
	          missing <- levels(values)[is.na(resolved)]
	          stop("Palette is missing color(s) for level(s): ", paste(missing, collapse = ", "))
	        }
	        if (isTRUE(store) && !is.null(name)) {
	          self$setPalette(name, entry$colors, axis = axis, palette.id = entry$palette.id)
	        }
	        return(resolved)
	      }

	      existing <- if (!is.null(name)) self$getPalette(name, axis = axis, colors.only = TRUE) else NULL
	      resolved <- .pagoda2_extend_palette_colors(existing = existing, values = values, s = s, v = v)
	      if (isTRUE(store) && !is.null(name)) {
	        entry <- .pagoda2_normalize_palette_entry(
	          resolved,
	          name = name,
	          axis = axis,
	          values = values,
	          palette.id = name,
	          generated.by = "sccore::fac2col"
	        )
	        self$palettes[[axis]][[name]] <- entry
	      }
	      resolved
	    },

	    #' @description Resolve cell-axis metadata onto specified cells.
	    #'
	    #' @param columns Optional metadata columns to return.
	    #' @param cells Optional cell names. NULL uses current count matrix cells.
	    #' @param allow.missing Whether missing cells are allowed.
	    #' @return data.frame aligned to cells.
	    resolveCellMeta=function(columns=NULL, cells=NULL, allow.missing=TRUE) {
	      self$syncMetadata()
	      if (is.null(cells)) {
	        cells <- .pagoda2_axis_names(self, "cell")
	      }
	      metadata <- self$cellMeta
	      if (!is.null(columns)) {
	        missing <- setdiff(columns, colnames(metadata))
	        if (length(missing) > 0) {
	          stop("Unknown cell metadata column(s): ", paste(missing, collapse = ", "))
	        }
	        metadata <- metadata[, columns, drop = FALSE]
	      }
	      missing.rows <- setdiff(cells, rownames(metadata))
	      resolved <- .pagoda2_align_metadata(metadata, cells, axis = "cell")
	      missing.values <- if (ncol(resolved) == 0) FALSE else !stats::complete.cases(resolved)
	      if (!allow.missing && (length(missing.rows) > 0 || any(missing.values))) {
	        stop("Cell metadata is missing values for ", sum(missing.values), " cell(s)")
	      }
	      resolved
	    },

	    #' @description Resolve gene-axis metadata onto specified genes.
	    #'
	    #' @param columns Optional metadata columns to return.
	    #' @param genes Optional gene names. NULL uses current count matrix genes.
	    #' @param allow.missing Whether missing genes are allowed.
	    #' @return data.frame aligned to genes.
	    resolveGeneMeta=function(columns=NULL, genes=NULL, allow.missing=TRUE) {
	      self$syncMetadata()
	      if (is.null(genes)) {
	        genes <- .pagoda2_axis_names(self, "gene")
	      }
	      metadata <- self$geneMeta
	      if (!is.null(columns)) {
	        missing <- setdiff(columns, colnames(metadata))
	        if (length(missing) > 0) {
	          stop("Unknown gene metadata column(s): ", paste(missing, collapse = ", "))
	        }
	        metadata <- metadata[, columns, drop = FALSE]
	      }
	      missing.rows <- setdiff(genes, rownames(metadata))
	      resolved <- .pagoda2_align_metadata(metadata, genes, axis = "gene")
	      missing.values <- if (ncol(resolved) == 0) FALSE else !stats::complete.cases(resolved)
	      if (!allow.missing && (length(missing.rows) > 0 || any(missing.values))) {
	        stop("Gene metadata is missing values for ", sum(missing.values), " gene(s)")
	      }
	      resolved
	    },

	    #' @description Store a discrete cell grouping as a cellMeta column.
	    #'
	    #' @param name Name of the grouping column.
	    #' @param groups Vector or factor of cell group labels.
	    #' @param source Optional source/provenance label.
	    #' @param setDefault Whether to make this the default grouping.
	    #' @param overwrite Whether to overwrite an existing grouping.
	    #' @return Invisibly returns self.
	    setGrouping=function(name, groups, source=NULL, setDefault=FALSE, overwrite=FALSE) {
	      if (!is.character(name) || length(name) != 1 || is.na(name) || name == "") {
	        stop("`name` must be a single non-empty string")
	      }
	      cells <- .pagoda2_axis_names(self, "cell")
	      groups <- .pagoda2_align_vector(groups, cells, what = paste0("grouping `", name, "`"))
	      groups <- .pagoda2_as_grouping(groups, name = name)
	      self$setCellMeta(name, groups, overwrite = overwrite)
	      if (is.null(self$history$groupings)) {
	        self$history$groupings <- list()
	      }
	      self$history$groupings[[name]] <- list(
	        source = source,
	        created = Sys.time(),
	        n.groups = length(levels(groups))
	      )
	      if (setDefault) {
	        self$setDefaultGrouping(name)
	      }
	      invisible(self)
	    },

	    #' @description Set the default grouping used when grouping is omitted.
	    #'
	    #' @param grouping Name of a discrete cellMeta column.
	    #' @return Invisibly returns self.
	    setDefaultGrouping=function(grouping) {
	      self$syncMetadata()
	      if (!is.character(grouping) || length(grouping) != 1 || is.na(grouping) || grouping == "") {
	        stop("`grouping` must be a single non-empty string")
	      }
	      if (!grouping %in% colnames(self$cellMeta)) {
	        stop("Unknown cell metadata column `", grouping, "`")
	      }
	      self$resolveGrouping(grouping = grouping, allow.missing = TRUE)
	      self$defaultGrouping <- grouping
	      invisible(self)
	    },

	    #' @description Get the current default grouping name.
	    #'
	    #' @return Default grouping name, or NULL if unset.
	    getDefaultGrouping=function() {
	      self$defaultGrouping
	    },

	    #' @description Calculate cell QC metrics and store them in cellMeta.
	    #'
	    #' @param method QC method. `gene_molecule` models detected genes versus molecule counts.
	    #' @param overwrite Whether to overwrite existing QC columns.
	    #' @param matrix Optional cell-by-gene matrix. Defaults to rawCounts when available.
	    #' @param min.molecules Minimum molecule count for a passing cell.
	    #' @param max.molecules Maximum molecule count for a passing cell.
	    #' @param p.level Two-sided outlier level for gene/molecule trend residuals.
	    #' @param verbose Whether to emit a succinct QC summary.
	    #' @return data.frame of QC metrics.
	    runQC=function(method=c("gene_molecule", "metrics"), overwrite=FALSE, matrix=NULL,
	                   min.molecules=500, max.molecules=5e4, p.level=NULL, verbose=FALSE) {
	      method <- match.arg(method)
	      if (is.null(matrix)) {
	        matrix <- self$rawCounts
	      }
	      if (is.null(matrix)) {
	        matrix <- self$misc[['rawCounts']]
	      }
	      if (is.null(matrix)) {
	        stop("Cannot run QC before counts are initialized")
	      }
	      qc.cols <- if (method == "metrics") {
	        c("n_molecules", "n_genes")
	      } else {
	        c(
	          "n_molecules", "n_genes", "qc_log_molecules", "qc_log_genes",
	          "qc_gene_molecule_fitted", "qc_gene_molecule_lower", "qc_gene_molecule_upper",
	          "qc_gene_molecule_residual", "qc_gene_molecule_z", "qc_size_outlier",
	          "qc_gene_molecule_outlier", "qc_pass"
	        )
	      }
	      if (!overwrite && all(qc.cols %in% colnames(self$cellMeta))) {
	        qc <- self$getCellMeta(qc.cols)
	        if (isTRUE(verbose)) {
	          message(.pagoda2_qc_summary(qc))
	        }
	        return(invisible(qc))
	      }
	      qc <- if (method == "metrics") {
	        data.frame(
	          n_molecules = as.numeric(Matrix::rowSums(matrix)),
	          n_genes = as.numeric(Matrix::rowSums(matrix > 0)),
	          row.names = rownames(matrix)
	        )
	      } else {
	        .pagoda2_qc_gene_molecule(
	          matrix,
	          min.molecules = min.molecules,
	          max.molecules = max.molecules,
	          p.level = p.level
	        )
	      }
	      self$setCellMeta(qc, overwrite = TRUE)
	      self$history$qc <- attr(qc, "pagoda2.qc")
	      if (isTRUE(verbose)) {
	        message(.pagoda2_qc_summary(qc))
	      }
	      invisible(qc)
	    },

	    #' @description Plot cell QC metrics.
	    #'
	    #' @param run.qc Whether to run QC automatically when QC columns are absent.
	    #' @param ... Arguments passed to runQC() if QC needs to be calculated.
	    #' @return ggplot object.
	    plotQC=function(run.qc=TRUE, ...) {
	      if (!requireNamespace("ggplot2", quietly = TRUE)) {
	        stop("Package `ggplot2` is required for plotQC()")
	      }
	      self$syncMetadata()
	      if (!all(c("n_molecules", "n_genes", "qc_pass") %in% colnames(self$cellMeta))) {
	        if (!isTRUE(run.qc)) {
	          stop("QC metrics are missing; call p2$runQC() first or set run.qc = TRUE")
	        }
	        self$runQC(...)
	      }
	      qc <- self$resolveCellMeta(
	        columns = intersect(
	          c("n_molecules", "n_genes", "qc_log_molecules", "qc_log_genes", "qc_pass", "qc_gene_molecule_fitted", "qc_gene_molecule_lower", "qc_gene_molecule_upper"),
	          colnames(self$cellMeta)
	        )
	      )
	      if (!"qc_log_molecules" %in% colnames(qc)) {
	        qc$qc_log_molecules <- ifelse(qc$n_molecules > 0, log10(qc$n_molecules), NA_real_)
	      }
	      if (!"qc_log_genes" %in% colnames(qc)) {
	        qc$qc_log_genes <- ifelse(qc$n_genes > 0, log10(qc$n_genes), NA_real_)
	      }
	      qc$qc_pass <- as.factor(ifelse(qc$qc_pass, "pass", "filter"))
	      qc$panel <- factor("Gene/molecule trend", levels = c("Molecule distribution", "Gene/molecule trend"))
	      hist.qc <- qc[is.finite(qc$n_molecules) & qc$n_molecules > 0, , drop = FALSE]
	      hist.qc$panel <- factor("Molecule distribution", levels = levels(qc$panel))
	      p <- ggplot2::ggplot() +
	        ggplot2::geom_histogram(
	          data = hist.qc,
	          ggplot2::aes(x = qc_log_molecules),
	          bins = 60,
	          fill = "wheat",
	          color = "grey45",
	          linewidth = 0.25
	        ) +
	        ggplot2::geom_point(
	          data = qc,
	          ggplot2::aes(x = qc_log_molecules, y = qc_log_genes, color = qc_pass),
	          size = 0.35,
	          alpha = 0.45
	        ) +
	        ggplot2::scale_color_manual(
	          values = c(pass = "grey35", filter = "firebrick3"),
	          name = "QC",
	          guide = ggplot2::guide_legend(override.aes = list(size = 3, alpha = 1))
	        ) +
	        ggplot2::facet_wrap(~ panel, nrow = 1, scales = "free_y") +
	        ggplot2::theme_bw() +
	        ggplot2::theme(
	          legend.key.size = grid::unit(4.5, "mm"),
	          strip.background = ggplot2::element_rect(fill = "grey92", color = "grey55")
	        ) +
	        ggplot2::labs(x = "log10 molecules per cell", y = "Count / log10 detected genes")
	      thresholds <- c(self$history$qc$min.molecules, self$history$qc$max.molecules)
	      thresholds <- thresholds[is.finite(thresholds) & thresholds > 0]
	      if (length(thresholds) > 0) {
	        threshold.df <- expand.grid(
	          qc_log_molecules = log10(thresholds),
	          panel = levels(qc$panel),
	          stringsAsFactors = FALSE
	        )
	        threshold.df$panel <- factor(threshold.df$panel, levels = levels(qc$panel))
	        p <- p + ggplot2::geom_vline(
	          data = threshold.df,
	          ggplot2::aes(xintercept = qc_log_molecules),
	          color = "firebrick3",
	          linetype = "dashed",
	          linewidth = 0.45
	        )
	      }
	      if (all(c("qc_gene_molecule_fitted", "qc_gene_molecule_lower", "qc_gene_molecule_upper") %in% colnames(qc)) &&
	          any(is.finite(qc$qc_gene_molecule_fitted))) {
	        fit <- qc[is.finite(qc$qc_gene_molecule_fitted) & qc$n_molecules > 0, , drop = FALSE]
	        fit <- fit[order(fit$n_molecules), , drop = FALSE]
	        p <- p +
	          ggplot2::geom_ribbon(
	            data = fit,
	            ggplot2::aes(x = qc_log_molecules, ymin = qc_gene_molecule_lower, ymax = qc_gene_molecule_upper),
	            inherit.aes = FALSE,
	            fill = "firebrick3",
	            alpha = 0.10
	          ) +
	          ggplot2::geom_line(
	            data = fit,
	            ggplot2::aes(x = qc_log_molecules, y = qc_gene_molecule_fitted),
	            inherit.aes = FALSE,
	            color = "firebrick3",
	            linewidth = 0.6
	          )
	      }
	      p
	    },

	    #' @description Filter cells using QC decisions or an explicit cell subset.
	    #'
	    #' @param cells Optional explicit cells to keep. NULL uses `pass.column`.
	    #' @param pass.column Cell metadata column containing TRUE/FALSE QC decisions.
	    #' @param run.qc Whether to run QC automatically if `pass.column` is missing.
	    #' @param force Whether to allow filtering after downstream results exist.
	    #' @param verbose Whether to emit progress messages.
	    #' @param ... Arguments passed to runQC() when QC needs to be calculated.
	    #' @return Invisibly returns self.
	    filterCells=function(cells=NULL, pass.column="qc_pass", run.qc=TRUE, force=FALSE, verbose=FALSE, ...) {
	      self$syncMetadata()
	      raw <- self$getRawCounts()
	      if (is.null(cells)) {
	        if (!pass.column %in% colnames(self$cellMeta)) {
	          if (!isTRUE(run.qc)) {
	            stop("Cell metadata column `", pass.column, "` is missing; call p2$runQC() first or set run.qc = TRUE")
	          }
	          if (isTRUE(verbose)) {
	            message("QC metrics not found; running runQC() with default settings.")
	          }
	          self$runQC(verbose = verbose, ...)
	        }
	        qc <- self$resolveCellMeta(pass.column)
	        keep <- qc[[pass.column]]
	        keep[is.na(keep)] <- FALSE
	        cells <- rownames(qc)[as.logical(keep)]
	      } else {
	        cells <- rownames(raw)[.pagoda2_axis_selection_index(cells, rownames(raw), what = "cell(s)")]
	      }
	      if (length(cells) < 3L) {
	        stop("Filtering would leave fewer than 3 cells")
	      }
	      if (identical(cells, rownames(raw))) {
	        return(invisible(self))
	      }
	      if (.pagoda2_has_downstream_results(self) && !isTRUE(force)) {
	        stop("Filtering cells would invalidate existing reductions, graphs, embeddings, clusterings, or markers. ",
	             "Create a fresh object or call filterCells(force = TRUE).")
	      }
	      removed <- setdiff(rownames(raw), cells)
	      old.clusterings <- names(self$clusterings)
	      self$rawCounts <- raw[cells, , drop = FALSE]
	      self$misc[['rawCounts']] <- self$rawCounts
	      if (!is.null(self$depth)) {
	        self$depth <- self$depth[cells]
	      }
	      if (!is.null(self$batch)) {
	        self$batch <- droplevels(self$batch[cells])
	      }
	      if (!is.null(self$matrixViews$analysis)) {
	        self$matrixViews$analysis <- .pagoda2_filter_analysis_view_cells(self$matrixViews$analysis, cells)
	      }
	      self$reductions <- list()
	      self$graphs <- list()
	      self$embeddings <- list()
	      self$diffgenes <- list()
	      self$markerResults <- list()
	      self$clusterings <- list()
	      self$clusters <- list()
	      self$genegraphs <- list()
	      self$misc[['varinfo']] <- NULL
	      self$misc[['odgenes']] <- NULL
	      self$misc[['rescaled.mat']] <- NULL
	      if (!is.null(self$defaultGrouping) && self$defaultGrouping %in% old.clusterings) {
	        self$defaultGrouping <- NULL
	      }
	      if (is.null(self$history$filters)) {
	        self$history$filters <- list()
	      }
	      self$history$filters[[length(self$history$filters) + 1L]] <- list(
	        method = if (is.null(pass.column)) "explicit" else pass.column,
	        kept = length(cells),
	        removed = length(removed),
	        removed.cells = removed,
	        time = Sys.time()
	      )
	      invisible(self)
	    },

	    #' @description Run the canonical pagoda2.1 single-dataset workflow.
	    #'
	    #' @param steps Optional workflow steps to run.
	    #' @param skip Optional workflow steps to exclude.
	    #' @param dependencies Whether to add missing dependencies automatically or error.
	    #' @param overwrite Whether to recompute existing canonical outputs.
	    #' @param profile Interaction profile: interactive, pipeline, or report.
	    #' @param plots Plot behavior: show, none, or collect.
	    #' @param verbose Whether to emit progress messages.
	    #' @param qc Step-specific argument list for runQC().
	    #' @param variance Step-specific argument list for runVariance().
	    #' @param pca Step-specific argument list for runPCA().
	    #' @param graph Step-specific argument list for runGraph().
	    #' @param umap Step-specific argument list for runUMAP().
	    #' @param leiden Step-specific argument list for runLeiden().
	    #' @param markers Step-specific argument list for runMarkers().
	    #' @return Invisibly returns self.
	    run=function(steps=NULL, skip=NULL, dependencies=c("auto", "error"), overwrite=FALSE,
	                 profile=c("interactive", "pipeline", "report"), plots=NULL,
	                 verbose=FALSE, qc=list(), variance=list(), pca=list(), graph=list(), umap=list(),
	                 leiden=list(), markers=list()) {
	      dependencies <- match.arg(dependencies)
	      profile <- match.arg(profile)
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
	        plots <- switch(profile, interactive = "show", pipeline = "none", report = "collect")
	      }
	      if (!plots %in% c("show", "none", "collect")) {
	        stop("`plots` must be one of show, none, or collect")
	      }
	      verbose.default <- isTRUE(verbose)
	      show.plots <- identical(plots, "show")

	      if (is.null(self$history$runs)) {
	        self$history$runs <- list()
	      }
	      run.id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", length(self$history$runs) + 1)
	      step.records <- list()
	      record_step <- function(step, status, params=list(), elapsed=NA_real_, message=NULL) {
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
	        if (!overwrite && all(expected.qc.cols %in% colnames(self$cellMeta))) {
	          skip_step("qc", args, "QC metrics already exist")
	        } else {
	          run_step("qc", args, do.call(self$runQC, args))
	        }
	        if ("qc_pass" %in% colnames(self$cellMeta)) {
	          qc.meta <- self$resolveCellMeta("qc_pass")
	          n.fail <- sum(!as.logical(qc.meta$qc_pass), na.rm = TRUE)
	          if (filter.after.qc) {
	            run_step("filter", list(pass.column = "qc_pass", verbose = verbose.default), self$filterCells(pass.column = "qc_pass", verbose = verbose.default))
	          } else if (n.fail > 0L) {
	            warning(
	              n.fail, " cell(s) did not pass QC. ",
	              "Call p2$plotQC() to inspect them and p2$filterCells() to filter, ",
	              "or run p2$run(qc = list(filter = TRUE)) to filter before analysis.",
	              call. = FALSE
	            )
	          }
	        }
	      }

	      if ("variance" %in% resolved.steps) {
	        args <- .pagoda2_step_args(variance, list(plot = show.plots, verbose = verbose.default))
	        if (!overwrite && !is.null(self$misc[['varinfo']])) {
	          skip_step("variance", args, "variance model already exists")
	        } else {
	          run_step("variance", args, do.call(self$runVariance, args))
	        }
	      }

	      if ("pca" %in% resolved.steps) {
	        pca.name <- if (!is.null(pca$name)) pca$name else self$defaults$reduction
	        args <- .pagoda2_step_args(pca, list(name = pca.name, verbose = verbose.default))
	        if (!overwrite && !is.null(self$reductions[[args$name]])) {
	          skip_step("pca", args, paste0("reduction `", args$name, "` already exists"))
	        } else {
	          run_step("pca", args, do.call(self$runPCA, args))
	        }
	      }

	      if ("graph" %in% resolved.steps) {
	        graph.reduction <- if (!is.null(graph$reduction)) graph$reduction else self$defaults$reduction
	        args <- .pagoda2_step_args(graph, list(reduction = graph.reduction, verbose = verbose.default))
	        if (!overwrite && !is.null(self$graphs[[args$reduction]])) {
	          skip_step("graph", args, paste0("graph `", args$reduction, "` already exists"))
	        } else {
	          run_step("graph", args, do.call(self$runGraph, args))
	        }
	      }

	      if ("umap" %in% resolved.steps) {
	        umap.reduction <- if (!is.null(umap$reduction)) umap$reduction else self$defaults$reduction
	        umap.name <- if (!is.null(umap$name)) umap$name else self$defaults$embedding
	        args <- .pagoda2_step_args(umap, list(reduction = umap.reduction, name = umap.name, verbose = verbose.default))
	        if (!overwrite && !is.null(self$embeddings[[args$reduction]]) && !is.null(self$embeddings[[args$reduction]][[args$name]])) {
	          skip_step("umap", args, paste0("embedding `", args$reduction, "/", args$name, "` already exists"))
	        } else {
	          run_step("umap", args, do.call(self$runUMAP, args))
	        }
	      }

	      if ("leiden" %in% resolved.steps) {
	        leiden.name <- if (!is.null(leiden$name)) leiden$name else "leiden"
	        args <- .pagoda2_step_args(leiden, list(name = leiden.name, setDefault = TRUE, overwrite = overwrite))
	        if (!overwrite && leiden.name %in% colnames(self$cellMeta)) {
	          skip_step("leiden", args, paste0("grouping `", leiden.name, "` already exists"))
	        } else {
	          run_step("leiden", args, do.call(self$runLeiden, args))
	        }
	      }

	      if ("markers" %in% resolved.steps) {
	        marker.name <- if (!is.null(markers$name)) markers$name else self$defaultGrouping
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
	        marker.type <- if (!is.null(args$type)) args$type else "counts"
	        if (!overwrite && !is.null(self$diffgenes[[marker.type]]) && !is.null(self$diffgenes[[marker.type]][[args$name]])) {
	          skip_step("markers", args, paste0("marker result `", args$name, "` already exists"))
	        } else {
	          run_step("markers", args, do.call(self$runMarkers, args))
	        }
	      }

	      self$history$runs[[run.id]] <- list(
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
	      invisible(self)
	    },

	    #' @description Resolve a grouping column or vector into a named factor.
	    #'
	    #' @param grouping Name of a discrete cellMeta column. NULL uses defaultGrouping.
	    #' @param groups Direct vector of group labels. Mutually exclusive with grouping.
	    #' @param cells Optional cell names to resolve onto.
	    #' @param allow.missing Whether missing labels are allowed.
	    #' @return Named factor aligned to cells.
	    resolveGrouping=function(grouping=NULL, groups=NULL, cells=NULL, allow.missing=TRUE) {
	      self$syncMetadata()
	      if (!is.null(grouping) && !is.null(groups)) {
	        stop("Specify only one of `grouping` or `groups`")
	      }
	      if (is.null(cells)) {
	        cells <- .pagoda2_axis_names(self, "cell")
	      }
	      if (is.null(groups)) {
	        if (is.null(grouping)) {
	          grouping <- self$defaultGrouping
	        }
	        if (is.null(grouping)) {
	          stop("No defaultGrouping is set and no grouping was supplied")
	        }
	        if (!is.character(grouping) || length(grouping) != 1) {
	          stop("`grouping` must be a single cell metadata column name")
	        }
	        if (!grouping %in% colnames(self$cellMeta)) {
	          stop("Unknown grouping `", grouping, "`. Available groupings: ", paste(self$listGroupings()$name, collapse = ", "))
	        }
	        values <- self$cellMeta[[grouping]]
	        names(values) <- rownames(self$cellMeta)
	      } else if (is.character(groups) && length(groups) == 1 && is.null(names(groups)) && groups %in% colnames(self$cellMeta)) {
	        values <- self$cellMeta[[groups]]
	        names(values) <- rownames(self$cellMeta)
	      } else {
	        values <- groups
	      }
	      values <- .pagoda2_align_vector(values, cells, what = "groups")
	      if (!allow.missing && any(is.na(values))) {
	        stop("Grouping is missing values for ", sum(is.na(values)), " cell(s)")
	      }
	      grouping.name <- if (is.null(grouping)) "groups" else grouping
	      .pagoda2_as_grouping(values, name = grouping.name)
	    },

	    #' @description Get a grouping aligned to cells.
	    #'
	    #' @param grouping Name of a discrete cellMeta column. NULL uses defaultGrouping.
	    #' @param groups Direct vector of group labels. Mutually exclusive with grouping.
	    #' @param cells Optional cell names to resolve onto.
	    #' @param allow.missing Whether missing labels are allowed.
	    #' @return Named factor aligned to cells.
	    getGrouping=function(grouping=NULL, groups=NULL, cells=NULL, allow.missing=TRUE) {
	      self$resolveGrouping(grouping = grouping, groups = groups, cells = cells, allow.missing = allow.missing)
	    },

	    #' @description List discrete cellMeta columns that can be used as groupings.
	    #'
	    #' @return data.frame with grouping summaries.
	    listGroupings=function() {
	      self$syncMetadata()
	      cols <- colnames(self$cellMeta)
	      if (length(cols) == 0) {
	        return(data.frame(
	          name = character(),
	          class = character(),
	          n.groups = integer(),
	          n.missing = integer(),
	          is.default = logical(),
	          stringsAsFactors = FALSE
	        ))
	      }
	      resolved <- self$resolveCellMeta(columns = cols)
	      keep <- vapply(resolved, .pagoda2_is_discrete_grouping, logical(1))
	      cols <- cols[keep]
	      if (length(cols) == 0) {
	        return(data.frame(
	          name = character(),
	          class = character(),
	          n.groups = integer(),
	          n.missing = integer(),
	          is.default = logical(),
	          stringsAsFactors = FALSE
	        ))
	      }
	      data.frame(
	        name = cols,
	        class = vapply(resolved[cols], function(x) class(x)[1], character(1)),
	        n.groups = vapply(resolved[cols], function(x) length(unique(x[!is.na(x)])), integer(1)),
	        n.missing = vapply(resolved[cols], function(x) sum(is.na(x)), integer(1)),
	        is.default = cols == self$defaultGrouping,
	        stringsAsFactors = FALSE
	      )
	    },

	    #' @description List stored reductions.
	    #'
	    #' @return data.frame with reduction summaries.
	    listReductions=function() {
	      nms <- names(self$reductions)
	      if (length(nms) == 0) {
	        return(data.frame(name = character(), n.cells = numeric(), n.dims = numeric(), stringsAsFactors = FALSE))
	      }
	      data.frame(
	        name = nms,
	        n.cells = vapply(self$reductions[nms], nrow, numeric(1)),
	        n.dims = vapply(self$reductions[nms], ncol, numeric(1)),
	        stringsAsFactors = FALSE
	      )
	    },

	    #' @description List stored graphs.
	    #'
	    #' @return data.frame with graph summaries.
	    listGraphs=function() {
	      nms <- names(self$graphs)
	      if (length(nms) == 0) {
	        return(data.frame(name = character(), n.nodes = numeric(), n.edges = numeric(), weighted = logical(), stringsAsFactors = FALSE))
	      }
	      data.frame(
	        name = nms,
	        n.nodes = vapply(self$graphs[nms], igraph::vcount, numeric(1)),
	        n.edges = vapply(self$graphs[nms], igraph::ecount, numeric(1)),
	        weighted = vapply(self$graphs[nms], igraph::is_weighted, logical(1)),
	        stringsAsFactors = FALSE
	      )
	    },

	    #' @description List stored embeddings.
	    #'
	    #' @return data.frame with embedding summaries.
	    listEmbeddings=function() {
	      reductions <- names(self$embeddings)
	      if (length(reductions) == 0) {
	        return(data.frame(reduction = character(), embedding = character(), n.cells = numeric(), n.dims = numeric(), stringsAsFactors = FALSE))
	      }
	      rows <- do.call(rbind, lapply(reductions, function(reduction) {
	        embeddings <- names(self$embeddings[[reduction]])
	        if (length(embeddings) == 0) {
	          return(NULL)
	        }
	        data.frame(
	          reduction = reduction,
	          embedding = embeddings,
	          n.cells = vapply(self$embeddings[[reduction]][embeddings], nrow, numeric(1)),
	          n.dims = vapply(self$embeddings[[reduction]][embeddings], ncol, numeric(1)),
	          stringsAsFactors = FALSE
	        )
	      }))
	      if (is.null(rows)) {
	        return(data.frame(reduction = character(), embedding = character(), n.cells = numeric(), n.dims = numeric(), stringsAsFactors = FALSE))
	      }
	      rownames(rows) <- NULL
	      rows
	    },

	    #' @description List stored marker results.
	    #'
	    #' @return data.frame with marker result summaries.
	    listMarkers=function() {
	      types <- names(self$diffgenes)
	      if (length(types) == 0) {
	        return(data.frame(type = character(), name = character(), n.groups = integer(), grouping = character(), schema = character(), stringsAsFactors = FALSE))
	      }
	      rows <- do.call(rbind, lapply(types, function(type) {
	        markers <- names(self$diffgenes[[type]])
	        if (length(markers) == 0) {
	          return(NULL)
	        }
	        data.frame(
	          type = type,
	          name = markers,
	          n.groups = vapply(self$diffgenes[[type]][markers], length, integer(1)),
	          grouping = vapply(self$diffgenes[[type]][markers], function(x) {
	            meta <- attr(x, "pagoda2.marker")
	            if (is.null(meta$grouping)) NA_character_ else meta$grouping
	          }, character(1)),
	          schema = vapply(self$diffgenes[[type]][markers], function(x) {
	            meta <- attr(x, "pagoda2.marker")
	            if (is.null(meta$schema)) "legacy" else meta$schema
	          }, character(1)),
	          stringsAsFactors = FALSE
	        )
	      }))
	      if (is.null(rows)) {
	        return(data.frame(type = character(), name = character(), n.groups = integer(), grouping = character(), schema = character(), stringsAsFactors = FALSE))
	      }
	      rownames(rows) <- NULL
	      rows
	    },

	    #' @description Summarize stored result namespaces.
	    #'
	    #' @return list of result summary data.frames.
	    listResults=function() {
	      list(
	        reductions = self$listReductions(),
	        graphs = self$listGraphs(),
	        embeddings = self$listEmbeddings(),
	        groupings = self$listGroupings(),
	        markers = self$listMarkers()
	      )
	    },

	    #' @description Convert a Pagoda2 object to another in-memory representation.
	    #'
	    #' @param format Conversion format: list, sce, or seurat.
	    #' @param ... Format-specific arguments.
	    #' @return Converted object.
	    as=function(format=c("list", "sce", "seurat"), ...) {
	      pagoda2As(self, format = format, ...)
	    },

	    #' @description Export a Pagoda2 object to disk.
	    #'
	    #' @param path Output path.
	    #' @param format Output format. NULL infers from extension.
	    #' @param ... Format-specific arguments.
	    #' @return Invisibly returns path.
	    export=function(path, format=NULL, ...) {
	      pagoda2Export(self, path = path, format = format, ...)
	    },

	    #' @description Export a Pagoda2 web app.
	    #'
	    #' @param path Output path.
	    #' @param ... App export arguments.
	    #' @return Invisibly returns path.
	    exportApp=function(path, ...) {
	      stop("p2app export is postponed while the app layer is refactored for matrix views.", call. = FALSE)
	    },

	    #' @description Resolve a reduction name.
	    #'
	    #' @param reduction Reduction name. NULL uses defaults$reduction.
	    #' @return Reduction name.
	    resolveReduction=function(reduction=NULL) {
	      if (is.null(reduction)) {
	        reduction <- self$defaults$reduction
	      }
	      if (!is.character(reduction) || length(reduction) != 1) {
	        stop("`reduction` must be a single reduction name")
	      }
	      if (is.null(self$reductions[[reduction]])) {
	        stop("Unknown reduction `", reduction, "`. Available reductions: ", paste(names(self$reductions), collapse = ", "))
	      }
	      reduction
	    },

	    #' @description Resolve a graph name.
	    #'
	    #' @param graph Graph name. NULL uses reduction or defaults$graph.
	    #' @param reduction Optional reduction name to use as graph default.
	    #' @return Graph name.
	    resolveGraph=function(graph=NULL, reduction=NULL) {
	      if (is.null(graph)) {
	        graph <- reduction
	      }
	      if (is.null(graph)) {
	        graph <- self$defaults$graph
	      }
	      if (!is.character(graph) || length(graph) != 1) {
	        stop("`graph` must be a single graph name")
	      }
	      if (is.null(self$graphs[[graph]])) {
	        stop("Unknown graph `", graph, "`. Available graphs: ", paste(names(self$graphs), collapse = ", "))
	      }
	      graph
	    },

	    #' @description Resolve an embedding selector.
	    #'
	    #' @param reduction Reduction namespace. NULL uses defaults$reduction.
	    #' @param embedding Embedding name. NULL uses defaults$embedding.
	    #' @return list with reduction, embedding, and value.
	    resolveEmbedding=function(reduction=NULL, embedding=NULL) {
	      if (is.null(reduction)) {
	        reduction <- self$defaults$reduction
	      }
	      if (is.null(embedding)) {
	        embedding <- self$defaults$embedding
	      }
	      if (!is.character(reduction) || length(reduction) != 1) {
	        stop("`reduction` must be a single embedding namespace")
	      }
	      if (!is.character(embedding) || length(embedding) != 1) {
	        stop("`embedding` must be a single embedding name")
	      }
	      if (is.null(self$embeddings[[reduction]]) || is.null(self$embeddings[[reduction]][[embedding]])) {
	        available <- self$listEmbeddings()
	        available.text <- if (nrow(available) == 0) "" else paste(paste(available$reduction, available$embedding, sep = "/"), collapse = ", ")
	        stop("Unknown embedding `", reduction, "/", embedding, "`. Available embeddings: ", available.text)
	      }
	      list(reduction = reduction, embedding = embedding, value = self$embeddings[[reduction]][[embedding]])
	    },

	    #' @description Resolve a marker result.
	    #'
	    #' @param markers Marker result name. NULL uses defaultGrouping.
	    #' @param type Marker result namespace.
	    #' @return list with type, name, value, and metadata.
	    resolveMarkers=function(markers=NULL, type='counts') {
	      if (is.null(markers)) {
	        markers <- self$defaultGrouping
	      }
	      if (is.null(markers)) {
	        stop("No marker result supplied and no defaultGrouping is set")
	      }
	      if (!is.character(markers) || length(markers) != 1) {
	        stop("`markers` must be a single marker result name")
	      }
	      if (is.null(self$diffgenes[[type]]) || is.null(self$diffgenes[[type]][[markers]])) {
	        available <- self$listMarkers()
	        available <- available$name[available$type == type]
	        stop("Unknown marker result `", markers, "`. Available markers: ", paste(available, collapse = ", "))
	      }
	      value <- self$diffgenes[[type]][[markers]]
	      result <- NULL
	      if (!is.null(self$markerResults[[type]]) && !is.null(self$markerResults[[type]][[markers]])) {
	        result <- self$markerResults[[type]][[markers]]
	      }
	      metadata <- attr(value, "pagoda2.marker")
	      if (is.null(result) && !is.null(metadata)) {
	        result <- metadata
	        result$tables <- value
	        class(result) <- c("pagoda2_marker_result", "list")
	      }
	      list(type = type, name = markers, value = value, tables = value, result = result, metadata = metadata)
	    },

	    #' @description Get a structured marker result.
	    #'
	    #' @param markers Marker result name. NULL uses defaultGrouping.
	    #' @param type Marker result namespace.
	    #' @return Structured marker result with tables and provenance.
	    getMarkerResult=function(markers=NULL, type='counts') {
	      resolved <- self$resolveMarkers(markers = markers, type = type)
	      if (is.null(resolved$result)) {
	        stop("Marker result `", resolved$name, "` does not have pagoda2.1 marker metadata")
	      }
	      resolved$result
	    },

	    #' @description Create a cell annotation by mapping one grouping to another.
	    #'
	    #' @param from Source grouping name.
	    #' @param to Output grouping name.
	    #' @param map Named vector mapping source levels to annotation labels.
	    #' @param unmapped How to handle unmapped source levels: NA, "keep", or "error".
	    #' @param setDefault Whether to make the output grouping the default.
	    #' @param overwrite Whether to overwrite an existing output grouping.
	    #' @return Invisibly returns self.
	    annotateClusters=function(from, to, map, unmapped=NA, setDefault=TRUE, overwrite=FALSE) {
	      source.groups <- self$getGrouping(grouping = from)
	      if (is.null(names(map)) || any(names(map) == "")) {
	        stop("`map` must be a named vector")
	      }
	      source.levels <- levels(source.groups)
	      unknown <- setdiff(names(map), source.levels)
	      if (length(unknown) > 0) {
	        stop("Mapping contains unknown source level(s): ", paste(unknown, collapse = ", "))
	      }
	      source.values <- as.character(source.groups)
	      target.values <- unname(as.character(map[source.values]))
	      missing.map <- is.na(target.values) & !is.na(source.values)
	      if (any(missing.map)) {
	        if (identical(unmapped, "error")) {
	          stop("No annotation provided for source level(s): ", paste(sort(unique(source.values[missing.map])), collapse = ", "))
	        } else if (identical(unmapped, "keep")) {
	          target.values[missing.map] <- source.values[missing.map]
	        } else if (!(length(unmapped) == 1 && is.na(unmapped))) {
	          stop("`unmapped` must be NA, \"keep\", or \"error\"")
	        }
	      }
	      names(target.values) <- names(source.groups)
	      self$setGrouping(to, target.values, source = list(method = "annotateClusters", from = from), setDefault = setDefault, overwrite = overwrite)
	      invisible(self)
	    },

	    #' @description Provide the initial count matrix, and estimate deviance residual matrix (correcting for depth and batch)
    #'
    #' @param countMatrix input count matrix 
    #' @param depthScale numeric Scaling factor for normalizing counts (defaul=1e3). If 'plain', counts are scaled by counts = counts/as.numeric(depth/depthScale).
    #' @return normalized count matrix (or if modelTye='raw', the unnormalized count matrix)
    setCountMatrix=function(countMatrix, depthScale=1e3, min.cells.per.gene=0, 
                            trim=round(min.cells.per.gene/2), min.transcripts.per.cell=10, 
                            lib.sizes=NULL, log.scale=FALSE, keep.genes=NULL, verbose=TRUE) {
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
      
      if (ncol(countMatrix)<3) { 
        stop("Too few cells remaining after min.count.per.cell filter applied - have you pre-filtered the count matrix to include only cells of a realistic size?") 
      }
      
      counts <- t(countMatrix)
      
      # Keep genes of sufficient coverage or genes that are in the keep.genes list
      counts <- counts[,diff(counts@p) >= min.cells.per.gene | colnames(counts) %in% keep.genes]

      self$misc$depthScale <- depthScale
      colBatch <- NULL
      if (!is.null(self$batch)) {
        if (!all(colnames(countMatrix) %in% names(self$batch))) { 
          stop("The supplied batch vector doesn't contain all the cells in its names attribute")
        }
        colBatch <- as.factor(self$batch[colnames(countMatrix)])
      }

      if (!is.null(lib.sizes)) {
        if (!all(colnames(countMatrix) %in% names(lib.sizes))) { 
          stop("The supplied lib.sizes vector doesn't contain all the cells in its names attribute")
        }
        lib.sizes <- lib.sizes[colnames(countMatrix)]
        depth <- lib.sizes/mean(lib.sizes)*mean(Matrix::colSums(countMatrix))
      } else {
        depth <- Matrix::colSums(countMatrix)
      }
      
      cell.filt.mask <- (depth >= min.transcripts.per.cell)
      counts <- counts[cell.filt.mask,]
      depth <- depth[cell.filt.mask]
      names(depth) <- rownames(counts)
      if (!is.null(colBatch)) {
        self$batch <- droplevels(colBatch[cell.filt.mask])
        names(self$batch) <- rownames(counts)
      }

      self$rawCounts <- counts
      self$misc[['rawCounts']] <- self$rawCounts
      analysis.view <- list(
        name = "analysis",
        source = "raw",
        model = self$modelType,
        depthScale = depthScale,
        depth = depth,
        log.scale = log.scale,
        trim = trim,
        batch = self$batch,
        batchFactors = NULL,
        winsorCaps = NULL,
        preWinsorDepth = NULL,
        postWinsorDepth = NULL
      )
      
      if (any(depth == 0)) {
        stop("Cells with zero expression over all genes are not allowed")
      }

	      if (self$modelType == 'raw') {
	        self$depth <- depth
	        self$matrixViews$analysis <- analysis.view
	        self$syncMetadata()
	        invisible(self)
	        return()
	      }

      if (self$modelType == 'linearObs') {
        stop("modelType `linearObs` is not supported without stored `$counts` yet")
      }

      counts <- self$rawCounts
      counts@x <- as.numeric(counts@x)

      if (verbose) message(nrow(counts)," cells, ",ncol(counts)," genes; normalizing ... ")

      # get normalized matrix
      if (self$modelType=='linearObs') { # this shouldn't work well, since the depth dependency is not completely normalized out

        # winsorize in normalized space first in hopes of getting a more stable depth estimate
        if (trim>0) {
          counts <- counts / as.numeric(depth)
          inplaceWinsorizeSparseCols(counts, trim, self$n.cores)
          counts <- counts*as.numeric(depth)
          if (is.null(lib.sizes)) {
            depth <- round(Matrix::rowSums(counts))
          }
        }

        ldepth <- log(depth)

        # rank cells, cut into n pieces
        n.depth.slices <- 20
        #depth.fac <- as.factor(floor(rank(depth)/(length(depth)+1)*n.depth.slices)+1); names(depth.fac) <- rownames(counts);
        depth.fac <- cut(cumsum(sort(depth)),breaks=seq(0,sum(depth),length.out=n.depth.slices))
        names(depth.fac) <- rownames(counts)
        depth.fac <- depth.fac[rank(depth)]
        # dataset-wide gene average
        gene.av <- (Matrix::colSums(counts)+n.depth.slices)/(sum(depth)+n.depth.slices)

        # pooled counts, df for all genes
        tc <- colSumByFac(counts, as.integer(depth.fac))[-1,,drop=FALSE]
        tc <- log(tc+1)- log(as.numeric(tapply(depth,depth.fac,sum))+1)
        md <- log(as.numeric(tapply(depth,depth.fac,mean)))
        # combined lm
        cm <- lm(tc ~ md)
        colnames(cm$coef) <- colnames(counts)
        # adjust counts
        # predict log(p) for each non-0 entry
        count.gene <- rep(1:counts@Dim[2], diff(counts@p))
        exp.x <- exp(log(gene.av)[count.gene] - cm$coef[1,count.gene] - ldepth[counts@i+1]*cm$coef[2,count.gene])
        counts@x <- as.numeric(counts@x*exp.x/(depth[counts@i+1]/depthScale)) # normalize by depth as well
        # perform a another round of trimming
        if (trim>0) {
          inplaceWinsorizeSparseCols(counts, trim, self$n.cores)
        }


        # regress out on non-0 observations of each gene
        #non0LogColLmS(counts,mx,ldepth)
      } else if (self$modelType=='plain') {
        if (verbose) message("Using plain model ")

        if (!is.null(self$batch)) {
          if (verbose) message("Batch ... ")

          # dataset-wide gene average
          gene.av <- (Matrix::colSums(counts)+length(levels(self$batch)))/(sum(depth)+length(levels(self$batch)))

          # pooled counts, df for all genes
          tc <- colSumByFac(counts,as.integer(self$batch))[-1,,drop=FALSE]
          tc <- t(log(tc+1)- log(as.numeric(tapply(depth,self$batch,sum))+1))
          bc <- exp(tc-log(gene.av))
          rownames(bc) <- colnames(counts)
          colnames(bc) <- levels(self$batch)
          analysis.view$batch <- self$batch
          analysis.view$batchFactors <- bc

          # adjust every non-0 entry
          count.gene <- rep(1:counts@Dim[2],diff(counts@p))
          
          counts@x <- as.numeric(counts@x/bc[cbind(count.gene,as.integer(self$batch)[counts@i+1])])
        }

        if (trim>0) {
          if (verbose) message("Winsorizing ... ")
          counts <- counts/as.numeric(depth)
          analysis.view$preWinsorDepth <- depth
          analysis.view$winsorCaps <- .pagoda2_sparse_winsor_caps(counts, trim)
          
          inplaceWinsorizeSparseCols(counts, trim, self$n.cores)
          counts <- counts*as.numeric(depth)
          
          if (is.null(lib.sizes)) {
            depth <- round(Matrix::rowSums(counts))
          }
          names(depth) <- rownames(counts)
          analysis.view$postWinsorDepth <- depth
        }

        counts <- counts/as.numeric(depth/depthScale)
      } else {
        stop('modelType ',self$modelType,' is not implemented')
      }
      if (log.scale) {
        if (verbose) message("log scale ... ")
        counts@x <- as.numeric(log(counts@x+1))
      }
      self$misc[['rescaled.mat']] <- NULL
      if (verbose) message("done.\n")

	      self$depth <- depth
	      analysis.view$depth <- depth
	      self$matrixViews$analysis <- analysis.view
	      self$syncMetadata()
	      invisible(self)
	    },

    #' @description Adjust variance of the residual matrix, determine overdispersed sites
    #' This is done to normalize the extent to which genes with (very) different expression magnitudes will contribute to the downstream anlaysis.
    #'
    #' @param gam.k integer The k used for the generalized additive model 'v ~ s(m, k =gam.k)' (default=5). If gam.k<2, linear regression is used 'lm(v ~ m)'.
    #' @param alpha numeric The Type I error probability or the significance level (default=5e-2). This is the criterion used to measure statistical significance, i.e. if the p-value < alpha, then it is statistically significant.
    #' @param use.raw.variance (default=FALSE). If modelType='raw', then this conditional will be used as TRUE.
    #' @param use.unadjusted.pvals boolean Whether to use Benjamini-Hochberg adjusted p-values (default=FALSE).
    #' @param do.par boolean Whether to put multiple graphs into a signle plot with par() (default=TRUE)
    #' @param max.adjusted.variance numeric Maximum adjusted variance (default=1e3). The gene scale factor is defined as sqrt(pmax(min.adjusted.variance,pmin(max.adjusted.variance,df$qv))/exp(df$v))
    #' @param min.adjusted.variance numeric Minimum adjusted variance (default=1e-3). The gene scale factor is defined as sqrt(pmax(min.adjusted.variance,pmin(max.adjusted.variance,df$qv))/exp(df$v))
    #' @param cells character vector Subset of cells upon which to perform variance normalization with adjustVariance() (default=NULL)
    #' @param min.gene.cells integer Minimum number of genes per cells (default=0). This parameter is used to filter counts.
    #' @param persist boolean Whether to save results (default=TRUE, i.e. is.null(cells)).
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 50 bone marrow cells as matrix
    #' cm <- readRDS(system.file("extdata", "sample_BM1_50.rds", package="pagoda2"))
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #'}
    #' 
    #' @return residual matrix with adjusted variance
	    adjustVariance=function(gam.k=5, alpha=5e-2, plot=FALSE, use.raw.variance=FALSE, 
	      use.unadjusted.pvals=FALSE, do.par=TRUE, max.adjusted.variance=1e3, min.adjusted.variance=1e-3, 
	      cells=NULL, verbose=TRUE, min.gene.cells=0, persist=is.null(cells), n.cores = self$n.cores,
	      .legacy.warn=TRUE) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("adjustVariance()", "p2$runVariance(...)")
	      }
	      #persist <- is.null(cells) # persist results only if variance normalization is performed for all cells (not a subset)
      all.cells <- .pagoda2_axis_names(self, "cell")
      all.genes <- .pagoda2_axis_names(self, "gene")
      if (!is.null(cells)) { # translate cells into a rowSel boolean vector
        if (is.logical(cells) && length(cells)==length(all.cells)) {
          rowSel <- cells
        } else {
          if (is.character(cells) || is.integer(cells)) {
            rowSel <- rep(FALSE, length(all.cells))
            names(rowSel) <- all.cells
            rowSel[cells] <- TRUE
          } else {
            stop("Cells argument must be either a logical vector over rows of the count matrix (cells), a vector of cell names or cell integer ids (row numbers)")
          }
        }
      } else {
        rowSel <- NULL
      }

      if (verbose) message("calculating variance fit ...")
      df <- if (!is.null(self$rawCounts) &&
                !is.null(self$matrixViews$analysis) &&
                self$matrixViews$analysis$model %in% c("plain", "raw")) {
        self$viewColMeanVar(name = "analysis", cells = cells, n.cores = n.cores)
      } else {
        stop("Variance calculation requires a supported matrix view")
      }

      if (use.raw.variance) { # use raw variance estimates without relative adjustments
        rownames(df) <- all.genes
        vi <- which(is.finite(df$v) & df$nobs>=min.gene.cells)
        df$lp <- df$lpa <- log(df$v)
        df$qv <- df$v
        df$gsf <- 1 # no rescaling of variance
        ods <- order(df$v, decreasing=TRUE)
        if (length(ods)>1e3) { 
          ods <- ods[1:1e3] 
        }
        if (persist) {
          self$misc[['odgenes']] <- rownames(df)[ods]
        }
      } else {
        # gene-relative normalizaton 
        df$m <- log(df$m)
        df$v <- log(df$v)
        rownames(df) <- all.genes
        vi <- which(is.finite(df$v) & df$nobs>=min.gene.cells)
        if (length(vi)<gam.k*1.5) { gam.k=1 } # too few genes
        if (gam.k<2) {
          if (verbose) message(" using lm ")
          m <- lm(v ~ m, data = df[vi,])
        } else {
          if (verbose) message(" using gam ")
          m <- mgcv::gam(as.formula(paste0('v ~ s(m, k = ',gam.k,')')), data = df[vi,])
        }
        df$res <- -Inf
        df$res[vi] <- resid(m,type='response')
        n.obs <- df$nobs #diff(counts@p)
        suppressWarnings(df$lp <- as.numeric(pf(exp(df$res),n.obs,n.obs,lower.tail=FALSE,log.p=TRUE)))
        df$lpa <- bh.adjust(df$lp,log=TRUE)
        n.cells <- length(all.cells)
        df$qv <- as.numeric(qchisq(df$lp, n.cells-1, lower.tail = FALSE, log.p=TRUE)/n.cells)

        if (use.unadjusted.pvals) {
          ods <- which(df$lp<log(alpha))
        } else {
          ods <- which(df$lpa<log(alpha))
        }
        
        if (persist) {
          self$misc[['odgenes']] <- rownames(df)[ods]
        }
        if (verbose) message(length(ods),' overdispersed genes ... ',length(ods) )

        df$gsf <- geneScaleFactors <- sqrt(pmax(min.adjusted.variance,pmin(max.adjusted.variance,df$qv))/exp(df$v))
        df$gsf[!is.finite(df$gsf)] <- 0
      }

      if (persist) {
        if (verbose) message('persisting ... ')
        self$misc[['varinfo']] <- df
      }

      # rescale mat variance
      ## if(rescale.mat) {
      ##   if(verbose) cat("rescaling signal matrix ... ")
      ##   #df$gsf <- geneScaleFactors <- sqrt(1/exp(df$v));
      ##   inplaceColMult(counts,geneScaleFactors,rowSel);  # normalize variance of each gene
      ##   #inplaceColMult(counts,rep(1/mean(Matrix::colSums(counts)),ncol(counts))); # normalize the column sums to be around 1
      ##   if(persist) misc[['rescaled.mat']] <- geneScaleFactors;
      ## }
      if (plot) {
        if (do.par) {
          adjvar_par <- par(mfrow=c(1,2), mar = c(3.5,3.5,2.0,0.5), mgp = c(2,0.65,0), cex = 1.0)
          on.exit(par(adjvar_par))
        }
        suppressWarnings(smoothScatter(log10(exp(1))*df$m, log10(exp(1))*df$v, main='', xlab='log10[ magnitude ]',ylab='log10[ variance ]'))
        vi <- which(is.finite(log10(exp(1))*df$v) & df$nobs>=min.gene.cells)
        grid <- seq(min(log10(exp(1))*df$m[vi]), max(log10(exp(1))*df$m[vi]), length.out=1000)
        ## re-calculate m
        if (gam.k < 2) {
          if (verbose) message(" using lm ")
          m <- lm(v ~ m, data = log10(exp(1))*df[vi,])
        } else {
          if (verbose) message(" using gam ")
          m <- mgcv::gam(as.formula(paste0('v ~ s(m, k = ',gam.k,')')), data = log10(exp(1))*df[vi,])
        }
        lines(grid,predict(m, newdata=data.frame(m=grid)), col="blue")
        if (length(ods)>0) {
          points(log10(exp(1))*df$m[ods], log10(exp(1))*df$v[ods], pch='.',col=2,cex=1)
        }
        suppressWarnings(smoothScatter(log10(exp(1))*df$m[vi], log10(exp(1))*df$qv[vi], xlab='log10[ magnitude ]',ylab='',main='adjusted'))
        abline(h=1,lty=2,col=8)
        if (is.finite(max.adjusted.variance)) { 
          abline(h=max.adjusted.variance, lty=2, col=1)
        }
        points(log10(exp(1))*df$m[ods], log10(exp(1))*df$qv[ods], col=2, pch='.')
      }
      if (verbose) message("done.")
	      invisible(df)
	    },

	    #' @description Run variance modeling using the pagoda2.1 API name.
	    #'
	    #' @param ... Arguments passed to adjustVariance().
	    #' @return residual matrix with adjusted variance.
	    runVariance=function(...) {
	      self$adjustVariance(..., .legacy.warn = FALSE)
	    },

	    #' @description Create k-nearest neighbor graph
    #' 
    #' @param k integer Number of k clusters for k-NN (default=30)
    #' @param nrand numeric Number of randomizations i.e. the gene sets (of the same size) to be evaluated in parallel with each gene set (default=1e3)
    #' @param type string Data type of the reduction (default='counts'). If type='counts', this will access the raw counts. Otherwise, 'type' must be name of the reductions.
    #' @param weight.type string 'none', 'cauchy', 'normal', 'constant', '1m' (default='1m')
    #' @param odgenes character vector Overdispersed genes to retrieve (default=NULL)
    #' @param distance string Distance metric used: 'cosine', 'L2', 'L1', 'cauchy', 'euclidean' (default='cosine')
    #' @param center boolean Whether to use centering when distance='cosine' (default=TRUE). The parameter is ignored otherwise.
    #' @param x counts or reduction to use (default=NULL). If NULL, uses counts. Otherwise, checks for the reduction in self$reductions[[type]] 
    #' @param p (default=NULL)
    #' @param var.scale boolean Apply scaling if using raw counts (default=TRUE). If type="counts", var.scale is TRUE by default.
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 50 bone marrow cells as matrix
    #' cm <- readRDS(system.file("extdata", "sample_BM1_50.rds", package="pagoda2"))
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=300)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object   
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=20, center=FALSE, distance='L2')
    #' }
    #' 
    #' @return kNN graph, stored in self$graphs
	    makeKnnGraph=function(k=30, nrand=1e3, type='counts', weight.type='1m',
	      odgenes=NULL, n.cores=self$n.cores, distance='cosine', center=TRUE, 
	      x=NULL, p=NULL, var.scale=(type == "counts"), verbose=TRUE, .legacy.warn=TRUE) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("makeKnnGraph()", "p2$runGraph(reduction = \"PCA\", ...)")
	      }
	      ## convert "euclidean" to "L2"
      if (tolower(distance)=="euclidean"){
        distance <- "L2"
      }
      if (is.null(x)) {
        x.was.given <- FALSE
        if (type=='counts') {
          genes <- NULL
          if (!is.null(odgenes)) {
            available.genes <- .pagoda2_axis_names(self, "gene")
            missing.genes <- setdiff(odgenes, available.genes)
            if (length(missing.genes) > 0) {
              warning("not all of the provided odgenes are present in the selected matrix")
            }
            genes <- intersect(odgenes, available.genes)
          }
          x <- self$getExpressionBlock(genes = genes)
          # Scale Raw counts
        } else {
          if (type %in% names(self$reductions)) {
            x <- self$reductions[[type]]
          } else {
            stop('Specified reduction does not exist')
          }
        }
        
        if (var.scale) {
          x <- .pagoda2_apply_variance_scaling(x, self$misc[['varinfo']])
        }

        if (!is.null(odgenes) && type != 'counts') {
          if (!all(odgenes %in% colnames(x))) { warning("not all of the provided odgenes are present in the selected matrix")}
          if (verbose) message("using provided odgenes ... ")
          x <- x[,odgenes]
        }

      } else { # is.null(x)
        x.was.given <- TRUE
      }

      if (distance %in% c('cosine','angular')) {
        if (center) {
          x<- x - Matrix::rowMeans(x) # centering for cosine distance
        }
        xn <- N2R::Knn(as.matrix(x), k, nThreads=n.cores, verbose=verbose, indexType='angular')
      } else if (distance == "L2") {
        xn <- N2R::Knn(as.matrix(x), k, nThreads=n.cores, verbose=verbose, indexType='L2')
      } else {
        stop("Unknown distance measure specified. Currently supported: angular, L2")
      }
      colnames(xn) <- rownames(xn) <- rownames(x)

      #if(weight.type=='rank') {
      #  xn$r <-  unlist(lapply(diff(c(0,which(diff(xn$s)>0),nrow(xn))),function(x) seq(x,1)))
      #}
      #xn <- xn[!xn$s==xn$e,]
      diag(xn) <- 0
      xn <- Matrix::drop0(xn)

      #if(n.cores==1) { # for reproducibility, sort by node names
      #  if(verbose) cat("ordering neighbors for reproducibility ... ");
      #  xn <- xn[order(xn$s+xn$e),]
      #  if(verbose) cat("done\n");
      #}
      #df <- data.frame(from=rownames(x)[xn$s+1],to=rownames(x)[xn$e+1],weight=xn$d,stringsAsFactors=F)
      #if(weight.type=='rank') { df$rank <- xn$r }

      if (weight.type %in% c("cauchy", "normal") && ncol(x)>sqrt(nrand)) {
        # generate some random pair data for scaling
        if (distance=='cosine') {
          #rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=T),sample(colnames(x),nrand,replace=T)),1,function(z) if(z[1]==z[2]) {return(NA); } else {1-cor(x[,z[1]],x[,z[2]])}))
          rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA) } else {1-sum(x[,z[1]]*x[,z[2]])/sqrt(sum(x[,z[1]]^2)*sum(x[,z[2]]^2))}))
        ## we no longer support 'JS'
        ## } else if(distance=='JS') {
        ##   rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA); } else {jw.disR(x[,z[1]],x[,z[2]])}))
        } else if (distance=='L2') {
          rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA) } else {sqrt(sum((x[,z[1]]-x[,z[2]])^2))}))
        } else if (distance=='L1') {
          rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA) } else {sum(abs(x[,z[1]]-x[,z[2]]))}))
        }
        suppressWarnings(rd.model <- MASS::fitdistr(rd,weight.type))
        if (weight.type=='cauchy') {
          xn@x <- 1/pcauchy(xn@x,location=rd.model$estimate['location'],scale=rd.model$estimate['scale'])-1
        } else {
          xn@x <- 1/pnorm(xn@x,mean=rd.model$estimate['mean'],sd=rd.model$estimate['sd'])-1
        }
      }
      xn@x <- pmax(0,xn@x)
      if (weight.type=='constant') { 
        xn@x <- 1
      }
      if (weight.type=='1m') { 
        xn@x <- pmax(0,1-xn@x) 
      }
      #if(weight.type=='rank') { xn@x <- sqrt(df$rank) }
      # make a weighted edge matrix for the largeVis as well
      sxn <- (xn+t(xn))/2
      g <- igraph::graph_from_adjacency_matrix(sxn,mode='undirected',weighted=TRUE)
      if (!x.was.given) {
        if (is.null(self$misc[['edgeMat']])) { self$misc[['edgeMat']] <- list() }
        self$misc[['edgeMat']][[type]] <- xn
        self$graphs[[type]] <- g
	      }
	      invisible(g)
	    },

	    #' @description Build a kNN graph using the pagoda2.1 API name.
	    #'
	    #' @param reduction Reduction or matrix namespace to use.
	    #' @param ... Arguments passed to makeKnnGraph().
	    #' @return Invisibly returns the graph.
	    runGraph=function(reduction=NULL, ...) {
	      if (is.null(reduction)) {
	        reduction <- self$defaults$reduction
	      }
	      self$makeKnnGraph(type = reduction, ..., .legacy.warn = FALSE)
	    },

	    #' @description Calculate clusters based on the kNN graph
    #' 
    #' @param method Method to use (default=igraph::multilevel.community). Accepted methods are either 'igraph::infomap.community' or 'igraph::multilevel.community'. 
    #'     If NULL, if the number of vertices of the graph is greater than or equal to 2000, 'igraph::multilevel.community' will be used. Otherwise, 'igraph::infomap.community' will be used.
    #' @param name string Name of the community structure calculated from 'method' (default='community')
    #' @param g Input graph (default=NULL). If NULL, access graph from self$graphs[[type]].
    #' @param min.cluster.size Minimum size of clusters (default=1). This parameter is primarily used to remove very small clusters.
    #' @param persist boolean Whether to save the clusters and community structure (default=TRUE)
    #' @param ... Additional parameters to pass to 'method'
    #'
	    #' @return the community structure calculated from 'method'
	    getKnnClusters=function(type='counts',method=igraph::multilevel.community, name='community', 
	      n.cores=self$n.cores, g=NULL, min.cluster.size=1, persist=TRUE, .legacy.warn=TRUE, ...) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("getKnnClusters()", "p2$runLeiden(...)")
	      }

	      if (is.null(g)) {
        if (is.null(self$graphs[[type]])) { 
          stop("Call makeKnnGraph(type='",type,"', ...) first")
        }
        g <- self$graphs[[type]]
      }

      if (is.null(method)) {
        if (length(vcount(g))<2000) {
          method <- igraph::infomap.community
        } else {
          method <- igraph::multilevel.community
        }
      }

      # method <- igraph::multilevel.community; n.cores <- 20
      # n.subsamplings <- 10; cluster.stability.dilution <- 1.5; cluster.stability.fraction <- 0.9; subsampling.rate <- 0.8; metaclustering.method<- 'ward.D'
      # g <- r$graphs$PCA
      # x <- r$counts
      # cls <- method(g)

      #library(parallel)
      #x <- mclapply(1:5,function(z) method(g),mc.cores=20)

      cls <- method(g,...)
      cls.groups <- as.factor(membership(cls))

      # cleanup the clusters to remove very small ones
      if (min.cluster.size>1) {
        cn <- names(cls.groups)
        vg <- which(unlist(tapply(cls.groups,cls.groups,length))>=min.cluster.size)
        cls.groups <- as.integer(cls.groups)
        cls.groups[!cls.groups %in% vg] <- NA
        cls.groups <- as.factor(cls.groups)
        names(cls.groups) <- cn
      }
       
      if (persist) {
        self$clusters[[type]][[name]] <- cls.groups
        self$misc[['community']][[type]][[name]] <- cls
      }
      invisible(cls)
    },

	    #' @description Run Leiden clustering and register the labels as a cellMeta grouping.
	    #'
	    #' @param reduction Reduction name associated with the graph (default=NULL).
	    #' @param graph Graph name in self$graphs (default=NULL, uses reduction or defaults$graph).
	    #' @param name Name of the output grouping (default='leiden').
	    #' @param setDefault Whether to make the output grouping the default.
	    #' @param overwrite Whether to overwrite existing output labels/provenance.
	    #' @param method Clustering function (default=leidenAlg::leiden.community).
	    #' @param ... Additional arguments passed to the clustering method.
	    #' @return Invisibly returns the clustering community object.
	    runLeiden=function(reduction=NULL, graph=NULL, name='leiden', setDefault=TRUE, overwrite=FALSE, method=NULL, ...) {
	      if (is.null(graph)) {
	        graph <- reduction
	      }
	      if (is.null(graph)) {
	        graph <- self$defaults$graph
	      }
	      if (is.null(graph)) {
	        graph <- "PCA"
	      }
	      if (!overwrite && name %in% colnames(self$cellMeta)) {
	        stop("Grouping `", name, "` already exists; use overwrite=TRUE")
	      }
	      if (!overwrite && !is.null(self$clusters[[graph]][[name]])) {
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
		      cls <- self$getKnnClusters(type = graph, method = method, name = name, persist = TRUE, .legacy.warn = FALSE, ...)
	      groups <- self$clusters[[graph]][[name]]
	      self$setGrouping(name, groups, source = list(method = "runLeiden", graph = graph), setDefault = setDefault, overwrite = TRUE)
	      community <- NULL
	      if (!is.null(self$misc[['community']]) && !is.null(self$misc[['community']][[graph]])) {
	        community <- self$misc[['community']][[graph]][[name]]
	      }
	      self$clusterings[[name]] <- list(
	        grouping = name,
	        reduction = reduction,
	        graph = graph,
	        method = method.name,
	        community = community,
	        created = Sys.time()
	      )
	      invisible(cls)
	    },

    #' @description Deprecated function. Use makeGeneKnnGraph() instead.
    #' 
    #' @keywords internal
    geneKnnbyPCA = function() {
      ##warning('geneKnnbyPCA is deprecated use makeGeneKnnGraph() instead')
      .Deprecated("makeGeneKnnGraph()")
      self$makeGeneKnnGraph()
    },

    #' @description Take a given clustering and generate a hierarchical clustering
    #' 
    #' @param type string Data type of the reduction (default='counts'). If type='counts', this will access the raw counts. Otherwise, 'type' must be name of the reductions.
    #' @param groups factor named with cell names specifying the clusters of cells to be compared (one against all) (default=NULL). To compare two cell clusters against each other, simply pass a factor containing only two levels.
    #' @param clusterName string Cluster name to access (default=NULL)
    #' @param method string The agglomeration method to be used in stats::hcust(method=method) (default='ward.D'). Accepted values are: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). For more information, see stats::hclust().
    #' @param dist string 'pearson', 'spearman', 'euclidean', 'L2', 'JS' (default='pearson')
    #' @param persist boolean Whether to save the clusters and community structure (default=TRUE)
    #' @param z.threshold numeric Threshold of z-scores to filter, >=z.threshold are kept (default=2)
    #' @param min.set.size integer Minimum threshold of sets to keep (default=5)
    #' 
    #' @return hierarchical clustering
    getHierarchicalDiffExpressionAspects = function(type='counts', groups=NULL, clusterName=NULL, method='ward.D',
      dist='pearson', persist=TRUE, z.threshold=2, n.cores=self$n.cores, min.set.size=5, verbose=TRUE ){
   
      if (tolower(dist)=="euclidean"){
        dist <- "L2"
      }

      if (type=='counts') {
        x <- self$getExpressionBlock()
      } else {
        x <- self$reductions[[type]]
      }
      if (is.null(groups)) {
        # retrieve clustering
        if (is.null(self$clusters)) {
          stop("Please generate clusters first")
        }
        if (is.null(self$clusters[[type]])) {
          stop(paste("Please generate clusters for",type,"first"))
        }
        if (is.null(clusterName)) { 
          if (length(self$clusters[[type]])<1){
            stop(paste("Please generate clusters for",type,"first"))
          }
          ## use last-generated clustering
          cl <- self$clusters[[type]][[length(self$clusters[[type]])]]
        } else {
          cl <- self$clusters[[type]][[clusterName]]
          if (is.null(cl)) {
            stop(paste("Unable to find clustering",clusterName,'for',type))
          }
          if (verbose) message("Using ",clusterName," clustering for ",type," space\n")
        }
      } else {
        if (!all(rownames(x) %in% names(groups))) { warning("Provided cluster vector doesn't list groups for all of the cells")}
        cl <- groups
      }
      cl <- as.factor(cl[match(rownames(x),names(cl))])

      if (dist %in% c('pearson','spearman')) {
        rx <- do.call(cbind,tapply(1:nrow(x),cl,function(ii) {
            if (length(ii) > 1) {
                Matrix::colMeans(x[ii,])
            } else {
                x[ii,]
            }
        }))
        d <- as.dist(1-cor(rx,method=dist))
      } else if (dist=="L2") {
        rx <- do.call(rbind,tapply(1:nrow(x),cl,function(ii) {
            if (legnth(ii) > 1) {
                Matrix::colMeans(x[ii,])
            } else {
                x[ii,]
            }
        }))
        d <- dist(rx)
      } else if (dist=='JS') {
        # this one has to be done on counts, even if we're working with a reduction
        cl <- as.factor(cl[match(rownames(self$misc[['rawCounts']]),names(cl))])
        lvec <- colSumByFac(self$misc[['rawCounts']],as.integer(cl))[-1,] + 1
        d <- sccore::jsDist(t(lvec/pmax(1,Matrix::rowSums(lvec))))
        colnames(d) <- rownames(d) <- which(table(cl)>0)
        d <- as.dist(d)
      } else {
        stop("Unknown distance",dist,"requested")
      }

      dd <- as.dendrogram(stats::hclust(d, method=method))

      # walk down the dendrogram to generate diff. expression on every split
      diffcontrasts <- function(l,env) {
        v <- mget("contrasts",envir=env,ifnotfound=0)[[1]]
        if (!is.list(v)) v <- list()
        if (is.leaf(l)) return(NULL)
        lcl <- rep(NA,nrow(x))
        names(lcl) <- rownames(x)
        lcl[names(lcl) %in% names(cl)[cl %in% unlist(l[[1]])]] <- paste(unlist(l[[1]]),collapse='.')
        lcl[names(lcl) %in% names(cl)[cl %in% unlist(l[[2]])]] <- paste(unlist(l[[2]]),collapse='.')
        v <- c(v,list(as.factor(lcl)))
        assign("contrasts",v,envir=env)
        return(1)
      }

      de <- environment()
      assign('contrasts',NULL,envir=de)
      dc <- dendrapply(dd,diffcontrasts,env=de)
      dc <- get("contrasts",env=de)
      names(dc) <- unlist(lapply(dc,function(x) paste(levels(x),collapse=".vs.")))

      #dexp <- papply(dc,function(x) getDifferentialGenes(groups=x,z.threshold=z.threshold),n.cores=n.cores)

      x <- self$getExpressionBlock(scale.variance = TRUE, orientation = "gene_by_cell")
      dexp <- papply(dc,function(g) {
        dg <- self$getDifferentialGenes(groups=g, z.threshold=z.threshold)
        dg <- lapply(dg,function(x) x[x$Z>=z.threshold,])
        # calculate average profiles
        x <- x[rownames(x) %in% unlist(lapply(dg,rownames)),]
        if (nrow(x)<1) return(NULL)
        x <- x-rowMeans(x[,!is.na(g)])
        sf <- rep(1,nrow(x))
        names(sf) <- rownames(x)
        if (nrow(dg[[1]])>0) {
          ig <- which(names(sf) %in% rownames(dg[[1]]))
          sf[ig] <- 1/length(ig)
        }
        if (nrow(dg[[2]])>0) {
          ig <- which(names(sf) %in% rownames(dg[[2]]))
          sf[ig] <- -1/length(ig)
        }
        sf <- sf*sqrt(length(sf))
        pt <- colSums(x*sf)
        pt[is.na(g)] <- 0
        return(list(dg=dg,pt=pt))
      }, n.cores=n.cores)

      dexp <- dexp[!unlist(lapply(dexp,is.null))] # remove cases where nothing was reported
      dexp <- dexp[!unlist(lapply(dexp,function(x){class(x) == 'try-error'}))] ## remove cases that failed

      # fake pathwayOD output
      tamr <- list(xv=do.call(rbind,lapply(dexp,function(x) x$pt)),
                   cnam=lapply(sn(names(dexp)),function(n) c(n)))


      dgl <- lapply(dexp,function(d) as.character(unlist(lapply(d$dg,function(x) rownames(x)[x$Z>=z.threshold]))))
      tamr$env <- list2env(dgl[unlist(lapply(dgl,length))>=min.set.size])

      self$misc[['pathwayOD']] <- tamr

      # fake pathwayODInfo
      zs <- unlist(lapply(dexp,function(x) max(unlist(lapply(x$dg,function(y) y$Z)))))
      mval <- unlist(lapply(dexp,function(x) max(unlist(lapply(x$dg,function(y) y$M)))))
      vdf <- data.frame(i=1:nrow(tamr$xv),npc=1,valid=TRUE,sd=apply(tamr$xv,1,sd),cz=zs,z=zs,oe=mval,n=unlist(lapply(dexp,function(x) sum(unlist(lapply(x$dg,nrow))))))
      vdf$name <- rownames(vdf) <- names(dexp)
      self$misc[['pathwayODInfo']] <- vdf

      invisible(tamr)
    },

    #' @description Calculates gene Knn network for gene similarity
    #'
    #' @author Simon Steiger
    #' @param nPcs integer Number of principal components (default=100). This is the parameter 'nv' in irlba::irlba(), the number of right singular vectors to estimate.
    #' @param center boolean Whether to center the PCA (default=TRUE)
    #' @param fastpath boolean Whether to try a (fast) C algorithm implementation if possible (default=TRUE). This parameter is equivalent to 'fastpath' in irlba::irlba().
    #' @param maxit integer Maximum number of iterations (default=1000). This parameter is equivalent to 'maxit' in irlba::irlba().
    #' @param k integer Number of k clusters for calculating k-NN on the resulting principal components (default=30).
    #' 
    #' @return graph with gene similarity
    makeGeneKnnGraph = function(nPcs=100, center=TRUE, fastpath=TRUE, maxit=1000, k=30, n.cores=self$n.cores, verbose=TRUE) {
       # Transpose first
       x <- self$getExpressionBlock(orientation = "gene_by_cell")

      # TODO: factor out gene PCA calculation
      # Do the PCA
      nPcs <- min(nrow(x)-1,ncol(x)-1,nPcs)
      if (center) {
          cm <- Matrix::colMeans(x)
          pcs <- irlba(x, nv=nPcs, nu =0, center=cm, right_only = FALSE, fastpath = fastpath, maxit= maxit, reorth = TRUE)
      } else {
         pcs <- irlba(x, nv=nPcs, nu =0, right_only = FALSE, fastpath = fastpath, maxit= maxit, reorth = TRUE)
      }
      rownames(pcs$v) <- colnames(x)

      # Optional centering
      if (center) {
        pcs$center <- cm
        pcas <- as.matrix(t(t(x %*% pcs$v) - t(cm  %*% pcs$v)))
      } else {
        pcas <- as.matrix(x %*% pcs$v)
      }

      # Keep the names
      rownames(pcas) <- rownames(x)
      colnames(pcas) <- paste0('PC',seq(ncol(pcas)))

      # Save into genegraphs slot
      #genegraphs$genePCs <- pcs
      #genegraphs$geneRotated <- pcas

      # Using cosine distance only here
      if (center) {
        pcas <- pcas - Matrix::rowMeans(pcas)
      }
      xn <- N2R::Knn(pcas, k, nThreads= n.cores, verbose=verbose)
      diag(xn) <- 0 # Remove self edges
      xn <- as(xn,"TsparseMatrix") # will drop 0s
      # Turn into a dataframe, convert from correlation distance into weight
      df <- data.frame('from'=rownames(pcas)[xn@i+1],'to'=rownames(pcas)[xn@j+1],'w'=pmax(1-xn@x,0),stringsAsFactors=FALSE)

      self$genegraphs$graph <- df
    },

    #' @description Calculate density-based clusters
    #' 
    #' @param embeddingType The type of embedding used when calculating with `getEmbedding()` (default=NULL). Accepted values are: 'largeVis', 'tSNE', 'FR', 'UMAP', 'UMAP_graph' 
    #' @param name string Name fo the clustering (default='density').
    #' @param eps numeric value of the eps parameter, fed into dbscan::dbscan(x=emb, eps=eps, ...)
    #' @param v numeric The “value” to be used to complete the HSV color descriptions (default=0.7). Equivalent to the 'v' parameter in grDevices::rainbow().
    #' @param s numeric The “saturation” to be used to complete the HSV color descriptions (default=1). Equivalent to the 's' parameter in grDevices::rainbow().
    #' @param verbose boolean Whether to give verbose output (default=TRUE)
    #' @param ... Additional parameters passed to dbscan::dbscan(emb, ...)
    #'
    #' @return density-based clusters
    getDensityClusters=function(type='counts', embeddingType=NULL, name='density', eps=0.5, v=0.7, s=1, verbose=TRUE, ...) {
      if (!requireNamespace("dbscan", quietly = TRUE)) {
        stop("Package \"dbscan\" needed for this function to work. Please install it.", call. = FALSE)
      }

      if (is.null(self$embeddings[[type]])) { 
        stop("First, generate embeddings for type ",type)
      }
      if (is.null(embeddingType)) {
        ## take the last embedding generated
        embeddingType <- names(self$embeddings[[type]][length(self$embeddings[[type]])])
        if (verbose) message("using ",embeddingType," embedding\n")
        emb <- self$embeddings[[type]][[embeddingType]]
        if (is.null(emb)) { 
          stop("embedding ",embeddingType," for type ", type," doesn't exist")
        }
      } else {
        emb <- self$embeddings[[type]][[embeddingType]]
        if (is.null(emb)) { 
          stop("embedding ",embeddingType," for type ", type," doesn't exist")
        }
      }

      cl <- dbscan::dbscan(emb, eps=eps, ...)$cluster
      cols <- rainbow(length(unique(cl)),v=v,s=s)[cl+1]
      cols[cl==0] <- "gray70"
      names(cols) <- rownames(emb)
      self$clusters[[type]][[name]] <- cols
      self$misc[['clusters']][[type]][[name]] <- cols
      invisible(cols)
    },
    # determine subpopulation-specific genes

    #' @description Determine differentially expressed genes, comparing each group against all others using Wilcoxon rank sum test
    #' 
    #' @param name string Slot to store the results in (default='customClustering')
    #' @param z.threshold numeric Minimal absolute Z score (adjusted) to report (default=3)
    #' @param upregulated.only boolean Whether to report only genes that are expressed significantly higher in each group (default=FALSE)
    #' @param verbose boolean Whether to give verbose output (default=FALSE)
    #' @param append.specificity.metrics boolean Whether to append specifity metrics (default=TRUE). Uses the function sccore::appendSpecificityMetricsToDE(). 
    #' @param append.auc boolean If TRUE, append AUC values (default=FALSE). Parameter ignored if append.specificity.metrics is FALSE.
    #'
    #' @return List with each element of the list corresponding to a cell group in the provided/used factor (i.e. factor levels) 
    #'     Each element of a list is a data frame listing the differentially epxressed genes (row names), with the following columns: 
    #'     Z - adjusted Z score, with positive values indicating higher expression in a given group compare to the rest
    #'     M - log2 fold change
    #'     highest- a boolean flag indicating whether the expression of a given gene in a given vcell group was on average higher than in every other cell group
    #'     fe - fraction of cells in a given group having non-zero expression level of a given gene
    getDifferentialGenes=function(type='counts', clusterType=NULL, groups=NULL, grouping=NULL, name='customClustering', z.threshold=3, upregulated.only=FALSE, verbose=FALSE, append.specificity.metrics=TRUE, append.auc=FALSE, .legacy.warn=TRUE) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("getDifferentialGenes()", "p2$runMarkers(...)")
	      }
	      name.missing <- missing(name)
	      if (!is.null(grouping) && !is.null(groups)) {
	        stop("Specify only one of `grouping` or `groups`")
	      }
	      if (is.null(groups) && (!is.null(grouping) || !is.null(self$defaultGrouping))) {
	        resolved.grouping <- if (is.null(grouping)) self$defaultGrouping else grouping
	        groups <- self$resolveGrouping(grouping = grouping, allow.missing = TRUE)
	        if (name.missing) {
	          name <- resolved.grouping
	        }
	      }
      # restrict counts to the cells for which non-NA value has been specified in groups
      if (is.null(groups)) {
        ## # look up the clustering based on a specified type
        ## if (is.null(clusterType)) {
        ##  # take the last clustering generated
        ##  cols <- self$clusters[[type]][[length(self$clusters[[type]])]]
        ##  if (is.null(cols)) { 
        ##    stop("Clustering ",clusterType," for type ", type," doesn't exist")
        ##  }
        ## } else {
        ##  cols <- self$clusters[[type]][[clusterType]]
        ##  if (is.null(cols)) { 
        ##    stop("Clustering ",clusterType," for type ", type," doesn't exist")
        ##  }
        ## }
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take the 
          cols <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        } else {
          cols <- self$clusters[[type]][[clusterType]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        cols <- groups
      }
      all.cells <- .pagoda2_axis_names(self, "cell")
      if (!all(all.cells %in% names(cols))) {
        warning("cluster vector doesn't specify groups for all of the cells, dropping missing cells from comparison")
      }
      # determine a subset of cells that's in the cols and cols[cell]!=NA
      valid.cells <- all.cells %in% names(cols)[!is.na(cols)]
      if (!any(valid.cells)) {
        stop("No cells with non-missing groups are present in counts")
      }
      cm <- self$getExpressionBlock(cells = all.cells[valid.cells])
      # reorder cols
      cols <- as.factor(cols[match(rownames(cm),names(cols))])

      cols <- as.factor(cols)
      if (verbose) {
        message("running differential expression with ",length(levels(cols))," clusters ... ")
      }
      # use offsets based on the base model

      # run wilcoxon test comparing each group with the rest
      lower.lpv.limit <- -100
      # calculate rank per-column (per-gene) average rank matrix
      xr <- sparse_matrix_column_ranks(cm)
      # calculate rank sums per group
      grs <- colSumByFac(xr,as.integer(cols))[-1,,drop=FALSE]
      # calculate number of non-zero entries per group
      xr@x <- numeric(length(xr@x))+1
      gnzz <- colSumByFac(xr,as.integer(cols))[-1,,drop=FALSE]
      #group.size <- as.numeric(tapply(cols,cols,length));
      group.size <- as.numeric(tapply(cols,cols,length))[1:nrow(gnzz)]
      group.size[is.na(group.size)]<-0 # trailing empty levels are cut off by colSumByFac
      # add contribution of zero entries to the grs
      gnz <- (group.size-gnzz)
      # rank of a 0 entry for each gene
      zero.ranks <- (nrow(xr)-diff(xr@p)+1)/2 # number of total zero entries per gene
      ustat <- t((t(gnz)*zero.ranks)) + grs - group.size*(group.size+1)/2
      # standardize
      n1n2 <- group.size*(nrow(cm)-group.size)
      # usigma <- sqrt(n1n2*(nrow(cm)+1)/12) # without tie correction
      # correcting for 0 ties, of which there are plenty
      usigma <- sqrt(n1n2*(nrow(cm)+1)/12)
      usigma <- sqrt((nrow(cm) +1 - (gnz^3 - gnz)/(nrow(cm)*(nrow(cm)-1)))*n1n2/12)
      x <- t((ustat - n1n2/2)/usigma) # standardized U value- z score


      # correct for multiple hypothesis
      if (verbose) {
        message("adjusting p-values ... ")
      }
      x <- matrix(qnorm(bh.adjust(pnorm(as.numeric(abs(x)), lower.tail = FALSE, log.p = TRUE), log = TRUE), lower.tail = FALSE, log.p = TRUE),ncol=ncol(x))*sign(x)
      rownames(x) <- colnames(cm)
      colnames(x) <- levels(cols)[1:ncol(x)]
      if (verbose) {
        message("done.\n")
      }

      # add fold change information
      log.gene.av <- log2(Matrix::colMeans(cm))
      group.gene.av <- colSumByFac(cm,as.integer(cols))[-1,,drop=FALSE] / (group.size+1)
      log2.fold.change <- log2(t(group.gene.av)) - log.gene.av
      # fraction of cells expressing
      f.expressing <- t(gnzz / group.size)
      max.group <- max.col(log2.fold.change)

      ds <- lapply(1:ncol(x),function(i) {
        z <- x[,i]
        vi <- which((if (upregulated.only) z else abs(z)) >= z.threshold)
        r <- data.frame(Z=z[vi],M=log2.fold.change[vi,i],highest=max.group[vi]==i,fe=f.expressing[vi,i], Gene=rownames(x)[vi])
        rownames(r) <- r$Gene
        r <- r[order(r$Z,decreasing=TRUE), ]
        r
      })
      names(ds) <- colnames(x)

      if (append.specificity.metrics) {
        ds <- names(ds) %>% setNames(., .) %>%
          papply(function(n) sccore::appendSpecificityMetricsToDE(ds[[n]], cols, n, p2.counts=cm, append.auc=append.auc), n.cores=self$n.cores)
      }

      if (is.null(groups)) {
        if (is.null(clusterType)) {
          # self$diffgenes[[type]][[ names(self$clusters[[type]])[1] ]] <- ds
          ## take last clustering generated
          self$diffgenes[[type]][[names(self$clusters[[type]])[length(self$clusters[[type]])]]] <- ds
        } else {
          self$diffgenes[[type]][[clusterType]] <- ds
        }
      } else {
        self$diffgenes[[type]][[name]] <- ds
      }
      return(ds)
    },

	    #' @description Run marker detection for a resolved grouping and record provenance.
	    #'
	    #' @param grouping Name of a discrete cellMeta column. NULL uses defaultGrouping.
	    #' @param groups Direct vector of group labels. Mutually exclusive with grouping.
	    #' @param name Name of the marker result. Defaults to the grouping name.
	    #' @param type Count matrix type passed to getDifferentialGenes().
	    #' @param z.threshold Z-score threshold passed to getDifferentialGenes().
	    #' @param upregulated.only Whether to keep only upregulated markers.
	    #' @param verbose Whether to emit progress messages.
	    #' @param append.specificity.metrics Whether to append specificity metrics.
	    #' @param append.auc Whether to append AUC to marker tables.
	    #' @return Marker result list returned by getDifferentialGenes().
	    runMarkers=function(grouping=NULL, groups=NULL, name=NULL, type='counts', z.threshold=3,
	                        upregulated.only=TRUE, verbose=FALSE, append.specificity.metrics=TRUE,
	                        append.auc=TRUE) {
	      resolved.grouping <- grouping
	      if (is.null(resolved.grouping) && is.null(groups)) {
	        resolved.grouping <- self$defaultGrouping
	      }
	      cols <- self$resolveGrouping(grouping = grouping, groups = groups, allow.missing = TRUE)
	      if (is.null(name)) {
	        name <- if (!is.null(resolved.grouping)) resolved.grouping else "customGrouping"
	      }
	      ds <- self$getDifferentialGenes(
	        type = type,
	        groups = cols,
	        name = name,
	        z.threshold = z.threshold,
	        upregulated.only = upregulated.only,
	        verbose = verbose,
	        append.specificity.metrics = append.specificity.metrics,
	        append.auc = append.auc,
	        .legacy.warn = FALSE
	      )
	      params <- list(
	        z.threshold = z.threshold,
	        upregulated.only = upregulated.only,
	        append.specificity.metrics = append.specificity.metrics,
	        append.auc = append.auc
	      )
	      result <- .pagoda2_marker_result(
	        name = name,
	        type = type,
	        grouping = resolved.grouping,
	        groups = cols,
	        tables = ds,
	        params = params
	      )
	      meta <- .pagoda2_marker_metadata(result)
	      meta$group.levels <- meta$levels
	      meta$cell.names <- meta$cells
	      attr(ds, "pagoda2.marker") <- meta
	      result$tables <- ds
	      self$diffgenes[[type]][[name]] <- ds
	      if (is.null(self$markerResults[[type]])) {
	        self$markerResults[[type]] <- list()
	      }
	      self$markerResults[[type]][[name]] <- result
	      if (is.null(self$history$markers)) {
	        self$history$markers <- list()
	      }
	      self$history$markers[[name]] <- meta
	      invisible(ds)
	    },


    #' @description Plot heatmap of DE results
    #' 
    #' @param n.genes integer Number of genes to plot (default=100)
    #' @param z.score numeric Threshold of z-scores to filter (default=2). Only greater than or equal to this value are kept.
    #' @param gradient.range.quantile numeric Trimming quantile (default=0.95)
    #' @param inner.clustering boolean Whether to cluster cells within each cluster (default=FALSE)
    #' @param gradientPalette palette of colors to use (default=NULL). If NULL, uses 'colorRampPalette(c('gray90','red'), space = "Lab")(1024)'
    #' @param v numeric The “value” to be used to complete the HSV color descriptions (default=0.7). Equivalent to the 'v' parameter in grDevices::rainbow().
    #' @param s numeric The “saturation” to be used to complete the HSV color descriptions (default=1). Equivalent to the 's' parameter in grDevices::rainbow().
    #' @param box boolean Whether to draw a box around the current plot in the given color and linetype (default=TRUE)
    #' @param drawGroupNames boolean Whether to draw group names (default=FALSE)
    #' @param ... Additional parameters passed to internal function used for heatmap plotting, my.heatmap2()
    #' 
    #' @return heatmap of DE results
    plotDiffGeneHeatmap=function(type='counts', clusterType=NULL, groups=NULL, n.genes=100, 
      z.score=2, gradient.range.quantile=0.95, inner.clustering=FALSE, gradientPalette=NULL, 
      v=0.8, s=1, box=TRUE, drawGroupNames=FALSE, .legacy.warn=TRUE, ... ) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("plotDiffGeneHeatmap()", "p2$plotMarkerHeatmap(...)")
	      }
      if (!is.null(clusterType)) {
        x <- self$diffgenes[[type]][[clusterType]]
        if (is.null(x)) { 
          stop("Differential genes for the specified cluster type ", clusterType, " haven't been calculated") 
        }
      } else {
        ## x <- self$diffgenes[[type]][[1]]
        ## take last generated item
        x <- self$diffgenes[[type]][[length(self$diffgenes[[type]])]]
        if (is.null(x)) { 
          stop("No differential genes found for data type ",type) 
        }
      }

      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          cols <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        } else {
          cols <- self$clusters[[type]][[clusterType]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        # use clusters information
        all.cells <- .pagoda2_axis_names(self, "cell")
        if (!all(all.cells %in% names(groups))) { warning("provided cluster vector doesn't list groups for all of the cells")}
        cols <- as.factor(groups[match(all.cells,names(groups))])
      }
      cols <- as.factor(cols)
      # select genes to show
      if (!is.null(z.score)) {
        x <- lapply(x,function(d) d[d$Z >= z.score & d$highest==TRUE,])
        if (!is.null(n.genes)) {
          x <- lapply(x,function(d) {if(nrow(d)>0) { d[1:min(nrow(d),n.genes),]}})
        }
      } else {
        if (!is.null(n.genes)) {
          x <- lapply(x,function(d) {if(nrow(d)>0) { d[1:min(nrow(d),n.genes),]}})
        }
      }
      x <- lapply(x,rownames)
      # make expression matrix
      #x <- x[!unlist(lapply(x,is.null))]
      #cols <- cols[cols %in% names(x)]
      #cols <- droplevels(cols)
      em <- self$getExpressionBlock(genes = unlist(x))
      # renormalize rows
      if (all(sign(em)>=0)) {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c('gray90','red'), space = "Lab")(1024)
        }
        em <- apply(em,1,function(x) {
          zlim <- as.numeric(quantile(x,p=c(1-gradient.range.quantile,gradient.range.quantile)))
          if (diff(zlim)==0) {
            zlim <- as.numeric(range(x))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        })
      } else {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c("blue", "grey90", "red"), space = "Lab")(1024)
        }
        em <- apply(em,1,function(x) {
          zlim <- c(-1,1)*as.numeric(quantile(abs(x),p=gradient.range.quantile))
          if (diff(zlim)==0) {
            zlim <- c(-1,1)*as.numeric(max(abs(x)))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        })
      }

      # cluster cell types by averages
      rowfac <- factor(rep(names(x),unlist(lapply(x,length))),levels=names(x))
      if (inner.clustering) {
        clclo <- stats::hclust(as.dist(1-cor(do.call(cbind,tapply(1:nrow(em),rowfac,function(ii) Matrix::colMeans(em[ii,,drop=FALSE]))))),method='complete')$order
      } else {
        clclo <- 1:length(levels(rowfac))
      }

      if (inner.clustering) {
        # cluster genes within each cluster
        clgo <- tapply(1:nrow(em),rowfac,function(ii) {
          ii[stats::hclust(as.dist(1-cor(t(em[ii,]))),method='complete')$order]
        })
      } else {
        clgo <- tapply(1:nrow(em),rowfac,I)
      }
      if (inner.clustering) {
        # cluster cells within each cluster
        clco <- tapply(1:ncol(em),cols,function(ii) {
          if (length(ii)>3) {
            ii[stats::hclust(as.dist(1-cor(em[,ii,drop=FALSE])),method='complete')$order]
          } else {
            ii
          }
        })
      } else {
        clco <- tapply(1:ncol(em),cols,I)
      }
      #clco <- clco[names(clgo)]
      # filter down to the clusters that are included
      #vic <- cols %in% clclo
      colors <- fac2col(cols,v=v,s=s,return.details=TRUE)
      cellcols <- colors$colors[unlist(clco[clclo])]
      genecols <- rev(rep(colors$palette,unlist(lapply(clgo,length)[clclo])))
      bottomMargin <- ifelse(drawGroupNames,4,0.5)
      my.heatmap2(em[rev(unlist(clgo[clclo])),unlist(clco[clclo])],col=gradientPalette,Colv=NA,Rowv=NA,labRow=NA,labCol=NA,RowSideColors=genecols,ColSideColors=cellcols,margins=c(bottomMargin,0.5),ColSideColors.unit.vsize=0.05,RowSideColors.hsize=0.05,useRaster=TRUE, box=box, ...)
      abline(v=cumsum(unlist(lapply(clco[clclo],length))),col=1,lty=3)
      abline(h=cumsum(rev(unlist(lapply(clgo[clclo],length)))),col=1,lty=3)
    },

    #' @description Recalculate library sizes using robust regression within clusters
    #' @param clusterType Name of cluster to access (default=NULL). If NULL, takes the most recently generated clustering. Parameter ignored if groups is not NULL.
    #' @param groups factor named with cell names specifying the clusters of cells (default=NULL)
    #' @param type string Either 'counts' or the name of a stored embedding, names(self$embeddings) (default=NULL)
    #' @param n.cores numeric Number of cores to use (default=self$n.cores=1)
    #'
    #' @return recalculated library sizes
    getRefinedLibSizes=function(clusterType=NULL, groups=NULL, type='counts', n.cores=self$n.cores) {

      if (!requireNamespace("robustbase", quietly = TRUE)) {
        stop("Package \"robustbase\" needed for this function to work. Please install it.", call. = FALSE)
      }
 
      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take the last-generated clustering
          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      }

      # calculated pooled profiles per cluster
      lvec <- colSumByFac(self$misc[['rawCounts']],as.integer(groups))[-1,,drop=FALSE]
      lvec <- t(lvec/pmax(1,Matrix::rowSums(lvec)))*1e4

      # TODO: implement internal robust regression
      ## x <- misc[['rawCounts']]
      ## x <- x/as.numeric(depth);
      ## inplaceWinsorizeSparseCols(x,10);
      ## x <- x*as.numeric(depth);

      x <- mclapply(1:length(levels(groups)),function(j) {
        ii <- names(groups)[which(groups==j)]
        av <- lvec[,j]
        avi <- which(av>0)
        av <- av[avi]
        cvm <- as.matrix(self$misc[['rawCounts']][ii,avi])
        x <- unlist(lapply(ii,function(i) {
          cv <- cvm[i,]
          #as.numeric(coef(glm(cv~av+0,family=poisson(link='identity'),start=sum(cv)/1e4)))
          as.numeric(coef(robustbase::glmrob(cv~av+0,family=poisson(link='identity'),start=sum(cv)/1e4)))
        }))
        names(x) <- ii
        x
      },mc.cores=n.cores)

      lib.sizes <- unlist(x)[rownames(self$misc[['rawCounts']])]
      lib.sizes <- lib.sizes/mean(lib.sizes)*mean(Matrix::rowSums(self$misc[['rawCounts']]))

      self$depth <- lib.sizes
      invisible(lib.sizes)
    },

    #' @description Plot heatmap for a given set of genes
    #' 
    #' @param genes character vector Gene names
    #' @param gradient.range.quantile numeric Trimming quantile (default=0.95)
    #' @param cluster.genes boolean Whether to cluster genes within each cluster using stats::hclust() (default=FALSE)
    #' @param inner.clustering boolean Whether to cluster cells within each cluster (default=FALSE)
    #' @param gradientPalette palette of colors to use (default=NULL). If NULL, uses 'colorRampPalette(c('gray90','red'), space = "Lab")(1024)'
    #' @param v numeric The “value” to be used to complete the HSV color descriptions (default=0.7). Equivalent to the 'v' parameter in grDevices::rainbow().
    #' @param s numeric The “saturation” to be used to complete the HSV color descriptions (default=1). Equivalent to the 's' parameter in grDevices::rainbow().
    #' @param box boolean Whether to draw a box around the current plot in the given color and linetype (default=TRUE)
    #' @param drawGroupNames boolean Whether to draw group names (default=FALSE)
    #' @param useRaster boolean If TRUE a bitmap raster is used to plot the image instead of polygons (default=TRUE). The grid must be regular in that case, otherwise an error is raised. For more information, see graphics::image().
    #' @param smooth.span Running mean span. NULL uses max(1, round(number of cells / 1024)).
    #' @param ... Additional parameters passed to internal function used for heatmap plotting, my.heatmap2()
    #'
	    #' @return plot of gene heatmap
	    plotGeneHeatmap=function(genes, type='counts', clusterType=NULL, groups=NULL, 
	      gradient.range.quantile=0.95, cluster.genes=FALSE, inner.clustering=FALSE, gradientPalette=NULL, 
	      v=0.8, s=1, box=TRUE, drawGroupNames=FALSE, useRaster=TRUE, smooth.span=NULL,
	      .legacy.warn=TRUE, ... ) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("plotGeneHeatmap()", "p2$plotHeatmap(...)")
	      }
	      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          cols <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        } else {
          cols <- self$clusters[[type]][[clusterType]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        # use clusters information
        all.cells <- .pagoda2_axis_names(self, "cell")
        if (!all(all.cells %in% names(groups))) { warning("provided cluster vector doesn't list groups for all of the cells")}
        cols <- as.factor(groups[match(all.cells,names(groups))])
      }
      cols <- as.factor(cols)
      # make expression matrix
      available.genes <- .pagoda2_axis_names(self, "gene")
      if (!all(genes %in% available.genes)) {
        warning(paste("The following specified genes were not found in the data: [",paste(genes[!genes %in% available.genes],collapse=" "),"], omitting",sep=""))
      }
      x <- intersect(genes,available.genes)
      if (length(x)<1) { 
        stop("Too few genes") 
      }
      em <- as.matrix(t(self$getExpressionBlock(genes = x)))
      if (is.null(smooth.span)) {
        smooth.span <- max(1, round(length(.pagoda2_axis_names(self, "cell")) / 1024))
      }

      # renormalize rows
      if (all(sign(em)>=0)) {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c('gray90','red'), space = "Lab")(1024)
        }
        em <- t(apply(em,1,function(x) {
          zlim <- as.numeric(quantile(x, p=c(1-gradient.range.quantile,gradient.range.quantile)))
          if (diff(zlim)==0) {
            zlim <- as.numeric(range(x))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        }))
      } else {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c("blue", "grey90", "red"), space = "Lab")(1024)
        }
        em <- t(apply(em,1,function(x) {
          zlim <- c(-1,1)*as.numeric(quantile(abs(x),p=gradient.range.quantile))
          if (diff(zlim)==0) {
            zlim <- c(-1,1)*as.numeric(max(abs(x)))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        }))
      }

      # cluster cell types by averages
      clclo <- 1:length(levels(cols))
      # quick function to set distance of NA/Inf/NaN to 1
      t.fixcor <- function(x) { x[!is.finite(x)]<-1; x }

      if (cluster.genes) {
        # cluster genes within each cluster
        clgo <- stats::hclust(as.dist(t.fixcor(1-cor(t(em)))), method='complete')$order
      } else {
        clgo <- 1:nrow(em)
      }

      if (inner.clustering) {
        # cluster cells within each cluster
        clco <- tapply(1:ncol(em),cols,function(ii) {
          if (length(ii)>3) {
            ii[stats::hclust(as.dist(t.fixcor(1-cor(em[,ii,drop=FALSE]))), method='single')$order]
          } else {
            ii
          }
          # TODO: implement smoothing span support
        })
      } else {
        clco <- tapply(1:ncol(em),cols,I)
      }

      cellcols <- fac2col(cols,v=v,s=s)[unlist(clco[clclo])]
      #genecols <- rev(rep(fac2col(cols,v=v,s=s,return.level.colors=T),unlist(lapply(clgo,length)[clclo])))
      bottomMargin <- 0.5
      # reorder and potentially smooth em
      em <- em[rev(clgo),unlist(clco[clclo])]

      my.heatmap2(em,col=gradientPalette,Colv=NA,Rowv=NA,labCol=NA,ColSideColors=cellcols,margins=c(bottomMargin,5),ColSideColors.unit.vsize=0.05,RowSideColors.hsize=0.05,useRaster=useRaster, box=box, ...)
      bp <- cumsum(unlist(lapply(clco[clclo],length))) # cluster border positions
      abline(v=bp,col=1,lty=3)
      #abline(h=cumsum(rev(unlist(lapply(clgo[clclo],length)))),col=1,lty=3)
      if (drawGroupNames) {
        clpos <- (c(0,bp[-length(bp)])+bp)/2
        labpos <- rev(seq(0,length(bp)+1)/(length(bp)+1)*nrow(em))
        labpos <- labpos[-1]
        labpos <- labpos[-length(labpos)]
        text(x=clpos,y=labpos,labels = levels(cols),cex=1)
        # par(xpd=TRUE)
        # clpos <- (c(0,bp[-length(bp)])+bp)/2;
        # labpos <- seq(0,length(bp)+1)/(length(bp)+1)*max(bp); labpos <- labpos[-1]; labpos <- labpos[-length(labpos)]
        # text(x=labpos,y=-2,labels = levels(col))
        # segments(labpos,-1,clpos,0.5,lwd=0.5)
        # par(xpd=FALSE)
	      }
	    },

	    #' @description Plot gene heatmap using the pagoda2.1 API name.
	    #'
	    #' @param genes Gene names to plot.
	    #' @param grouping Name of a discrete cellMeta column. NULL uses defaultGrouping.
	    #' @param groups Direct vector of group labels.
	    #' @param type Count matrix namespace.
	    #' @param ... Arguments passed to plotGeneHeatmap().
	    #' @return Heatmap side effect from plotGeneHeatmap().
	    plotHeatmap=function(genes, grouping=NULL, groups=NULL, type='counts', ...) {
	      resolved.groups <- NULL
	      if (!is.null(grouping) || !is.null(groups) || !is.null(self$defaultGrouping)) {
	        resolved.groups <- self$resolveGrouping(grouping = grouping, groups = groups, allow.missing = TRUE)
	      }
	      self$plotGeneHeatmap(genes = genes, type = type, groups = resolved.groups, ..., .legacy.warn = FALSE)
	    },

	    #' @description Plot marker expression as a grouped dot plot.
	    #'
	    #' @param markers Marker result name. NULL uses defaultGrouping.
	    #' @param type Marker result namespace.
	    #' @param genes Optional explicit genes to plot. NULL selects top marker genes.
	    #' @param grouping Optional grouping column. NULL uses marker provenance when available, then defaultGrouping.
	    #' @param groups Optional direct grouping vector.
	    #' @param n.genes.per.group Number of marker genes to select per group when genes is NULL.
	    #' @param z.threshold Optional marker Z threshold used during selection.
	    #' @param highest.only Whether to keep genes marked as highest in their group.
	    #' @param ordering Marker table ordering preference.
	    #' @param remove.duplicates Whether to keep only the first selected occurrence of each gene.
	    #' @param count.matrix Optional cell-by-gene matrix. Defaults to the selected analysis expression block.
	    #' @param n.cores Number of cores passed to sccore::dotPlot().
	    #' @param cols Two-color expression gradient passed to sccore::dotPlot().
	    #' @param dot.scale Maximum dot size passed to sccore::dotPlot().
	    #' @param scale.by Dot size scaling mode, `size` or `radius`.
	    #' @param text.angle X-axis marker label angle.
	    #' @param ... Arguments passed to sccore::dotPlot().
	    #' @return ggplot object.
	    plotMarkerDotPlot=function(markers=NULL, type='counts', genes=NULL, grouping=NULL, groups=NULL,
	                               n.genes.per.group=5, z.threshold=3, highest.only=TRUE,
	                               ordering=c("-AUC", "-Z", "-Precision", "-Specificity", "-M"),
	                               remove.duplicates=TRUE, count.matrix=NULL, n.cores=self$n.cores,
	                               cols=c("grey88", "firebrick3"), dot.scale=7,
	                               scale.by="size", text.angle=45, ...) {
	      resolved <- self$resolveMarkers(markers = markers, type = type)
	      selected <- .pagoda2_select_marker_genes(
	        resolved$tables,
	        n.genes.per.group = n.genes.per.group,
	        genes = genes,
	        z.threshold = z.threshold,
	        highest.only = highest.only,
	        ordering = ordering,
	        remove.duplicates = remove.duplicates
	      )
	      available.genes <- if (is.null(count.matrix)) .pagoda2_axis_names(self, "gene") else colnames(count.matrix)
	      missing.genes <- setdiff(selected$genes, available.genes)
	      if (length(missing.genes) > 0) {
	        warning("Omitting marker genes absent from count matrix: ", paste(missing.genes, collapse = ", "))
	      }
	      selected.genes <- intersect(selected$genes, available.genes)
	      if (length(selected.genes) == 0) {
	        stop("No selected marker genes are present in count matrix")
	      }
	      if (is.null(count.matrix)) {
	        count.matrix <- self$getExpressionBlock(genes = selected.genes)
	      }
	      if (is.null(rownames(count.matrix)) || is.null(colnames(count.matrix))) {
	        stop("`count.matrix` must have cell row names and gene column names")
	      }
	      if (is.null(grouping) && is.null(groups) && !is.null(resolved$result$grouping)) {
	        grouping <- resolved$result$grouping
	      }
	      resolved.groups <- self$resolveGrouping(
	        grouping = grouping,
	        groups = groups,
	        cells = rownames(count.matrix),
	        allow.missing = TRUE
	      )
	      sccore::dotPlot(
	        markers = selected.genes,
	        count.matrix = count.matrix,
	        cell.groups = resolved.groups,
	        n.cores = n.cores,
	        gene.order = selected.genes,
	        cols = cols,
	        dot.scale = dot.scale,
	        scale.by = scale.by,
	        text.angle = text.angle,
	        ...
	      ) +
	        ggplot2::theme(
	          axis.text = ggplot2::element_text(size = 10.5),
	          axis.title = ggplot2::element_text(size = 11.5),
	          legend.text = ggplot2::element_text(size = 10),
	          legend.title = ggplot2::element_text(size = 10.5),
	          plot.title = ggplot2::element_text(size = 13)
	        )
	    },

		    #' @description Plot marker heatmap using the pagoda2.1 API name.
	    #'
	    #' @param markers Marker result name. NULL uses defaultGrouping.
	    #' @param type Marker result namespace.
	    #' @param engine Heatmap engine: native for lightweight grid raster, complex for ComplexHeatmap, or legacy for plotDiffGeneHeatmap().
	    #' @param genes Optional explicit genes to plot. NULL selects top marker genes.
	    #' @param grouping Optional grouping column. NULL uses marker provenance when available, then defaultGrouping.
	    #' @param groups Optional direct grouping vector.
	    #' @param n.genes.per.group Number of marker genes to select per group when genes is NULL.
	    #' @param additional.genes Optional extra genes to append to the heatmap.
	    #' @param exclude.genes Optional genes to exclude after marker selection.
	    #' @param z.threshold Optional marker Z threshold used during selection.
	    #' @param highest.only Whether to keep genes marked as highest in their group.
	    #' @param ordering Marker table ordering preference.
	    #' @param remove.duplicates Whether to keep only the first selected occurrence of each gene.
	    #' @param expression.quantile Quantile used to trim each gene before 0-1 scaling.
	    #' @param pal Color palette for expression heatmap.
	    #' @param column.metadata Optional cell metadata columns, data.frame, or named list to show as top annotations.
	    #' @param column.metadata.colors Optional ComplexHeatmap annotation color list.
	    #' @param show.gene.groups Whether to show marker-origin groups as a row annotation.
	    #' @param show.group.legend Whether to show group legends.
	    #' @param show_heatmap_legend Whether to show expression heatmap legend.
	    #' @param border Whether to draw annotation/heatmap borders.
	    #' @param row.label.font.size Gene label font size.
	    #' @param labeled.gene.subset Optional genes, or top n marker genes per group, to label with anno_mark().
	    #' @param group.colors Optional named colors for cell groups.
	    #' @param gene.group.colors Optional named colors for marker-origin row groups.
	    #' @param order.groups Whether to cluster group order by shown marker expression.
	    #' @param cluster.rows Whether to hierarchically cluster genes within marker groups.
	    #' @param cluster.columns Whether to hierarchically cluster cells within cell groups.
	    #' @param cluster.max.items Maximum rows/cells to cluster within any one group.
	    #' @param cluster.method hclust method used for row/column clustering.
	    #' @param split Whether to split rows and columns by marker/cell group.
	    #' @param split.gap Split gap in mm when split=TRUE.
	    #' @param cell.order Optional explicit cell order or ordered cell subset.
	    #' @param averaging.window Optional left-aligned running mean width within each group.
	    #' @param annotation.grobs Optional list of grid grobs with top/right/bottom/left entries for the native engine.
	    #' @param legend.max.levels Maximum discrete levels to show per native legend before truncation.
	    #' @param legend.columns Optional number of columns for native legend packing.
	    #' @param native.newpage Whether the native engine should start a new grid page.
	    #' @param v HSV value used for generated group colors.
	    #' @param s HSV saturation used for generated group colors.
	    #' @param max.cells Maximum cells per group to show.
	    #' @param max.dense.entries Warn when selected genes by selected cells exceeds this many entries.
	    #' @param use.raster Whether ComplexHeatmap should rasterize the expression layer.
	    #' @param raster.by.magick Whether ComplexHeatmap should use magick for rasterization.
	    #' @param return.details Whether to return internals along with the heatmap object.
	    #' @param ... Arguments passed to ComplexHeatmap::Heatmap() or the legacy heatmap.
	    #' @return ComplexHeatmap object, details list, or legacy heatmap side effect.
	    plotMarkerHeatmap=function(markers=NULL, type='counts', engine=c("native", "complex", "legacy"),
	                               genes=NULL, grouping=NULL, groups=NULL, n.genes.per.group=5,
	                               additional.genes=NULL, exclude.genes=NULL,
	                               z.threshold=2, highest.only=TRUE,
	                               ordering=c("-AUC", "-Z", "-Precision", "-Specificity", "-M"),
	                               remove.duplicates=TRUE, expression.quantile=0.99,
	                               pal=colorRampPalette(c('grey95','firebrick3'), space = "Lab")(1024),
	                               column.metadata=NULL, column.metadata.colors=NULL,
	                               show.gene.groups=TRUE, show.group.legend=TRUE,
	                               show_heatmap_legend=FALSE, border=TRUE,
	                               row.label.font.size=10, labeled.gene.subset=NULL,
	                               group.colors=NULL, gene.group.colors=NULL,
	                               order.groups=FALSE, cluster.rows=FALSE, cluster.columns=FALSE,
	                               cluster.max.items=2000, cluster.method="complete",
	                               split=FALSE, split.gap=0,
	                               cell.order=NULL, averaging.window=0,
	                               annotation.grobs=NULL, legend.max.levels=18,
	                               legend.columns=NULL, native.newpage=TRUE,
	                               v=1, s=1,
	                               max.cells=Inf,
	                               max.dense.entries=5e7,
	                               use.raster=TRUE, raster.by.magick=FALSE,
	                               return.details=FALSE, ...) {
	      engine <- match.arg(engine)
	      resolved <- self$resolveMarkers(markers = markers, type = type)
	      if (engine == "legacy") {
	        legacy.groups <- groups
	        if (is.null(legacy.groups) && !is.null(grouping)) {
	          legacy.groups <- self$resolveGrouping(grouping = grouping, allow.missing = TRUE)
	        } else if (is.null(legacy.groups) && !is.null(resolved$result$grouping)) {
	          legacy.groups <- self$resolveGrouping(grouping = resolved$result$grouping, allow.missing = TRUE)
	        }
	        return(self$plotDiffGeneHeatmap(type = type, clusterType = resolved$name, groups = legacy.groups, ..., .legacy.warn = FALSE))
	      }
	      spec <- .pagoda2_prepare_marker_heatmap(
	        self,
	        markers = markers,
	        type = type,
	        genes = genes,
	        grouping = grouping,
	        groups = groups,
	        n.genes.per.group = n.genes.per.group,
	        additional.genes = additional.genes,
	        exclude.genes = exclude.genes,
	        z.threshold = z.threshold,
	        highest.only = highest.only,
	        ordering = ordering,
	        remove.duplicates = remove.duplicates,
	        expression.quantile = expression.quantile,
	        pal = pal,
	        column.metadata = column.metadata,
	        column.metadata.colors = column.metadata.colors,
	        show.gene.groups = show.gene.groups,
	        show.group.legend = show.group.legend,
	        show_heatmap_legend = show_heatmap_legend,
	        border = border,
	        row.label.font.size = row.label.font.size,
	        labeled.gene.subset = labeled.gene.subset,
	        group.colors = group.colors,
	        gene.group.colors = gene.group.colors,
	        order.groups = order.groups,
	        split = split,
	        split.gap = split.gap,
	        cell.order = cell.order,
	        averaging.window = averaging.window,
	        v = v,
	        s = s,
	        max.cells = max.cells,
	        max.dense.entries = max.dense.entries,
	        cluster.rows = cluster.rows,
	        cluster.columns = cluster.columns,
	        cluster.max.items = cluster.max.items,
	        cluster.method = cluster.method,
	        annotation.grobs = annotation.grobs,
	        legend.max.levels = legend.max.levels,
	        legend.columns = legend.columns
	      )
	      if (engine == "native") {
	        .pagoda2_draw_native_heatmap(spec, newpage = native.newpage)
	        if (return.details) {
	          return(.pagoda2_marker_heatmap_details(spec, heatmap = NULL, engine = "native"))
	        }
	        return(invisible(spec))
	      }
	      ht <- .pagoda2_render_marker_heatmap_complex(spec, use.raster = use.raster, raster.by.magick = raster.by.magick, ...)
	      if (return.details) {
	        return(.pagoda2_marker_heatmap_details(spec, heatmap = ht, engine = "complex"))
	      }
	      ht
	    },

    #' @description Show embedding
    #' 
    #' @param type string Either 'counts' or the name of a stored embedding, names(self$embeddings) (default=NULL)
    #' @param embeddingType string Embedding type (default=NULL). If NULL, takes the most recently generated embedding.
    #' @param clusterType Name of cluster to access (default=NULL). If NULL, takes the most recently generated clustering. Parameter ignored if groups is not NULL.
    #' @param groups factor named with cell names specifying the clusters of cells (default=NULL)
    #' @param colors character vector List of gene names (default=NULL)
    #' @param gene (default=NULL)
    #' @param plot.theme (default=ggplot2::theme_bw()) 
    #' @param ... Additional parameters passed to sccore::embeddingPlot()
    #' 
    #' @return plot of the embedding
	    plotEmbedding=function(type=NULL, embeddingType=NULL, reduction=NULL, embedding=NULL, clusterType=NULL,
	      groups=NULL, grouping=NULL, colors=NULL, gene=NULL, plot.theme=ggplot2::theme_bw(), .legacy.warn=TRUE, ...) {
	      dots <- list(...)

	      if (!is.null(reduction)) {
	        type <- reduction
	      }
	      if (!is.null(embedding)) {
	        embeddingType <- embedding
	      }
	      using.legacy.selector <- (!missing(type) && is.null(reduction)) ||
	        (!missing(embeddingType) && is.null(embedding)) ||
	        !is.null(clusterType)
	      if (.legacy.warn && using.legacy.selector) {
	        warning(
	          "Legacy plotEmbedding selectors `type`, `embeddingType`, and `clusterType` are deprecated and will be removed in the next pagoda2 version. ",
	          "Use `p2$plotEmbedding(reduction = ..., embedding = ..., grouping = ...)` instead.",
	          call. = FALSE
	        )
	      }

	      if (is.null(type)) {
	        if (!is.null(self$defaults$reduction) && self$defaults$reduction %in% names(self$embeddings)) {
	          type <- self$defaults$reduction
	        } else if ('counts' %in% names(self$embeddings)) {
	          type <- 'counts'
	        } else if (length(self$embeddings) > 0) {
	          # type <- names(self$embeddings)[1]
	          ## Use the last-generated embedding
	          if (.legacy.warn) {
	            warning(
	              "Implicit latest-embedding namespace selection is deprecated and will be removed in the next pagoda2 version. ",
	              "Use `p2$plotEmbedding(reduction = ..., embedding = ...)` instead.",
	              call. = FALSE
	            )
	          }
	          type <- names(self$embeddings[length(self$embeddings)])
	        } else {
	          stop("First, generate an embedding")
	        }
      }

      if (is.null(self$embeddings[[type]])){
        stop("First, generate embeddings for type ",type)
      }

	      if (is.null(embeddingType)){
	        if (!is.null(self$defaults$embedding) && !is.null(self$embeddings[[type]][[self$defaults$embedding]])) {
	          embeddingType <- self$defaults$embedding
	          emb <- self$embeddings[[type]][[embeddingType]]
	        } else {
	          ## take the most recently generated embedding
	          if (.legacy.warn) {
	            warning(
	              "Implicit latest embedding selection is deprecated and will be removed in the next pagoda2 version. ",
	              "Use `p2$plotEmbedding(embedding = ...)` instead.",
	              call. = FALSE
	            )
	          }
	          emb <- self$embeddings[[type]][[length(self$embeddings[[type]])]]
	        }
	      } else{
        ## check embeddingType exists
        if (is.null(self$embeddings[[type]][[embeddingType]])){
          stop("Embedding does not exist for embeddingType ", embeddingType)
        }
        emb <- self$embeddings[[type]][[embeddingType]]
      }

      if (!is.null(gene)) {
        if (!(gene %in% .pagoda2_axis_names(self, "gene"))){
          stop("Gene '", gene, "' isn't presented in the count matrix")
        }
        colors <- self$getExpressionBlock(genes = gene)[,gene]
      }

	      if (!is.null(grouping) && !is.null(clusterType)) {
	        stop("Specify only one of `grouping` or `clusterType`")
	      }

	      grouping.palette.name <- NULL
	      if (is.null(colors) && (!is.null(grouping) || !is.null(groups) || (is.null(clusterType) && !is.null(self$defaultGrouping)))) {
	        if (!is.null(grouping) && is.character(grouping) && length(grouping) == 1L && grouping %in% colnames(self$cellMeta)) {
	          grouping.palette.name <- grouping
	        } else if (is.character(groups) && length(groups) == 1L && is.null(names(groups)) && groups %in% colnames(self$cellMeta)) {
	          grouping.palette.name <- groups
	        } else if (is.null(grouping) && is.null(groups) && is.null(clusterType) && !is.null(self$defaultGrouping)) {
	          grouping.palette.name <- self$defaultGrouping
	        }
	        groups <- self$resolveGrouping(grouping = grouping, groups = groups, cells = rownames(emb), allow.missing = TRUE)
	      }

      if (is.null(colors) && is.null(groups)) {
        # look up the clustering based on a specified type
	        if (is.null(clusterType)) {
	          # groups <- self$clusters[[type]][[1]]
	          ## Take last-genereated clustering
	          if (.legacy.warn) {
	            warning(
	              "Implicit latest clustering selection is deprecated and will be removed in the next pagoda2 version. ",
	              "Use `p2$plotEmbedding(grouping = ...)` or set `p2$defaultGrouping` instead.",
	              call. = FALSE
	            )
	          }
	          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      }

	      if (is.null(colors) && !is.null(groups) && is.null(dots$palette) && !is.null(grouping.palette.name)) {
	        dots$palette <- self$resolveFactorColors(
	          axis = "cell",
	          name = grouping.palette.name,
	          values = groups,
	          store = FALSE
	        )
	      }

	      do.call(
	        sccore::embeddingPlot,
	        c(list(object = emb, groups = groups, colors = colors, plot.theme = plot.theme), dots)
	      )
    },

    #' @description Get overdispersed genes
    #' 
    #' @param alpha numeric The Type I error probability or the significance level (default=5e-2). This is the criterion used to measure statistical significance, i.e. if the p-value < alpha, then it is statistically significant.
    #' @param use.unadjusted.pvals boolean Whether to use Benjamini-Hochberg adjusted p-values (default=FALSE).
    #'
    #' @return vector of overdispersed genes
    getOdGenes=function(n.odgenes=NULL, alpha=5e-2, use.unadjusted.pvals=FALSE) {
      if (is.null(self$misc[['varinfo']])) { 
        stop("Please run adjustVariance first")
      }
      if (is.null(n.odgenes)) { #return according to alpha
        if (use.unadjusted.pvals) {
          rownames(self$misc[['varinfo']])[self$misc[['varinfo']]$lp <= log(alpha)]
        } else {
          rownames(self$misc[['varinfo']])[self$misc[['varinfo']]$lpa <= log(alpha)]
        }
      } else { # return top n.odgenes sites
        rownames(self$misc[['varinfo']])[(order(self$misc[['varinfo']]$lp, decreasing=FALSE)[1:min(length(.pagoda2_axis_names(self, "gene")),n.odgenes)])]
      }
    },

    #' @description Return variance-normalized matrix for specified genes or a number of OD genes
    #'
    #' @param genes vector of gene names to explicitly return (default=NULL)
    #' 
    #' @return cell by gene matrix
    getNormalizedExpressionMatrix=function(genes=NULL, n.odgenes=NULL) {
      if (is.null(genes)) {
        genes <- self$getOdGenes(n.odgenes)
      }
      self$getExpressionBlock(genes = genes, scale.variance = TRUE)
    },


    #' @description Calculate PCA reduction of the data
    #' 
    #' @param nPcs numeric Number of principal components (PCs) (default=50)
    #' @param type string Dataset view to reduce (counts by default, but can specify a name of an existing reduction) (default='counts')
    #' @param name string Name for the PCA reduction to be created (default='PCA')
    #' @param use.odgenes boolean Whether pre-calculated set of overdispersed genes should be used (default=TRUE)
    #' @param n.odgenes integer Number of top overdispersed genes to use (default=3000).
    #' @param odgenes Explicitly specify a set of overdispersed genes to use for the reduction (default=NULL)
    #' @param center boolean Whether data should be centered prior to PCA (default=TRUE)
    #' @param cells optional subset of cells on which PCA should be run (default=NULL)
    #' @param fastpath boolean Use C implementation for speedup (default=TRUE)
    #' @param maxit numeric Maximum number of iterations (default=100). For more information, see 'maxit' parameter in irlba::irlba(). 
    #' @param var.scale boolean Apply scaling if using raw counts (default=TRUE). If type="counts", var.scale is TRUE by default.
    #' @param ... additional arguments forwarded to irlba::irlba
    #' 
	    #' @return Invisible PCA result (the reduction itself is saved in self$reductions[[name]])"
	    calculatePcaReduction=function(nPcs=50, type='counts', name='PCA', use.odgenes=TRUE, n.odgenes=3000,
	      odgenes=NULL, center=TRUE, cells=NULL, fastpath=TRUE, maxit=100, verbose=TRUE, var.scale=(type == "counts"),
	      .legacy.warn=TRUE, ...) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("calculatePcaReduction()", "p2$runPCA(...)")
	      }

	      if (type!='counts') {
        if (!type %in% names(self$reductions)) { 
          stop("Reduction ",type,' not found')
        }
        x <- self$reductions[[type]]
      }
      if ((use.odgenes || !is.null(n.odgenes)) && is.null(odgenes)) {
        if (is.null(self$misc[['odgenes']] )) { stop("Please run adjustVariance() first")}
        odgenes <- self$misc[['odgenes']]
        if (!is.null(n.odgenes)) {
          if (n.odgenes>length(odgenes)) {
            #warning("number of specified odgenes is higher than the number of the statistically significant sites, will take top ",n.odgenes,' sites')
            odgenes <- rownames(self$misc[['varinfo']])[(order(self$misc[['varinfo']]$lp,decreasing=FALSE)[1:min(length(.pagoda2_axis_names(self, "gene")),n.odgenes)])]
          } else {
            odgenes <- odgenes[1:n.odgenes]
          }
        }
      }
      if (type == 'counts') {
        x <- self$getExpressionBlock(genes = odgenes)
      } else if (!is.null(odgenes)) {
        x <- x[,odgenes]
      }
      if (!is.null(odgenes)) {
        if (verbose) message('running PCA using ',length(odgenes),' OD genes .')
      } else { #all genes?
        if (verbose) message('running PCA all ',ncol(x),' genes .')
      }
      # apply scaling if using raw counts
      if (var.scale) {
        x <- .pagoda2_apply_variance_scaling(x, self$misc[['varinfo']])
      }
      if (verbose) message('.')
      
      if (!is.null(cells)) {
        # cell subset is just for PC determination
        nPcs <- min(min(length(cells),ncol(x))-1,nPcs)
        cm <- Matrix::colMeans(x[cells,])
        pcs <- irlba(x[cells,], nv=nPcs, nu=0, center=cm, right_only=FALSE,fastpath=fastpath,maxit=maxit,reorth=TRUE, ...)
        total.variance <- .pagoda2_matrix_sumsq(x[cells, , drop = FALSE]) - length(cells) * sum(cm^2)
      } else {
        nPcs <- min(min(nrow(x),ncol(x))-1,nPcs)
        if (center) {
          cm <- Matrix::colMeans(x)
          pcs <- irlba(x, nv=nPcs, nu=0, center=cm, right_only=FALSE,fastpath=fastpath,maxit=maxit,reorth=TRUE, ...)
          total.variance <- .pagoda2_matrix_sumsq(x) - nrow(x) * sum(cm^2)
        } else {
          pcs <- irlba(x, nv=nPcs, nu=0, right_only=FALSE,fastpath=fastpath,maxit=maxit,reorth=TRUE, ...)
          total.variance <- .pagoda2_matrix_sumsq(x)
        }
      }
      rownames(pcs$v) <- colnames(x)

      if (verbose) message('.')

      # adjust for centering!
      if (center) {
        pcs$center <- cm
        pcas <- as.matrix(t(as(t(x %*% pcs$v), "dgeMatrix") - t(cm %*% pcs$v)))
      } else {
        pcas <- as.matrix(x %*% pcs$v)
      }
      self$misc$PCA <- pcs
      if (verbose) message('.')
      #pcas <- scde::winsorize.matrix(pcas,0.05)
      # # control for sequencing depth
      # if(is.null(batch)) {
      #   mx <- model.matrix(x ~ d,data=data.frame(x=1,d=depth))
      # } else {
      #   mx <- model.matrix(x ~ d*b,data=data.frame(x=1,d=depth,b=batch))
      # }
      # # TODO: how to get rid of residual depth effects in the PCA-based clustering?
      # #pcas <- t(t(colLm(pcas,mx,returnResid=TRUE))+Matrix::colMeans(pcas))
      # pcas <- colLm(pcas,mx,returnResid=TRUE)
      rownames(pcas) <- rownames(x)
      colnames(pcas) <- paste0('PC', seq(ncol(pcas)))
      #pcas <- pcas[,-1]
      #pcas <- scde::winsorize.matrix(pcas,0.1)
      if (verbose) message(' done\n')
      self$reductions[[name]] <- pcas
      percent.variance <- if (is.finite(total.variance) && total.variance > 0) {
        100 * pcs$d^2 / total.variance
      } else {
        rep(NA_real_, length(pcs$d))
      }
      pca.variance <- data.frame(
        component = seq_along(percent.variance),
        percent_variance = percent.variance,
        cumulative_percent_variance = cumsum(percent.variance),
        stringsAsFactors = FALSE
      )
      if (is.null(self$history$pca)) {
        self$history$pca <- list()
      }
      self$history$pca[[name]] <- list(
        reduction = name,
        total_variance = total.variance,
        n.cells = if (is.null(cells)) nrow(x) else length(cells),
        n.genes = ncol(x),
        genes = colnames(x),
        variance = pca.variance,
        params = list(nPcs = nPcs, type = type, use.odgenes = use.odgenes, center = center)
      )
      ## nIcs <- nPcs;
      ## a <- ica.R.def(t(pcas),nIcs,tol=1e-3,fun='logcosh',maxit=200,verbose=T,alpha=1,w.init=matrix(rnorm(nIcs*nPcs),nIcs,nPcs))
      ## reductions[['ICA']] <- as.matrix( x %*% pcs$v %*% a);
      ## colnames(reductions[['ICA']]) <- paste('IC',seq(ncol(reductions[['ICA']])),sep='');

	      invisible(pcas)
	    },

	    #' @description Calculate PCA using the pagoda2.1 API name.
	    #'
	    #' @param ... Arguments passed to calculatePcaReduction().
	    #' @return Invisible PCA result.
	    runPCA=function(...) {
	      self$calculatePcaReduction(..., .legacy.warn = FALSE)
	    },

	    #' @description Plot PCA variance explained.
	    #'
	    #' @param reduction Reduction name. NULL uses the default reduction.
	    #' @param max.components Optional maximum number of components to show.
	    #' @param plot.theme ggplot theme.
	    #' @return ggplot object.
	    plotPCAElbow=function(reduction=NULL, max.components=NULL, plot.theme=ggplot2::theme_bw()) {
	      if (!requireNamespace("ggplot2", quietly = TRUE)) {
	        stop("Package `ggplot2` is required for plotPCAElbow()")
	      }
	      if (is.null(reduction)) {
	        reduction <- self$defaults$reduction
	      }
	      info <- if (!is.null(self$history$pca)) self$history$pca[[reduction]] else NULL
	      if (is.null(info) || is.null(info$variance)) {
	        stop("PCA variance information is not available for reduction `", reduction, "`. Re-run p2$runPCA(name = \"", reduction, "\").")
	      }
	      df <- info$variance
	      if (!is.null(max.components)) {
	        df <- df[df$component <= max.components, , drop = FALSE]
	      }
	      plot.df <- rbind(
	        data.frame(component = df$component, percent = df$percent_variance, curve = "Per component"),
	        data.frame(component = df$component, percent = df$cumulative_percent_variance, curve = "Cumulative")
	      )
	      plot.df$curve <- factor(plot.df$curve, levels = c("Per component", "Cumulative"))
	      ggplot2::ggplot(plot.df, ggplot2::aes(x = component, y = percent, color = curve)) +
	        ggplot2::geom_line(linewidth = 0.7) +
	        ggplot2::geom_point(size = 1.8) +
	        ggplot2::scale_color_manual(values = c("Per component" = "grey15", "Cumulative" = "#2c7fb8"), name = NULL) +
	        ggplot2::theme_bw() +
	        plot.theme +
	        ggplot2::labs(
	          x = "Principal component",
	          y = "% total variance explained",
	          title = paste0(reduction, " variance explained")
	        )
	    },

	    #' @description Reset overdispersed genes 'odgenes' to be a superset of the standard odgene selection (guided by n.odgenes or alpha), 
    #'     and a set of recursively determined odgenes based on a given group (or a cluster info)
    #' 
    #' @param min.group.size integer Number of minimum cells for filtering out group size (default=30)
    #' @param od.alpha numeric The Type I error probability or the significance level for calculating overdispersed genes (default=1e-1). This is the criterion used to measure statistical significance, i.e. if the p-value < alpha, then it is statistically significant.
    #' @param use.odgenes boolean Whether pre-calculated set of overdispersed genes should be used (default=FALSE)
    #' @param odgenes Explicitly specify a set of overdispersed genes to use for the reduction (default=NULL) #' @param odgenes (default=NULL)
    #' @param n.odgene.multiplier numeric (default=1)
    #' @param gam.k integer The k used for the generalized additive model 'v ~ s(m, k =gam.k)' (default=10). If gam.k<2, linear regression is used 'lm(v ~ m)'.
    #' @param min.odgenes integer Minimum number of overdispersed genes to use (default=10)
    #' @param max.odgenes integer Maximum number of overdispersed genes to use (default=Inf)
    #' @param recursive boolean Whether to determine groups for which variance normalization will be rerun (default=TRUE)
    #' 
    #' @return List of overdispersed genes
    expandOdGenes=function(type='counts', clusterType=NULL, groups=NULL , min.group.size=30, od.alpha=1e-1, 
      use.odgenes=FALSE, n.odgenes=NULL, odgenes=NULL, n.odgene.multiplier=1, gam.k=10,verbose=FALSE,n.cores=self$n.cores,
      min.odgenes=10,max.odgenes=Inf,recursive=TRUE) {
      # determine groups
      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        groups <- as.factor(groups[names(groups) %in% .pagoda2_axis_names(self, "cell")])
        groups <- droplevels(groups)
      }

      # determine initial set of odgenes
      if ((use.odgenes || !is.null(n.odgenes)) && is.null(odgenes)) {
        if (is.null(self$misc[['varinfo']] )) { stop("Please run adjustVariance() first")}
        df <- self$misc$varinfo
        odgenes <- rownames(df)[!is.na(df$lpa) & df$lpa<log(od.alpha)]
        #odgenes <- misc[['odgenes']];
        if (!is.null(n.odgenes)) {
          if (n.odgenes>length(odgenes)) {
            #warning("number of specified odgenes is higher than the number of the statistically significant sites, will take top ",n.odgenes,' sites')
            odgenes <- rownames(self$misc[['varinfo']])[(order(self$misc[['varinfo']]$lp,decreasing=FALSE)[1:min(length(.pagoda2_axis_names(self, "gene")),n.odgenes)])]
          } else {
            odgenes <- odgenes[1:n.odgenes]
          }
        }
      }

      # filter out small groups
      if (min.group.size>1) { 
        groups[groups %in% levels(groups)[unlist(tapply(groups,groups,length))<min.group.size]] <- NA
        groups <- droplevels(groups) 
      }
      if (sum(!is.na(groups))<min.group.size) {
        warning("clustering specifies fewer cells than min.group.size")
        return(odgenes)
      }


      if (length(levels(groups))<2) {
        warning("cannot expand od genes based on a single group")
        return(odgenes)
      }

      # determine groups for which variance normalization will be rerun
      if (recursive) {
        if (verbose) message("recursive group enumeration ...")
        # derive cluster hierarchy

        # use raw counts to derive clustering
        z <- self$misc$rawCounts
        rowFac <- rep(-1,nrow(z))
        names(rowFac) <- rownames(z)
        rowFac[match(names(groups),rownames(z))] <- as.integer(groups)
        tc <- colSumByFac(z,as.integer(rowFac))[-1,,drop=FALSE]
        rownames(tc) <- levels(groups)
        d <- 1-cor(t(log10(tc/pmax(1,Matrix::rowSums(tc))*1e3+1)))
        hc <- stats::hclust(as.dist(d),method='average',members=unlist(tapply(groups,groups,length)))

        dlab <- function(l) {
          if (is.leaf(l)) {
            return(list(labels(l)))
          } else {
            return(c(list(labels(l)),dlab(l[[1]]),dlab(l[[2]])))
          }
        }

        # for each level in the cluster hierarchy, except for the top
        rgroups <- dlab(as.dendrogram(hc))[-1]
        rgroups <- c(list(levels(groups)),rgroups)
        if (verbose) message("done.\n")
      } else {
        rgroups <- lapply(levels(groups),I)
      }
      names(rgroups) <- unlist(lapply(rgroups,paste,collapse="+"))

      # run local variance normalization
      if (verbose) message("running local variance normalization ")
      # run variance normalization, determine PCs
      gpcs <- papply(rgroups,function(group) {
        cells <- names(groups)[groups %in% group]

        # variance normalization
        df <- self$adjustVariance(persist=FALSE, gam.k=gam.k, verbose=FALSE, cells=cells, n.cores=1)
        #if(!is.null(n.odgenes)) {
        #  odgenes <- rownames(df)[order(df$lp,decreasing=F)[1:n.odgenes]]
        #} else {
        df <- df[!is.na(df$lp),,drop=FALSE]
        df <- df[order(df$lp,decreasing=FALSE),,drop=FALSE]
        n.od <- min(max(sum(df$lpa<log(od.alpha)),min.odgenes),max.odgenes)
        if (n.od>0) {
          odgenes <- rownames(df)[1:min(n.od*n.odgene.multiplier,nrow(df))]
        } else {
          return(NULL)
        }
        sf <- df$gsf[match(odgenes,rownames(df))]
        return(list(sf=sf,cells=cells,odgenes=odgenes))
      },n.cores=n.cores, mc.preschedule=TRUE)
      if (verbose) message(" done\n")
      odg <- unique(unlist(lapply(gpcs,function(z) z$odgenes)))
      # TODO: consider gsf?
      odgenes <- unique(c(odgenes,odg))
      self$misc[['odgenes']] <- odgenes
      invisible(odgenes)
    },


    #' @description local PCA implementation
    #' 
    #' @param nPcs integer Number of principal components (default=5)
    #' @param k integer Number of components for kNN graph (default=30)
    #' @param b numeric Constant within exp(-b*(ncid/cldsd)^2), used for calculating cell relevance per cluster (default=1)
    #' @param a numeric Constant within "(1-exp(-a*(dsq)/(p$pcs$trsd^2)))*(pk /outerproduct pk)" (default=1)
    #' @param min.group.size integer Number of minimum cells for filtering out group size (default=30)
    #' @param name string Title (default='localPCA')
    #' @param od.alpha numeric Significance level for calculating overdispersed genes (default=1e-1). P-values will be filtered by <log(od.alpha).
    #' @param gam.k integer The k used for the generalized additive model 'v ~ s(m, k =gam.k)' (default=10). If gam.k<2, linear regression is used 'lm(v ~ m)'.
    #' @param min.odgenes integer Minimum number of overdispersed genes to use (default=5)
    #' @param take.top.odgenes boolean Take top overdispersed genes in decreasing order (default=FALSE)
    #' @param recursive boolean Whether to recursively determine groups for which variance normalization will be rerun (default=FALSE)
    #' @param euclidean boolean Whether to applied euclidean-based distance similarity during variance normalization (default=FALSE)
    #' @param perplexity integer Perplexity parameter within Rtsne::Rtsne() (default=k). Please see Rtsne for more details.
    #' @param return.pca boolean Whether to return the PCs (default=FALSE)
    #' @param skip.pca boolean If TRUE and return.pca=TRUE, will return a list of scale factors, cells, and overdispersed genes, i.e. list(sf=sf, cells=cells, odgenes=odgenes) (default=FALSE). Otherwise, ignored.
    #'
    #' @return localPcaKnn return here
    localPcaKnn=function(nPcs=5, type='counts', clusterType=NULL, groups=NULL,
      k=30, b=1, a=1, min.group.size=30, name='localPCA', od.alpha=1e-1, 
      n.odgenes=NULL, gam.k=10, verbose=FALSE, n.cores=self$n.cores, min.odgenes=5,
      take.top.odgenes=FALSE, recursive=TRUE, euclidean=FALSE, perplexity=k,
      return.pca=FALSE, skip.pca=FALSE) {

      if (type=='counts') {
        x <- self$getExpressionBlock()
      } else {
        if (!type %in% names(self$reductions)) { 
          stop("Reduction ",type,' not found')
        }
        x <- self$reductions[[type]]
      }

      if (is.null(groups)){
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        groups <- as.factor(groups[names(groups) %in% rownames(x)])
        groups <- droplevels(groups)
      }

      if (min.group.size>1) { 
        groups[groups %in% levels(groups)[unlist(tapply(groups,groups,length))<min.group.size]] <- NA
        groups <- droplevels(groups) 
      }
      if (sum(!is.na(groups))<min.group.size) { 
        stop("clustering specifies fewer cells than min.group.size") 
      }


      if (recursive) {
        if (verbose) message("recursive group enumeration ...")
        ## # derive cluster hierarchy
        ## rowFac <- rep(-1,nrow(x)); names(rowFac) <- rownames(x);
        ## rowFac[names(groups)] <- as.integer(groups);
        ## tc <- colSumByFac(x,as.integer(rowFac))[-1,]
        ## rownames(tc) <- levels(groups)
        ## #tc <- rbind("total"=Matrix::colSums(tc),tc)
        ## #d <- jsDist(t(((tc/pmax(1,Matrix::rowSums(tc)))))); rownames(d) <- colnames(d) <- rownames(tc)
        ## d <- 1-cor(t(tc))
        ## hc <- stats::hclust(as.dist(d),method='ward.D')

        # use raw counts to derive clustering
        z <- self$misc$rawCounts
        rowFac <- rep(-1,nrow(z))
        names(rowFac) <- rownames(z)
        rowFac[match(names(groups),rownames(z))] <- as.integer(groups)
        tc <- colSumByFac(z,as.integer(rowFac))[-1,,drop=FALSE]
        rownames(tc) <- levels(groups)
        d <- 1-cor(t(log10(tc/pmax(1,Matrix::rowSums(tc))*1e3+1)))
        hc <- stats::hclust(as.dist(d),method='average',members=unlist(tapply(groups,groups,length)))


        dlab <- function(l) {
          if (is.leaf(l)) {
            return(list(labels(l)))
          } else {
            return(c(list(labels(l)),dlab(l[[1]]),dlab(l[[2]])))
          }
        }

        # for each level in the cluster hierarchy, except for the top
        rgroups <- dlab(as.dendrogram(hc))[-1]
        rgroups <- c(list(levels(groups)),rgroups)
        if (verbose) message("done.\n")
      } else {
        rgroups <- lapply(levels(groups),I)
      }
      names(rgroups) <- unlist(lapply(rgroups, paste, collapse="+"))


      if (verbose) message("determining local PCs ")
      # run variance normalization, determine PCs
      gpcs <- papply(rgroups,function(group) {
        cells <- names(groups)[groups %in% group]

        # variance normalization
        df <- self$adjustVariance(persist=FALSE, gam.k=gam.k, verbose=FALSE, cells=cells, n.cores=1)
        if (!is.null(n.odgenes)) {
          odgenes <- rownames(df)[order(df$lp, decreasing=FALSE)[1:n.odgenes]]
        } else {
          odgenes <- rownames(df)[!is.na(df$lpa) & df$lpa<log(od.alpha)]
        }
        if (length(odgenes)<min.odgenes) {
          if (take.top.odgenes) {
            odgenes <- rownames(df)[order(df$lp, decreasing=FALSE)[1:min.odgenes]]
          } else {
            return(NULL)
          }
        }
        sf <- df$gsf[match(odgenes,rownames(df))]

        if (return.pca && skip.pca) {
          return(list(sf=sf, cells=cells, odgenes=odgenes))
        }


        y <- t(t(x[cells,odgenes])*sf)
        cm <- Matrix::colMeans(y)
        # PCA
        pcs <- irlba(y, nv=nPcs, nu=0, center=cm, right_only=FALSE,fastpath=TRUE,reorth=TRUE)
        rownames(pcs$v) <- colnames(y)
        pcs$center <- cm
        # row-randomize x to get a sense for the pcs
        m1 <- y
        if (euclidean) {
          #for (i in 1:nrow(m1)) m1[i,] <- m1[i,order(runif(length(m1[i,])))]
          m1@i <- sample(m1@i)
          rpcas <- t(t(m1 %*% pcs$v) - t(pcs$center %*% pcs$v))
          pcs$rsd <- apply(rpcas,2,sd)
          pcs$trsd <- sd(dist(rpcas))
        }

        # sample within-cluster distances (based on main PCA)

        pcas <- as.matrix(t(t(t(t(x[,odgenes])*sf) %*% pcs$v) - t(pcs$center %*% pcs$v)))
        if (verbose) message(".")
        return(list(pcs=pcs, sf=sf, df=df, cells=cells, pcas=pcas, odgenes=odgenes))
      },n.cores=n.cores)
      if (verbose) message(" done\n")
      if (return.pca){
        return(gpcs)
      }

      ivi <- unlist(lapply(gpcs,is.null))
      if (any(ivi)) {
        gpcs <- gpcs[!ivi]
        rgroups <- rgroups[!ivi]
      }


      # calculate cell relevance to each cluster (p_k,i matrix)
      # use global PCA distances
      if (verbose) message("calculating global distances ...")
      gcdist <- as.matrix(dist(gpcs[[1]]$pcas))
      if (verbose) message(" done.\n")

      # for each PCA
        # for each cell, determine p_k_i
        # for each PC
          # determine cell projections
            # subset genes
            # subtract center
            # scale, multiply
          # for each pair, determine cell distances
          # use cell distances to complete weight matrix
          # add to the w*d^2 and w matrices
      # normalize by the sqrt(sum(w))


      if (euclidean) {
        if (verbose) message("calculating local Euclidean distances .")
        dcs <- papply(gpcs,function(p) {
          pk <- rep(1,nrow(p$pcas))
          names(pk) <- rownames(p$pcas)
          nci <- setdiff(rownames(gcdist),p$cells)
          if (length(nci)>0) {
            # determine within cluster sd
            scells <- sample(p$cells,min(1e3,length(p$cells)))
            cldsd <- sd(as.numeric(gcdist[scells,scells]))
            ncid <- rowMeans(gcdist[nci,scells])
            pk[nci] <- exp(-b*(ncid/cldsd)^2)
          }
          dsq <- as.matrix(dist(p$pcas)^2)
          w <- (1-exp(-a*(dsq)/(p$pcs$trsd^2))) * (pk %o% pk)
          if (verbose) message(".")
          list(dsq=dsq,w=w)
        },n.cores=n.cores)
        if (verbose) message(".")
        d <- Reduce('+',lapply(dcs,function(x) x$dsq*x$w))
        if (verbose) message(".")
        d <- sqrt(d/Reduce('+',lapply(dcs,function(x) x$w)))
        diag(d) <- 0
        if (verbose) message(" done.\n")
      } else {
        # weighted correlation
        if (verbose) message("calculating local correlation distances .")
        dcs <- papply(gpcs,function(p) {
          pk <- rep(1,nrow(p$pcas))
          names(pk) <- rownames(p$pcas)
          nci <- setdiff(rownames(gcdist),p$cells)
          if (length(nci)>0) {
            # determine within cluster sd
            scells <- sample(p$cells,min(1e3,length(p$cells)))
            cldsd <- sd(as.numeric(gcdist[scells,scells]))
            ncid <- rowMeans(gcdist[nci,scells])
            pk[nci] <- exp(-b*(ncid/cldsd)^2)
          }
          x <- cov(t(p$pcas))*(ncol(p$pcas)-1)
          xc <- x / sqrt(diag(x) %o% diag(x)) # correlation
          w <- (1-exp(-a*(1-xc))) * (pk %o% pk)

          if (verbose) message(".")
          list(x=x,w=w)
        },n.cores=n.cores)
        if (verbose) message(".")
        # calculate sum_{k}_{w_k*v} matrix
        wm <- Reduce('+',lapply(dcs,function(z) z$w*diag(z$x)))
        d <- Reduce('+',lapply(dcs,function(z) z$x*z$w))
        d <- 1-d/sqrt(wm*t(wm))
        diag(d) <- 0
        if (verbose) message(" done.\n")
      }

      ## d <- dcs[[1]]$dsq
      ## d <- as.matrix(dist(r$reductions$PCA))
      ## knn <- apply(d,2,function(x) order(x,decreasing=F)[1:(k+1)])
      ## cat(".")
      ## m <- sparseMatrix(i=as.numeric(knn),p=c(0,(1:ncol(knn))*nrow(knn)),dims=rep(ncol(knn),2),x=rep(1,nrow(knn)*ncol(knn)))
      ## m <- m+t(m); # symmetrize
      ## diag(m) <- 0;
      ## rownames(m) <- colnames(m) <- rownames(d)
      ## x <- list(m=m)
      ## i <- 5; cl <- rep(NA,nrow(x$m)); names(cl) <- rownames(x$m); cl[rownames(x$m)[i]] <- 1; cl[which(x$m[,i]>0)] <- 2; cl <- as.factor(cl);
      ## r$plotEmbedding(type='PCA',embeddingType='tSNE',groups=cl,alpha=0.2,min.group.size=00,mark.clusters = TRUE, mark.cluster.cex=0.8,unclassified.cell.color=adjustcolor(1,alpha=0.1))

      ## i <- 5; cl <- 1/(dcs[[1]]$dsq[,i]+1e-6); names(cl) <- rownames(x$m);
      ## i <- 5; cl <- 1/(d[,i]+1e-6); names(cl) <- rownames(x$m);
      ## r$plotEmbedding(type='PCA',embeddingType='tSNE',colors=cl,alpha=0.2,min.group.size=00,mark.clusters = TRUE, mark.cluster.cex=0.8,unclassified.cell.color=adjustcolor(1,alpha=0.1))

      # kNN
      if (verbose) message("creating kNN graph .")
      knn <- apply(d,2,function(x) order(x,decreasing=FALSE)[1:(k+1)])
      if (verbose) message(".")
      #m <- sparseMatrix(i=as.numeric(knn),p=c(0,(1:ncol(knn))*nrow(knn)),dims=rep(ncol(knn),2),x=rep(1,nrow(knn)*ncol(knn)))
      m <- sparseMatrix(i=as.numeric(knn),p=c(0,(1:ncol(knn))*nrow(knn)),dims=rep(ncol(knn),2),x=d[as.integer(t(t(knn)+((1:ncol(knn))-1)*nrow(d)))])
      m <- m+t(m) # symmetrize
      diag(m) <- 0
      rownames(m) <- colnames(m) <- rownames(d)
      if (verbose) message(".")
      g <- graph_from_adjacency_matrix(m,mode='undirected',weighted=TRUE)
      if (verbose) message(".")
      self$graphs[[name]] <- g
      if (verbose) message(" done.\n")

      emb <- Rtsne::Rtsne(d,is_distance=TRUE, perplexity=perplexity, num_threads=n.cores)$Y
      rownames(emb) <- colnames(d)
      self$embeddings[[type]][[name]] <- emb

      # calculate cell-cell distance, considering weighting

      # getting total cell-cell distance

      invisible(list(d=d,m=m,gpcs=gpcs))
    },

    ## env - pathway to gene environment

    #' @description Test pathway overdispersion
    #' Note: this is a compressed version of the PAGODA1 approach in SCDE <https://hms-dbmi.github.io/scde/>
    #' 
    #' @param setenv Specific environment for pathway analysis
    #' @param min.pathway.size integer Minimum number of observed genes that should be contained in a valid gene set (default=10)
    #' @param max.pathway.size integer Maximum number of observed genes in a valid gene set (default=1e3)
    #' @param n.randomizations numeric Number of random gene sets (of the same size) to be evaluated in parallel with each gene set (default=5). (This can be kept at 5 or 10, but should be increased to 50-100 if the significance of pathway overdispersion will be determined relative to random gene set models.)
    #' @param score.alpha numeric Significance level of the confidence interval for determining upper/lower bounds (default=0.05)
    #' @param cells character vector Specific cells to investigate (default=NULL)
    #' @param adjusted.pvalues boolean Whether to use adjusted p-values (default=TRUE)
    #' @param z.score numeric Z-score to be used as a cutoff for statistically significant patterns (default=qnorm(0.05/2, lower.tail = FALSE))
    #' @param use.oe.scale boolean Whether the variance of the returned aspect patterns should be normalized using observed/expected value instead of the default chi-squared derived variance corresponding to overdispersion Z-score (default=FALSE)
    #' @param return.table boolean Whether to return a text table with results (default=FALSE)
    #' @param name string Title (default='pathwayPCA')
    #' @param correlation.distance.threshold numeric Similarity threshold for grouping interdependent aspects in pagoda.reduce.redundancy() (default=0.2)
    #' @param loading.distance.threshold numeric Similarity threshold for grouping interdependent aspects in pagoda.reduce.loading.redundancy() (default=0.2)
    #' @param top.aspects Restrict output to the top N aspects of heterogeneity (default=Inf)
    #' @param recalculate.pca boolean Whether to recalculate PCA (default=FALSE)
    #' @param save.pca boolean Whether to save the PCA results (default=TRUE). If TRUE, caches them in self$misc[['pwpca']].
    #'
    #' @return pathway output
    testPathwayOverdispersion=function(setenv, type='counts', max.pathway.size=1e3, min.pathway.size=10, 
      n.randomizations=5, verbose=FALSE, n.cores=self$n.cores, score.alpha=0.05, plot=FALSE, cells=NULL, adjusted.pvalues=TRUE,
      z.score = qnorm(0.05/2, lower.tail = FALSE), use.oe.scale = FALSE, return.table=FALSE, name='pathwayPCA',
      correlation.distance.threshold=0.2, loading.distance.threshold=0.01, top.aspects=Inf, recalculate.pca=FALSE, save.pca=TRUE) {
  
      if (!requireNamespace("scde", quietly=TRUE)){
        stop("You need to install package 'scde' to be able to use testPathwayOverdispersion().")
      }

      nPcs <- 1
      if (type=='counts') {
        x <- self$getExpressionBlock(scale.variance = TRUE)
      } else {
        if (!type %in% names(self$reductions)) { stop("Reduction ",type,' not found')}
        x <- self$reductions[[type]]
      }
      if (!is.null(cells)) {
        x <- x[cells,]
      }

      proper.gene.names <- colnames(x)

      if (is.null(self$misc[['pwpca']]) || recalculate.pca) {
        if (verbose) {
          message("determining valid pathways")
        }

        # determine valid pathways
        gsl <- ls(envir = setenv)
        gsl.ng <- unlist(mclapply(sn(gsl), function(go) sum(unique(get(go, envir = setenv)) %in% proper.gene.names),mc.cores=n.cores,mc.preschedule=TRUE))
        gsl <- gsl[gsl.ng >= min.pathway.size & gsl.ng<= max.pathway.size]
        names(gsl) <- gsl

        if (verbose) {
          message("processing ", length(gsl), " valid pathways")
        }

        cm <- Matrix::colMeans(x)

        pwpca <- papply(gsl, function(sn) {
          lab <- proper.gene.names %in% get(sn, envir = setenv)
          if (sum(lab)<1) { 
            return(NULL)
          }
          pcs <- irlba(x[,lab], nv=nPcs, nu=0, center=cm[lab])
          pcs$d <- pcs$d/sqrt(nrow(x))
          pcs$rotation <- pcs$v
          pcs$v <- NULL

          # get standard deviations for the random samples
          ngenes <- sum(lab)
          z <- do.call(rbind,lapply(seq_len(n.randomizations), function(i) {
            si <- sample(ncol(x), ngenes)
            pcs <- irlba(x[,si], nv=nPcs, nu=0, center=cm[si])$d
          }))
          z <- z/sqrt(nrow(x))

          # local normalization of each component relative to sampled PC1 sd
          avar <- pmax(0, (pcs$d^2-mean(z[, 1]^2))/sd(z[, 1]^2))

          if (avar>0.5) {
            # flip orientations to roughly correspond with the means
            pcs$scores <- as.matrix(t(x[,lab] %*% pcs$rotation) - as.numeric((cm[lab] %*% pcs$rotation)))
            cs <- unlist(lapply(seq_len(nrow(pcs$scores)), function(i) sign(cor(pcs$scores[i,], colMeans(t(x[, lab, drop = FALSE])*abs(pcs$rotation[, i]))))))
            pcs$scores <- pcs$scores*cs
            pcs$rotation <- pcs$rotation*cs
            rownames(pcs$rotation) <- colnames(x)[lab]
          } # don't bother otherwise - it's not significant
          return(list(xp=pcs,z=z,n=ngenes))
        }, n.cores = n.cores,mc.preschedule=TRUE)
        if (save.pca) {
          self$misc[['pwpca']] <- pwpca
        }
      } else {
        if (verbose) {
          message("reusing previous overdispersion calculations")
          pwpca <- self$misc[['pwpca']]
        }
      }

      if (verbose) {
        message("scoring pathway od signifcance")
      }

      # score overdispersion
      true.n.cells <- nrow(x)

      pagoda.effective.cells <- function(pwpca, start = NULL) {
        n.genes <- unlist(lapply(pwpca, function(x) rep(x$n, nrow(x$z))))
        var <- unlist(lapply(pwpca, function(x) x$z[, 1]))
        if (is.null(start)) { start <- true.n.cells*2 } # start with a high value
        of <- function(p, v, sp) {
          sn <- p[1]
          vfit <- (sn+sp)^2/(sn*sn+1/2) -1.2065335745820*(sn+sp)*((1/sn + 1/sp)^(1/3))/(sn*sn+1/2)
          residuals <- (v-vfit)^2
          return(sum(residuals))
        }
        x <- nlminb(objective = of, start = c(start), v = var, sp = sqrt(n.genes-1/2), lower = c(1), upper = c(true.n.cells))
        return((x$par)^2+1/2)
      }
      n.cells <- pagoda.effective.cells(pwpca)

      vdf <- data.frame(do.call(rbind, lapply(seq_along(pwpca), function(i) {
        vars <- as.numeric((pwpca[[i]]$xp$d))
        cbind(i = i, var = vars, n = pwpca[[i]]$n, npc = seq(1:ncol(pwpca[[i]]$xp$rotation)))
      })))

      # fix p-to-q mistake in qWishartSpike
      qWishartSpikeFixed <- function (q, spike, ndf = NA, pdim = NA, var = 1, beta = 1, lower.tail = TRUE, log.p = FALSE)  {
        params <- RMTstat::WishartSpikePar(spike, ndf, pdim, var, beta)
        qnorm(q, mean = params$centering, sd = params$scaling, lower.tail, log.p)
      }

      # add right tail approximation to ptw, which gives up quite early
      pWishartMaxFixed <- function (q, ndf, pdim, var = 1, beta = 1, lower.tail = TRUE) {
        params <- RMTstat::WishartMaxPar(ndf, pdim, var, beta)
        q.tw <- (q - params$centering)/(params$scaling)
        p <- RMTstat::ptw(q.tw, beta, lower.tail, log.p = TRUE)
        p[p == -Inf] <- pgamma((2/3)*q.tw[p == -Inf]^(3/2), 2/3, lower.tail = FALSE, log.p = TRUE) + lgamma(2/3) + log((2/3)^(1/3))
        p
      }

      vshift <- 0
      ev <- 0

      vdf$var <- vdf$var-(vshift-ev)*vdf$n
      basevar <- 1
      vdf$exp <- RMTstat::qWishartMax(0.5, n.cells, vdf$n, var = basevar, lower.tail = FALSE)
      #vdf$z <- qnorm(pWishartMax(vdf$var, n.cells, vdf$n, log.p = TRUE, lower.tail = FALSE, var = basevar), lower.tail = FALSE, log.p = TRUE)
      vdf$z <- qnorm(pWishartMaxFixed(vdf$var, n.cells, vdf$n, lower.tail = FALSE, var = basevar), lower.tail = FALSE, log.p = TRUE)
      vdf$cz <- qnorm(bh.adjust(pnorm(as.numeric(vdf$z), lower.tail = FALSE, log.p = TRUE), log = TRUE), lower.tail = FALSE, log.p = TRUE)
      vdf$ub <- RMTstat::qWishartMax(score.alpha/2, n.cells, vdf$n, var = basevar, lower.tail = FALSE)
      vdf$ub.stringent <- RMTstat::qWishartMax(score.alpha/nrow(vdf)/2, n.cells, vdf$n, var = basevar, lower.tail = FALSE)

      if (plot) {
        test_pathway_par <- par(mfrow = c(1, 1), mar = c(3.5, 3.5, 1.0, 1.0), mgp = c(2, 0.65, 0))
        on.exit(par(test_pathway_par))
        un <- sort(unique(vdf$n))
        on <- order(vdf$n, decreasing = FALSE)
        pccol <- colorRampPalette(c("black", "grey70"), space = "Lab")(max(vdf$npc))
        plot(vdf$n, vdf$var/vdf$n, xlab = "gene set size", ylab = "PC1 var/n", ylim = c(0, max(vdf$var/vdf$n)), col = adjustcolor(pccol[vdf$npc],alpha=0.1),pch=19)
        lines(vdf$n[on], (vdf$exp/vdf$n)[on], col = 2, lty = 1)
        lines(vdf$n[on], (vdf$ub.stringent/vdf$n)[on], col = 2, lty = 2)
      }

      rs <- (vshift-ev)*vdf$n
      vdf$oe <- (vdf$var+rs)/(vdf$exp+rs)
      vdf$oec <- (vdf$var+rs)/(vdf$ub+rs)

      df <- data.frame(name = names(pwpca)[vdf$i], npc = vdf$npc, n = vdf$n, score = vdf$oe, z = vdf$z, adj.z = vdf$cz, stringsAsFactors = FALSE)
      if (adjusted.pvalues) {
        vdf$valid <- vdf$cz  >=  z.score
      } else {
        vdf$valid <- vdf$z  >=  z.score
      }

      if (!any(vdf$valid)) { 
        stop("No significantly overdispersed pathways found at z.score threshold of ",z.score) 
      }

      # apply additional filtering based on >0.5 sd above the local random estimate
      vdf$valid <- vdf$valid & unlist(lapply(pwpca,function(x) !is.null(x$xp$scores)))
      vdf$name <- names(pwpca)[vdf$i]

      if (return.table) {
        df <- df[vdf$valid, ]
        df <- df[order(df$score, decreasing = TRUE), ]
        return(df)
      }
      if (verbose) {
        message("compiling pathway reduction")
      }
      # calculate pathway reduction matrix

      # return scaled patterns
      xmv <- do.call(rbind, lapply(pwpca[vdf$valid], function(x) {
        xm <- x$xp$scores
      }))

      if (use.oe.scale) {
        xmv <- (xmv -rowMeans(xmv))* (as.numeric(vdf$oe[vdf$valid])/sqrt(apply(xmv, 1, var)))
        vdf$sd <- as.numeric(vdf$oe)
      } else {
        # chi-squared
        xmv <- (xmv-rowMeans(xmv)) * sqrt((qchisq(pnorm(vdf$z[vdf$valid], lower.tail = FALSE, log.p = TRUE), n.cells, lower.tail = FALSE, log.p = TRUE)/n.cells)/apply(xmv, 1, var))
        vdf$sd <- sqrt((qchisq(pnorm(vdf$z, lower.tail = FALSE, log.p = TRUE), n.cells, lower.tail = FALSE, log.p = TRUE)/n.cells))

      }
      rownames(xmv) <- paste("#PC", vdf$npc[vdf$valid], "# ", names(pwpca)[vdf$i[vdf$valid]], sep = "")
      rownames(vdf) <- paste("#PC", vdf$npc, "# ", vdf$name, sep = "")
      self$misc[['pathwayODInfo']] <- vdf

      # collapse gene loading
      if (verbose) {
        message("clustering aspects based on gene loading ... ",appendLF=FALSE)
      }
      tam2 <- pagoda.reduce.loading.redundancy(list(xv=xmv,xvw=matrix(1,ncol=ncol(xmv),nrow=nrow(xmv))),pwpca,NULL,plot=FALSE,distance.threshold=loading.distance.threshold,n.cores=n.cores)
      if (verbose) {
        message(nrow(tam2$xv)," aspects remaining")
      }
      if (verbose) {
        message("clustering aspects based on pattern similarity ... ",appendLF=FALSE)
      }
      tam3 <- pagoda.reduce.redundancy(tam2, distance.threshold=correlation.distance.threshold,top=top.aspects)
      if (verbose) {
        message(nrow(tam3$xv)," aspects remaining\n")
      }
      tam2$xvw <- tam3$xvw <- NULL # to save space
      tam3$env <- setenv

      # clean up aspect names, as GO ids are meaningless
      names(tam3$cnam) <- rownames(tam3$xv) <- paste0('aspect',1:nrow(tam3$xv))

      self$misc[['pathwayOD']] <- tam3
      self$reductions[[name]] <- tam3$xv
      invisible(tam3)
    },


    #' @description Return embedding
    #' 
    #' @param embeddingType string Type of embedding to construct (default='largeVis'). Possible values are: 'largeVis', 'tSNE', 'FR' (Fruchterman–Reingold), 'UMAP', 'UMAP_graph' 
    #' @param name string Name of the embedding (default=NULL). If NULL, the name = embeddingType.
    #' @param dims integer Parameter 'dims' Matrix::sparseMatrix(); a non-negative, integer, dimensions vector of length 2 (default=2). See Matrix package documentation for more details.
    #' @param M numeric (largeVis) The number of negative edges to sample for each positive edge (default=5). Parameter only used if embeddingType is 'largeVis'.
    #' @param gamma numeric (largeVis) The strength of the force pushing non-neighbor nodes apart (default=7). Parameter only used if embeddingType is 'largeVis'.
    #' @param perplexity numeric Parameter 'perplexity' within largeVis::buildWijMatrix() (default=50). Please see the largeVis documentation for more details.
    #' @param verbose boolean Whether to give verbose output (default=TRUE)
    #' @param sgd_batches numeric The number of edges to process during SGD (default=NULL). Passed to projectKNNs(). Defaults to a value set based on the size of the dataset. If the parameter given is
    #'     between \code{0} and \code{1}, the default value will be multiplied by the parameter.
    #' @param diffusion.steps integer Iteration steps to use. If 0, no steps are run. (default=0)
    #' @param diffusion.power numeric Factor to be used when calculating diffusion, (default=0.5)
    #' @param distance string 'pearson', 'spearman', 'euclidean', 'L2', 'JS' (default='pearson')
    #' @param n.sgd.cores numeric Number of cores to use (default=n.cores)
    #' @param ...  Additional parameters passed to embedding functions, Rtsne::Rtsne() if 'L2', uwot::umap() if 'UMAP', embedKnnGraphUmap() if 'UMAP_graph'
    #'
	    #' @return embedding stored in self$embedding
	    getEmbedding=function(type='counts', embeddingType='largeVis', name=NULL, dims=2, M=1, gamma=1/M, perplexity=50, verbose=TRUE,
	      sgd_batches=NULL, diffusion.steps=0, diffusion.power=0.5, distance='pearson', n.cores = self$n.cores, n.sgd.cores=n.cores,
	      .legacy.warn=TRUE, ... ) {
	      if (.legacy.warn) {
	        .pagoda2_deprecated_call("getEmbedding()", "p2$runEmbedding(...) or p2$runUMAP(...)")
	      }
	      
	      if (dims<1) {
        stop("Dimensions parameter 'dims' must be >=1")
      }
      if (type=='counts') {
        x <- self$getExpressionBlock()
      } else {
        if (!type %in% names(self$reductions)) { 
          stop("Reduction ",type,' not found')
        }
        x <- self$reductions[[type]]
      }
      if (is.null(name)) { 
        name <- embeddingType 
      }

      if (embeddingType=='largeVis') {
        edgeMat <- self$misc[['edgeMat']][[type]]
        if (is.null(edgeMat)){ 
          stop(paste0('KNN graph for type ',type,' not found. Please run makeKnnGraph with type=',type)) 
        }
        if (is.null(sgd_batches)){ 
          sgd_batches <- nrow(edgeMat)*1e3 
        }
        #edgeMat <- sparseMatrix(i=xn$s+1,j=xn$e+1,x=xn$rd,dims=c(nrow(x),nrow(x)))
        edgeMat <- (edgeMat + t(edgeMat))/2 # symmetrize
        #edgeMat <- sparseMatrix(i=c(xn$s,xn$e)+1,j=c(xn$e,xn$s)+1,x=c(xn$rd,xn$rd),dims=c(nrow(x),nrow(x)))
        # if(diffusion.steps>0) {
        #   Dinv <- Diagonal(nrow(edgeMat),1/colSums(edgeMat))
        #   Im <- Diagonal(nrow(edgeMat))
        #   W <- (Diagonal(nrow(edgeMat)) + edgeMat %*% Dinv)/2
        #   for(i in 1:diffusion.steps) {
        #     edgeMat <- edgeMat %*% W
        #   }
        # }
        #require(largeVis)
        #if(!is.null(seed)) { set.seed(seed) }
        if (!is.na(perplexity)) {
          wij <- buildWijMatrix(edgeMat, perplexity=perplexity, threads=n.cores)
        } else {
          wij <- edgeMat
        }

        if (diffusion.steps>0) {
          Dinv <- Diagonal(nrow(wij),1/colSums(wij))
          W <- Dinv %*% wij 
          W <- 
          #W <- (Diagonal(nrow(wij)) + W)/2
          #W <- (Diagonal(nrow(wij)) + sign(W)*(abs(W)^(diffusion.power)))/2

          #W <- sign(W)*(abs(W)^diffusion.power)
          #W <- (Diagonal(nrow(wij)) + W)/2
          for(i in 1:diffusion.steps) {
            wij <- wij %*% W
          }
          if (!is.na(perplexity)) {
            wij <- buildWijMatrix(wij, perplexity=perplexity, threads=n.cores)
          }
          
        }
        coords <- projectKNNs(wij = wij, M = M, dim=dims, verbose = verbose, sgd_batches = sgd_batches, gamma=gamma, seed=1, threads=n.cores, ...)
        colnames(coords) <- rownames(x)
        emb <- t(coords)
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType=='tSNE') {
        if (nrow(x)>4e4) {
          warning('Too many cells to pre-calculate correlation distances, switching to L2. Please consider using UMAP.')
          distance <- 'L2'
        }
        
        dup.ids <- which(duplicated(x))
        if (length(dup.ids) > 0) {
          max.vals <- abs(x[dup.ids,] * 0.01)
          x[dup.ids,] <- runif(length(x[dup.ids,]), -max.vals, max.vals)
        }
        
        if (distance=='L2') {
          if (verbose) message("running tSNE using ",n.cores," cores:\n")
          emb <- Rtsne::Rtsne(x, perplexity=perplexity, dims=dims, num_threads=n.cores, ... )$Y
        } else {
          if (verbose) message('calculating distance ... ')
          if (verbose) message('pearson ...')
          d <- 1-cor(t(x))
          if (verbose) message("running tSNE using ",n.cores," cores:\n")
          emb <- Rtsne::Rtsne(d, is_distance=TRUE, perplexity=perplexity, dims=dims, num_threads=n.cores, ... )$Y
        }
        rownames(emb) <- rownames(x)
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType=='FR') {
        g <- self$graphs[[type]]
        if (is.null(g)){ 
          stop(paste0("Generate kNN graph first (type=",type,")"))
        }
        emb <- layout.fruchterman.reingold(g, weights=E(g)$weight)
        rownames(emb) <- rownames(x)
        colnames(emb) <- c("D1","D2")
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType == "UMAP") {
        if (!requireNamespace("uwot", quietly=TRUE)){
          stop("You need to install package 'uwot' to be able to use UMAP embedding.")
        }
        
        distance <- switch(distance, pearson = "cosine", L2 = "euclidean", distance)
        
        emb <- uwot::umap(as.matrix(x), metric=distance, verbose=verbose, n_threads=n.cores, n_sgd_threads=n.sgd.cores, n_components=dims, ...)
        rownames(emb) <- rownames(x)
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType == "UMAP_graph") {
        g <- self$graphs[[type]]
        if (is.null(g)){ 
          stop(paste0("generate kNN graph first (type=",type,")"))
        }
        emb <- embedKnnGraphUmap(g, verbose=verbose, n_threads=n.cores, n_sgd_threads=n.sgd.cores, n_components=dims, ...)
        self$embeddings[[type]][[name]] <- emb
      } else {
        stop('Unknown embeddingType ',embeddingType,' specified')
      }

	      invisible(emb)
	     },

	    #' @description Run an embedding using the pagoda2.1 API name.
	    #'
	    #' @param reduction Reduction namespace.
	    #' @param embedding Embedding method/name.
	    #' @param name Stored embedding name.
	    #' @param ... Arguments passed to getEmbedding().
	    #' @return Invisibly returns embedding matrix.
	    runEmbedding=function(reduction=NULL, embedding=NULL, name=NULL, ...) {
	      if (is.null(reduction)) {
	        reduction <- self$defaults$reduction
	      }
	      if (is.null(embedding)) {
	        embedding <- self$defaults$embedding
	      }
	      self$getEmbedding(type = reduction, embeddingType = embedding, name = name, ..., .legacy.warn = FALSE)
	    },

	    #' @description Run UMAP using the pagoda2.1 API name.
	    #'
	    #' @param reduction Reduction namespace.
	    #' @param name Stored embedding name.
	    #' @param ... Arguments passed to getEmbedding().
	    #' @return Invisibly returns UMAP matrix.
	    runUMAP=function(reduction=NULL, name='UMAP', ...) {
	      self$runEmbedding(reduction = reduction, embedding = "UMAP", name = name, ...)
	    }
	  ),

  active = list(
    #' @field counts Removed legacy normalized matrix slot.
    counts = function(value) {
      if (missing(value)) {
        stop(.pagoda2_counts_removed_message("access"), call. = FALSE)
      }
      stop(.pagoda2_counts_removed_message("assign"), call. = FALSE)
    }
  )

)
