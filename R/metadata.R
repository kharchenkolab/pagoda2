## Metadata, grouping, and palette implementation for Pagoda2


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

.pagoda2_axis_names <- function(p2, axis = c("cell", "gene"), facet = NULL) {
  axis <- match.arg(axis)
  f <- p2$resolveFacet(facet)
  matrix <- f$rawCounts
  if (is.null(matrix) && isTRUE(f$primary)) {
    matrix <- p2$misc[["rawCounts"]]
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
    s = s, v = v, return.details = TRUE
  )$palette
  if (!is.null(existing) && length(existing) > 0L) {
    generated[names(existing)] <- existing[names(existing)]
  }
  generated[levels]
}


.pagoda2_r6_set_cell_meta <- function(p2, metadata, value = NULL, overwrite = TRUE) {
  cells <- .pagoda2_axis_names(p2, "cell")
  p2$syncMetadata()
  if (is.character(metadata) && length(metadata) == 1 && !is.null(value)) {
    metadata <- .pagoda2_vector_metadata(metadata, value, target = cells, axis = "cell")
  } else {
    if (!is.null(value)) {
      stop("`value` can only be supplied when `metadata` is a single column name")
    }
  }
  p2$cellMeta <- .pagoda2_store_metadata(p2$cellMeta, metadata, target = cells, axis = "cell", overwrite = overwrite)
  invisible(p2)
}

.pagoda2_r6_get_cell_meta <- function(p2, columns = NULL, resolved = FALSE, cells = NULL, allow.missing = TRUE) {
  p2$syncMetadata()
  if (isTRUE(resolved)) {
    return(p2$resolveCellMeta(columns = columns, cells = cells, allow.missing = allow.missing))
  }
  if (!is.null(cells)) {
    stop("`cells` can only be supplied when `resolved = TRUE`")
  }
  if (is.null(columns)) {
    return(p2$cellMeta)
  }
  missing <- setdiff(columns, colnames(p2$cellMeta))
  if (length(missing) > 0) {
    stop("Unknown cell metadata column(s): ", paste(missing, collapse = ", "))
  }
  p2$cellMeta[, columns, drop = FALSE]
}

.pagoda2_r6_set_gene_meta <- function(p2, metadata, value = NULL, overwrite = TRUE) {
  genes <- .pagoda2_axis_names(p2, "gene")
  p2$syncMetadata()
  if (is.character(metadata) && length(metadata) == 1 && !is.null(value)) {
    metadata <- .pagoda2_vector_metadata(metadata, value, target = genes, axis = "gene")
  } else {
    if (!is.null(value)) {
      stop("`value` can only be supplied when `metadata` is a single column name")
    }
  }
  p2$geneMeta <- .pagoda2_store_metadata(p2$geneMeta, metadata, target = genes, axis = "gene", overwrite = overwrite)
  invisible(p2)
}

.pagoda2_r6_get_gene_meta <- function(p2, columns = NULL, resolved = FALSE, genes = NULL, allow.missing = TRUE) {
  p2$syncMetadata()
  if (isTRUE(resolved)) {
    return(p2$resolveGeneMeta(columns = columns, genes = genes, allow.missing = allow.missing))
  }
  if (!is.null(genes)) {
    stop("`genes` can only be supplied when `resolved = TRUE`")
  }
  if (is.null(columns)) {
    return(p2$geneMeta)
  }
  missing <- setdiff(columns, colnames(p2$geneMeta))
  if (length(missing) > 0) {
    stop("Unknown gene metadata column(s): ", paste(missing, collapse = ", "))
  }
  p2$geneMeta[, columns, drop = FALSE]
}

.pagoda2_r6_set_palette <- function(p2, name, colors, axis = c("cell", "gene", "cellMeta", "geneMeta"), palette.id = name) {
  axis <- .pagoda2_palette_axis_key(axis)
  if (!is.character(name) || length(name) != 1L || is.na(name) || name == "") {
    stop("`name` must be a single non-empty metadata column name")
  }
  entry <- .pagoda2_normalize_palette_entry(colors, name = name, axis = axis, palette.id = palette.id)
  if (is.null(p2$palettes) || !is.list(p2$palettes)) {
    p2$palettes <- list(cellMeta = list(), geneMeta = list())
  }
  if (is.null(p2$palettes[[axis]])) {
    p2$palettes[[axis]] <- list()
  }
  p2$palettes[[axis]][[name]] <- entry
  invisible(p2)
}

.pagoda2_r6_get_palette <- function(p2, name, axis = c("cell", "gene", "cellMeta", "geneMeta"), colors.only = FALSE) {
  axis <- .pagoda2_palette_axis_key(axis)
  entry <- NULL
  if (!is.null(p2$palettes) && !is.null(p2$palettes[[axis]])) {
    entry <- p2$palettes[[axis]][[name]]
  }
  if (isTRUE(colors.only)) {
    return(if (is.null(entry)) NULL else entry$colors)
  }
  entry
}

.pagoda2_r6_resolve_factor_colors <- function(p2, axis = c("cell", "gene", "cellMeta", "geneMeta"),
                                              name = NULL, values = NULL, colors = NULL,
                                              store = FALSE, s = 1, v = 1) {
  axis <- .pagoda2_palette_axis_key(axis)
  p2$syncMetadata()
  if (is.null(p2$palettes) || !is.list(p2$palettes)) {
    p2$palettes <- list(cellMeta = list(), geneMeta = list())
  }
  if (is.null(p2$palettes[[axis]])) {
    p2$palettes[[axis]] <- list()
  }
  metadata.values <- NULL
  if (!is.null(name)) {
    metadata <- if (axis == "cellMeta") p2$cellMeta else p2$geneMeta
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
    entry <- .pagoda2_normalize_palette_entry(colors,
      name = if (is.null(name)) "groups" else name,
      axis = axis, values = values, generated.by = "user"
    )
    resolved <- entry$colors[levels(values)]
    if (anyNA(resolved)) {
      missing <- levels(values)[is.na(resolved)]
      stop("Palette is missing color(s) for level(s): ", paste(missing, collapse = ", "))
    }
    if (isTRUE(store) && !is.null(name)) {
      p2$setPalette(name, entry$colors, axis = axis, palette.id = entry$palette.id)
    }
    return(resolved)
  }

  existing <- if (!is.null(name)) p2$getPalette(name, axis = axis, colors.only = TRUE) else NULL
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
    p2$palettes[[axis]][[name]] <- entry
  }
  resolved
}

.pagoda2_r6_resolve_cell_meta <- function(p2, columns = NULL, cells = NULL, allow.missing = TRUE) {
  p2$syncMetadata()
  if (is.null(cells)) {
    cells <- .pagoda2_axis_names(p2, "cell")
  }
  metadata <- p2$cellMeta
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
}

.pagoda2_r6_resolve_gene_meta <- function(p2, columns = NULL, genes = NULL, allow.missing = TRUE) {
  p2$syncMetadata()
  if (is.null(genes)) {
    genes <- .pagoda2_axis_names(p2, "gene")
  }
  metadata <- p2$geneMeta
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
}

.pagoda2_r6_set_grouping <- function(p2, name, groups, source = NULL, setDefault = FALSE, overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || name == "") {
    stop("`name` must be a single non-empty string")
  }
  cells <- .pagoda2_axis_names(p2, "cell")
  groups <- .pagoda2_align_vector(groups, cells, what = paste0("grouping `", name, "`"))
  groups <- .pagoda2_as_grouping(groups, name = name)
  p2$setCellMeta(name, groups, overwrite = overwrite)
  if (is.null(p2$history$groupings)) {
    p2$history$groupings <- list()
  }
  p2$history$groupings[[name]] <- list(
    source = source,
    created = Sys.time(),
    n.groups = length(levels(groups))
  )
  if (setDefault) {
    p2$setDefaultGrouping(name)
  }
  invisible(p2)
}

.pagoda2_r6_set_default_grouping <- function(p2, grouping) {
  p2$syncMetadata()
  if (!is.character(grouping) || length(grouping) != 1 || is.na(grouping) || grouping == "") {
    stop("`grouping` must be a single non-empty string")
  }
  if (!grouping %in% colnames(p2$cellMeta)) {
    stop("Unknown cell metadata column `", grouping, "`")
  }
  p2$resolveGrouping(grouping = grouping, allow.missing = TRUE)
  p2$defaultGrouping <- grouping
  invisible(p2)
}

.pagoda2_r6_get_default_grouping <- function(p2) {
  p2$defaultGrouping
}

.pagoda2_r6_resolve_grouping <- function(p2, grouping = NULL, groups = NULL, cells = NULL, allow.missing = TRUE) {
  p2$syncMetadata()
  if (!is.null(grouping) && !is.null(groups)) {
    stop("Specify only one of `grouping` or `groups`")
  }
  if (is.null(cells)) {
    cells <- .pagoda2_axis_names(p2, "cell")
  }
  if (is.null(groups)) {
    if (is.null(grouping)) {
      grouping <- p2$defaultGrouping
    }
    if (is.null(grouping)) {
      stop("No defaultGrouping is set and no grouping was supplied")
    }
    if (!is.character(grouping) || length(grouping) != 1) {
      stop("`grouping` must be a single cell metadata column name")
    }
    if (!grouping %in% colnames(p2$cellMeta)) {
      stop("Unknown grouping `", grouping, "`. Available groupings: ", paste(p2$listGroupings()$name, collapse = ", "))
    }
    values <- p2$cellMeta[[grouping]]
    names(values) <- rownames(p2$cellMeta)
  } else if (is.character(groups) && length(groups) == 1 && is.null(names(groups)) && groups %in% colnames(p2$cellMeta)) {
    values <- p2$cellMeta[[groups]]
    names(values) <- rownames(p2$cellMeta)
  } else {
    values <- groups
  }
  values <- .pagoda2_align_vector(values, cells, what = "groups")
  if (!allow.missing && any(is.na(values))) {
    stop("Grouping is missing values for ", sum(is.na(values)), " cell(s)")
  }
  grouping.name <- if (is.null(grouping)) "groups" else grouping
  .pagoda2_as_grouping(values, name = grouping.name)
}

.pagoda2_r6_get_grouping <- function(p2, grouping = NULL, groups = NULL, cells = NULL, allow.missing = TRUE) {
  p2$resolveGrouping(grouping = grouping, groups = groups, cells = cells, allow.missing = allow.missing)
}

.pagoda2_r6_list_groupings <- function(p2) {
  p2$syncMetadata()
  cols <- colnames(p2$cellMeta)
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
  resolved <- p2$resolveCellMeta(columns = cols)
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
    is.default = cols == p2$defaultGrouping,
    stringsAsFactors = FALSE
  )
}

.pagoda2_r6_annotate_clusters <- function(p2, from, to, map, unmapped = NA, setDefault = TRUE, overwrite = FALSE) {
  source.groups <- p2$getGrouping(grouping = from)
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
  p2$setGrouping(to, target.values, source = list(method = "annotateClusters", from = from), setDefault = setDefault, overwrite = overwrite)
  invisible(p2)
}
