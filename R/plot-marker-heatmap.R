## Marker heatmap implementation for Pagoda2


.pagoda2_prepare_marker_heatmap <- function(p2, markers = NULL, type = "counts", facet = NULL,
                                            genes = NULL, grouping = NULL, groups = NULL,
                                            n.genes.per.group = 5, additional.genes = NULL,
                                            exclude.genes = NULL, z.threshold = 2,
                                            highest.only = TRUE,
                                            ordering = NULL, selection = "balanced",
                                            min.expression.fraction = NULL,
                                            min.precision = NULL,
                                            min.specificity = NULL,
                                            min.auc = NULL,
                                            min.m = NULL,
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
                                            annotation.grobs = NULL, legend.max.levels = Inf,
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

  key <- if (!is.null(facet)) facet else .pagoda2_markers_lookup_key(p2, type)
  resolved <- p2$resolveMarkers(markers = markers, type = key)
  selected <- .pagoda2_select_marker_genes(
    resolved$tables,
    n.genes.per.group = n.genes.per.group,
    genes = genes,
    z.threshold = z.threshold,
    highest.only = highest.only,
    ordering = ordering,
    selection = selection,
    min.expression.fraction = min.expression.fraction,
    min.precision = min.precision,
    min.specificity = min.specificity,
    min.auc = min.auc,
    min.m = min.m,
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
  available.genes <- .pagoda2_axis_names(p2, "gene", facet = key)
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

  x <- as.matrix(t(p2$getExpressionBlock(cells = cells, genes = selected.genes, facet = key)))
  dimnames(x) <- list(selected.genes, cells)
  if (isTRUE(order.groups) && length(levels(resolved.groups)) > 1L) {
    xc <- do.call(cbind, tapply(seq_len(ncol(x)), resolved.groups[colnames(x)], function(ii) {
      rowMeans(x[, ii, drop = FALSE])
    }))
    group.order <- tryCatch(
      {
        hc <- stats::hclust(stats::as.dist(2 - stats::cor(xc)), method = "ward.D2")
        hc$labels[hc$order]
      },
      error = function(e) NULL
    )
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
  annotation.colors <- sccore::heatmapAnnotationColors(
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

  sccore::heatmapSpec(
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
  HeatmapAnnotation <- getExportedValue("ComplexHeatmap", "HeatmapAnnotation")
  Heatmap <- getExportedValue("ComplexHeatmap", "Heatmap")
  rowAnnotation <- getExportedValue("ComplexHeatmap", "rowAnnotation")
  anno_mark <- getExportedValue("ComplexHeatmap", "anno_mark")
  if (sum(lengths(spec$annotation.grobs)) > 0L) {
    warning("`annotation.grobs` are currently rendered only by `engine = \"native\"`.", call. = FALSE)
  }
  x <- spec$matrix
  top.annotation <- HeatmapAnnotation(
    df = spec$column.annotation,
    col = spec$annotation.colors$palettes,
    border = spec$border,
    show_legend = spec$show.group.legend
  )
  row.annotation <- NULL
  if (isTRUE(spec$show.gene.groups) && !is.null(spec$gene.groups)) {
    row.annotation <- HeatmapAnnotation(
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
  ht <- do.call(Heatmap, heatmap.args)
  if (!is.null(spec$label.indices) && length(spec$label.indices) > 0L) {
    ht <- ht + rowAnnotation(
      link = anno_mark(
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
#' @param min.cells.per.gene integer Legacy deferred default for the minimum detected cells required by filterData() analysis gene masking (default=0)
#' @param min.transcripts.per.cell integer Legacy deferred default for the minimum molecule count used by runQC()/filterData() (default=10)
#' @param keep.genes list of genes to keep in the filterData() analysis gene mask regardless of coverage (default=NULL)
#' @param trim numeric Parameter used for winsorizing count data (default=round(min.cells.per.gene/2)). If value>0, will winsorize counts in normalized space in the hopes of getting a more stable depth estimates. If value<=0, ignored.
#' @param clusterType Optional cluster type to use as a group-defining factor (default=NULL)
#' @param groups factor named with cell names specifying the clusters of cells to be compared (one against all) (default=NULL). To compare two cell clusters against each other, simply pass a factor containing only two levels.
#' @param plot boolean Whether to output the plot (default=FALSE)
#'
#' @export Pagoda2


.pagoda2_r6_plot_marker_heatmap <- function(p2, markers = NULL, type = "counts", facet = NULL, engine = c("native", "complex", "legacy"),
                                            genes = NULL, grouping = NULL, groups = NULL, n.genes.per.group = 5,
                                            additional.genes = NULL, exclude.genes = NULL,
                                            z.threshold = 2, highest.only = TRUE,
                                            ordering = NULL, selection = "balanced",
                                            min.expression.fraction = NULL,
                                            min.precision = NULL, min.specificity = NULL,
                                            min.auc = NULL, min.m = NULL,
                                            remove.duplicates = TRUE, expression.quantile = 0.99,
                                            pal = colorRampPalette(c("grey95", "firebrick3"), space = "Lab")(1024),
                                            column.metadata = NULL, column.metadata.colors = NULL,
                                            show.gene.groups = TRUE, show.group.legend = TRUE,
                                            show_heatmap_legend = FALSE, border = TRUE,
                                            row.label.font.size = 10, labeled.gene.subset = NULL,
                                            group.colors = NULL, gene.group.colors = NULL,
                                            order.groups = FALSE, cluster.rows = FALSE, cluster.columns = FALSE,
                                            cluster.max.items = 2000, cluster.method = "complete",
                                            split = FALSE, split.gap = 0,
                                            cell.order = NULL, averaging.window = 0,
                                            annotation.grobs = NULL, legend.max.levels = Inf,
                                            legend.columns = NULL, native.newpage = TRUE,
                                            v = 1, s = 1,
                                            max.cells = Inf,
                                            max.dense.entries = 5e7,
                                            use.raster = TRUE, raster.by.magick = FALSE,
                                            return.details = FALSE, ...) {
  engine <- match.arg(engine)
  key <- if (!is.null(facet)) facet else .pagoda2_markers_lookup_key(p2, type)
  resolved <- p2$resolveMarkers(markers = markers, type = key)
  if (engine == "legacy") {
    legacy.groups <- groups
    if (is.null(legacy.groups) && !is.null(grouping)) {
      legacy.groups <- p2$resolveGrouping(grouping = grouping, allow.missing = TRUE)
    } else if (is.null(legacy.groups) && !is.null(resolved$result$grouping)) {
      legacy.groups <- p2$resolveGrouping(grouping = resolved$result$grouping, allow.missing = TRUE)
    }
    return(p2$plotDiffGeneHeatmap(type = type, clusterType = resolved$name, groups = legacy.groups, ..., .legacy.warn = FALSE))
  }
  spec <- .pagoda2_prepare_marker_heatmap(
    p2,
    markers = markers,
    type = type,
    facet = key,
    genes = genes,
    grouping = grouping,
    groups = groups,
    n.genes.per.group = n.genes.per.group,
    additional.genes = additional.genes,
    exclude.genes = exclude.genes,
    z.threshold = z.threshold,
    highest.only = highest.only,
    ordering = ordering,
    selection = selection,
    min.expression.fraction = min.expression.fraction,
    min.precision = min.precision,
    min.specificity = min.specificity,
    min.auc = min.auc,
    min.m = min.m,
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
    sccore::drawHeatmap(spec, newpage = native.newpage)
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
}
