## Marker dot plot implementation for Pagoda2

.pagoda2_r6_plot_marker_dot_plot <- function(p2, markers = NULL, type = "counts", genes = NULL, grouping = NULL, groups = NULL,
                                             n.genes.per.group = 5, z.threshold = 3, highest.only = TRUE,
                                             ordering = NULL, selection = "balanced",
                                             min.expression.fraction = NULL,
                                             min.precision = NULL, min.specificity = NULL,
                                             min.auc = NULL, min.m = NULL,
                                             remove.duplicates = TRUE, count.matrix = NULL, n.cores = p2$n.cores,
                                             cols = c("grey88", "firebrick3"), dot.scale = 7,
                                             scale.by = "size", text.angle = 45, order.groups = TRUE,
                                             group.order = NULL, plot.theme = NULL, ...) {
  order_dotplot_groups <- function(cell.groups, marker.groups, order.groups = TRUE,
                                   group.order = NULL) {
    cell.groups <- droplevels(cell.groups)
    current.levels <- levels(cell.groups)
    if (!is.null(group.order)) {
      group.order <- unique(as.character(group.order))
      missing.levels <- setdiff(group.order, current.levels)
      if (length(missing.levels) > 0L) {
        warning(
          "Ignoring group.order level(s) absent from plotted groups: ",
          paste(missing.levels, collapse = ", "),
          call. = FALSE
        )
      }
      group.order <- group.order[group.order %in% current.levels]
      if (length(group.order) == 0L) {
        return(cell.groups)
      }
      ordered.levels <- c(group.order, setdiff(current.levels, group.order))
      return(stats::setNames(factor(as.character(cell.groups), levels = ordered.levels), names(cell.groups)))
    }
    if (!isTRUE(order.groups) || is.null(marker.groups) || length(marker.groups) == 0L) {
      return(cell.groups)
    }
    marker.order <- unique(as.character(marker.groups[!is.na(marker.groups)]))
    marker.order <- marker.order[marker.order %in% current.levels]
    if (length(marker.order) == length(current.levels) && setequal(marker.order, current.levels)) {
      return(stats::setNames(factor(as.character(cell.groups), levels = marker.order), names(cell.groups)))
    }
    cell.groups
  }

  resolved <- p2$resolveMarkers(markers = markers, type = type)
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
  available.genes <- if (is.null(count.matrix)) .pagoda2_axis_names(p2, "gene") else colnames(count.matrix)
  missing.genes <- setdiff(selected$genes, available.genes)
  if (length(missing.genes) > 0) {
    warning("Omitting marker genes absent from count matrix: ", paste(missing.genes, collapse = ", "))
  }
  selected.genes <- selected$genes[selected$genes %in% available.genes]
  if (length(selected.genes) == 0) {
    stop("No selected marker genes are present in count matrix")
  }
  selected.groups <- selected$groups[selected.genes]
  if (is.null(count.matrix)) {
    count.matrix <- p2$getExpressionBlock(genes = selected.genes)
  }
  if (is.null(rownames(count.matrix)) || is.null(colnames(count.matrix))) {
    stop("`count.matrix` must have cell row names and gene column names")
  }
  if (is.null(grouping) && is.null(groups) && !is.null(resolved$result$grouping)) {
    grouping <- resolved$result$grouping
  }
  resolved.groups <- p2$resolveGrouping(
    grouping = grouping,
    groups = groups,
    cells = rownames(count.matrix),
    allow.missing = TRUE
  )
  resolved.groups <- order_dotplot_groups(
    resolved.groups,
    marker.groups = selected.groups,
    order.groups = order.groups,
    group.order = group.order
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
    .pagoda2_plot_theme(p2, plot.theme = plot.theme, local.theme = ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10.5),
      axis.title = ggplot2::element_text(size = 11.5),
      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10.5),
      plot.title = ggplot2::element_text(size = 13)
    ))
}
