## General heatmap plotting wrappers for Pagoda2

.pagoda2_r6_plot_heatmap <- function(p2, genes, grouping = NULL, groups = NULL, type = "counts", ...) {
  resolved.groups <- NULL
  if (!is.null(grouping) || !is.null(groups) || !is.null(p2$defaultGrouping)) {
    resolved.groups <- p2$resolveGrouping(grouping = grouping, groups = groups, allow.missing = TRUE)
  }
  p2$plotGeneHeatmap(genes = genes, type = type, groups = resolved.groups, ..., .legacy.warn = FALSE)
}
