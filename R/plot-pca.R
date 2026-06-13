## PCA plotting implementation for Pagoda2

.pagoda2_r6_plot_pca_elbow <- function(p2, reduction = NULL, max.components = NULL, plot.theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for plotPCAElbow()")
  }
  if (is.null(reduction)) {
    reduction <- p2$defaults$reduction
  }
  info <- if (!is.null(p2$history$pca)) p2$history$pca[[reduction]] else NULL
  if (is.null(info) || is.null(info$variance)) {
    stop("PCA variance information is not available for reduction `", reduction, "`. Re-run p2$runReduction(name = \"", reduction, "\").")
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
    .pagoda2_plot_theme(p2, plot.theme = plot.theme) +
    ggplot2::labs(
      x = "Principal component",
      y = "% total variance explained",
      title = paste0(reduction, " variance explained")
    )
}
