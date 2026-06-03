## Variance-model diagnostic plots for Pagoda2

.pagoda2_r6_plot_variance_qc <- function(p2, run.variance = FALSE, plot.theme = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for plotVarianceQC()")
  }
  if (is.null(p2$misc[["varinfo"]])) {
    if (!isTRUE(run.variance)) {
      stop("Variance information is not available; call p2$runVariance() first or set run.variance = TRUE")
    }
    p2$runVariance(plot = FALSE, ...)
  }
  df <- p2$misc[["varinfo"]]
  if (is.null(df) || nrow(df) == 0L) {
    stop("Variance information is empty")
  }
  if (!all(c("m", "v", "qv") %in% colnames(df))) {
    stop("Variance information is missing required columns; re-run p2$runVariance()")
  }

  history <- p2$history$variance
  value.scale <- if (!is.null(history$value.scale)) history$value.scale else "log"
  if (identical(value.scale, "raw")) {
    log10.magnitude <- log10(df$m)
    log10.variance <- log10(df$v)
  } else {
    log10.magnitude <- log10(exp(1)) * df$m
    log10.variance <- log10(exp(1)) * df$v
  }
  log10.adjusted <- log10(df$qv)
  odgenes <- intersect(rownames(df), p2$misc[["odgenes"]])
  status <- ifelse(rownames(df) %in% odgenes, "overdispersed", "background")

  plot.df <- rbind(
    data.frame(
      gene = rownames(df),
      panel = "Mean-variance fit",
      log10_magnitude = log10.magnitude,
      value = log10.variance,
      status = status,
      stringsAsFactors = FALSE
    ),
    data.frame(
      gene = rownames(df),
      panel = "Adjusted variance",
      log10_magnitude = log10.magnitude,
      value = log10.adjusted,
      status = status,
      stringsAsFactors = FALSE
    )
  )
  plot.df <- plot.df[is.finite(plot.df$log10_magnitude) & is.finite(plot.df$value), , drop = FALSE]
  if (nrow(plot.df) == 0L) {
    stop("Variance information contains no finite values to plot")
  }
  plot.df$panel <- factor(plot.df$panel, levels = c("Mean-variance fit", "Adjusted variance"))
  plot.df$status <- factor(plot.df$status, levels = c("background", "overdispersed"))

  background <- plot.df[plot.df$status == "background", , drop = FALSE]
  od <- plot.df[plot.df$status == "overdispersed", , drop = FALSE]
  thresholds <- data.frame(
    panel = factor("Adjusted variance", levels = levels(plot.df$panel)),
    yintercept = 0,
    threshold = "expected",
    stringsAsFactors = FALSE
  )
  if (!is.null(history$max.adjusted.variance) && is.finite(history$max.adjusted.variance) &&
    history$max.adjusted.variance > 0) {
    thresholds <- rbind(
      thresholds,
      data.frame(
        panel = factor("Adjusted variance", levels = levels(plot.df$panel)),
        yintercept = log10(history$max.adjusted.variance),
        threshold = "cap",
        stringsAsFactors = FALSE
      )
    )
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = background,
      ggplot2::aes(x = log10_magnitude, y = value),
      color = "grey35",
      size = 0.35,
      alpha = 0.22
    )
  if (nrow(od) > 0L) {
    p <- p + ggplot2::geom_point(
      data = od,
      ggplot2::aes(x = log10_magnitude, y = value),
      color = "firebrick3",
      size = 0.45,
      alpha = 0.55
    )
  }
  if (!is.null(history$fit_curve) && nrow(history$fit_curve) > 0L) {
    fit.curve <- history$fit_curve
    fit.curve$panel <- factor("Mean-variance fit", levels = levels(plot.df$panel))
    p <- p + ggplot2::geom_line(
      data = fit.curve,
      ggplot2::aes(x = log10_magnitude, y = log10_variance),
      inherit.aes = FALSE,
      color = "#2c7fb8",
      linewidth = 0.7
    )
  }
  p +
    ggplot2::geom_hline(
      data = thresholds,
      ggplot2::aes(yintercept = yintercept, linetype = threshold),
      color = "grey30",
      linewidth = 0.4
    ) +
    ggplot2::scale_linetype_manual(values = c(expected = "dashed", cap = "dotted"), name = NULL) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") +
    .pagoda2_plot_theme(p2, plot.theme = plot.theme, local.theme = ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey92", color = "grey55"),
      legend.key.size = grid::unit(4.5, "mm")
    )) +
    ggplot2::labs(
      x = "log10 magnitude",
      y = "log10 value",
      title = "Variance model QC",
      subtitle = paste0(length(odgenes), " overdispersed genes")
    )
}
