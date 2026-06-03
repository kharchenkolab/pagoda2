## Plot theme helpers for pagoda2 ggplot-based figures.

#' Pagoda2 ggplot theme
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return ggplot2 theme object.
#' @export
themePagoda2 <- function(base_size = 11, base_family = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for themePagoda2()")
  }
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        fill = ggplot2::alpha("white", 0.75),
        color = NA
      ),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
    )
}

.pagoda2_theme_value <- function(value, name = "plot.theme") {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.function(value)) {
    value <- value()
  }
  if (!inherits(value, "theme")) {
    stop("`", name, "` must be a ggplot2 theme object or a function returning one")
  }
  value
}

.pagoda2_plot_theme <- function(p2 = NULL, plot.theme = NULL, local.theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for pagoda2 plots")
  }
  base <- NULL
  if (!is.null(p2) && !is.null(p2$defaults$plot.theme)) {
    base <- p2$defaults$plot.theme
  }
  if (is.null(base)) {
    base <- getOption("pagoda2.plot.theme")
  }
  if (is.null(base)) {
    base <- themePagoda2()
  } else {
    base <- .pagoda2_theme_value(base, "p2$defaults$plot.theme")
  }
  local.theme <- .pagoda2_theme_value(local.theme, "local.theme")
  plot.theme <- .pagoda2_theme_value(plot.theme, "plot.theme")
  out <- base
  if (!is.null(local.theme)) {
    out <- out + local.theme
  }
  if (!is.null(plot.theme)) {
    out <- out + plot.theme
  }
  out
}

.pagoda2_r6_set_plot_theme <- function(p2, plot.theme = NULL) {
  if (is.null(plot.theme)) {
    p2$defaults$plot.theme <- NULL
  } else {
    p2$defaults$plot.theme <- .pagoda2_theme_value(plot.theme, "plot.theme")
  }
  invisible(p2)
}
