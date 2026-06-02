.pagoda2_heatmap_annotation_colors <- function(annotation, color.list = list(), s = 1, v = 1) {
  continuous_palette <- function(colors = NULL, n = 256) {
    if (is.null(colors)) {
      colors <- c("grey95", "steelblue4")
    }
    grDevices::colorRampPalette(colors, space = "Lab")(n)
  }

  map_continuous_colors <- function(x, palette, na.color = "grey85") {
    out <- rep(na.color, length(x))
    finite <- is.finite(x)
    if (!any(finite)) {
      return(out)
    }
    rng <- range(x[finite])
    if (diff(rng) == 0) {
      out[finite] <- palette[length(palette)]
      return(out)
    }
    idx <- floor((x[finite] - rng[1]) / diff(rng) * (length(palette) - 1L)) + 1L
    idx <- pmax(1L, pmin(length(palette), idx))
    out[finite] <- palette[idx]
    out
  }

  track_color_info <- function(values, name, colors = NULL, s = 1, v = 1,
                               na.color = "grey85") {
    if (is.numeric(values) && !is.factor(values)) {
      palette <- continuous_palette(colors)
      color.values <- map_continuous_colors(as.numeric(values), palette, na.color = na.color)
      names(color.values) <- names(values)
      finite <- is.finite(values)
      value.range <- if (any(finite)) range(values[finite]) else c(NA_real_, NA_real_)
      return(list(
        colors = color.values,
        palette = palette,
        legend = list(type = "continuous", name = name, palette = palette, range = value.range, na.color = na.color)
      ))
    }

    if (is.logical(values)) {
      values <- factor(values, levels = c(FALSE, TRUE), exclude = NULL)
    } else {
      values <- as.factor(values)
    }
    levels <- levels(droplevels(values))
    if (is.null(colors)) {
      colors <- .pagoda2_discrete_palette(levels, s = s, v = v)
    } else if (is.null(names(colors))) {
      if (length(colors) < length(levels)) {
        stop("Color vector for annotation `", name, "` has fewer colors than levels")
      }
      colors <- stats::setNames(colors[seq_along(levels)], levels)
    } else {
      missing.colors <- setdiff(levels, names(colors))
      if (length(missing.colors) > 0) {
        stop("Color vector for annotation `", name, "` is missing color(s) for: ", paste(missing.colors, collapse = ", "))
      }
      colors <- colors[levels]
    }
    color.values <- colors[as.character(values)]
    color.values[is.na(color.values)] <- na.color
    names(color.values) <- names(values)
    list(
      colors = color.values,
      palette = colors,
      legend = list(type = "discrete", name = name, colors = colors, na.color = na.color)
    )
  }

  if (is.null(color.list)) {
    color.list <- list()
  }
  if (!is.list(color.list)) {
    stop("Annotation colors must be supplied as a list")
  }
  tracks <- list()
  legends <- list()
  palettes <- list()
  for (nm in colnames(annotation)) {
    values <- annotation[[nm]]
    names(values) <- rownames(annotation)
    info <- track_color_info(values, nm, colors = color.list[[nm]], s = s, v = v)
    tracks[[nm]] <- info$colors
    legends[[nm]] <- info$legend
    if (!is.null(info$palette) && !identical(info$legend$type, "continuous")) {
      palettes[[nm]] <- info$palette
    } else if (!is.null(color.list[[nm]])) {
      palettes[[nm]] <- color.list[[nm]]
    }
  }
  list(tracks = tracks, legends = legends, palettes = palettes)
}

.pagoda2_native_heatmap_spec <- function(x, column.groups = NULL, row.groups = NULL,
                                         column.annotation = NULL,
                                         annotation.colors = NULL,
                                         column.annotation.colors = list(),
                                         row.group.colors = NULL,
                                         expression.palette = grDevices::colorRampPalette(c("grey95", "firebrick3"), space = "Lab")(1024),
                                         labeled.row.subset = NULL,
                                         label.indices = NULL,
                                         show.row.groups = TRUE,
                                         show.group.legend = TRUE,
                                         show_heatmap_legend = FALSE,
                                         border = TRUE,
                                         row.label.font.size = 10,
                                         split = FALSE,
                                         split.gap = 0,
                                         annotation.grobs = NULL,
                                         legend.max.levels = 18,
                                         legend.columns = NULL,
                                         s = 1,
                                         v = 1,
                                         extra = list(),
                                         class = character()) {
  normalize_annotation_grobs <- function(annotation.grobs = NULL) {
    empty <- list(top = list(), right = list(), bottom = list(), left = list())
    if (is.null(annotation.grobs)) {
      return(empty)
    }
    is.grob <- function(x) inherits(x, "grob") || inherits(x, "gList")
    if (is.grob(annotation.grobs)) {
      empty$top <- list(annotation.grobs)
      return(empty)
    }
    if (!is.list(annotation.grobs)) {
      stop("`annotation.grobs` must be a grid grob or a list of grobs")
    }
    known <- intersect(names(annotation.grobs), names(empty))
    if (length(known) == 0L && all(vapply(annotation.grobs, is.grob, logical(1)))) {
      empty$top <- annotation.grobs
      return(empty)
    }
    for (nm in known) {
      value <- annotation.grobs[[nm]]
      if (is.null(value)) {
        next
      }
      if (is.grob(value)) {
        value <- list(value)
      }
      if (!is.list(value) || !all(vapply(value, is.grob, logical(1)))) {
        stop("`annotation.grobs$", nm, "` must be a grob or a list of grobs")
      }
      empty[[nm]] <- value
    }
    empty
  }

  x <- as.matrix(x)
  if (is.null(rownames(x))) {
    rownames(x) <- paste0("row", seq_len(nrow(x)))
  }
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("column", seq_len(ncol(x)))
  }

  if (is.null(column.groups)) {
    column.groups <- stats::setNames(factor(rep("all", ncol(x)), levels = "all"), colnames(x))
  } else {
    if (is.null(names(column.groups))) {
      if (length(column.groups) != ncol(x)) {
        stop("Unnamed `column.groups` must have one value per heatmap column")
      }
      names(column.groups) <- colnames(x)
    }
    missing.columns <- setdiff(colnames(x), names(column.groups))
    if (length(missing.columns) > 0L) {
      stop("`column.groups` is missing heatmap column(s): ", paste(missing.columns, collapse = ", "))
    }
    column.groups <- droplevels(as.factor(column.groups[colnames(x)]))
  }

  if (is.null(row.groups)) {
    row.groups <- stats::setNames(factor(rep("rows", nrow(x)), levels = "rows"), rownames(x))
  } else {
    if (is.null(names(row.groups))) {
      if (length(row.groups) != nrow(x)) {
        stop("Unnamed `row.groups` must have one value per heatmap row")
      }
      names(row.groups) <- rownames(x)
    }
    missing.rows <- setdiff(rownames(x), names(row.groups))
    if (length(missing.rows) > 0L) {
      stop("`row.groups` is missing heatmap row(s): ", paste(missing.rows, collapse = ", "))
    }
    row.groups <- droplevels(as.factor(row.groups[rownames(x)]))
  }

  if (is.null(column.annotation)) {
    column.annotation <- data.frame(group = column.groups, row.names = colnames(x))
  } else {
    column.annotation <- as.data.frame(column.annotation, check.names = FALSE)
    if (is.null(rownames(column.annotation))) {
      if (nrow(column.annotation) != ncol(x)) {
        stop("Unnamed `column.annotation` must have one row per heatmap column")
      }
      rownames(column.annotation) <- colnames(x)
    }
    missing.annotation <- setdiff(colnames(x), rownames(column.annotation))
    if (length(missing.annotation) > 0L) {
      stop("`column.annotation` is missing heatmap column(s): ", paste(missing.annotation, collapse = ", "))
    }
    column.annotation <- column.annotation[colnames(x), , drop = FALSE]
    if (!"group" %in% colnames(column.annotation)) {
      column.annotation <- cbind(
        data.frame(group = column.groups, row.names = colnames(x)),
        column.annotation
      )
    }
  }

  if (is.null(annotation.colors)) {
    annotation.colors <- .pagoda2_heatmap_annotation_colors(
      column.annotation,
      color.list = column.annotation.colors,
      s = s,
      v = v
    )
  }

  if (is.null(row.group.colors)) {
    row.group.colors <- annotation.colors$palettes$group[levels(row.groups)]
    if (anyNA(row.group.colors) || length(row.group.colors) != length(levels(row.groups))) {
      row.group.colors <- .pagoda2_discrete_palette(levels(row.groups), s = s, v = v)
    }
  } else {
    row.group.colors <- row.group.colors[levels(row.groups)]
    if (anyNA(row.group.colors) || is.null(names(row.group.colors))) {
      stop("`row.group.colors` must be a named color vector containing all displayed row group levels")
    }
  }

  annotation.grobs <- normalize_annotation_grobs(annotation.grobs)
  spec <- list(
    matrix = x,
    expression.palette = expression.palette,
    groups = column.groups,
    column.groups = column.groups,
    genes = rownames(x),
    gene.groups = row.groups,
    row.groups = row.groups,
    column.annotation = column.annotation,
    annotation.colors = annotation.colors,
    gene.group.colors = row.group.colors,
    row.group.colors = row.group.colors,
    labeled.gene.subset = labeled.row.subset,
    labeled.row.subset = labeled.row.subset,
    label.indices = label.indices,
    show.gene.groups = show.row.groups,
    show.row.groups = show.row.groups,
    show.group.legend = show.group.legend,
    show_heatmap_legend = show_heatmap_legend,
    border = border,
    row.label.font.size = row.label.font.size,
    split = split,
    split.gap = split.gap,
    use.raster = TRUE,
    annotation.grobs = annotation.grobs,
    legend.max.levels = legend.max.levels,
    legend.columns = legend.columns
  )
  if (length(extra) > 0L) {
    spec[names(extra)] <- extra
  }
  structure(spec, class = unique(c(class, "pagoda2_native_heatmap_spec")))
}

.pagoda2_draw_native_heatmap <- function(spec, newpage = TRUE) {
  heatmap_color_matrix <- function(x, palette, na.color = "grey90") {
    idx <- floor(pmax(0, pmin(1, x)) * (length(palette) - 1L)) + 1L
    colors <- palette[idx]
    colors[!is.finite(x)] <- na.color
    dim(colors) <- dim(x)
    dimnames(colors) <- dimnames(x)
    colors
  }

  group_boundaries <- function(x) {
    if (length(x) == 0L) {
      return(integer())
    }
    boundaries <- cumsum(rle(as.character(x))$lengths)
    boundaries[boundaries < length(x)]
  }

  spread_npc_positions <- function(x, min.gap, lower = 0.01, upper = 0.99) {
    if (length(x) <= 1L || !is.finite(min.gap) || min.gap <= 0) {
      return(x)
    }
    ord <- order(x)
    xx <- x[ord]
    width <- upper - lower
    span <- (length(xx) - 1L) * min.gap
    if (span >= width) {
      xx <- seq(lower, upper, length.out = length(xx))
    } else {
      for (i in seq.int(2L, length(xx))) {
        xx[i] <- max(xx[i], xx[i - 1L] + min.gap)
      }
      if (xx[length(xx)] > upper) {
        xx[length(xx)] <- upper
        for (i in seq.int(length(xx) - 1L, 1L)) {
          xx[i] <- min(xx[i], xx[i + 1L] - min.gap)
        }
      }
      if (xx[1L] < lower) {
        xx[1L] <- lower
        for (i in seq.int(2L, length(xx))) {
          xx[i] <- max(xx[i], xx[i - 1L] + min.gap)
        }
        if (xx[length(xx)] > upper) {
          xx <- seq(lower, upper, length.out = length(xx))
        }
      }
    }
    out <- x
    out[ord] <- pmax(lower, pmin(upper, xx))
    out
  }

  text_extents_mm <- function(labels, gp) {
    grid::pushViewport(grid::viewport(gp = gp))
    widths <- vapply(labels, function(label) {
      grid::convertWidth(grid::stringWidth(label), "mm", valueOnly = TRUE)
    }, numeric(1))
    heights <- vapply(labels, function(label) {
      grid::convertHeight(grid::stringHeight(label), "mm", valueOnly = TRUE)
    }, numeric(1))
    grid::popViewport()
    list(width = widths, height = heights)
  }

  prepare_column_label_layout <- function(labels, lengths, n.cols, heatmap.width.mm,
                                          right.label.width.mm, font.size = 11) {
    starts <- c(1L, cumsum(lengths)[-length(lengths)] + 1L)
    mids <- (starts - 0.5 + lengths / 2) / n.cols
    gp <- grid::gpar(fontsize = font.size)
    extents <- text_extents_mm(labels, gp = gp)
    pad.mm <- grid::convertWidth(grid::unit(5, "pt"), "mm", valueOnly = TRUE)
    x.max <- if (is.finite(heatmap.width.mm) && heatmap.width.mm > 0) {
      1 + right.label.width.mm / heatmap.width.mm
    } else {
      1
    }
    available <- max(0, x.max - 0.025)
    horizontal.gap <- (max(extents$width, na.rm = TRUE) + pad.mm) / heatmap.width.mm
    horizontal.x <- spread_npc_positions(mids, horizontal.gap, lower = 0.01, upper = x.max - 0.015)
    horizontal.shift <- max(abs(horizontal.x - mids), na.rm = TRUE)
    horizontal.span <- if (length(labels) > 1L) (length(labels) - 1L) * horizontal.gap else 0
    group.widths <- lengths / n.cols
    labels.fit.groups <- all((extents$width + pad.mm) / heatmap.width.mm <= pmax(group.widths, 0.005))
    use.angled <- length(labels) > 1L && (
      !labels.fit.groups || horizontal.shift > 0.10 || horizontal.span > available * 0.95
    )

    angle <- if (use.angled) 35 else 0
    if (angle == 0) {
      gap <- horizontal.gap
      row.height.mm <- 6
      projected.width <- extents$width
      projected.height <- extents$height
    } else {
      radians <- angle * pi / 180
      max.row.height.mm <- 45
      while (font.size > 8) {
        projected.height <- extents$width * sin(radians) + extents$height * cos(radians)
        if (max(projected.height, na.rm = TRUE) + 3 <= max.row.height.mm) {
          break
        }
        font.size <- font.size - 1
        gp <- grid::gpar(fontsize = font.size)
        extents <- text_extents_mm(labels, gp = gp)
      }
      projected.width <- extents$width * cos(radians) + extents$height * sin(radians)
      projected.height <- extents$width * sin(radians) + extents$height * cos(radians)
      gap <- (max(projected.width, na.rm = TRUE) + pad.mm) / heatmap.width.mm
      row.height.mm <- max(12, max(projected.height, na.rm = TRUE) + 7)
    }

    list(
      labels = labels,
      starts = starts,
      mids = mids,
      font.size = font.size,
      angle = angle,
      gap = gap,
      max.projected.width.mm = max(projected.width, na.rm = TRUE),
      max.projected.height.mm = max(projected.height, na.rm = TRUE),
      row.height.mm = row.height.mm,
      x.max = x.max,
      extents = extents
    )
  }

  draw_discrete_legend <- function(legend, max.levels = 18,
                                   title.font.size = 10,
                                   label.font.size = 8,
                                   level.columns = 1) {
    truncate_to_width <- function(labels, max.width.npc, gp) {
      vapply(labels, function(label) {
        label <- as.character(label)
        grid::pushViewport(grid::viewport(gp = gp))
        width <- grid::convertWidth(grid::stringWidth(label), "npc", valueOnly = TRUE)
        grid::popViewport()
        if (!is.finite(width) || width <= max.width.npc || nchar(label) <= 4L) {
          return(label)
        }
        out <- label
        while (nchar(out) > 4L) {
          out <- paste0(substr(label, 1L, nchar(out) - 4L), "...")
          grid::pushViewport(grid::viewport(gp = gp))
          width <- grid::convertWidth(grid::stringWidth(out), "npc", valueOnly = TRUE)
          grid::popViewport()
          if (!is.finite(width) || width <= max.width.npc) {
            return(out)
          }
        }
        out
      }, character(1))
    }

    colors <- legend$colors
    n.total <- length(colors)
    if (n.total == 0L) {
      return(invisible(NULL))
    }
    shown <- utils::head(seq_along(colors), max.levels)
    n <- length(shown)
    level.columns <- max(1L, min(as.integer(level.columns), n))
    level.rows <- ceiling(n / level.columns)
    viewport.height.mm <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    title.y.mm <- max(0, viewport.height.mm - 1)
    first.row.y.mm <- max(0, viewport.height.mm - 8)
    step.mm <- min(4.4, max(2.6, (viewport.height.mm - 13) / max(1, level.rows + as.integer(n.total > n))))
    grid::grid.text(
      legend$name,
      x = grid::unit(0.02, "npc"),
      y = grid::unit(title.y.mm, "mm"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = title.font.size, fontface = "bold")
    )
    label.gp <- grid::gpar(fontsize = label.font.size)
    column.width <- 0.96 / level.columns
    display.labels <- truncate_to_width(names(colors)[shown], max.width.npc = max(0.05, column.width - 0.16), gp = label.gp)
    for (i in seq_len(n)) {
      col <- floor((i - 1L) / level.rows) + 1L
      row <- ((i - 1L) %% level.rows) + 1L
      x0 <- 0.02 + (col - 1L) * column.width
      y <- first.row.y.mm - (row - 1L) * step.mm
      grid::grid.rect(x = grid::unit(x0, "npc"), y = grid::unit(y, "mm"),
                      width = grid::unit(3.2, "mm"), height = grid::unit(2.4, "mm"),
                      just = c("left", "center"),
                      gp = grid::gpar(fill = colors[shown[i]], col = NA))
      grid::grid.text(display.labels[i],
                      x = grid::unit(x0, "npc") + grid::unit(4.4, "mm"),
                      y = grid::unit(y, "mm"), just = c("left", "center"), gp = label.gp)
    }
    if (n.total > n) {
      grid::grid.text(paste0("+", n.total - n, " more"),
                      x = grid::unit(0.02, "npc"),
                      y = grid::unit(first.row.y.mm - level.rows * step.mm, "mm"),
                      just = c("left", "center"), gp = grid::gpar(fontsize = label.font.size, col = "grey30"))
    }
    invisible(NULL)
  }

  draw_continuous_legend <- function(legend,
                                     title.font.size = 10,
                                     label.font.size = 8) {
    grid::grid.text(
      legend$name,
      x = 0.07,
      y = 0.5,
      rot = 90,
      just = "center",
      gp = grid::gpar(fontsize = title.font.size, fontface = "bold")
    )
    pal <- legend$palette
    grid::grid.raster(matrix(rev(pal), ncol = 1), x = 0.22, y = 0.5, width = 0.07, height = 0.72,
                      interpolate = FALSE)
    rng <- legend$range
    if (all(is.finite(rng))) {
      grid::grid.text(format(signif(rng[2], 3)), x = 0.33, y = 0.86, just = c("left", "center"), gp = grid::gpar(fontsize = label.font.size))
      grid::grid.text(format(signif(rng[1], 3)), x = 0.33, y = 0.14, just = c("left", "center"), gp = grid::gpar(fontsize = label.font.size))
    }
    invisible(NULL)
  }

  draw_legends <- function(spec) {
    discrete_legend_height_mm <- function(legend, level.columns, max.levels) {
      n <- min(length(legend$colors), max.levels)
      10 + ceiling(n / level.columns) * 4.4 + 5
    }

    discrete_labels_fit_one_column <- function(legend, max.levels, label.font.size = 8) {
      labels <- names(legend$colors)[utils::head(seq_along(legend$colors), max.levels)]
      if (length(labels) == 0L) {
        return(TRUE)
      }
      gp <- grid::gpar(fontsize = label.font.size)
      grid::pushViewport(grid::viewport(gp = gp))
      widths <- grid::convertWidth(grid::stringWidth(labels), "npc", valueOnly = TRUE)
      grid::popViewport()
      all(is.finite(widths)) && max(widths, na.rm = TRUE) <= 0.80
    }

    legends <- list()
    if (isTRUE(spec$show_heatmap_legend)) {
      legends$expression <- list(
        type = "continuous",
        name = "expression",
        palette = spec$expression.palette,
        range = c(0, 1)
      )
    }
    if (isTRUE(spec$show.group.legend)) {
      legends <- c(legends, spec$annotation.colors$legends)
    }
    if (length(spec$annotation.grobs$right) == 0L && length(legends) == 0L) {
      return(invisible(NULL))
    }

    n.grob <- length(spec$annotation.grobs$right)
    ncol <- spec$legend.columns
    if (is.null(ncol)) {
      ncol <- if (length(legends) > 4L) 2L else 1L
    }
    ncol <- max(1L, as.integer(ncol))
    wide.legend <- vapply(legends, function(legend) {
      identical(legend$type, "discrete") && length(legend$colors) > 6L && ncol > 1L
    }, logical(1))
    wide.legends <- legends[wide.legend]
    regular.legends <- legends[!wide.legend]
    n.wide <- length(wide.legends)
    n.regular <- length(regular.legends)
    nrow.regular <- if (n.regular == 0L) 0L else ceiling(n.regular / ncol)
    regular.row.height.mm <- 36
    available.height.mm <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    fixed.height.mm <- 12 * n.grob + regular.row.height.mm * nrow.regular
    wide.available.mm <- max(0, available.height.mm - fixed.height.mm)
    if (n.wide > 0L) {
      wide.layouts <- lapply(wide.legends, function(legend) {
        two.column <- min(2L, ncol)
        one.height <- discrete_legend_height_mm(legend, 1L, spec$legend.max.levels)
        one.fits <- ncol > 1L &&
          discrete_labels_fit_one_column(legend, spec$legend.max.levels) &&
          one.height <= max(0, wide.available.mm / n.wide)
        level.columns <- if (one.fits) 1L else two.column
        list(
          level.columns = level.columns,
          height.mm = discrete_legend_height_mm(legend, level.columns, spec$legend.max.levels)
        )
      })
      wide.heights <- vapply(wide.layouts, `[[`, numeric(1), "height.mm")
    } else {
      wide.layouts <- list()
      wide.heights <- numeric()
    }
    use.bottom.regular <- n.wide > 0L && n.regular > 0L &&
      any(vapply(wide.layouts, function(layout) layout$level.columns == 1L, logical(1)))

    append_heights <- function(heights, new.heights) {
      if (is.null(heights)) {
        new.heights
      } else {
        grid::unit.c(heights, new.heights)
      }
    }

    heights <- NULL
    if (n.grob > 0L) {
      heights <- append_heights(heights, grid::unit(rep(12, n.grob), "mm"))
    }
    if (n.wide > 0L) {
      heights <- append_heights(heights, grid::unit(wide.heights, "mm"))
    }
    if (use.bottom.regular) {
      heights <- append_heights(heights, grid::unit(1, "null"))
    }
    regular.start.row <- n.grob + n.wide + as.integer(use.bottom.regular) + 1L
    if (nrow.regular > 0L) {
      heights <- append_heights(heights, grid::unit(rep(regular.row.height.mm, nrow.regular), "mm"))
    }
    if (!use.bottom.regular) {
      heights <- append_heights(heights, grid::unit(1, "null"))
    }
    layout.rows <- length(heights)
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(layout.rows, ncol, heights = heights)))
    if (n.grob > 0L) {
      for (i in seq_len(n.grob)) {
        grid::pushViewport(grid::viewport(layout.pos.row = i, layout.pos.col = 1:ncol))
        grid::grid.draw(spec$annotation.grobs$right[[i]])
        grid::popViewport()
      }
    }
    if (n.wide > 0L) {
      for (i in seq_len(n.wide)) {
        row <- n.grob + i
        grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = 1:ncol))
        draw_discrete_legend(
          wide.legends[[i]],
          max.levels = spec$legend.max.levels,
          level.columns = wide.layouts[[i]]$level.columns
        )
        grid::popViewport()
      }
    }
    if (n.regular > 0L) {
      for (i in seq_len(n.regular)) {
        row <- regular.start.row + ceiling(i / ncol) - 1L
        col <- ((i - 1L) %% ncol) + 1L
        grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
        if (identical(regular.legends[[i]]$type, "continuous")) {
          draw_continuous_legend(regular.legends[[i]])
        } else {
          draw_discrete_legend(regular.legends[[i]], max.levels = spec$legend.max.levels)
        }
        grid::popViewport()
      }
    }
    grid::popViewport()
    invisible(NULL)
  }

  if (isTRUE(newpage)) {
    grid::grid.newpage()
  }
  x <- spec$matrix
  n.rows <- nrow(x)
  n.cols <- ncol(x)
  row.groups <- if (!is.null(spec$row.groups)) spec$row.groups else spec$gene.groups
  row.group.colors <- if (!is.null(spec$row.group.colors)) spec$row.group.colors else spec$gene.group.colors
  show.row.groups <- if (!is.null(spec$show.row.groups)) spec$show.row.groups else spec$show.gene.groups
  separator.gp <- grid::gpar(col = "grey25", lwd = 0.55)
  border.gp <- grid::gpar(fill = NA, col = "grey25", lwd = 0.55)
  label.leader.gp <- grid::gpar(col = "grey35", lwd = 0.45)
  top.tracks <- spec$annotation.colors$tracks
  n.top.tracks <- length(top.tracks)
  n.top.grobs <- length(spec$annotation.grobs$top)
  n.left.grobs <- length(spec$annotation.grobs$left)
  n.bottom.grobs <- length(spec$annotation.grobs$bottom)
  legend.count <- length(spec$annotation.colors$legends) + as.integer(spec$show_heatmap_legend)
  legend.columns <- spec$legend.columns
  if (is.null(legend.columns)) {
    legend.columns <- if (legend.count > 4L) 2L else 1L
  }
  legend.width <- if (legend.count > 0L || length(spec$annotation.grobs$right) > 0L) {
    grid::unit(30 * legend.columns, "mm")
  } else {
    grid::unit(1, "mm")
  }
  label.width <- if (is.null(spec$label.indices) && n.rows > 80L) grid::unit(2, "mm") else grid::unit(35, "mm")
  column.label.spill.width <- label.width
  custom.left.width <- if (n.left.grobs > 0L) grid::unit(8 * n.left.grobs, "mm") else grid::unit(1, "mm")
  if (isTRUE(show.row.groups)) {
    row.group.label.gp <- grid::gpar(fontsize = min(10, spec$row.label.font.size))
    grid::pushViewport(grid::viewport(gp = row.group.label.gp))
    row.group.label.width.mm <- max(grid::convertWidth(
      grid::stringWidth(rle(as.character(row.groups))$values),
      "mm",
      valueOnly = TRUE
    ), na.rm = TRUE)
    grid::popViewport()
    row.group.label.width <- grid::unit(min(62, max(10, row.group.label.width.mm + 8)), "mm")
  } else {
    row.group.label.width <- grid::unit(1, "mm")
  }
  group.strip.width <- if (isTRUE(show.row.groups)) grid::unit(5, "mm") else grid::unit(1, "mm")
  column.label.rle <- rle(as.character(spec$groups[colnames(x)]))
  page.width.mm <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  fixed.width.mm <- grid::convertWidth(
    custom.left.width + row.group.label.width + group.strip.width + label.width + legend.width,
    "mm",
    valueOnly = TRUE
  )
  estimated.heatmap.width.mm <- max(1, page.width.mm - fixed.width.mm)
  right.label.width.mm <- grid::convertWidth(column.label.spill.width, "mm", valueOnly = TRUE)
  column.label.layout <- prepare_column_label_layout(
    labels = column.label.rle$values,
    lengths = column.label.rle$lengths,
    n.cols = n.cols,
    heatmap.width.mm = estimated.heatmap.width.mm,
    right.label.width.mm = right.label.width.mm
  )
  top.height <- if (n.top.tracks > 0L || n.top.grobs > 0L) {
    grid::unit(5 * n.top.tracks + 8 * n.top.grobs, "mm")
  } else {
    grid::unit(1, "mm")
  }
  bottom.height <- if (n.bottom.grobs > 0L) grid::unit(8 * n.bottom.grobs, "mm") else grid::unit(1, "mm")
  layout <- grid::grid.layout(
    nrow = 4,
    ncol = 6,
    heights = grid::unit.c(grid::unit(column.label.layout$row.height.mm, "mm"), top.height, grid::unit(1, "null"), bottom.height),
    widths = grid::unit.c(custom.left.width, row.group.label.width, group.strip.width, grid::unit(1, "null"), label.width, legend.width)
  )
  grid::pushViewport(grid::viewport(layout = layout))

  # Column group labels can spill into the row-label column, but not into the
  # legend column; otherwise long annotations drift too far from their groups.
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 4))
  heatmap.width.mm <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  grid::popViewport()
  right.label.width.mm <- grid::convertWidth(column.label.spill.width, "mm", valueOnly = TRUE)
  x.max <- if (is.finite(heatmap.width.mm) && heatmap.width.mm > 0) {
    1 + right.label.width.mm / heatmap.width.mm
  } else {
    1
  }
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 4:5,
                                    xscale = c(0, x.max), clip = "off"))
  column.label.layout <- prepare_column_label_layout(
    labels = column.label.rle$values,
    lengths = column.label.rle$lengths,
    n.cols = n.cols,
    heatmap.width.mm = heatmap.width.mm,
    right.label.width.mm = right.label.width.mm
  )
  mids <- column.label.layout$mids
  min.x.gap <- column.label.layout$gap
  label.upper <- if (column.label.layout$angle == 0) {
    x.max - 0.015
  } else {
    max(0.01, x.max - column.label.layout$max.projected.width.mm / heatmap.width.mm - 0.015)
  }
  label.x <- spread_npc_positions(mids, min.x.gap, lower = 0.01, upper = label.upper)
  shifted <- abs(label.x - mids) > 0.003 | column.label.layout$angle != 0
  if (column.label.layout$angle == 0) {
    leader.end.mm <- rep(0.8, length(label.x))
    label.y.mm <- NA_real_
  } else {
    min.leader.angle <- 9 * pi / 180
    desired.end.mm <- pmax(2.2, tan(min.leader.angle) * abs(label.x - mids) * heatmap.width.mm)
    max.end.mm <- max(
      1.2,
      column.label.layout$row.height.mm - column.label.layout$max.projected.height.mm - 2
    )
    leader.anchor.mm <- min(
      column.label.layout$row.height.mm - column.label.layout$max.projected.height.mm - 0.5,
      max(desired.end.mm, na.rm = TRUE) + 1.2
    )
    leader.anchor.mm <- pmax(2.2, pmin(leader.anchor.mm, max.end.mm))
    leader.end.mm <- rep(leader.anchor.mm, length(label.x))
    label.y.mm <- leader.anchor.mm
  }
  if (any(shifted)) {
    grid::grid.segments(x0 = grid::unit(mids[shifted], "native"),
                        x1 = grid::unit(label.x[shifted], "native"),
                        y0 = grid::unit(0, "mm"),
                        y1 = grid::unit(leader.end.mm[shifted], "mm"),
                        gp = label.leader.gp)
  }
  if (column.label.layout$angle == 0) {
    grid::grid.text(column.label.layout$labels, x = grid::unit(label.x, "native"), y = 0.52,
                    gp = grid::gpar(fontsize = column.label.layout$font.size), just = "center")
  } else {
    grid::grid.text(column.label.layout$labels,
                    x = grid::unit(label.x, "native"),
                    y = grid::unit(label.y.mm, "mm"),
                    rot = column.label.layout$angle,
                    gp = grid::gpar(fontsize = column.label.layout$font.size),
                    just = c("left", "bottom"))
  }
  grid::popViewport()

  # Top arbitrary grobs and metadata tracks.
  if (n.top.tracks > 0L || n.top.grobs > 0L) {
    top.rows <- n.top.grobs + n.top.tracks
    top.heights <- NULL
    if (n.top.grobs > 0L) {
      top.heights <- grid::unit(rep(8, n.top.grobs), "mm")
    }
    if (n.top.tracks > 0L) {
      track.heights <- grid::unit(rep(5, n.top.tracks), "mm")
      top.heights <- if (is.null(top.heights)) track.heights else grid::unit.c(top.heights, track.heights)
    }
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 4,
                                      layout = grid::grid.layout(top.rows, 1, heights = top.heights)))
    if (n.top.grobs > 0L) {
      for (i in seq_len(n.top.grobs)) {
        grid::pushViewport(grid::viewport(layout.pos.row = i, layout.pos.col = 1))
        grid::grid.draw(spec$annotation.grobs$top[[i]])
        grid::popViewport()
      }
    }
    if (n.top.tracks > 0L) {
      track.names <- names(top.tracks)
      for (i in seq_len(n.top.tracks)) {
        row <- n.top.grobs + i
        grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = 1))
        track.raster <- grDevices::as.raster(matrix(top.tracks[[i]][colnames(x)], nrow = 1))
        grid::grid.raster(track.raster, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                          interpolate = FALSE)
        cb <- group_boundaries(spec$groups[colnames(x)])
        if (isTRUE(spec$split) && length(cb) > 0L) {
          grid::grid.segments(x0 = cb / n.cols, x1 = cb / n.cols, y0 = 0, y1 = 1,
                              gp = separator.gp)
        }
        grid::grid.rect(gp = border.gp)
        grid::grid.text(track.names[i], x = 1.002, y = 0.5, just = c("left", "center"), gp = grid::gpar(fontsize = 7))
        grid::popViewport()
      }
    }
    grid::popViewport()
  }

  # Left custom grobs.
  if (n.left.grobs > 0L) {
    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 1,
                                      layout = grid::grid.layout(1, n.left.grobs)))
    for (i in seq_len(n.left.grobs)) {
      grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = i))
      grid::grid.draw(spec$annotation.grobs$left[[i]])
      grid::popViewport()
    }
    grid::popViewport()
  }

  # Row group labels.
  if (isTRUE(show.row.groups)) {
    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 2))
    rr <- rle(as.character(row.groups))
    starts <- c(1L, cumsum(rr$lengths)[-length(rr$lengths)] + 1L)
    mids <- 1 - (starts - 0.5 + rr$lengths / 2) / n.rows
    row.group.font.size <- min(10, spec$row.label.font.size)
    min.y.gap <- grid::convertHeight(grid::unit(row.group.font.size * 1.25, "pt"), "npc", valueOnly = TRUE)
    label.y <- spread_npc_positions(mids, min.y.gap, lower = 0.01, upper = 0.99)
    shifted <- abs(label.y - mids) > 0.003
    if (any(shifted)) {
      grid::grid.segments(x0 = 0.99, x1 = 0.96, y0 = mids[shifted], y1 = label.y[shifted],
                          gp = label.leader.gp)
    }
    grid::grid.text(rr$values, x = 0.93, y = label.y, just = c("right", "center"),
                    gp = grid::gpar(fontsize = row.group.font.size))
    grid::popViewport()
  }

  # Row group strip.
  if (isTRUE(show.row.groups)) {
    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 3))
    row.colors <- row.group.colors[as.character(row.groups)]
    grid::grid.raster(grDevices::as.raster(matrix(row.colors, ncol = 1)),
                      width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                      interpolate = FALSE)
    rb <- group_boundaries(row.groups)
    if (isTRUE(spec$split) && length(rb) > 0L) {
      grid::grid.segments(x0 = 0, x1 = 1, y0 = 1 - rb / n.rows, y1 = 1 - rb / n.rows,
                          gp = separator.gp)
    }
    grid::grid.rect(gp = border.gp)
    grid::popViewport()
  }

  # Main heatmap.
  grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 4))
  grid::grid.raster(grDevices::as.raster(heatmap_color_matrix(x, spec$expression.palette)),
                    width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                    interpolate = FALSE)
  if (isTRUE(spec$split)) {
    cb <- group_boundaries(spec$groups[colnames(x)])
    if (length(cb) > 0L) {
      grid::grid.segments(x0 = cb / n.cols, x1 = cb / n.cols, y0 = 0, y1 = 1,
                          gp = separator.gp)
    }
    rb <- group_boundaries(row.groups)
    if (length(rb) > 0L) {
      grid::grid.segments(x0 = 0, x1 = 1, y0 = 1 - rb / n.rows, y1 = 1 - rb / n.rows,
                          gp = separator.gp)
    }
  }
  if (isTRUE(spec$border)) {
    grid::grid.rect(gp = border.gp)
  }
  grid::popViewport()

  # Row labels.
  grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 5))
  label.indices <- spec$label.indices
  if (is.null(label.indices) && n.rows <= 80L) {
    label.indices <- seq_len(n.rows)
  }
  if (!is.null(label.indices) && length(label.indices) > 0L) {
    row.y <- 1 - (label.indices - 0.5) / n.rows
    min.gap <- grid::convertHeight(grid::unit(spec$row.label.font.size * 1.25, "pt"), "npc", valueOnly = TRUE)
    y <- spread_npc_positions(row.y, min.gap)
    grid::grid.segments(x0 = 0, x1 = 0.08, y0 = row.y, y1 = y, gp = label.leader.gp)
    grid::grid.text(rownames(x)[label.indices], x = 0.1, y = y, just = c("left", "center"),
                    gp = grid::gpar(fontsize = spec$row.label.font.size))
  }
  grid::popViewport()

  # Bottom custom grobs.
  if (n.bottom.grobs > 0L) {
    grid::pushViewport(grid::viewport(layout.pos.row = 4, layout.pos.col = 4,
                                      layout = grid::grid.layout(n.bottom.grobs, 1)))
    for (i in seq_len(n.bottom.grobs)) {
      grid::pushViewport(grid::viewport(layout.pos.row = i, layout.pos.col = 1))
      grid::grid.draw(spec$annotation.grobs$bottom[[i]])
      grid::popViewport()
    }
    grid::popViewport()
  }

  # Legends and right-side custom grobs.
  grid::pushViewport(grid::viewport(layout.pos.row = 2:4, layout.pos.col = 6))
  draw_legends(spec)
  grid::popViewport()
  grid::popViewport()
  spec$native.layout <- list(
    column.labels = list(
      angle = column.label.layout$angle,
      font.size = column.label.layout$font.size,
      leader.end.mm = leader.end.mm,
      label.y.mm = label.y.mm,
      x = label.x,
      mids = mids,
      row.height.mm = column.label.layout$row.height.mm
    )
  )
  invisible(spec)
}
