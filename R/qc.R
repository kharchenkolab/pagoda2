## QC implementation and QC plots for Pagoda2


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

.pagoda2_resolve_qc_gene_set <- function(gene.names, genes = NULL, pattern = NULL,
                                         auto.patterns = character(), kind = "QC",
                                         infer = TRUE, verbose = FALSE) {
  gene.names <- as.character(gene.names)
  if (!is.null(genes)) {
    matched <- intersect(as.character(genes), gene.names)
    if (length(matched) == 0) {
      warning("No ", kind, " genes from the supplied gene set were found in the count matrix", call. = FALSE)
    }
    return(list(genes = matched, source = "genes", pattern = NA_character_))
  }
  if (!is.null(pattern)) {
    matched <- grep(pattern, gene.names, value = TRUE)
    if (length(matched) == 0) {
      warning("No ", kind, " genes matched pattern `", pattern, "`", call. = FALSE)
    }
    return(list(genes = matched, source = "pattern", pattern = pattern))
  }
  if (isTRUE(infer)) {
    for (p in auto.patterns) {
      matched <- grep(p, gene.names, value = TRUE)
      if (length(matched) > 0) {
        return(list(genes = matched, source = "auto", pattern = p))
      }
    }
  }
  list(genes = character(), source = "none", pattern = NA_character_)
}

.pagoda2_qc_percent_from_genes <- function(matrix, genes) {
  if (length(genes) == 0) {
    return(rep(NA_real_, nrow(matrix)))
  }
  idx <- match(genes, colnames(matrix))
  idx <- idx[!is.na(idx)]
  total <- as.numeric(Matrix::rowSums(matrix))
  selected <- as.numeric(Matrix::rowSums(matrix[, idx, drop = FALSE]))
  percent <- rep(NA_real_, length(total))
  keep <- is.finite(total) & total > 0
  percent[keep] <- 100 * selected[keep] / total[keep]
  percent
}

.pagoda2_qc_gene_set_summary <- function(gene.sets) {
  gene.sets <- gene.sets[!vapply(gene.sets, is.null, logical(1))]
  gene.sets <- gene.sets[vapply(gene.sets, function(x) length(x$genes) > 0, logical(1))]
  if (length(gene.sets) == 0) {
    return(NULL)
  }
  parts <- vapply(gene.sets, function(x) {
    source <- switch(x$source,
      auto = paste0("auto pattern `", x$pattern, "`"),
      pattern = paste0("pattern `", x$pattern, "`"),
      genes = "supplied gene set",
      x$source
    )
    paste0(x$column, ": ", length(x$genes), " genes (", source, ")")
  }, character(1))
  paste0("QC composition: ", paste(parts, collapse = "; "))
}

.pagoda2_gene_qc <- function(matrix, min.cells = 5, min.molecules = 0, keep.genes = NULL) {
  if (is.null(colnames(matrix))) {
    stop("Gene QC matrix must have gene names as colnames")
  }
  n.cells.detected <- as.numeric(diff(matrix@p))
  n.molecules <- as.numeric(Matrix::colSums(matrix))
  analysis.pass <- n.cells.detected >= min.cells & n.molecules >= min.molecules
  if (!is.null(keep.genes)) {
    analysis.pass <- analysis.pass | colnames(matrix) %in% keep.genes
  }
  qc <- data.frame(
    n_cells_detected = n.cells.detected,
    n_molecules = n.molecules,
    analysis_pass = analysis.pass,
    row.names = colnames(matrix)
  )
  attr(qc, "pagoda2.gene.qc") <- list(
    min.cells = min.cells,
    min.molecules = min.molecules,
    keep.genes = keep.genes
  )
  qc
}

.pagoda2_gene_qc_summary <- function(qc) {
  n.genes <- nrow(qc)
  n.pass <- if ("analysis_pass" %in% colnames(qc)) sum(as.logical(qc$analysis_pass), na.rm = TRUE) else NA_integer_
  detected.q <- stats::quantile(qc$n_cells_detected, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  molecule.q <- stats::quantile(qc$n_molecules, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  msg <- paste0(
    "Gene QC: ", n.genes, " genes; detected-cell median ", signif(detected.q[2], 4),
    " [IQR ", signif(detected.q[1], 4), "-", signif(detected.q[3], 4), "]; molecules median ",
    signif(molecule.q[2], 4), " [IQR ", signif(molecule.q[1], 4), "-", signif(molecule.q[3], 4), "]"
  )
  if (is.finite(n.pass)) {
    msg <- paste0(msg, "; ", n.pass, " analysis genes (", signif(100 * n.pass / n.genes, 3), "%)")
  }
  msg
}


.pagoda2_r6_run_qc <- function(p2, method = c("gene_molecule", "metrics"), overwrite = FALSE, matrix = NULL,
                               min.molecules = 500, max.molecules = 5e4, p.level = NULL, verbose = FALSE,
                               mt.genes = NULL, ribo.genes = NULL, mt.pattern = NULL, ribo.pattern = NULL,
                               infer.qc.genes = TRUE) {
  explicit.min.molecules <- !missing(min.molecules)
  explicit.max.molecules <- !missing(max.molecules)
  explicit.mt <- !missing(mt.genes) || !missing(mt.pattern)
  explicit.ribo <- !missing(ribo.genes) || !missing(ribo.pattern)
  method <- match.arg(method)
  min.molecules <- .pagoda2_filter_default(p2, "min.molecules", min.molecules, explicit.min.molecules)
  max.molecules <- .pagoda2_filter_default(p2, "max.molecules", max.molecules, explicit.max.molecules)
  if (is.null(matrix)) {
    matrix <- p2$rawCounts
  }
  if (is.null(matrix)) {
    matrix <- p2$misc[["rawCounts"]]
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
  base.exists <- !overwrite && all(qc.cols %in% colnames(p2$cellMeta))
  qc <- NULL
  if (base.exists) {
    qc <- p2$getCellMeta(qc.cols)
  } else {
    qc <- if (method == "metrics") {
      qc.metrics <- data.frame(
        n_molecules = as.numeric(Matrix::rowSums(matrix)),
        n_genes = as.numeric(Matrix::rowSums(matrix > 0)),
        row.names = rownames(matrix)
      )
      attr(qc.metrics, "pagoda2.qc") <- list(method = "metrics")
      qc.metrics
    } else {
      .pagoda2_qc_gene_molecule(
        matrix,
        min.molecules = min.molecules,
        max.molecules = max.molecules,
        p.level = p.level
      )
    }
  }
  gene.sets <- list(
    mitochondrial = .pagoda2_resolve_qc_gene_set(
      colnames(matrix),
      genes = mt.genes,
      pattern = mt.pattern,
      auto.patterns = c("^MT-", "^mt-", "^Mt-"),
      kind = "mitochondrial",
      infer = infer.qc.genes,
      verbose = verbose
    ),
    ribosomal = .pagoda2_resolve_qc_gene_set(
      colnames(matrix),
      genes = ribo.genes,
      pattern = ribo.pattern,
      auto.patterns = c("^RP[SL]", "^Rp[sl]", "^rp[sl]"),
      kind = "ribosomal",
      infer = infer.qc.genes,
      verbose = verbose
    )
  )
  gene.sets$mitochondrial$column <- "percent_mito"
  gene.sets$ribosomal$column <- "percent_ribo"
  if ((overwrite || explicit.mt || !"percent_mito" %in% colnames(p2$cellMeta)) &&
    length(gene.sets$mitochondrial$genes) > 0) {
    qc$percent_mito <- .pagoda2_qc_percent_from_genes(matrix, gene.sets$mitochondrial$genes)
  }
  if ((overwrite || explicit.ribo || !"percent_ribo" %in% colnames(p2$cellMeta)) &&
    length(gene.sets$ribosomal$genes) > 0) {
    qc$percent_ribo <- .pagoda2_qc_percent_from_genes(matrix, gene.sets$ribosomal$genes)
  }
  if (!"percent_mito" %in% colnames(qc) && "percent_mito" %in% colnames(p2$cellMeta)) {
    qc$percent_mito <- p2$getCellMeta("percent_mito")$percent_mito
  }
  if (!"percent_ribo" %in% colnames(qc) && "percent_ribo" %in% colnames(p2$cellMeta)) {
    qc$percent_ribo <- p2$getCellMeta("percent_ribo")$percent_ribo
  }
  p2$setCellMeta(qc, overwrite = TRUE)
  history.qc <- attr(qc, "pagoda2.qc")
  if (is.null(history.qc)) {
    history.qc <- p2$history$qc
  }
  if (is.null(history.qc)) {
    history.qc <- list(method = method)
  }
  matched.sets <- gene.sets[vapply(gene.sets, function(x) length(x$genes) > 0, logical(1))]
  if (length(matched.sets) > 0) {
    history.qc$composition <- lapply(matched.sets, function(x) {
      list(column = x$column, genes = x$genes, source = x$source, pattern = x$pattern)
    })
  }
  p2$history$qc <- history.qc
  if (isTRUE(verbose)) {
    message(.pagoda2_qc_summary(qc))
    composition.summary <- .pagoda2_qc_gene_set_summary(gene.sets)
    if (!is.null(composition.summary)) {
      message(composition.summary)
    }
  }
  invisible(qc)
}

.pagoda2_r6_plot_qc <- function(p2, run.qc = TRUE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for plotQC()")
  }
  p2$syncMetadata()
  if (!all(c("n_molecules", "n_genes", "qc_pass") %in% colnames(p2$cellMeta))) {
    if (!isTRUE(run.qc)) {
      stop("QC metrics are missing; call p2$runQC() first or set run.qc = TRUE")
    }
    p2$runQC(...)
  }
  qc <- p2$resolveCellMeta(
    columns = intersect(
      c("n_molecules", "n_genes", "qc_log_molecules", "qc_log_genes", "qc_pass", "qc_gene_molecule_fitted", "qc_gene_molecule_lower", "qc_gene_molecule_upper"),
      colnames(p2$cellMeta)
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
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.key.size = grid::unit(4.5, "mm"),
      strip.background = ggplot2::element_rect(fill = "grey92", color = "grey55")
    ) +
    ggplot2::labs(x = "log10 molecules per cell", y = "Count / log10 detected genes")
  thresholds <- c(p2$history$qc$min.molecules, p2$history$qc$max.molecules)
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
}

.pagoda2_r6_plot_qc_violin <- function(p2, metrics = c("percent_ribo", "percent_mito"), thresholds = NULL, run.qc = FALSE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for plotQCViolin()")
  }
  normalize_thresholds <- function(thresholds, metrics) {
    if (is.null(thresholds)) {
      return(NULL)
    }
    if (is.data.frame(thresholds)) {
      if (!all(c("metric", "value") %in% colnames(thresholds))) {
        stop("Threshold data.frame must contain `metric` and `value` columns")
      }
      out <- thresholds[, c("metric", "value"), drop = FALSE]
    } else {
      values <- unlist(thresholds, use.names = TRUE)
      if (is.null(names(values)) || any(names(values) == "")) {
        if (length(values) != length(metrics)) {
          stop("Unnamed thresholds must have one value per requested metric")
        }
        names(values) <- metrics
      }
      out <- data.frame(metric = names(values), value = as.numeric(values), stringsAsFactors = FALSE)
    }
    out <- out[out$metric %in% metrics & is.finite(out$value), , drop = FALSE]
    if (nrow(out) == 0) {
      return(NULL)
    }
    out
  }
  metric_label <- function(x) {
    labels <- c(percent_ribo = "Ribosomal", percent_mito = "Mitochondrial")
    out <- labels[x]
    out[is.na(out)] <- x[is.na(out)]
    unname(out)
  }
  p2$syncMetadata()
  metrics <- unique(as.character(metrics))
  if (length(metrics) == 0) {
    stop("`metrics` must contain at least one cell metadata column")
  }
  available <- intersect(metrics, colnames(p2$cellMeta))
  if (length(available) == 0 && isTRUE(run.qc)) {
    p2$runQC(...)
    p2$syncMetadata()
    available <- intersect(metrics, colnames(p2$cellMeta))
  }
  if (length(available) == 0) {
    stop(
      "No requested QC composition metrics are available. ",
      "Call p2$runQC(mt.genes = ..., ribo.genes = ...) or set run.qc = TRUE."
    )
  }
  cols <- available
  has.pass <- "qc_pass" %in% colnames(p2$cellMeta)
  if (has.pass) {
    cols <- c(cols, "qc_pass")
  }
  qc <- p2$resolveCellMeta(cols)
  pass <- NULL
  if (has.pass) {
    pass <- factor(ifelse(as.logical(qc$qc_pass), "pass", "filter"), levels = c("pass", "filter"))
  }
  long <- do.call(rbind, lapply(available, function(metric) {
    data.frame(
      cell = rownames(qc),
      metric = metric,
      metric_label = metric_label(metric),
      value = as.numeric(qc[[metric]]),
      qc_pass = if (has.pass) pass else factor("cells"),
      stringsAsFactors = FALSE
    )
  }))
  long <- long[is.finite(long$value), , drop = FALSE]
  if (nrow(long) == 0) {
    stop("Requested QC composition metrics contain no finite values")
  }
  long$metric_label <- factor(long$metric_label, levels = metric_label(available))
  p <- ggplot2::ggplot(long, ggplot2::aes(x = "", y = value)) +
    ggplot2::geom_violin(fill = "grey92", color = "grey45", linewidth = 0.35, width = 0.85, trim = TRUE)
  if (has.pass) {
    p <- p +
      ggplot2::geom_jitter(
        ggplot2::aes(color = qc_pass),
        width = 0.14,
        height = 0,
        size = 0.25,
        alpha = 0.25
      ) +
      ggplot2::scale_color_manual(
        values = c(pass = "grey35", filter = "firebrick3"),
        name = "QC",
        guide = ggplot2::guide_legend(override.aes = list(size = 2.5, alpha = 1))
      )
  } else {
    p <- p + ggplot2::geom_jitter(width = 0.14, height = 0, size = 0.25, alpha = 0.25, color = "grey30")
  }
  threshold.df <- normalize_thresholds(thresholds, available)
  if (!is.null(threshold.df)) {
    threshold.df$metric_label <- factor(metric_label(threshold.df$metric), levels = levels(long$metric_label))
    p <- p + ggplot2::geom_hline(
      data = threshold.df,
      ggplot2::aes(yintercept = value),
      inherit.aes = FALSE,
      color = "firebrick3",
      linetype = "dashed",
      linewidth = 0.45
    )
  }
  p +
    ggplot2::facet_wrap(~metric_label, ncol = 1, strip.position = "left") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey92", color = "grey55"),
      strip.placement = "outside",
      legend.key.size = grid::unit(4.5, "mm")
    ) +
    ggplot2::labs(x = NULL, y = "Percent of molecules")
}
