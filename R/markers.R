## Marker detection and marker result helpers for Pagoda2


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

.pagoda2_marker_selection_preset <- function(selection) {
  if (is.null(selection)) {
    selection <- "balanced"
  }
  if (is.function(selection)) {
    return(list(name = "custom", score = selection, score.name = "score", ordering = c("-score")))
  }
  if (is.list(selection)) {
    if (is.null(selection$name)) {
      selection$name <- "custom"
    }
    if (!is.null(selection$score) && is.null(selection$score.name)) {
      selection$score.name <- "score"
    }
    return(selection)
  }
  if (!is.character(selection) || length(selection) != 1L) {
    stop("`selection` must be a preset name, function, or list")
  }
  selection <- match.arg(selection, c("balanced", "auc", "precision", "effect", "custom"))
  switch(selection,
    balanced = list(
      name = "balanced",
      score.name = "BalancedF1",
      score = function(d) {
        if (!all(c("Precision", "ExpressionFraction") %in% colnames(d))) {
          return(NULL)
        }
        p <- d$Precision
        r <- d$ExpressionFraction
        ifelse(is.finite(p + r) & (p + r) > 0, 2 * p * r / (p + r), NA_real_)
      },
      ordering = c("-BalancedF1", "-AUC", "-M", "-Precision", "-Z")
    ),
    auc = list(
      name = "auc",
      ordering = c("-AUC", "-Z", "-Precision", "-Specificity", "-M")
    ),
    precision = list(
      name = "precision",
      min.expression.fraction = 0.45,
      required.columns = c("Precision", "ExpressionFraction"),
      ordering = c("-Precision", "-ExpressionFraction", "-AUC", "-M", "-Specificity", "-Z")
    ),
    effect = list(
      name = "effect",
      ordering = c("-M", "-Z", "-AUC", "-Precision", "-Specificity")
    ),
    custom = list(
      name = "custom",
      ordering = NULL
    )
  )
}

.pagoda2_filter_marker_table <- function(d, highest.only = TRUE, z.threshold = NULL,
                                         min.expression.fraction = NULL,
                                         min.precision = NULL,
                                         min.specificity = NULL,
                                         min.auc = NULL,
                                         min.m = NULL,
                                         filter = NULL) {
  apply_min <- function(d, column, value) {
    if (is.null(value)) {
      return(d)
    }
    if (!column %in% colnames(d)) {
      stop("Marker selection requires column `", column, "`")
    }
    d[is.finite(d[[column]]) & d[[column]] >= value, , drop = FALSE]
  }
  if (!is.null(z.threshold) && "Z" %in% colnames(d)) {
    d <- d[is.finite(d$Z) & d$Z >= z.threshold, , drop = FALSE]
  }
  if (highest.only && "highest" %in% colnames(d)) {
    d <- d[d$highest %in% TRUE, , drop = FALSE]
  }
  d <- apply_min(d, "ExpressionFraction", min.expression.fraction)
  d <- apply_min(d, "Precision", min.precision)
  d <- apply_min(d, "Specificity", min.specificity)
  d <- apply_min(d, "AUC", min.auc)
  d <- apply_min(d, "M", min.m)
  if (!is.null(filter)) {
    keep <- filter(d)
    if (!is.logical(keep) || length(keep) != nrow(d)) {
      stop("Custom marker selection filter must return one logical value per marker row")
    }
    d <- d[keep %in% TRUE, , drop = FALSE]
  }
  d
}

# Shared marker display selection. Plotting methods call this instead of each
# having their own ranking rules, so dotplots and heatmaps show the same genes
# for the same `selection` preset.
.pagoda2_select_marker_genes <- function(tables, n.genes.per.group = 5, genes = NULL,
                                         z.threshold = NULL, highest.only = TRUE,
                                         ordering = NULL, selection = "balanced",
                                         min.expression.fraction = NULL,
                                         min.precision = NULL,
                                         min.specificity = NULL,
                                         min.auc = NULL,
                                         min.m = NULL,
                                         remove.duplicates = TRUE,
                                         return.tables = FALSE) {
  if (!is.null(genes)) {
    genes <- unique(as.character(genes))
    out <- list(
      genes = genes,
      groups = stats::setNames(rep("selected", length(genes)), genes),
      tables = list(selected = data.frame(Gene = genes, stringsAsFactors = FALSE)),
      selection = "explicit"
    )
    return(out)
  }
  if (is.null(tables) || length(tables) == 0) {
    stop("Marker result does not contain marker tables")
  }
  preset <- .pagoda2_marker_selection_preset(selection)
  if (!is.null(preset$required.columns)) {
    missing.required <- unique(unlist(lapply(tables, function(d) {
      if (is.null(d) || nrow(d) == 0) {
        return(character())
      }
      setdiff(preset$required.columns, colnames(d))
    }), use.names = FALSE))
    if (length(missing.required) > 0) {
      stop(
        "Marker selection `", preset$name, "` requires marker table column(s): ",
        paste(missing.required, collapse = ", "),
        ". Re-run markers with specificity metrics enabled."
      )
    }
  }
  if (is.null(ordering)) {
    ordering <- preset$ordering
  }
  if (is.null(ordering)) {
    ordering <- c("-AUC", "-Z", "-Precision", "-Specificity", "-M")
  }
  if (is.null(min.expression.fraction) && !is.null(preset$min.expression.fraction)) {
    min.expression.fraction <- preset$min.expression.fraction
  }
  selected <- lapply(names(tables), function(group) {
    d <- tables[[group]]
    if (is.null(d) || nrow(d) == 0) {
      return(character())
    }
    if (!is.null(preset$score)) {
      score <- preset$score(d)
      if (!is.null(score)) {
        if (!is.numeric(score) || length(score) != nrow(d)) {
          stop("Custom marker selection score must return one numeric value per marker row")
        }
        d[[preset$score.name]] <- score
      }
    }
    d <- .pagoda2_filter_marker_table(
      d,
      highest.only = highest.only,
      z.threshold = z.threshold,
      min.expression.fraction = min.expression.fraction,
      min.precision = min.precision,
      min.specificity = min.specificity,
      min.auc = min.auc,
      min.m = min.m,
      filter = preset$filter
    )
    d <- .pagoda2_order_marker_table(d, ordering = ordering)
    if (!is.null(n.genes.per.group)) {
      d <- utils::head(d, n.genes.per.group)
    }
    if (nrow(d) == 0) {
      return(character())
    }
    genes <- if ("Gene" %in% colnames(d)) as.character(d$Gene) else rownames(d)
    stats::setNames(genes, genes)
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
  out <- list(
    genes = genes,
    groups = stats::setNames(gene.groups, genes),
    tables = selected,
    selection = preset$name,
    ordering = ordering
  )
  if (isTRUE(return.tables)) {
    return(out)
  }
  out
}


.pagoda2_r6_run_markers <- function(p2, grouping = NULL, groups = NULL, name = NULL, type = "counts", facet = NULL, z.threshold = 3,
                                    upregulated.only = TRUE, verbose = FALSE, append.specificity.metrics = TRUE,
                                    append.auc = TRUE, genes = NULL, use.analysis.genes = TRUE, n.cores = NULL, threads = NULL) {
  fkey <- p2$resolveFacet(facet)$name
  resolved.grouping <- grouping
  if (is.null(resolved.grouping) && is.null(groups)) {
    resolved.grouping <- p2$defaultGrouping
  }
  cols <- p2$resolveGrouping(grouping = grouping, groups = groups, allow.missing = TRUE)
  tp <- .pagoda2_resolve_threads(
    p2,
    n.cores = n.cores,
    threads = threads,
    method = "markers",
    tasks = length(levels(droplevels(as.factor(cols[!is.na(cols)]))))
  )
  if (is.null(name)) {
    name <- if (!is.null(resolved.grouping)) resolved.grouping else "customGrouping"
  }
  ds <- p2$getDifferentialGenes(
    type = type,
    facet = facet,
    groups = cols,
    name = name,
    z.threshold = z.threshold,
    upregulated.only = upregulated.only,
    verbose = verbose,
    append.specificity.metrics = append.specificity.metrics,
    append.auc = append.auc,
    genes = genes,
    use.analysis.genes = use.analysis.genes,
    n.cores = tp$r.workers,
    .legacy.warn = FALSE
  )
  params <- list(
    z.threshold = z.threshold,
    upregulated.only = upregulated.only,
    append.specificity.metrics = append.specificity.metrics,
    append.auc = append.auc,
    genes = genes,
    use.analysis.genes = use.analysis.genes,
    threads = tp
  )
  result <- .pagoda2_marker_result(
    name = name,
    type = fkey,
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
  p2$diffgenes[[fkey]][[name]] <- ds
  if (is.null(p2$markerResults[[fkey]])) {
    p2$markerResults[[fkey]] <- list()
  }
  p2$markerResults[[fkey]][[name]] <- result
  if (is.null(p2$history$markers)) {
    p2$history$markers <- list()
  }
  p2$history$markers[[name]] <- meta
  invisible(ds)
}
