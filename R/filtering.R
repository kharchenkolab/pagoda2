## Filtering implementation for Pagoda2


.pagoda2_analysis_genes <- function(p2, allow.empty = FALSE, facet = NULL) {
  f <- p2$resolveFacet(facet)
  if (isTRUE(f$primary)) {
    genes <- .pagoda2_axis_names(p2, "gene")
    if (is.null(p2$geneMeta) || !"analysis_pass" %in% colnames(p2$geneMeta)) {
      return(genes)
    }
    meta <- p2$resolveGeneMeta("analysis_pass")
    pass <- as.logical(meta$analysis_pass)
    pass[is.na(pass)] <- FALSE
    selected <- rownames(meta)[pass]
    if (length(selected) == 0L && !isTRUE(allow.empty)) {
      stop("No genes pass the current analysis gene mask")
    }
    return(selected)
  }
  ## non-default facet: analysis mask (if any) lives in the facet's featureMeta
  genes <- .pagoda2_axis_names(p2, "gene", facet = facet)
  fm <- f$featureMeta
  if (is.null(fm) || !"analysis_pass" %in% colnames(fm)) {
    return(genes)
  }
  present <- intersect(genes, rownames(fm))
  pv <- as.logical(fm[present, "analysis_pass"])
  pv[is.na(pv)] <- FALSE
  selected <- present[pv]
  if (length(selected) == 0L && !isTRUE(allow.empty)) {
    stop("No features pass the analysis mask for facet `", f$name, "`")
  }
  selected
}

.pagoda2_filter_data_complete <- function(p2, pass.column = "qc_pass", gene.pass.column = "analysis_pass") {
  p2$syncMetadata()
  if (!pass.column %in% colnames(p2$cellMeta)) {
    return(FALSE)
  }
  cell.pass <- p2$resolveCellMeta(pass.column)
  keep <- as.logical(cell.pass[[pass.column]])
  keep[is.na(keep)] <- FALSE
  if (!all(keep)) {
    return(FALSE)
  }
  gene.pass.column %in% colnames(p2$geneMeta)
}

.pagoda2_has_downstream_results <- function(p2) {
  length(p2$reductions) > 0L ||
    length(p2$graphs) > 0L ||
    length(p2$embeddings) > 0L ||
    length(p2$diffgenes) > 0L ||
    length(p2$markerResults) > 0L ||
    length(p2$clusterings) > 0L
}

.pagoda2_filter_analysis_view_cells <- function(view, cells) {
  for (nm in c("depth", "batch", "preWinsorDepth", "postWinsorDepth")) {
    if (!is.null(view[[nm]])) {
      view[[nm]] <- view[[nm]][cells]
      if (is.factor(view[[nm]])) {
        view[[nm]] <- droplevels(view[[nm]])
      }
    }
  }
  view
}


.pagoda2_r6_filter_cells <- function(p2, cells = NULL, pass.column = "qc_pass", run.qc = TRUE, force = FALSE, verbose = FALSE, ...) {
  p2$syncMetadata()
  raw <- p2$getRawCounts()
  if (is.null(cells)) {
    if (!pass.column %in% colnames(p2$cellMeta)) {
      if (!isTRUE(run.qc)) {
        stop("Cell metadata column `", pass.column, "` is missing; call p2$runQC() first or set run.qc = TRUE")
      }
      if (isTRUE(verbose)) {
        message("QC metrics not found; running runQC() with default settings.")
      }
      p2$runQC(verbose = verbose, ...)
    }
    qc <- p2$resolveCellMeta(pass.column)
    keep <- qc[[pass.column]]
    keep[is.na(keep)] <- FALSE
    cells <- rownames(qc)[as.logical(keep)]
  } else {
    cells <- rownames(raw)[.pagoda2_axis_selection_index(cells, rownames(raw), what = "cell(s)")]
  }
  if (length(cells) < 3L) {
    stop("Filtering would leave fewer than 3 cells")
  }
  if (identical(cells, rownames(raw))) {
    return(invisible(p2))
  }
  if (.pagoda2_has_downstream_results(p2) && !isTRUE(force)) {
    stop(
      "Filtering cells would invalidate existing reductions, graphs, embeddings, clusterings, or markers. ",
      "Create a fresh object or call filterCells(force = TRUE)."
    )
  }
  removed <- setdiff(rownames(raw), cells)
  old.clusterings <- names(p2$clusterings)
  p2$rawCounts <- raw[cells, , drop = FALSE]
  p2$misc[["rawCounts"]] <- p2$rawCounts
  if (!is.null(p2$depth)) {
    p2$depth <- p2$depth[cells]
  }
  if (!is.null(p2$batch)) {
    p2$batch <- droplevels(p2$batch[cells])
  }
  if (!is.null(p2$matrixViews$analysis)) {
    p2$matrixViews$analysis <- .pagoda2_filter_analysis_view_cells(p2$matrixViews$analysis, cells)
  }
  p2$reductions <- list()
  p2$graphs <- list()
  p2$embeddings <- list()
  p2$diffgenes <- list()
  p2$markerResults <- list()
  p2$clusterings <- list()
  p2$clusters <- list()
  p2$genegraphs <- list()
  p2$misc[["varinfo"]] <- NULL
  p2$misc[["odgenes"]] <- NULL
  p2$misc[["rescaled.mat"]] <- NULL
  if (!is.null(p2$defaultGrouping) && p2$defaultGrouping %in% old.clusterings) {
    p2$defaultGrouping <- NULL
  }
  if (is.null(p2$history$filters)) {
    p2$history$filters <- list()
  }
  p2$history$filters[[length(p2$history$filters) + 1L]] <- list(
    method = if (is.null(pass.column)) "explicit" else pass.column,
    kept = length(cells),
    removed = length(removed),
    removed.cells = removed,
    time = Sys.time()
  )
  invisible(p2)
}

.pagoda2_r6_filter_data <- function(p2, cells = TRUE, genes = TRUE, pass.column = "qc_pass",
                                    min.molecules = 500, max.molecules = 5e4,
                                    min.cells.per.gene = 5, min.molecules.per.gene = 0,
                                    keep.genes = NULL,
                                    force = FALSE, overwrite = FALSE, verbose = FALSE, ...) {
  explicit.min.molecules <- !missing(min.molecules)
  explicit.max.molecules <- !missing(max.molecules)
  explicit.min.cells.per.gene <- !missing(min.cells.per.gene)
  explicit.min.molecules.per.gene <- !missing(min.molecules.per.gene)
  explicit.keep.genes <- !missing(keep.genes)
  qc.extra <- list(...)
  min.molecules <- .pagoda2_filter_default(p2, "min.molecules", min.molecules, explicit.min.molecules)
  max.molecules <- .pagoda2_filter_default(p2, "max.molecules", max.molecules, explicit.max.molecules)
  min.cells.per.gene <- .pagoda2_filter_default(p2, "min.cells.per.gene", min.cells.per.gene, explicit.min.cells.per.gene)
  min.molecules.per.gene <- .pagoda2_filter_default(p2, "min.molecules.per.gene", min.molecules.per.gene, explicit.min.molecules.per.gene)
  keep.genes <- .pagoda2_filter_default(p2, "keep.genes", keep.genes, explicit.keep.genes)
  p2$syncMetadata()
  raw <- p2$getRawCounts()
  downstream <- .pagoda2_has_downstream_results(p2)
  n.cells.before <- nrow(raw)
  n.genes.before <- ncol(raw)
  removed.cells <- character()
  explicit.qc <- explicit.min.molecules || explicit.max.molecules ||
    any(names(qc.extra) %in% c(
      "method", "matrix", "p.level", "mt.genes", "ribo.genes",
      "mt.pattern", "ribo.pattern", "infer.qc.genes"
    ))

  if (isTRUE(cells)) {
    if (isTRUE(overwrite) || explicit.qc || !pass.column %in% colnames(p2$cellMeta)) {
      do.call(
        p2$runQC,
        c(
          list(
            overwrite = isTRUE(overwrite) || explicit.qc,
            min.molecules = min.molecules,
            max.molecules = max.molecules,
            verbose = verbose
          ),
          qc.extra
        )
      )
    } else if (isTRUE(verbose)) {
      qc <- p2$getCellMeta(intersect(c("n_molecules", "n_genes", pass.column), colnames(p2$cellMeta)))
      message(.pagoda2_qc_summary(qc))
    }
    qc <- p2$resolveCellMeta(pass.column)
    keep <- as.logical(qc[[pass.column]])
    keep[is.na(keep)] <- FALSE
    target.cells <- rownames(qc)[keep]
  } else if (identical(cells, FALSE)) {
    target.cells <- rownames(raw)
  } else {
    target.cells <- rownames(raw)[.pagoda2_axis_selection_index(cells, rownames(raw), what = "cell(s)")]
  }

  if (length(target.cells) < 3L) {
    stop("Filtering would leave fewer than 3 cells")
  }
  if (!identical(target.cells, rownames(raw))) {
    if (downstream && !isTRUE(force)) {
      stop(
        "Filtering cells would invalidate existing downstream results. ",
        "Call filterData(force = TRUE) or start from a fresh object."
      )
    }
    removed.cells <- setdiff(rownames(raw), target.cells)
    p2$filterCells(cells = target.cells, force = force, verbose = verbose)
    raw <- p2$getRawCounts()
    downstream <- .pagoda2_has_downstream_results(p2)
  }

  gene.qc <- NULL
  if (isTRUE(genes)) {
    gene.qc <- .pagoda2_gene_qc(
      raw,
      min.cells = min.cells.per.gene,
      min.molecules = min.molecules.per.gene,
      keep.genes = keep.genes
    )
  } else if (identical(genes, FALSE)) {
    gene.qc <- NULL
  } else {
    selected.genes <- colnames(raw)[.pagoda2_axis_selection_index(genes, colnames(raw), what = "gene(s)")]
    gene.qc <- .pagoda2_gene_qc(raw, min.cells = 0, min.molecules = 0)
    gene.qc$analysis_pass <- rownames(gene.qc) %in% selected.genes
    attr(gene.qc, "pagoda2.gene.qc") <- list(
      min.cells = NA_real_,
      min.molecules = NA_real_,
      explicit.genes = selected.genes
    )
  }

  if (!is.null(gene.qc)) {
    current.pass <- NULL
    if ("analysis_pass" %in% colnames(p2$geneMeta)) {
      current.meta <- p2$resolveGeneMeta("analysis_pass")
      current.pass <- current.meta$analysis_pass
      names(current.pass) <- rownames(current.meta)
    }
    pass.changed <- isTRUE(overwrite) ||
      is.null(current.pass) ||
      !identical(unname(as.logical(current.pass[rownames(gene.qc)])), unname(as.logical(gene.qc$analysis_pass)))
    if (downstream && pass.changed && !isTRUE(force)) {
      stop(
        "Changing the analysis gene mask would invalidate existing downstream results. ",
        "Call filterData(force = TRUE) or start from a fresh object."
      )
    }
    if (pass.changed || !all(c("n_cells_detected", "n_molecules", "analysis_pass") %in% colnames(p2$geneMeta))) {
      p2$setGeneMeta(gene.qc, overwrite = TRUE)
      p2$history$gene.qc <- attr(gene.qc, "pagoda2.gene.qc")
      p2$misc[["varinfo"]] <- NULL
      p2$misc[["odgenes"]] <- NULL
      p2$misc[["rescaled.mat"]] <- NULL
    }
    if (isTRUE(verbose)) {
      message(.pagoda2_gene_qc_summary(gene.qc))
    }
  }

  if (is.null(p2$history$filterData)) {
    p2$history$filterData <- list()
  }
  p2$history$filterData[[length(p2$history$filterData) + 1L]] <- list(
    cells = list(
      requested = cells,
      before = n.cells.before,
      after = nrow(p2$getRawCounts()),
      removed = length(removed.cells),
      removed.cells = removed.cells
    ),
    genes = list(
      requested = genes,
      before = n.genes.before,
      after = ncol(p2$getRawCounts()),
      analysis.pass = if (is.null(gene.qc)) NA_integer_ else sum(as.logical(gene.qc$analysis_pass), na.rm = TRUE)
    ),
    parameters = list(
      min.molecules = min.molecules,
      max.molecules = max.molecules,
      min.cells.per.gene = min.cells.per.gene,
      min.molecules.per.gene = min.molecules.per.gene,
      keep.genes = keep.genes
    ),
    time = Sys.time()
  )
  invisible(p2)
}
