## Embedding plotting implementation for Pagoda2

.pagoda2_r6_plot_embedding <- function(p2, type = NULL, embeddingType = NULL, reduction = NULL, embedding = NULL, clusterType = NULL,
                                       groups = NULL, grouping = NULL, colors = NULL, gene = NULL, plot.theme = NULL, .legacy.warn = TRUE, ...) {
  dots <- list(...)

  if (!is.null(reduction)) {
    type <- reduction
  }
  if (!is.null(embedding)) {
    embeddingType <- embedding
  }
  using.legacy.selector <- (!missing(type) && is.null(reduction)) ||
    (!missing(embeddingType) && is.null(embedding)) ||
    !is.null(clusterType)
  if (.legacy.warn && using.legacy.selector) {
    warning(
      "Legacy plotEmbedding selectors `type`, `embeddingType`, and `clusterType` are deprecated and will be removed in the next pagoda2 version. ",
      "Use `p2$plotEmbedding(reduction = ..., embedding = ..., grouping = ...)` instead.",
      call. = FALSE
    )
  }

  if (is.null(type)) {
    if (!is.null(p2$defaults$reduction) && p2$defaults$reduction %in% names(p2$embeddings)) {
      type <- p2$defaults$reduction
    } else if ("counts" %in% names(p2$embeddings)) {
      type <- "counts"
    } else if (length(p2$embeddings) > 0) {
      # type <- names(p2$embeddings)[1]
      ## Use the last-generated embedding
      if (.legacy.warn) {
        warning(
          "Implicit latest-embedding namespace selection is deprecated and will be removed in the next pagoda2 version. ",
          "Use `p2$plotEmbedding(reduction = ..., embedding = ...)` instead.",
          call. = FALSE
        )
      }
      type <- names(p2$embeddings[length(p2$embeddings)])
    } else {
      stop("First, generate an embedding")
    }
  }

  if (is.null(p2$embeddings[[type]])) {
    stop("First, generate embeddings for type ", type)
  }

  if (is.null(embeddingType)) {
    if (!is.null(p2$defaults$embedding) && !is.null(p2$embeddings[[type]][[p2$defaults$embedding]])) {
      embeddingType <- p2$defaults$embedding
      emb <- p2$embeddings[[type]][[embeddingType]]
    } else {
      ## take the most recently generated embedding
      if (.legacy.warn) {
        warning(
          "Implicit latest embedding selection is deprecated and will be removed in the next pagoda2 version. ",
          "Use `p2$plotEmbedding(embedding = ...)` instead.",
          call. = FALSE
        )
      }
      emb <- p2$embeddings[[type]][[length(p2$embeddings[[type]])]]
    }
  } else {
    ## check embeddingType exists
    if (is.null(p2$embeddings[[type]][[embeddingType]])) {
      stop("Embedding does not exist for embeddingType ", embeddingType)
    }
    emb <- p2$embeddings[[type]][[embeddingType]]
  }

  if (!is.null(gene)) {
    if (!(gene %in% .pagoda2_axis_names(p2, "gene"))) {
      stop("Gene '", gene, "' isn't presented in the count matrix")
    }
    colors <- p2$getExpressionBlock(genes = gene)[, gene]
  }

  if (!is.null(grouping) && !is.null(clusterType)) {
    stop("Specify only one of `grouping` or `clusterType`")
  }

  grouping.palette.name <- NULL
  if (is.null(colors) && (!is.null(grouping) || !is.null(groups) || (is.null(clusterType) && !is.null(p2$defaultGrouping)))) {
    if (!is.null(grouping) && is.character(grouping) && length(grouping) == 1L && grouping %in% colnames(p2$cellMeta)) {
      grouping.palette.name <- grouping
    } else if (is.character(groups) && length(groups) == 1L && is.null(names(groups)) && groups %in% colnames(p2$cellMeta)) {
      grouping.palette.name <- groups
    } else if (is.null(grouping) && is.null(groups) && is.null(clusterType) && !is.null(p2$defaultGrouping)) {
      grouping.palette.name <- p2$defaultGrouping
    }
    groups <- p2$resolveGrouping(grouping = grouping, groups = groups, cells = rownames(emb), allow.missing = TRUE)
  }

  if (is.null(colors) && is.null(groups)) {
    # look up the clustering based on a specified type
    if (is.null(clusterType)) {
      # groups <- p2$clusters[[type]][[1]]
      ## Take last-genereated clustering
      if (.legacy.warn) {
        warning(
          "Implicit latest clustering selection is deprecated and will be removed in the next pagoda2 version. ",
          "Use `p2$plotEmbedding(grouping = ...)` or set `p2$defaultGrouping` instead.",
          call. = FALSE
        )
      }
      groups <- p2$clusters[[type]][[length(p2$clusters[[type]])]]
      if (is.null(groups)) {
        stop(paste("Please generate clusters for", type, "first"))
      }
    } else {
      groups <- p2$clusters[[type]][[clusterType]]
      if (is.null(groups)) {
        stop("Clustering ", clusterType, " for type ", type, " doesn't exist")
      }
    }
  }

  if (is.null(colors) && !is.null(groups) && is.null(dots$palette) && !is.null(grouping.palette.name)) {
    dots$palette <- p2$resolveFactorColors(
      axis = "cell",
      name = grouping.palette.name,
      values = groups,
      store = FALSE
    )
  }

  do.call(
    sccore::embeddingPlot,
    c(list(object = emb, groups = groups, colors = colors, plot.theme = .pagoda2_plot_theme(p2, plot.theme = plot.theme)), dots)
  )
}
