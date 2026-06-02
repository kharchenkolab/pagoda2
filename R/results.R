## Result registry implementation for Pagoda2

.pagoda2_r6_list_reductions <- function(p2) {
  nms <- names(p2$reductions)
  if (length(nms) == 0) {
    return(data.frame(name = character(), n.cells = numeric(), n.dims = numeric(), stringsAsFactors = FALSE))
  }
  data.frame(
    name = nms,
    n.cells = vapply(p2$reductions[nms], nrow, numeric(1)),
    n.dims = vapply(p2$reductions[nms], ncol, numeric(1)),
    stringsAsFactors = FALSE
  )
}

.pagoda2_r6_list_graphs <- function(p2) {
  nms <- names(p2$graphs)
  if (length(nms) == 0) {
    return(data.frame(name = character(), n.nodes = numeric(), n.edges = numeric(), weighted = logical(), stringsAsFactors = FALSE))
  }
  data.frame(
    name = nms,
    n.nodes = vapply(p2$graphs[nms], igraph::vcount, numeric(1)),
    n.edges = vapply(p2$graphs[nms], igraph::ecount, numeric(1)),
    weighted = vapply(p2$graphs[nms], igraph::is_weighted, logical(1)),
    stringsAsFactors = FALSE
  )
}

.pagoda2_r6_list_embeddings <- function(p2) {
  reductions <- names(p2$embeddings)
  if (length(reductions) == 0) {
    return(data.frame(reduction = character(), embedding = character(), n.cells = numeric(), n.dims = numeric(), stringsAsFactors = FALSE))
  }
  rows <- do.call(rbind, lapply(reductions, function(reduction) {
    embeddings <- names(p2$embeddings[[reduction]])
    if (length(embeddings) == 0) {
      return(NULL)
    }
    data.frame(
      reduction = reduction,
      embedding = embeddings,
      n.cells = vapply(p2$embeddings[[reduction]][embeddings], nrow, numeric(1)),
      n.dims = vapply(p2$embeddings[[reduction]][embeddings], ncol, numeric(1)),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(rows)) {
    return(data.frame(reduction = character(), embedding = character(), n.cells = numeric(), n.dims = numeric(), stringsAsFactors = FALSE))
  }
  rownames(rows) <- NULL
  rows
}

.pagoda2_r6_list_markers <- function(p2) {
  types <- names(p2$diffgenes)
  if (length(types) == 0) {
    return(data.frame(type = character(), name = character(), n.groups = integer(), grouping = character(), schema = character(), stringsAsFactors = FALSE))
  }
  rows <- do.call(rbind, lapply(types, function(type) {
    markers <- names(p2$diffgenes[[type]])
    if (length(markers) == 0) {
      return(NULL)
    }
    data.frame(
      type = type,
      name = markers,
      n.groups = vapply(p2$diffgenes[[type]][markers], length, integer(1)),
      grouping = vapply(p2$diffgenes[[type]][markers], function(x) {
        meta <- attr(x, "pagoda2.marker")
        if (is.null(meta$grouping)) NA_character_ else meta$grouping
      }, character(1)),
      schema = vapply(p2$diffgenes[[type]][markers], function(x) {
        meta <- attr(x, "pagoda2.marker")
        if (is.null(meta$schema)) "legacy" else meta$schema
      }, character(1)),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(rows)) {
    return(data.frame(type = character(), name = character(), n.groups = integer(), grouping = character(), schema = character(), stringsAsFactors = FALSE))
  }
  rownames(rows) <- NULL
  rows
}

.pagoda2_r6_list_results <- function(p2) {
  list(
    reductions = p2$listReductions(),
    graphs = p2$listGraphs(),
    embeddings = p2$listEmbeddings(),
    groupings = p2$listGroupings(),
    markers = p2$listMarkers()
  )
}

.pagoda2_r6_as <- function(p2, format = c("list", "sce", "seurat"), ...) {
  pagoda2As(p2, format = format, ...)
}

.pagoda2_r6_export <- function(p2, path, format = NULL, ...) {
  pagoda2Export(p2, path = path, format = format, ...)
}

.pagoda2_r6_resolve_reduction <- function(p2, reduction = NULL) {
  if (is.null(reduction)) {
    reduction <- p2$defaults$reduction
  }
  if (!is.character(reduction) || length(reduction) != 1) {
    stop("`reduction` must be a single reduction name")
  }
  if (is.null(p2$reductions[[reduction]])) {
    stop("Unknown reduction `", reduction, "`. Available reductions: ", paste(names(p2$reductions), collapse = ", "))
  }
  reduction
}

.pagoda2_r6_resolve_graph <- function(p2, graph = NULL, reduction = NULL) {
  if (is.null(graph)) {
    graph <- reduction
  }
  if (is.null(graph)) {
    graph <- p2$defaults$graph
  }
  if (!is.character(graph) || length(graph) != 1) {
    stop("`graph` must be a single graph name")
  }
  if (is.null(p2$graphs[[graph]])) {
    stop("Unknown graph `", graph, "`. Available graphs: ", paste(names(p2$graphs), collapse = ", "))
  }
  graph
}

.pagoda2_r6_resolve_embedding <- function(p2, reduction = NULL, embedding = NULL) {
  if (is.null(reduction)) {
    reduction <- p2$defaults$reduction
  }
  if (is.null(embedding)) {
    embedding <- p2$defaults$embedding
  }
  if (!is.character(reduction) || length(reduction) != 1) {
    stop("`reduction` must be a single embedding namespace")
  }
  if (!is.character(embedding) || length(embedding) != 1) {
    stop("`embedding` must be a single embedding name")
  }
  if (is.null(p2$embeddings[[reduction]]) || is.null(p2$embeddings[[reduction]][[embedding]])) {
    available <- p2$listEmbeddings()
    available.text <- if (nrow(available) == 0) "" else paste(paste(available$reduction, available$embedding, sep = "/"), collapse = ", ")
    stop("Unknown embedding `", reduction, "/", embedding, "`. Available embeddings: ", available.text)
  }
  list(reduction = reduction, embedding = embedding, value = p2$embeddings[[reduction]][[embedding]])
}

.pagoda2_r6_resolve_markers <- function(p2, markers = NULL, type = "counts") {
  if (is.null(markers)) {
    markers <- p2$defaultGrouping
  }
  if (is.null(markers)) {
    stop("No marker result supplied and no defaultGrouping is set")
  }
  if (!is.character(markers) || length(markers) != 1) {
    stop("`markers` must be a single marker result name")
  }
  if (is.null(p2$diffgenes[[type]]) || is.null(p2$diffgenes[[type]][[markers]])) {
    available <- p2$listMarkers()
    available <- available$name[available$type == type]
    stop("Unknown marker result `", markers, "`. Available markers: ", paste(available, collapse = ", "))
  }
  value <- p2$diffgenes[[type]][[markers]]
  result <- NULL
  if (!is.null(p2$markerResults[[type]]) && !is.null(p2$markerResults[[type]][[markers]])) {
    result <- p2$markerResults[[type]][[markers]]
  }
  metadata <- attr(value, "pagoda2.marker")
  if (is.null(result) && !is.null(metadata)) {
    result <- metadata
    result$tables <- value
    class(result) <- c("pagoda2_marker_result", "list")
  }
  list(type = type, name = markers, value = value, tables = value, result = result, metadata = metadata)
}

.pagoda2_r6_get_marker_result <- function(p2, markers = NULL, type = "counts") {
  resolved <- p2$resolveMarkers(markers = markers, type = type)
  if (is.null(resolved$result)) {
    stop("Marker result `", resolved$name, "` does not have pagoda2.1 marker metadata")
  }
  resolved$result
}

.pagoda2_r6_get_top_markers <- function(p2, markers = NULL, type = "counts", genes = NULL, n.genes.per.group = 5,
                                        selection = "balanced", z.threshold = 3, highest.only = TRUE,
                                        ordering = NULL, min.expression.fraction = NULL,
                                        min.precision = NULL, min.specificity = NULL,
                                        min.auc = NULL, min.m = NULL,
                                        remove.duplicates = TRUE, as.data.frame = TRUE) {
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
    remove.duplicates = remove.duplicates,
    return.tables = TRUE
  )
  if (!isTRUE(as.data.frame)) {
    return(selected)
  }
  group <- unname(selected$groups)
  data.frame(
    group = group,
    rank = stats::ave(seq_along(group), group, FUN = seq_along),
    gene = selected$genes,
    selection = selected$selection,
    stringsAsFactors = FALSE
  )
}
