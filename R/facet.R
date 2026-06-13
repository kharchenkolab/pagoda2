## Facet abstraction (pagoda2.1 multimodal, Phase 0).
##
## A "facet" is one molecular modality's bundle: raw counts + matrix-view recipes + feature
## metadata + depth/batch + variance state. The default facet (`p2$defaultFacet`, "RNA") is backed
## by the object's top-level fields (`rawCounts`/`matrixViews`/`geneMeta`/`depth`/`batch`/`modelType`
## and `misc$varinfo`/`misc$odgenes`); additional facets are stored in `misc$facetStore[[name]]`.
##
## `Pagoda2Facet` is a lightweight *view*: it stores no data of its own, only a reference to the
## parent Pagoda2 object and the facet name, and delegates every field to the parent's storage. This
## keeps the copy constructor and serialization trivial (all data lives in ordinary public fields),
## and lets the view kernels / materializer operate on a facet's `(rawCounts, matrixViews)` pair
## unchanged.

Pagoda2Facet <- R6::R6Class("Pagoda2Facet",
  lock_objects = FALSE,
  public = list(
    #' @field name Facet name (e.g. "RNA", "ADT", "ATAC").
    name = NULL,
    #' @field parent Owning Pagoda2 object (reference).
    parent = NULL,
    #' @field primary TRUE if this facet is backed by the parent's top-level storage (the default facet).
    primary = FALSE,

    #' @description Create a facet view.
    #' @param name Facet name.
    #' @param parent Owning Pagoda2 object.
    #' @param primary Whether this is the default facet backed by top-level storage.
    initialize = function(name, parent, primary = FALSE) {
      self$name <- name
      self$parent <- parent
      self$primary <- primary
    }
  ),
  private = list(
    get_field = function(field) {
      p <- self$parent
      if (isTRUE(self$primary)) {
        v <- switch(field,
          rawCounts = p$rawCounts,
          matrixViews = p$matrixViews,
          featureMeta = p$geneMeta,
          depth = p$depth,
          batch = p$batch,
          modelType = p$modelType,
          varinfo = p$misc[["varinfo"]],
          odgenes = p$misc[["odgenes"]],
          loadings = p$misc[["loadings"]],
          featureType = p$misc[["featureType"]],
          defaultReduction = p$defaults$reduction,
          backend = "memory",
          bp = NULL,
          stop("unknown facet field: ", field, call. = FALSE)
        )
      } else {
        v <- p$misc$facetStore[[self$name]][[field]]
      }
      if (field == "loadings" && is.null(v)) {
        return(list())
      }
      if (field == "featureType" && is.null(v)) {
        return("gene")
      }
      if (field == "defaultReduction" && is.null(v)) {
        return("PCA")
      }
      if (field == "backend" && is.null(v)) {
        return("memory")
      }
      v
    },
    set_field = function(field, value) {
      p <- self$parent
      if (isTRUE(self$primary)) {
        switch(field,
          rawCounts = {
            p$rawCounts <- value
          },
          matrixViews = {
            p$matrixViews <- value
          },
          featureMeta = {
            p$geneMeta <- value
          },
          depth = {
            p$depth <- value
          },
          batch = {
            p$batch <- value
          },
          modelType = {
            p$modelType <- value
          },
          varinfo = {
            p$misc[["varinfo"]] <- value
          },
          odgenes = {
            p$misc[["odgenes"]] <- value
          },
          loadings = {
            p$misc[["loadings"]] <- value
          },
          featureType = {
            p$misc[["featureType"]] <- value
          },
          defaultReduction = {
            p$defaults$reduction <- value
          },
          stop("unknown facet field: ", field, call. = FALSE)
        )
      } else {
        p$misc$facetStore[[self$name]][[field]] <- value
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field rawCounts This facet's raw count matrix (cells x features).
    rawCounts = function(value) if (missing(value)) private$get_field("rawCounts") else private$set_field("rawCounts", value),
    #' @field matrixViews This facet's named view recipes.
    matrixViews = function(value) if (missing(value)) private$get_field("matrixViews") else private$set_field("matrixViews", value),
    #' @field featureMeta This facet's per-feature metadata table.
    featureMeta = function(value) if (missing(value)) private$get_field("featureMeta") else private$set_field("featureMeta", value),
    #' @field depth Per-cell library size for this facet.
    depth = function(value) if (missing(value)) private$get_field("depth") else private$set_field("depth", value),
    #' @field batch Per-cell batch for this facet.
    batch = function(value) if (missing(value)) private$get_field("batch") else private$set_field("batch", value),
    #' @field modelType Normalization model for this facet ("plain"/"raw"/"clr"/"tfidf").
    modelType = function(value) if (missing(value)) private$get_field("modelType") else private$set_field("modelType", value),
    #' @field varinfo Overdispersion fit for this facet.
    varinfo = function(value) if (missing(value)) private$get_field("varinfo") else private$set_field("varinfo", value),
    #' @field odgenes Overdispersed features for this facet.
    odgenes = function(value) if (missing(value)) private$get_field("odgenes") else private$set_field("odgenes", value),
    #' @field loadings Per-facet (feature-space) loadings, keyed by reduction name.
    loadings = function(value) if (missing(value)) private$get_field("loadings") else private$set_field("loadings", value),
    #' @field featureType Feature kind hint ("gene"/"protein"/"peak").
    featureType = function(value) if (missing(value)) private$get_field("featureType") else private$set_field("featureType", value),
    #' @field defaultReduction Reduction implied by this facet (RNA->"PCA", ATAC->"LSI").
    defaultReduction = function(value) if (missing(value)) private$get_field("defaultReduction") else private$set_field("defaultReduction", value),
    #' @field backend Storage backend ("memory" or "bpcells"); disk-backed facets stream out-of-core.
    backend = function(value) if (missing(value)) private$get_field("backend") else private$set_field("backend", value),
    #' @field bp Disk-backed handle (BPCells IterableMatrix, features x cells) for backend == "bpcells".
    bp = function(value) if (missing(value)) private$get_field("bp") else private$set_field("bp", value)
  )
)

## ---- Pagoda2 facet helpers (called by the R6 methods) ----

.pagoda2_r6_list_facets <- function(p2) {
  c(p2$defaultFacet, setdiff(names(p2$misc$facetStore), p2$defaultFacet))
}

.pagoda2_r6_get_facet <- function(p2, name = NULL) {
  if (is.null(name)) {
    name <- p2$defaultFacet
  }
  if (identical(name, p2$defaultFacet)) {
    return(Pagoda2Facet$new(name, p2, primary = TRUE))
  }
  if (is.null(p2$misc$facetStore[[name]])) {
    stop("no facet named '", name, "' (have: ", paste(.pagoda2_r6_list_facets(p2), collapse = ", "), ")", call. = FALSE)
  }
  Pagoda2Facet$new(name, p2, primary = FALSE)
}

.pagoda2_r6_facets <- function(p2) {
  out <- list()
  out[[p2$defaultFacet]] <- Pagoda2Facet$new(p2$defaultFacet, p2, primary = TRUE)
  store <- p2$misc$facetStore
  for (nm in names(store)) {
    if (identical(nm, p2$defaultFacet)) {
      next
    }
    out[[nm]] <- Pagoda2Facet$new(nm, p2, primary = FALSE)
  }
  out
}

.pagoda2_r6_add_facet <- function(p2, name, countMatrix, modelType = "plain",
                                  featureType = "gene", defaultReduction = "PCA", depth = NULL,
                                  backend = c("memory", "bpcells"), backend.dir = NULL) {
  backend <- match.arg(backend)
  if (identical(name, p2$defaultFacet)) {
    stop("facet '", name, "' is the default facet, backed by top-level storage; use setCountMatrix() instead", call. = FALSE)
  }
  if (!inherits(countMatrix, "dgCMatrix")) {
    if (is.matrix(countMatrix)) {
      countMatrix <- as(Matrix::Matrix(countMatrix, sparse = TRUE), "CsparseMatrix")
    } else {
      stop("countMatrix must be a dgCMatrix or matrix (cells x features)", call. = FALSE)
    }
  }
  if (is.null(rownames(countMatrix)) || is.null(colnames(countMatrix))) {
    stop("countMatrix must have cell row names and feature column names", call. = FALSE)
  }
  if (is.null(depth)) {
    depth <- Matrix::rowSums(countMatrix)
  }
  names(depth) <- rownames(countMatrix)
  if (any(depth == 0)) {
    stop("facet '", name, "' has cells with zero counts over all features", call. = FALSE)
  }
  ## A "plain"/"raw"/"clr"/"tfidf" analysis-view recipe (winsorization off by default for non-RNA facets;
  ## CLR/TF-IDF are materialized by the corresponding `.pagoda2_materialize_view` branches added in 2a/2b).
  analysis.view <- list(
    name = "analysis",
    source = "raw",
    model = modelType,
    depthScale = 1000,
    depth = depth,
    log.scale = TRUE,
    trim = 0,
    batch = NULL,
    batchFactors = NULL,
    winsorCaps = NULL,
    preWinsorDepth = NULL,
    postWinsorDepth = NULL
  )
  bp <- NULL
  stored.raw <- countMatrix
  if (identical(backend, "bpcells")) {
    if (!requireNamespace("BPCells", quietly = TRUE)) {
      stop("backend='bpcells' requires the BPCells package", call. = FALSE)
    }
    if (is.null(backend.dir)) {
      backend.dir <- tempfile(paste0("pagoda2_facet_", name, "_"))
    }
    ## BPCells convention: features x cells on disk. Stream from disk; keep no in-memory rawCounts.
    bp <- BPCells::write_matrix_dir(methods::as(Matrix::t(countMatrix), "IterableMatrix"), dir = backend.dir, compress = FALSE)
    stored.raw <- NULL
  }
  store <- p2$misc$facetStore
  if (is.null(store)) {
    store <- list()
  }
  store[[name]] <- list(
    rawCounts = stored.raw,
    matrixViews = list(analysis = analysis.view),
    featureMeta = data.frame(row.names = colnames(countMatrix)),
    depth = depth,
    batch = NULL,
    modelType = modelType,
    featureType = featureType,
    defaultReduction = defaultReduction,
    varinfo = NULL,
    odgenes = NULL,
    loadings = list(),
    backend = backend,
    bp = bp
  )
  p2$misc$facetStore <- store
  invisible(p2)
}

## Disk-backed (BPCells) per-feature mean/variance for a plain-model facet: stream the depth-normalized
## + log1p view off disk and reduce, with BPCells' sample variance rescaled to the population convention
## (/n) used by the C++ kernel, so a disk-backed facet matches its in-memory twin. (§8.6 out-of-core seam.)
.pagoda2_facet_bpcells_col_mean_var <- function(facet, view) {
  M <- facet$bp # features x cells
  if (!identical(view$model, "plain") && !identical(view$model, "raw")) {
    stop("disk-backed (bpcells) viewColMeanVar currently supports the plain/raw model only", call. = FALSE)
  }
  cells <- colnames(M)
  Y <- M
  if (identical(view$model, "plain")) {
    sf <- as.numeric(view$depthScale) / as.numeric(view$depth[cells])
    Y <- BPCells::multiply_cols(Y, sf)
  }
  if (isTRUE(view$log.scale)) {
    Y <- log1p(Y)
  }
  n <- ncol(M)
  m <- as.numeric(BPCells::rowMeans(Y))
  v <- as.numeric(BPCells::rowVars(Y)) * (n - 1) / n # BPCells uses /(n-1); kernel uses /n
  v[v < 0] <- 0
  data.frame(m = m, v = v, nobs = NA_real_, row.names = rownames(M))
}

## ---- resolution & keying (Phase 1, §4.5.1) ----

## Resolve a facet argument (NULL/name/Pagoda2Facet) to a Pagoda2Facet view.
.pagoda2_r6_resolve_facet <- function(p2, facet = NULL) {
  if (inherits(facet, "Pagoda2Facet")) {
    return(facet)
  }
  if (length(facet) > 1L) {
    stop("`facet` must be a single facet name", call. = FALSE)
  }
  .pagoda2_r6_get_facet(p2, name = facet)
}

## Parse a possibly facet-qualified name ("ADT:CD3" -> facet ADT, item CD3; "MS4A1" -> default facet).
## Splits on the FIRST ":" only, so item names may themselves contain ":".
.pagoda2_parse_qualified <- function(name, default_facet) {
  if (length(name) != 1L || is.na(name)) {
    stop("name must be a single non-NA string", call. = FALSE)
  }
  pos <- regexpr(":", name, fixed = TRUE)
  if (pos > 0L) {
    list(facet = substr(name, 1L, pos - 1L), item = substr(name, pos + 1L, nchar(name)))
  } else {
    list(facet = default_facet, item = name)
  }
}

## Canonical storage key for a per-facet reduction (§4.5.1): bare for the default facet, facet-qualified
## otherwise. `reduction` NULL -> the facet's defaultReduction.
.pagoda2_reduction_key <- function(p2, facet = NULL, reduction = NULL) {
  f <- if (is.null(facet)) p2$defaultFacet else facet
  red <- if (is.null(reduction)) .pagoda2_r6_get_facet(p2, f)$defaultReduction else reduction
  if (identical(f, p2$defaultFacet)) red else paste0(f, ":", red)
}

## No-shadow validator (§4.5.1 rule 2): a joint/integration product name must be distinct from any
## per-facet reduction method name, and must not be facet-qualified (no ":"). This is what makes
## "a per-facet PCA *and* a joint one" unambiguous — the joint simply cannot be called "PCA".
.pagoda2_validate_joint_name <- function(p2, name) {
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("joint product name must be a single non-empty string", call. = FALSE)
  }
  if (grepl(":", name, fixed = TRUE)) {
    stop("joint product name '", name, "' must not contain ':' (reserved for facet qualification)", call. = FALSE)
  }
  facets <- .pagoda2_r6_list_facets(p2)
  dr <- unique(vapply(facets, function(f) .pagoda2_r6_get_facet(p2, f)$defaultReduction, character(1)))
  if (name %in% dr) {
    stop("joint product name '", name, "' collides with a per-facet reduction method name; choose a distinct name (e.g. 'WNN', 'MOFA', 'jointPCA')", call. = FALSE)
  }
  invisible(name)
}
