#' @keywords internal
.pagoda2_read_mtx <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    con <- gzcon(file(path, open = "rb"))
    on.exit(close(con), add = TRUE)
    return(as(Matrix::readMM(con), "CsparseMatrix"))
  }
  as(Matrix::readMM(path), "CsparseMatrix")
}

#' @keywords internal
.pagoda2_read_tsv <- function(path, header = FALSE) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(as.data.frame(data.table::fread(path, header = header)))
  }
  utils::read.delim(path, header = header, stringsAsFactors = FALSE)
}

#' @keywords internal
.pagoda2_10x_role <- function(path) {
  b <- basename(path)
  if (grepl("\\.mtx(\\.gz)?$", b, ignore.case = TRUE) && grepl("matrix", b, ignore.case = TRUE)) {
    return("matrix")
  }
  if (grepl("\\.(tsv|txt)(\\.gz)?$", b, ignore.case = TRUE) && grepl("barcodes", b, ignore.case = TRUE)) {
    return("barcodes")
  }
  if (grepl("\\.(tsv|txt)(\\.gz)?$", b, ignore.case = TRUE) && grepl("features", b, ignore.case = TRUE)) {
    return("features")
  }
  if (grepl("\\.(tsv|txt)(\\.gz)?$", b, ignore.case = TRUE) && grepl("genes", b, ignore.case = TRUE)) {
    return("genes")
  }
  NA_character_
}

#' @keywords internal
.pagoda2_10x_prefix <- function(path, role) {
  b <- basename(path)
  b <- sub("\\.gz$", "", b, ignore.case = TRUE)
  b <- sub("\\.(mtx|tsv|txt)$", "", b, ignore.case = TRUE)
  prefix <- sub(paste0("[-_.]?", role, ".*$"), "", b, ignore.case = TRUE)
  prefix <- sub("[-_.]+$", "", prefix)
  prefix
}

#' @keywords internal
.pagoda2_detect_10x_triplet <- function(path, version = c("auto", "V3", "V2"), sample.pattern = NULL) {
  version <- match.arg(version)
  files <- list.files(path, full.names = TRUE, recursive = FALSE)
  if (!is.null(sample.pattern)) {
    files <- files[grepl(sample.pattern, basename(files))]
  }
  roles <- vapply(files, .pagoda2_10x_role, character(1))
  files <- files[!is.na(roles)]
  roles <- roles[!is.na(roles)]
  if (length(files) == 0) {
    stop("No 10x matrix, barcode, feature, or gene files found in `", path, "`")
  }
  prefixes <- mapply(.pagoda2_10x_prefix, files, roles, USE.NAMES = FALSE)
  prefixes[!nzchar(prefixes)] <- "."
  candidates <- split(seq_along(files), prefixes)
  valid <- lapply(names(candidates), function(prefix) {
    ii <- candidates[[prefix]]
    r <- roles[ii]
    has.features <- any(r == "features")
    has.genes <- any(r == "genes")
    if (!any(r == "matrix") || !any(r == "barcodes") || !(has.features || has.genes)) {
      return(NULL)
    }
    feature.role <- if (version == "V2") {
      "genes"
    } else if (version == "V3") {
      "features"
    } else if (has.features) {
      "features"
    } else {
      "genes"
    }
    if (!any(r == feature.role)) {
      return(NULL)
    }
    pick <- function(role) {
      values <- files[ii][r == role]
      if (length(values) != 1) {
        stop("Expected one ", role, " file for 10x prefix `", prefix, "`, found ", length(values))
      }
      values
    }
    list(
      matrix = pick("matrix"),
      barcodes = pick("barcodes"),
      features = pick(feature.role),
      version = if (identical(feature.role, "features")) "V3" else "V2"
    )
  })
  names(valid) <- names(candidates)
  valid <- valid[!vapply(valid, is.null, logical(1))]
  if (length(valid) == 0) {
    stop("No complete 10x triplet found in `", path, "`")
  }
  if (length(valid) > 1) {
    stop(
      "Multiple 10x triplets found in `", path, "`. ",
      "Use `sample.pattern` or place each triplet in its own directory. Prefixes: ",
      paste(names(valid), collapse = ", ")
    )
  }
  valid[[1]]
}

#' Read Count Matrices
#'
#' Read count matrices with explicit format and naming policies. The initial
#' implementation supports 10x Matrix Market triplets with canonical or renamed
#' GEO-style filenames.
#'
#' @param path Directory containing a 10x triplet.
#' @param format Input format. Currently `auto` and `10x` resolve to the 10x reader.
#' @param version 10x feature file version: `auto`, `V3`, or `V2`.
#' @param gene.id Which feature column to use as matrix row names: `symbol` or `id`.
#' @param feature.type Optional 10x V3 feature type to retain.
#' @param make.unique.genes Whether to make duplicated selected gene names unique.
#' @param cell.prefix Optional string to prefix to cell barcodes.
#' @param sample.name Optional sample name recorded in cell metadata.
#' @param sample.pattern Optional regex used to select one triplet from a directory with several renamed triplets.
#' @param validate.integer Whether to reject non-integer count values.
#' @param return.metadata Whether to return a list with counts, cellMeta, geneMeta, and files.
#' @param verbose Whether to report detected files.
#'
#' @return A gene-by-cell sparse count matrix, or a list when return.metadata is TRUE.
#' @export
readCounts <- function(path, format = c("auto", "10x"), version = c("auto", "V3", "V2"),
                       gene.id = c("symbol", "id"), feature.type = NULL,
                       make.unique.genes = FALSE, cell.prefix = NULL, sample.name = NULL,
                       sample.pattern = NULL, validate.integer = TRUE,
                       return.metadata = FALSE, verbose = TRUE) {
  format <- match.arg(format)
  version <- match.arg(version)
  gene.id <- match.arg(gene.id)
  if (!dir.exists(path)) {
    stop("`path` must be a directory for 10x input")
  }
  triplet <- .pagoda2_detect_10x_triplet(path, version = version, sample.pattern = sample.pattern)
  if (verbose) {
    message("Reading 10x matrix: ", triplet$matrix)
  }
  counts <- .pagoda2_read_mtx(triplet$matrix)
  features <- .pagoda2_read_tsv(triplet$features, header = FALSE)
  barcodes <- .pagoda2_read_tsv(triplet$barcodes, header = FALSE)
  if (nrow(features) != nrow(counts)) {
    stop("Feature file has ", nrow(features), " rows but matrix has ", nrow(counts), " rows")
  }
  if (nrow(barcodes) != ncol(counts)) {
    stop("Barcode file has ", nrow(barcodes), " rows but matrix has ", ncol(counts), " columns")
  }
  if (validate.integer && any(abs(counts@x - round(counts@x)) > sqrt(.Machine$double.eps))) {
    stop("Count matrix contains non-integer values")
  }
  gene.meta <- data.frame(
    gene_id = as.character(features[[1]]),
    gene_symbol = if (ncol(features) >= 2) as.character(features[[2]]) else as.character(features[[1]]),
    stringsAsFactors = FALSE
  )
  if (triplet$version == "V3" && ncol(features) >= 3) {
    gene.meta$feature_type <- as.character(features[[3]])
  }
  gene.names <- if (gene.id == "id") gene.meta$gene_id else gene.meta$gene_symbol
  if (!is.null(feature.type)) {
    if (!"feature_type" %in% colnames(gene.meta)) {
      stop("`feature.type` was supplied but the feature file does not contain feature types")
    }
    keep <- gene.meta$feature_type %in% feature.type
    counts <- counts[keep, , drop = FALSE]
    gene.meta <- gene.meta[keep, , drop = FALSE]
    gene.names <- gene.names[keep]
  }
  if (anyDuplicated(gene.names) > 0) {
    if (make.unique.genes) {
      gene.names <- make.unique(gene.names)
    } else {
      warning("Selected gene names contain duplicates; use `make.unique.genes = TRUE` if constructing a Pagoda2 object.")
    }
  }
  cell.names <- as.character(barcodes[[1]])
  if (!is.null(cell.prefix)) {
    cell.names <- paste(cell.prefix, cell.names, sep = "_")
  }
  rownames(counts) <- gene.names
  colnames(counts) <- cell.names
  rownames(gene.meta) <- make.unique(gene.names)
  cell.meta <- data.frame(row.names = cell.names)
  if (!is.null(sample.name)) {
    cell.meta$sample <- sample.name
  }
  files <- list(
    matrix = triplet$matrix,
    barcodes = triplet$barcodes,
    features = triplet$features,
    version = triplet$version
  )
  attr(counts, "pagoda2.geneMeta") <- gene.meta
  attr(counts, "pagoda2.cellMeta") <- cell.meta
  attr(counts, "pagoda2.files") <- files
  if (return.metadata) {
    return(list(counts = counts, cellMeta = cell.meta, geneMeta = gene.meta, files = files))
  }
  counts
}

#' Create A Pagoda2 Object From Input
#'
#' @param x Matrix-like object or input path.
#' @param format Input format for paths. NULL guesses from input.
#' @param reader.args Named list of arguments passed to readCounts().
#' @param ... Arguments passed to Pagoda2$new().
#'
#' @return Pagoda2 object.
#' @export
pagoda2From <- function(x, format = NULL, reader.args = list(), ...) {
  if (is.character(x) && length(x) == 1 && dir.exists(x)) {
    if (is.null(format)) {
      format <- "auto"
    }
    reader.args <- utils::modifyList(list(path = x, format = format, return.metadata = TRUE, make.unique.genes = TRUE), reader.args)
    imported <- do.call(readCounts, reader.args)
    p2 <- Pagoda2$new(imported$counts, ...)
    p2$setCellMeta(imported$cellMeta)
    p2$setGeneMeta(imported$geneMeta)
    p2$history$input <- list(format = reader.args$format, files = imported$files)
    return(p2)
  }
  Pagoda2$new(x, ...)
}

#' @keywords internal
pagoda2As <- function(p2, format = c("list", "sce", "seurat"), assay = "RNA", ...) {
  format <- match.arg(format)
  raw.counts <- p2$misc$rawCounts
  if (is.null(raw.counts)) {
    raw.counts <- p2$counts
  }
  counts <- Matrix::t(raw.counts)
  gene.meta <- p2$getGeneMeta()
  cell.meta <- p2$getCellMeta()
  if (format == "list") {
    return(list(
      counts = counts,
      normalized = Matrix::t(p2$counts),
      cellMeta = cell.meta,
      geneMeta = gene.meta,
      reductions = p2$reductions,
      embeddings = p2$embeddings,
      graphs = p2$graphs,
      markers = p2$markerResults
    ))
  }
  if (format == "sce") {
    if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
      stop("Package `SingleCellExperiment` is required for `format = \"sce\"`.")
    }
    assays <- list(counts = counts)
    if (!is.null(p2$counts)) {
      assays$logcounts <- Matrix::t(p2$counts)
    }
    return(SingleCellExperiment::SingleCellExperiment(
      assays = assays,
      colData = S4Vectors::DataFrame(cell.meta),
      rowData = S4Vectors::DataFrame(gene.meta)
    ))
  }
  if (format == "seurat") {
    if (!requireNamespace("Seurat", quietly = TRUE)) {
      stop("Package `Seurat` is required for `format = \"seurat\"`.")
    }
    object <- Seurat::CreateSeuratObject(counts = counts, assay = assay, meta.data = cell.meta, ...)
    for (reduction in names(p2$embeddings)) {
      for (embedding in names(p2$embeddings[[reduction]])) {
        coordinates <- p2$embeddings[[reduction]][[embedding]]
        if (is.null(rownames(coordinates)) || !all(rownames(cell.meta) %in% rownames(coordinates))) {
          stop("Embedding `", embedding, "` under reduction `", reduction, "` is not named for all cells")
        }
        key <- paste0(gsub("[^A-Za-z0-9]", "", toupper(embedding)), "_")
        name <- tolower(paste(reduction, embedding, sep = "_"))
        object[[name]] <- Seurat::CreateDimReducObject(
          embeddings = coordinates[rownames(cell.meta), , drop = FALSE],
          key = key,
          assay = assay
        )
      }
    }
    return(object)
  }
  stop("Unsupported conversion format `", format, "`")
}

#' @keywords internal
pagoda2Export <- function(p2, path, format = NULL, ...) {
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(path))
    format <- if (identical(ext, "rds")) "rds" else ext
  }
  if (format == "rds") {
    saveRDS(p2, file = path, ...)
    return(invisible(path))
  }
  stop("Export format `", format, "` is not implemented yet")
}
