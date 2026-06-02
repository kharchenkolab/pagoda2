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
.pagoda2_normalize_format <- function(format) {
  if (is.null(format)) {
    format <- "auto"
  }
  if (length(format) > 1) {
    format <- format[[1]]
  }
  format <- tolower(gsub("[.-]", "_", format))
  switch(
    format,
    auto = "auto",
    `10x` = "10x",
    `10x_h5` = "10x_h5",
    `10x_hdf5` = "10x_h5",
    cellranger_h5 = "10x_h5",
    cellranger_hdf5 = "10x_h5",
    h5ad = "h5ad",
    anndata = "h5ad",
    h5seurat = "h5seurat",
    loom = "loom",
    stop("Unsupported input format `", format, "`")
  )
}

#' @keywords internal
.pagoda2_h5_open <- function(path, mode = "r") {
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package `hdf5r` is required to read HDF5-based formats.")
  }
  hdf5r::H5File$new(filename = path, mode = mode)
}

#' @keywords internal
.pagoda2_h5_exists <- function(h5, name) {
  tryCatch(isTRUE(h5$exists(name = name)), error = function(e) FALSE)
}

#' @keywords internal
.pagoda2_h5_group_has_sparse <- function(group) {
  all(c("data", "indices", "indptr", "shape") %in% names(group))
}

#' @keywords internal
.pagoda2_detect_h5_format <- function(path) {
  h5 <- .pagoda2_h5_open(path, mode = "r")
  on.exit(h5$close_all())
  root.names <- names(h5)
  if ("matrix" %in% root.names && inherits(h5[["matrix"]], "H5Group") && .pagoda2_h5_group_has_sparse(h5[["matrix"]])) {
    return("10x_h5")
  }
  if (all(c("assays", "cell.names") %in% root.names)) {
    return("h5seurat")
  }
  if ("matrix" %in% root.names && inherits(h5[["matrix"]], "H5D") &&
      all(c("row_attrs", "col_attrs") %in% root.names)) {
    return("loom")
  }
  for (n in root.names) {
    if (inherits(h5[[n]], "H5Group") && .pagoda2_h5_group_has_sparse(h5[[n]]) && "barcodes" %in% names(h5[[n]])) {
      return("10x_h5")
    }
  }
  stop("Could not identify HDF5 file format for `", path, "`")
}

#' @keywords internal
.pagoda2_detect_input_format <- function(path) {
  if (dir.exists(path)) {
    return("10x")
  }
  if (!file.exists(path)) {
    stop("Input path does not exist: ", path)
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "h5ad")) {
    return("h5ad")
  }
  if (identical(ext, "h5seurat")) {
    return("h5seurat")
  }
  if (identical(ext, "loom")) {
    return("loom")
  }
  if (ext %in% c("h5", "hdf5")) {
    return(.pagoda2_detect_h5_format(path))
  }
  stop("Cannot infer input format from extension `", ext, "`")
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

#' @keywords internal
.pagoda2_read_h5_sparse_csc <- function(group) {
  data <- as.numeric(group[["data"]][])
  indices <- as.integer(group[["indices"]][] + 1L)
  indptr <- as.integer(group[["indptr"]][])
  dims <- as.integer(group[["shape"]][] )
  as(Matrix::sparseMatrix(i = indices, p = indptr, x = data, dims = dims), "CsparseMatrix")
}

#' @keywords internal
.pagoda2_select_gene_names <- function(gene.meta, fallback, gene.id = c("symbol", "id")) {
  gene.id <- match.arg(gene.id)
  fallback <- as.character(fallback)
  if (gene.id == "id") {
    for (column in c("gene_id", "id", "ensembl_id")) {
      if (column %in% colnames(gene.meta)) {
        return(as.character(gene.meta[[column]]))
      }
    }
    return(fallback)
  }
  for (column in c("gene_symbol", "symbol", "name", "gene_name")) {
    if (column %in% colnames(gene.meta)) {
      return(as.character(gene.meta[[column]]))
    }
  }
  fallback
}

#' @keywords internal
.pagoda2_align_import_metadata <- function(metadata, names, axis) {
  if (is.null(metadata) || (ncol(metadata) == 0 && nrow(metadata) == 0)) {
    return(data.frame(row.names = names))
  }
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  if (nrow(metadata) != length(names)) {
    stop("Imported ", axis, " metadata has ", nrow(metadata), " rows but expected ", length(names))
  }
  if (!is.null(rownames(metadata)) && all(names %in% rownames(metadata))) {
    metadata <- metadata[names, , drop = FALSE]
  }
  rownames(metadata) <- names
  metadata
}

#' @keywords internal
.pagoda2_finalize_import <- function(counts, gene.names, cell.names, gene.meta = NULL, cell.meta = NULL,
                                     files = list(), make.unique.genes = FALSE,
                                     validate.integer = TRUE) {
  counts <- as(counts, "CsparseMatrix")
  gene.names <- as.character(gene.names)
  cell.names <- as.character(cell.names)
  if (nrow(counts) != length(gene.names)) {
    stop("Gene names have length ", length(gene.names), " but matrix has ", nrow(counts), " rows")
  }
  if (ncol(counts) != length(cell.names)) {
    stop("Cell names have length ", length(cell.names), " but matrix has ", ncol(counts), " columns")
  }
  if (validate.integer && any(abs(counts@x - round(counts@x)) > sqrt(.Machine$double.eps))) {
    stop("Count matrix contains non-integer values")
  }
  if (anyDuplicated(gene.names) > 0) {
    if (make.unique.genes) {
      gene.names <- make.unique(gene.names)
    } else {
      warning("Selected gene names contain duplicates; use `make.unique.genes = TRUE` if constructing a Pagoda2 object.")
    }
  }
  rownames(counts) <- gene.names
  colnames(counts) <- cell.names
  if (is.null(gene.meta)) {
    gene.meta <- data.frame(row.names = make.unique(gene.names))
  } else {
    gene.meta <- .pagoda2_align_import_metadata(gene.meta, make.unique(gene.names), axis = "gene")
  }
  cell.meta <- .pagoda2_align_import_metadata(cell.meta, cell.names, axis = "cell")
  attr(counts, "pagoda2.geneMeta") <- gene.meta
  attr(counts, "pagoda2.cellMeta") <- cell.meta
  attr(counts, "pagoda2.files") <- files
  list(counts = counts, cellMeta = cell.meta, geneMeta = gene.meta, files = files)
}

#' @keywords internal
.pagoda2_read_10x_dir <- function(path, version = c("auto", "V3", "V2"), gene.id = c("symbol", "id"),
                                  feature.type = NULL, make.unique.genes = FALSE,
                                  cell.prefix = NULL, sample.name = NULL,
                                  sample.pattern = NULL, validate.integer = TRUE,
                                  verbose = TRUE) {
  version <- match.arg(version)
  gene.id <- match.arg(gene.id)
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
  gene.meta <- data.frame(
    gene_id = as.character(features[[1]]),
    gene_symbol = if (ncol(features) >= 2) as.character(features[[2]]) else as.character(features[[1]]),
    stringsAsFactors = FALSE
  )
  if (triplet$version == "V3" && ncol(features) >= 3) {
    gene.meta$feature_type <- as.character(features[[3]])
  }
  gene.names <- .pagoda2_select_gene_names(gene.meta, fallback = gene.meta$gene_symbol, gene.id = gene.id)
  if (!is.null(feature.type)) {
    if (!"feature_type" %in% colnames(gene.meta)) {
      stop("`feature.type` was supplied but the feature file does not contain feature types")
    }
    keep <- gene.meta$feature_type %in% feature.type
    counts <- counts[keep, , drop = FALSE]
    gene.meta <- gene.meta[keep, , drop = FALSE]
    gene.names <- gene.names[keep]
  }
  cell.names <- as.character(barcodes[[1]])
  if (!is.null(cell.prefix)) {
    cell.names <- paste(cell.prefix, cell.names, sep = "_")
  }
  cell.meta <- data.frame(row.names = cell.names)
  if (!is.null(sample.name)) {
    cell.meta$sample <- sample.name
  }
  .pagoda2_finalize_import(
    counts = counts,
    gene.names = gene.names,
    cell.names = cell.names,
    gene.meta = gene.meta,
    cell.meta = cell.meta,
    files = list(matrix = triplet$matrix, barcodes = triplet$barcodes, features = triplet$features, version = triplet$version, format = "10x"),
    make.unique.genes = make.unique.genes,
    validate.integer = validate.integer
  )
}

#' @keywords internal
.pagoda2_read_10x_h5 <- function(path, gene.id = c("symbol", "id"), feature.type = NULL,
                                 genome = NULL, make.unique.genes = FALSE,
                                 cell.prefix = NULL, sample.name = NULL,
                                 validate.integer = TRUE, verbose = TRUE) {
  gene.id <- match.arg(gene.id)
  h5 <- .pagoda2_h5_open(path, mode = "r")
  on.exit(h5$close_all())
  root.names <- names(h5)
  group.name <- NULL
  if ("matrix" %in% root.names && .pagoda2_h5_group_has_sparse(h5[["matrix"]])) {
    group.name <- "matrix"
  } else {
    candidates <- root.names[vapply(root.names, function(n) {
      inherits(h5[[n]], "H5Group") && .pagoda2_h5_group_has_sparse(h5[[n]]) && "barcodes" %in% names(h5[[n]])
    }, logical(1))]
    if (length(candidates) == 0) {
      stop("No CellRanger sparse matrix group found in `", path, "`")
    }
    if (is.null(genome)) {
      if (length(candidates) > 1) {
        stop("Multiple CellRanger genome groups found; supply `genome`. Groups: ", paste(candidates, collapse = ", "))
      }
      group.name <- candidates[[1]]
    } else {
      if (!genome %in% candidates) {
        stop("Genome group `", genome, "` not found. Available groups: ", paste(candidates, collapse = ", "))
      }
      group.name <- genome
    }
  }
  if (verbose) {
    message("Reading CellRanger HDF5 matrix: ", path)
  }
  group <- h5[[group.name]]
  counts <- .pagoda2_read_h5_sparse_csc(group)
  cell.names <- as.character(group[["barcodes"]][])
  if ("features" %in% names(group)) {
    features <- group[["features"]]
    gene.meta <- data.frame(
      gene_id = if ("id" %in% names(features)) as.character(features[["id"]][]) else as.character(seq_len(nrow(counts))),
      gene_symbol = if ("name" %in% names(features)) as.character(features[["name"]][]) else if ("id" %in% names(features)) as.character(features[["id"]][]) else as.character(seq_len(nrow(counts))),
      stringsAsFactors = FALSE
    )
    if ("feature_type" %in% names(features)) {
      gene.meta$feature_type <- as.character(features[["feature_type"]][])
    }
    if ("genome" %in% names(features)) {
      gene.meta$genome <- as.character(features[["genome"]][])
    }
  } else {
    gene.meta <- data.frame(
      gene_id = if ("genes" %in% names(group)) as.character(group[["genes"]][]) else as.character(seq_len(nrow(counts))),
      gene_symbol = if ("gene_names" %in% names(group)) as.character(group[["gene_names"]][]) else if ("genes" %in% names(group)) as.character(group[["genes"]][]) else as.character(seq_len(nrow(counts))),
      stringsAsFactors = FALSE
    )
  }
  gene.names <- .pagoda2_select_gene_names(gene.meta, fallback = gene.meta$gene_symbol, gene.id = gene.id)
  if (is.null(feature.type) && "feature_type" %in% colnames(gene.meta)) {
    types <- unique(gene.meta$feature_type)
    if (length(types) > 1 && "Gene Expression" %in% types) {
      feature.type <- "Gene Expression"
    }
  }
  if (!is.null(feature.type)) {
    if (!"feature_type" %in% colnames(gene.meta)) {
      stop("`feature.type` was supplied but the HDF5 file does not contain feature types")
    }
    keep <- gene.meta$feature_type %in% feature.type
    counts <- counts[keep, , drop = FALSE]
    gene.meta <- gene.meta[keep, , drop = FALSE]
    gene.names <- gene.names[keep]
  }
  if (!is.null(cell.prefix)) {
    cell.names <- paste(cell.prefix, cell.names, sep = "_")
  }
  cell.meta <- data.frame(row.names = cell.names)
  if (!is.null(sample.name)) {
    cell.meta$sample <- sample.name
  }
  .pagoda2_finalize_import(
    counts = counts,
    gene.names = gene.names,
    cell.names = cell.names,
    gene.meta = gene.meta,
    cell.meta = cell.meta,
    files = list(path = path, group = group.name, format = "10x_h5"),
    make.unique.genes = make.unique.genes,
    validate.integer = validate.integer
  )
}

#' @keywords internal
.pagoda2_h5_attr <- function(x, name, default = NULL) {
  if (name %in% hdf5r::h5attr_names(x)) {
    return(hdf5r::h5attr(x, name))
  }
  default
}

#' @keywords internal
.pagoda2_h5_read_sparse_or_dense <- function(x, dims = NULL) {
  if (inherits(x, "H5Group")) {
    data <- as.numeric(x[["data"]][])
    indices <- as.integer(x[["indices"]][] + 1L)
    indptr <- as.integer(x[["indptr"]][])
    if (is.null(dims)) {
      dims <- .pagoda2_h5_attr(x, "shape", default = .pagoda2_h5_attr(x, "dims", default = NULL))
    }
    dims <- as.integer(dims)
    encoding <- .pagoda2_h5_attr(x, "encoding-type", default = "csc_matrix")
    if (identical(encoding, "csr_matrix")) {
      return(as(Matrix::sparseMatrix(j = indices, p = indptr, x = data, dims = dims, repr = "R"), "CsparseMatrix"))
    }
    return(as(Matrix::sparseMatrix(i = indices, p = indptr, x = data, dims = dims, repr = "C"), "CsparseMatrix"))
  }
  as(Matrix::Matrix(x[], sparse = TRUE), "CsparseMatrix")
}

#' @keywords internal
.pagoda2_h5_read_vector <- function(x) {
  value <- if (inherits(x, "H5D")) x$read() else x[]
  if (is.matrix(value) && any(dim(value) == 1)) {
    value <- as.vector(value)
  }
  value
}

#' @keywords internal
.pagoda2_h5_read_column <- function(x) {
  if (inherits(x, "H5Group")) {
    if (all(c("categories", "codes") %in% names(x))) {
      categories <- as.character(.pagoda2_h5_read_vector(x[["categories"]]))
      codes <- as.integer(.pagoda2_h5_read_vector(x[["codes"]]))
      out <- rep(NA_character_, length(codes))
      keep <- codes >= 0
      out[keep] <- categories[codes[keep] + 1L]
      if (isTRUE(.pagoda2_h5_attr(x, "ordered", default = FALSE))) {
        return(factor(out, levels = categories, ordered = TRUE))
      }
      return(factor(out, levels = categories))
    }
    if (all(c("levels", "values") %in% names(x))) {
      levels <- as.character(.pagoda2_h5_read_vector(x[["levels"]]))
      values <- as.integer(.pagoda2_h5_read_vector(x[["values"]]))
      return(factor(levels[values], levels = levels))
    }
    return(rep(NA_character_, 0))
  }
  .pagoda2_h5_read_vector(x)
}

#' @keywords internal
.pagoda2_h5_read_dense_as_sparse <- function(x, chunk.size = 1000L) {
  dims <- as.integer(x$dims)
  if (length(dims) != 2) {
    stop("Expected a two-dimensional HDF5 dataset")
  }
  chunk.size <- max(1L, as.integer(chunk.size))
  i.list <- list()
  j.list <- list()
  x.list <- list()
  block.index <- 0L
  for (start in seq.int(1L, dims[[1]], by = chunk.size)) {
    end <- min(start + chunk.size - 1L, dims[[1]])
    rows <- seq.int(start, end)
    block <- x[rows, seq_len(dims[[2]]), drop = FALSE]
    nz <- which(block != 0, arr.ind = TRUE)
    if (nrow(nz) > 0) {
      block.index <- block.index + 1L
      i.list[[block.index]] <- rows[nz[, 1]]
      j.list[[block.index]] <- nz[, 2]
      x.list[[block.index]] <- block[nz]
    }
  }
  if (block.index == 0L) {
    return(as(Matrix::sparseMatrix(i = integer(), j = integer(), x = numeric(), dims = dims), "CsparseMatrix"))
  }
  i.out <- unlist(i.list, use.names = FALSE)
  j.out <- unlist(j.list, use.names = FALSE)
  x.out <- unlist(x.list, use.names = FALSE)
  as(Matrix::sparseMatrix(i = i.out, j = j.out, x = x.out, dims = dims), "CsparseMatrix")
}

#' @keywords internal
.pagoda2_h5_read_dataframe <- function(group, expected.n = NULL) {
  if (!inherits(group, "H5Group")) {
    stop("Expected an HDF5 group containing dataframe metadata")
  }
  index.name <- .pagoda2_h5_attr(group, "_index", default = NULL)
  if (is.null(index.name)) {
    index.name <- if ("_index" %in% names(group)) "_index" else if ("index" %in% names(group)) "index" else NULL
  }
  row.names <- if (!is.null(index.name) && index.name %in% names(group)) {
    as.character(.pagoda2_h5_read_vector(group[[index.name]]))
  } else if (!is.null(expected.n)) {
    as.character(seq_len(expected.n))
  } else {
    character()
  }
  columns <- .pagoda2_h5_attr(group, "column-order", default = .pagoda2_h5_attr(group, "colnames", default = NULL))
  if (is.null(columns)) {
    columns <- setdiff(names(group), c(index.name, "__categories"))
  }
  columns <- as.character(columns)
  if (length(columns) == 0) {
    return(data.frame(row.names = row.names))
  }
  out <- lapply(columns, function(column) .pagoda2_h5_read_column(group[[column]]))
  names(out) <- columns
  out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  if (length(row.names) > 0) {
    rownames(out) <- row.names
  }
  out
}

#' @keywords internal
.pagoda2_h5_read_attr_dataframe <- function(group, expected.n = NULL) {
  if (!inherits(group, "H5Group")) {
    stop("Expected an HDF5 group containing loom attributes")
  }
  columns <- names(group)
  values <- list()
  lengths <- integer()
  for (column in columns) {
    value <- .pagoda2_h5_read_column(group[[column]])
    value.dim <- dim(value)
    if (!is.null(value.dim) && length(value.dim) > 1 && all(value.dim > 1)) {
      next
    }
    value <- as.vector(value)
    if (length(value) > 0) {
      values[[column]] <- value
      lengths[[column]] <- length(value)
    }
  }
  if (is.null(expected.n)) {
    if (length(lengths) == 0) {
      return(data.frame())
    }
    size.table <- sort(table(lengths), decreasing = TRUE)
    expected.n <- as.integer(names(size.table)[[1]])
  }
  out <- values[lengths == expected.n]
  if (length(out) == 0) {
    return(data.frame(row.names = as.character(seq_len(expected.n))))
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  rownames(out) <- as.character(seq_len(nrow(out)))
  out
}

#' @keywords internal
.pagoda2_match_column <- function(metadata, candidates) {
  if (is.null(metadata) || ncol(metadata) == 0) {
    return(NULL)
  }
  nms <- colnames(metadata)
  idx <- match(tolower(candidates), tolower(nms), nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) == 0) {
    return(NULL)
  }
  nms[[idx[[1]]]]
}

#' @keywords internal
.pagoda2_h5_write_scalar_attr <- function(x, name, value) {
  if (is.character(value)) {
    x$create_attr(
      name,
      robj = as.character(value)[[1]],
      dtype = hdf5r::h5types$H5T_STRING$new(size = Inf),
      space = hdf5r::H5S$new("scalar")
    )
  } else {
    x$create_attr(name, robj = value, space = hdf5r::H5S$new("scalar"))
  }
  invisible(x)
}

#' @keywords internal
.pagoda2_h5_write_encoding <- function(x, type, version) {
  .pagoda2_h5_write_scalar_attr(x, "encoding-type", type)
  .pagoda2_h5_write_scalar_attr(x, "encoding-version", version)
  invisible(x)
}

#' @keywords internal
.pagoda2_h5_write_string_array <- function(group, name, values) {
  dataset <- group$create_dataset(
    name,
    robj = as.character(values),
    dtype = hdf5r::h5types$H5T_STRING$new(size = Inf)
  )
  .pagoda2_h5_write_encoding(dataset, "string-array", "0.2.0")
}

#' @keywords internal
.pagoda2_h5_write_array <- function(group, name, values) {
  dataset <- group$create_dataset(name, robj = values)
  .pagoda2_h5_write_encoding(dataset, "array", "0.2.0")
}

#' @keywords internal
.pagoda2_h5_write_dataframe <- function(group, metadata, index, axis) {
  if (is.null(metadata) || (nrow(metadata) == 0 && ncol(metadata) == 0)) {
    metadata <- data.frame(row.names = index)
  }
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE, optional = TRUE)
  if (nrow(metadata) != length(index)) {
    stop("AnnData ", axis, " metadata has ", nrow(metadata), " rows but the matrix axis has ", length(index))
  }
  if (is.null(rownames(metadata)) || !identical(rownames(metadata), index)) {
    stop("AnnData ", axis, " metadata rownames must exactly match the matrix axis")
  }
  if ("_index" %in% colnames(metadata)) {
    stop("AnnData ", axis, " metadata column `_index` is reserved")
  }
  .pagoda2_h5_write_scalar_attr(group, "_index", "_index")
  group$create_attr("column-order", as.character(colnames(metadata)))
  .pagoda2_h5_write_encoding(group, "dataframe", "0.2.0")
  .pagoda2_h5_write_string_array(group, "_index", index)
  for (column in colnames(metadata)) {
    value <- metadata[[column]]
    if (is.list(value) && !is.factor(value)) {
      stop("AnnData ", axis, " metadata column `", column, "` has unsupported list values")
    }
    if (is.factor(value)) {
      .pagoda2_h5_write_string_array(group, column, as.character(value))
    } else if (is.character(value)) {
      .pagoda2_h5_write_string_array(group, column, value)
    } else if (is.logical(value)) {
      if (any(is.na(value))) {
        .pagoda2_h5_write_string_array(group, column, as.character(value))
      } else {
        .pagoda2_h5_write_array(group, column, value)
      }
    } else if (is.integer(value) && any(is.na(value))) {
      .pagoda2_h5_write_array(group, column, as.numeric(value))
    } else if (is.numeric(value) || is.integer(value)) {
      .pagoda2_h5_write_array(group, column, value)
    } else {
      .pagoda2_h5_write_string_array(group, column, as.character(value))
    }
  }
  invisible(group)
}

#' @keywords internal
.pagoda2_h5_write_sparse_csr <- function(group, matrix) {
  matrix <- as(matrix, "RsparseMatrix")
  group$create_dataset("data", robj = matrix@x)
  group$create_dataset("indices", robj = as.integer(matrix@j))
  group$create_dataset("indptr", robj = as.integer(matrix@p))
  group$create_attr("shape", as.integer(dim(matrix)))
  .pagoda2_h5_write_encoding(group, "csr_matrix", "0.1.0")
  invisible(group)
}

#' @keywords internal
.pagoda2_h5_write_obsm_matrix <- function(group, name, matrix, cells) {
  if (is.null(rownames(matrix)) || !all(cells %in% rownames(matrix))) {
    stop("AnnData obsm matrix `", name, "` is not named for all cells")
  }
  matrix <- as.matrix(matrix[cells, , drop = FALSE])
  dataset <- group$create_dataset(name, robj = t(matrix))
  .pagoda2_h5_write_encoding(dataset, "array", "0.2.0")
  invisible(dataset)
}

#' @keywords internal
.pagoda2_export_axis_metadata <- function(metadata, names, axis) {
  if (is.null(metadata) || (nrow(metadata) == 0 && ncol(metadata) == 0)) {
    return(data.frame(row.names = names))
  }
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE, optional = TRUE)
  if (nrow(metadata) != length(names)) {
    stop("AnnData ", axis, " metadata must have exactly ", length(names), " rows")
  }
  if (is.null(rownames(metadata)) || !identical(rownames(metadata), names)) {
    stop("AnnData ", axis, " metadata rownames must exactly match the matrix axis")
  }
  metadata
}

#' @keywords internal
.pagoda2_h5ad_key <- function(...) {
  key <- paste(..., sep = "_")
  key <- tolower(gsub("[^A-Za-z0-9]+", "_", key))
  key <- gsub("^_+|_+$", "", key)
  paste0("X_", key)
}

#' @keywords internal
.pagoda2_get_normalized_matrix <- function(p2) {
  if (!is.null(p2$matrixViews$analysis)) {
    return(p2$getExpressionBlock())
  }
  stop("Normalized analysis matrix is not available")
}

#' @keywords internal
.pagoda2_export_h5ad <- function(p2, path, x = c("normalized", "counts"),
                                 counts.layer = "counts", include.counts = TRUE,
                                 include.reductions = TRUE, include.embeddings = TRUE,
                                 overwrite = FALSE) {
  x <- match.arg(x)
  if (file.exists(path)) {
    if (!overwrite) {
      stop("Output file exists; use `overwrite = TRUE` to replace it: ", path)
    }
    unlink(path)
  }
  raw.counts <- p2$getRawCounts()
  export.counts <- as(raw.counts, "CsparseMatrix")
  export.x <- if (identical(x, "normalized")) .pagoda2_get_normalized_matrix(p2) else export.counts
  export.x <- as(export.x, "CsparseMatrix")
  if (!identical(dim(export.x), dim(export.counts)) ||
      !identical(rownames(export.x), rownames(export.counts)) ||
      !identical(colnames(export.x), colnames(export.counts))) {
    stop("AnnData X and counts layer must have identical cell and gene axes")
  }
  cells <- rownames(export.x)
  genes <- colnames(export.x)
  cell.meta <- .pagoda2_export_axis_metadata(p2$resolveCellMeta(cells = cells), cells, axis = "cell")
  gene.meta <- .pagoda2_export_axis_metadata(p2$resolveGeneMeta(genes = genes), genes, axis = "gene")
  h5 <- .pagoda2_h5_open(path, mode = "w")
  on.exit(h5$close_all())
  .pagoda2_h5_write_encoding(h5, "anndata", "0.1.0")
  .pagoda2_h5_write_sparse_csr(h5$create_group("X"), export.x)
  layers <- h5$create_group("layers")
  .pagoda2_h5_write_encoding(layers, "dict", "0.1.0")
  if (isTRUE(include.counts) && !is.null(counts.layer)) {
    .pagoda2_h5_write_sparse_csr(layers$create_group(counts.layer), export.counts)
  }
  .pagoda2_h5_write_dataframe(h5$create_group("obs"), cell.meta, cells, axis = "cell")
  .pagoda2_h5_write_dataframe(h5$create_group("var"), gene.meta, genes, axis = "gene")
  obsm <- h5$create_group("obsm")
  .pagoda2_h5_write_encoding(obsm, "dict", "0.1.0")
  used.keys <- character()
  if (isTRUE(include.reductions)) {
    for (reduction in names(p2$reductions)) {
      key <- .pagoda2_h5ad_key(reduction)
      if (!key %in% used.keys) {
        .pagoda2_h5_write_obsm_matrix(obsm, key, p2$reductions[[reduction]], cells)
        used.keys <- c(used.keys, key)
      }
    }
  }
  if (isTRUE(include.embeddings)) {
    embedding.names <- unlist(lapply(p2$embeddings, names), use.names = FALSE)
    duplicated.embeddings <- embedding.names[duplicated(embedding.names)]
    for (reduction in names(p2$embeddings)) {
      for (embedding in names(p2$embeddings[[reduction]])) {
        key <- if (embedding %in% duplicated.embeddings) {
          .pagoda2_h5ad_key(reduction, embedding)
        } else {
          .pagoda2_h5ad_key(embedding)
        }
        if (!key %in% used.keys) {
          .pagoda2_h5_write_obsm_matrix(obsm, key, p2$embeddings[[reduction]][[embedding]], cells)
          used.keys <- c(used.keys, key)
        }
      }
    }
  }
  for (group.name in c("varm", "obsp", "varp", "uns")) {
    group <- h5$create_group(group.name)
    .pagoda2_h5_write_encoding(group, "dict", "0.1.0")
  }
  invisible(path)
}

#' @keywords internal
.pagoda2_read_loom <- function(path, gene.id = c("symbol", "id"), layer = NULL,
                               make.unique.genes = FALSE, cell.prefix = NULL,
                               sample.name = NULL, validate.integer = TRUE,
                               chunk.size = 1000L, verbose = TRUE) {
  gene.id <- match.arg(gene.id)
  h5 <- .pagoda2_h5_open(path, mode = "r")
  on.exit(h5$close_all())
  if (!all(c("matrix", "row_attrs", "col_attrs") %in% names(h5))) {
    stop("Loom file must contain /matrix, /row_attrs, and /col_attrs")
  }
  matrix.path <- if (is.null(layer) || identical(layer, "matrix")) {
    "matrix"
  } else {
    paste0("layers/", layer)
  }
  if (!.pagoda2_h5_exists(h5, matrix.path)) {
    stop("Loom matrix/layer `", matrix.path, "` not found")
  }
  if (verbose) {
    message("Reading loom matrix from /", matrix.path, ": ", path)
  }
  matrix.node <- h5[[matrix.path]]
  if (!inherits(matrix.node, "H5D")) {
    stop("Loom matrix/layer `", matrix.path, "` must be a two-dimensional dataset")
  }
  raw.counts <- .pagoda2_h5_read_dense_as_sparse(matrix.node, chunk.size = chunk.size)
  row.meta <- .pagoda2_h5_read_attr_dataframe(h5[["row_attrs"]])
  col.meta <- .pagoda2_h5_read_attr_dataframe(h5[["col_attrs"]])
  transposed <- FALSE
  if ((nrow(row.meta) != nrow(raw.counts) || nrow(col.meta) != ncol(raw.counts)) &&
      nrow(row.meta) == ncol(raw.counts) && nrow(col.meta) == nrow(raw.counts)) {
    raw.counts <- Matrix::t(raw.counts)
    transposed <- TRUE
  }
  if (nrow(row.meta) != nrow(raw.counts) || nrow(col.meta) != ncol(raw.counts)) {
    stop("Loom row_attrs/col_attrs dimensions do not match the selected matrix")
  }
  gene.symbol.column <- .pagoda2_match_column(row.meta, c("Gene", "GeneName", "gene_symbol", "gene", "name"))
  gene.id.column <- .pagoda2_match_column(row.meta, c("Accession", "GeneID", "gene_id", "id", "ensembl_id"))
  cell.column <- .pagoda2_match_column(col.meta, c("CellID", "CellName", "Barcode", "barcodes", "cell_id", "cell"))
  cell.names <- if (is.null(cell.column)) as.character(seq_len(ncol(raw.counts))) else as.character(col.meta[[cell.column]])
  gene.fallback <- if (is.null(gene.symbol.column)) as.character(seq_len(nrow(raw.counts))) else as.character(row.meta[[gene.symbol.column]])
  if (!is.null(gene.id.column) && !"gene_id" %in% colnames(row.meta)) {
    row.meta$gene_id <- as.character(row.meta[[gene.id.column]])
  }
  if (!is.null(gene.symbol.column) && !"gene_symbol" %in% colnames(row.meta)) {
    row.meta$gene_symbol <- as.character(row.meta[[gene.symbol.column]])
  }
  gene.names <- .pagoda2_select_gene_names(row.meta, fallback = gene.fallback, gene.id = gene.id)
  if (!is.null(cell.prefix)) {
    cell.names <- paste(cell.prefix, cell.names, sep = "_")
  }
  if (!is.null(sample.name)) {
    col.meta$sample <- sample.name
  }
  .pagoda2_finalize_import(
    counts = raw.counts,
    gene.names = gene.names,
    cell.names = cell.names,
    gene.meta = row.meta,
    cell.meta = col.meta,
    files = list(path = path, format = "loom", layer = matrix.path, transposed = transposed),
    make.unique.genes = make.unique.genes,
    validate.integer = validate.integer
  )
}

#' @keywords internal
.pagoda2_read_h5ad <- function(path, gene.id = c("symbol", "id"), layer = NULL,
                               use.raw = FALSE, make.unique.genes = FALSE,
                               cell.prefix = NULL, sample.name = NULL,
                               validate.integer = TRUE, verbose = TRUE) {
  gene.id <- match.arg(gene.id)
  h5 <- .pagoda2_h5_open(path, mode = "r")
  on.exit(h5$close_all())
  source <- "X"
  feature.source <- "var"
  if (!is.null(layer)) {
    source <- paste0("layers/", layer)
    if (!.pagoda2_h5_exists(h5, source)) {
      stop("h5ad layer `", layer, "` not found")
    }
  } else if (isTRUE(use.raw) && .pagoda2_h5_exists(h5, "raw/X")) {
    source <- "raw/X"
    feature.source <- "raw/var"
  } else if (.pagoda2_h5_exists(h5, "layers/counts")) {
    source <- "layers/counts"
  }
  if (verbose) {
    message("Reading h5ad matrix from ", source, ": ", path)
  }
  counts <- Matrix::t(.pagoda2_h5_read_sparse_or_dense(h5[[source]]))
  cell.meta <- .pagoda2_h5_read_dataframe(h5[["obs"]], expected.n = ncol(counts))
  gene.meta <- .pagoda2_h5_read_dataframe(h5[[feature.source]], expected.n = nrow(counts))
  cell.names <- rownames(cell.meta)
  gene.fallback <- rownames(gene.meta)
  if (!"gene_id" %in% colnames(gene.meta)) {
    gene.meta$gene_id <- gene.fallback
  }
  if (!is.null(cell.prefix)) {
    cell.names <- paste(cell.prefix, cell.names, sep = "_")
  }
  if (!is.null(sample.name)) {
    cell.meta$sample <- sample.name
  }
  gene.names <- .pagoda2_select_gene_names(gene.meta, fallback = gene.fallback, gene.id = gene.id)
  .pagoda2_finalize_import(
    counts = counts,
    gene.names = gene.names,
    cell.names = cell.names,
    gene.meta = gene.meta,
    cell.meta = cell.meta,
    files = list(path = path, format = "h5ad", source = source),
    make.unique.genes = make.unique.genes,
    validate.integer = validate.integer
  )
}

#' @keywords internal
.pagoda2_h5seurat_default_assay <- function(h5, assay = NULL) {
  if (!is.null(assay)) {
    return(assay)
  }
  assay <- .pagoda2_h5_attr(h5, "active.assay", default = NULL)
  if (!is.null(assay)) {
    return(as.character(assay))
  }
  assays <- names(h5[["assays"]])
  if (length(assays) == 0) {
    stop("h5Seurat file contains no assays")
  }
  assays[[1]]
}

#' @keywords internal
.pagoda2_read_h5seurat <- function(path, assay = NULL, layer = NULL,
                                   make.unique.genes = FALSE, cell.prefix = NULL,
                                   sample.name = NULL, validate.integer = TRUE,
                                   verbose = TRUE) {
  h5 <- .pagoda2_h5_open(path, mode = "r")
  on.exit(h5$close_all())
  assay <- .pagoda2_h5seurat_default_assay(h5, assay = assay)
  layer <- if (is.null(layer)) "counts" else layer
  layer.path <- paste0("assays/", assay, "/layers/", layer)
  if (!.pagoda2_h5_exists(h5, layer.path)) {
    legacy.path <- paste0("assays/", assay, "/", layer)
    if (.pagoda2_h5_exists(h5, legacy.path)) {
      layer.path <- legacy.path
    } else {
      stop("h5Seurat layer `", layer, "` not found in assay `", assay, "`")
    }
  }
  if (verbose) {
    message("Reading h5Seurat layer ", assay, "/", layer, ": ", path)
  }
  counts <- .pagoda2_h5_read_sparse_or_dense(h5[[layer.path]])
  cell.names <- as.character(.pagoda2_h5_read_vector(h5[["cell.names"]]))
  if (length(cell.names) != ncol(counts)) {
    cell.names <- as.character(seq_len(ncol(counts)))
  }
  feature.path <- paste0("assays/", assay, "/meta.data")
  if (.pagoda2_h5_exists(h5, feature.path)) {
    gene.meta <- .pagoda2_h5_read_dataframe(h5[[feature.path]], expected.n = nrow(counts))
  } else {
    gene.meta <- data.frame(row.names = as.character(seq_len(nrow(counts))))
  }
  features.path <- paste0("assays/", assay, "/features")
  if (.pagoda2_h5_exists(h5, features.path)) {
    features <- .pagoda2_h5_read_vector(h5[[features.path]])
    features <- as.character(features)
    if (length(features) == nrow(counts) && !anyDuplicated(features)) {
      rownames(gene.meta) <- features
    }
  }
  if (length(rownames(gene.meta)) != nrow(counts)) {
    rownames(gene.meta) <- as.character(seq_len(nrow(counts)))
  }
  if (.pagoda2_h5_exists(h5, "meta.data")) {
    cell.meta <- .pagoda2_h5_read_dataframe(h5[["meta.data"]], expected.n = ncol(counts))
  } else {
    cell.meta <- data.frame(row.names = cell.names)
  }
  if (!is.null(cell.prefix)) {
    cell.names <- paste(cell.prefix, cell.names, sep = "_")
  }
  if (!is.null(sample.name)) {
    cell.meta$sample <- sample.name
  }
  .pagoda2_finalize_import(
    counts = counts,
    gene.names = rownames(gene.meta),
    cell.names = cell.names,
    gene.meta = gene.meta,
    cell.meta = cell.meta,
    files = list(path = path, format = "h5seurat", assay = assay, layer = layer),
    make.unique.genes = make.unique.genes,
    validate.integer = validate.integer
  )
}

#' Read Count Matrices
#'
#' Read count matrices with explicit format and naming policies. Supported
#' inputs include 10x Matrix Market triplets, 10x/CellRanger HDF5, AnnData h5ad,
#' h5Seurat, and loom files.
#'
#' @param path Directory containing a 10x triplet, or a supported count file.
#' @param format Input format. `auto` detects 10x triplet directories, h5ad,
#' h5Seurat, CellRanger HDF5, and loom files.
#' @param version 10x feature file version: `auto`, `V3`, or `V2`.
#' @param gene.id Which feature column to use as matrix row names: `symbol` or `id`.
#' @param feature.type Optional 10x V3 feature type to retain.
#' @param genome Optional CellRanger HDF5 genome group.
#' @param assay Optional h5Seurat assay.
#' @param layer Optional h5ad, h5Seurat, or loom layer.
#' @param use.raw Whether h5ad input should read raw/X when no layer is supplied.
#' @param chunk.size Row chunk size when reading dense HDF5 matrices such as loom.
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
readCounts <- function(path, format = c("auto", "10x", "10x_h5", "h5ad", "h5seurat", "loom"),
                       version = c("auto", "V3", "V2"),
                       gene.id = c("symbol", "id"), feature.type = NULL,
                       genome = NULL, assay = NULL, layer = NULL, use.raw = FALSE,
                       make.unique.genes = FALSE, cell.prefix = NULL, sample.name = NULL,
                       sample.pattern = NULL, validate.integer = TRUE,
                       return.metadata = FALSE, chunk.size = 1000L, verbose = TRUE) {
  format <- .pagoda2_normalize_format(format)
  version <- match.arg(version)
  gene.id <- match.arg(gene.id)
  if (format == "auto") {
    format <- .pagoda2_detect_input_format(path)
  }
  imported <- switch(
    format,
    `10x` = .pagoda2_read_10x_dir(
      path = path,
      version = version,
      gene.id = gene.id,
      feature.type = feature.type,
      make.unique.genes = make.unique.genes,
      cell.prefix = cell.prefix,
      sample.name = sample.name,
      sample.pattern = sample.pattern,
      validate.integer = validate.integer,
      verbose = verbose
    ),
    `10x_h5` = .pagoda2_read_10x_h5(
      path = path,
      gene.id = gene.id,
      feature.type = feature.type,
      genome = genome,
      make.unique.genes = make.unique.genes,
      cell.prefix = cell.prefix,
      sample.name = sample.name,
      validate.integer = validate.integer,
      verbose = verbose
    ),
    h5ad = .pagoda2_read_h5ad(
      path = path,
      gene.id = gene.id,
      layer = layer,
      use.raw = use.raw,
      make.unique.genes = make.unique.genes,
      cell.prefix = cell.prefix,
      sample.name = sample.name,
      validate.integer = validate.integer,
      verbose = verbose
    ),
    h5seurat = .pagoda2_read_h5seurat(
      path = path,
      assay = assay,
      layer = layer,
      make.unique.genes = make.unique.genes,
      cell.prefix = cell.prefix,
      sample.name = sample.name,
      validate.integer = validate.integer,
      verbose = verbose
    ),
    loom = .pagoda2_read_loom(
      path = path,
      gene.id = gene.id,
      layer = layer,
      make.unique.genes = make.unique.genes,
      cell.prefix = cell.prefix,
      sample.name = sample.name,
      validate.integer = validate.integer,
      chunk.size = chunk.size,
      verbose = verbose
    ),
    stop("Unsupported input format `", format, "`")
  )
  if (return.metadata) {
    return(imported)
  }
  imported$counts
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
  constructor.args <- list(...)
  if (is.character(x) && length(x) == 1 && (dir.exists(x) || file.exists(x))) {
    if (is.null(format)) {
      format <- "auto"
    }
    if ("verbose" %in% names(constructor.args) && is.null(reader.args$verbose)) {
      reader.args$verbose <- constructor.args$verbose
    }
    reader.args <- utils::modifyList(list(path = x, format = format, return.metadata = TRUE, make.unique.genes = TRUE), reader.args)
    imported <- do.call(readCounts, reader.args)
    p2 <- do.call(Pagoda2$new, c(list(x = imported$counts), constructor.args))
    p2$setCellMeta(imported$cellMeta)
    p2$setGeneMeta(imported$geneMeta)
    p2$history$input <- list(format = reader.args$format, files = imported$files)
    return(p2)
  }
  do.call(Pagoda2$new, c(list(x = x), constructor.args))
}

#' @rdname pagoda2From
#' @export
pagoda2From10x <- function(path, reader.args = list(), ...) {
  pagoda2From(path, format = "10x", reader.args = reader.args, ...)
}

#' @rdname pagoda2From
#' @export
pagoda2From10xH5 <- function(path, reader.args = list(), ...) {
  pagoda2From(path, format = "10x_h5", reader.args = reader.args, ...)
}

#' @rdname pagoda2From
#' @export
pagoda2FromAnnData <- function(path, reader.args = list(), ...) {
  pagoda2From(path, format = "h5ad", reader.args = reader.args, ...)
}

#' @rdname pagoda2From
#' @export
pagoda2FromH5Seurat <- function(path, reader.args = list(), ...) {
  pagoda2From(path, format = "h5seurat", reader.args = reader.args, ...)
}

#' @rdname pagoda2From
#' @export
pagoda2FromLoom <- function(path, reader.args = list(), ...) {
  pagoda2From(path, format = "loom", reader.args = reader.args, ...)
}

#' @rdname pagoda2From
#' @export
readPagoda2 <- function(path, format = NULL, reader.args = list(), ...) {
  pagoda2From(path, format = format, reader.args = reader.args, ...)
}

#' @keywords internal
.pagoda2_load_optional_namespace <- function(package, purpose) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package `", package, "` is required for ", purpose, ".")
  }
  asNamespace(package)
}

#' @keywords internal
pagoda2As <- function(p2, format = c("list", "sce", "seurat"), assay = "RNA",
                      include.normalized = TRUE, include.geneMeta = TRUE,
                      include.embeddings = TRUE, ...) {
  format <- match.arg(format)
  raw.counts <- p2$getRawCounts()
  counts <- Matrix::t(raw.counts)
  gene.meta <- p2$resolveGeneMeta(genes = rownames(counts))
  cell.meta <- p2$resolveCellMeta(cells = colnames(counts))
  normalized <- if (isTRUE(include.normalized)) {
    Matrix::t(.pagoda2_get_normalized_matrix(p2))
  } else {
    NULL
  }
  if (format == "list") {
    return(list(
      counts = counts,
      normalized = normalized,
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
    if (!is.null(normalized)) {
      assays$logcounts <- normalized
    }
    return(SingleCellExperiment::SingleCellExperiment(
      assays = assays,
      colData = S4Vectors::DataFrame(cell.meta),
      rowData = S4Vectors::DataFrame(gene.meta)
    ))
  }
  if (format == "seurat") {
    seurat.ns <- .pagoda2_load_optional_namespace("Seurat", "`format = \"seurat\"`")
    object <- get("CreateSeuratObject", envir = seurat.ns)(counts = counts, assay = assay, meta.data = cell.meta, ...)
    if (!is.null(normalized)) {
      data <- normalized
      object <- tryCatch(
        get("SetAssayData", envir = seurat.ns)(object, assay = assay, layer = "data", new.data = data),
        error = function(e) get("SetAssayData", envir = seurat.ns)(object, assay = assay, slot = "data", new.data = data)
      )
    }
    if (isTRUE(include.geneMeta) && ncol(gene.meta) > 0) {
      if (!all(rownames(object) %in% rownames(gene.meta))) {
        stop("Gene metadata is not named for all Seurat features")
      }
      object[[assay]] <- get("AddMetaData", envir = seurat.ns)(
        object = object[[assay]],
        metadata = gene.meta[rownames(object), , drop = FALSE]
      )
    }
    if (isTRUE(include.embeddings)) {
      for (reduction in names(p2$embeddings)) {
        for (embedding in names(p2$embeddings[[reduction]])) {
          coordinates <- p2$embeddings[[reduction]][[embedding]]
          if (is.null(rownames(coordinates)) || !all(rownames(cell.meta) %in% rownames(coordinates))) {
            stop("Embedding `", embedding, "` under reduction `", reduction, "` is not named for all cells")
          }
          key <- paste0(gsub("[^A-Za-z0-9]", "", toupper(embedding)), "_")
          name <- tolower(paste(reduction, embedding, sep = "_"))
          object[[name]] <- get("CreateDimReducObject", envir = seurat.ns)(
            embeddings = coordinates[rownames(cell.meta), , drop = FALSE],
            key = key,
            assay = assay
          )
        }
      }
    }
    return(object)
  }
  stop("Unsupported conversion format `", format, "`")
}

#' @keywords internal
pagoda2Export <- function(p2, path, format = NULL, overwrite = FALSE, ...) {
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(path))
    format <- if (identical(ext, "rds")) "rds" else ext
  }
  format <- tolower(format)
  if (format == "rds") {
    saveRDS(p2, file = path, ...)
    return(invisible(path))
  }
  if (format %in% c("h5ad", "anndata")) {
    return(.pagoda2_export_h5ad(p2, path = path, overwrite = overwrite, ...))
  }
  stop("Export format `", format, "` is not implemented yet")
}
