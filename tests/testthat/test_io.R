library(pagoda2)

write_10x_triplet <- function(path, matrix, prefix = "", version = "V3", gzip = FALSE) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  named <- nzchar(prefix)
  matrix.file <- file.path(path, if (named) paste0(prefix, ".matrix.mtx") else "matrix.mtx")
  barcode.file <- file.path(path, if (named) paste0(prefix, ".barcodes.tsv") else "barcodes.tsv")
  feature.file <- file.path(path, if (version == "V3") {
    if (named) paste0(prefix, ".features.tsv") else "features.tsv"
  } else {
    if (named) paste0(prefix, ".genes.tsv") else "genes.tsv"
  })

  Matrix::writeMM(matrix, matrix.file)
  if (version == "V3") {
    write.table(
      data.frame(
        id = paste0("ens", seq_len(nrow(matrix))),
        symbol = rownames(matrix),
        type = "Gene Expression"
      ),
      feature.file,
      quote = FALSE,
      sep = "\t",
      row.names = FALSE,
      col.names = FALSE
    )
  } else {
    write.table(
      data.frame(id = paste0("ens", seq_len(nrow(matrix))), symbol = rownames(matrix)),
      feature.file,
      quote = FALSE,
      sep = "\t",
      row.names = FALSE,
      col.names = FALSE
    )
  }
  write.table(
    colnames(matrix),
    barcode.file,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE
  )
  if (gzip) {
    matrix.file <- R.utils::gzip(matrix.file, overwrite = TRUE)
    barcode.file <- R.utils::gzip(barcode.file, overwrite = TRUE)
    feature.file <- R.utils::gzip(feature.file, overwrite = TRUE)
  }
  invisible(c(matrix = matrix.file, barcodes = barcode.file, features = feature.file))
}

write_h5_strings <- function(group, name, values) {
  group$create_dataset(
    name,
    robj = as.character(values),
    dtype = hdf5r::h5types$H5T_STRING$new(size = Inf)
  )
}

write_h5_sparse_csc <- function(group, matrix) {
  matrix <- as(matrix, "CsparseMatrix")
  group$create_dataset("data", robj = as.numeric(matrix@x))
  group$create_dataset("indices", robj = as.integer(matrix@i))
  group$create_dataset("indptr", robj = as.integer(matrix@p))
  group$create_attr("dims", as.integer(dim(matrix)))
  invisible(group)
}

write_h5_sparse_csr <- function(group, matrix) {
  matrix <- as(matrix, "RsparseMatrix")
  group$create_dataset("data", robj = as.numeric(matrix@x))
  group$create_dataset("indices", robj = as.integer(matrix@j))
  group$create_dataset("indptr", robj = as.integer(matrix@p))
  group$create_attr("shape", as.integer(dim(matrix)))
  group$create_attr("encoding-type", "csr_matrix")
  invisible(group)
}

write_h5_dataframe <- function(group, index, columns) {
  write_h5_strings(group, "_index", index)
  group$create_attr("_index", "_index")
  column.names <- names(columns)
  if (is.null(column.names)) {
    column.names <- character()
  }
  if (length(column.names) > 0) {
    group$create_attr("column-order", column.names)
  }
  for (name in names(columns)) {
    value <- columns[[name]]
    if (is.numeric(value) || is.integer(value)) {
      group$create_dataset(name, robj = value)
    } else {
      write_h5_strings(group, name, value)
    }
  }
}

write_cellranger_h5 <- function(path, matrix) {
  h5 <- hdf5r::H5File$new(path, mode = "w")
  on.exit(h5$close_all())
  g <- h5$create_group("matrix")
  matrix <- as(matrix, "CsparseMatrix")
  g$create_dataset("data", robj = as.numeric(matrix@x))
  g$create_dataset("indices", robj = as.integer(matrix@i))
  g$create_dataset("indptr", robj = as.integer(matrix@p))
  g$create_dataset("shape", robj = as.integer(dim(matrix)))
  write_h5_strings(g, "barcodes", colnames(matrix))
  features <- g$create_group("features")
  write_h5_strings(features, "id", paste0("ens", seq_len(nrow(matrix))))
  write_h5_strings(features, "name", rownames(matrix))
  write_h5_strings(features, "feature_type", rep("Gene Expression", nrow(matrix)))
  invisible(path)
}

write_h5ad_file <- function(path, matrix) {
  h5 <- hdf5r::H5File$new(path, mode = "w")
  on.exit(h5$close_all())
  write_h5_sparse_csr(h5$create_group("X"), Matrix::t(matrix))
  layers <- h5$create_group("layers")
  write_h5_sparse_csr(layers$create_group("counts"), Matrix::t(matrix))
  write_h5_dataframe(
    h5$create_group("obs"),
    index = colnames(matrix),
    columns = list(sample = rep("sampleA", ncol(matrix)))
  )
  write_h5_dataframe(
    h5$create_group("var"),
    index = paste0("ens", seq_len(nrow(matrix))),
    columns = list(
      gene_id = paste0("ens", seq_len(nrow(matrix))),
      gene_symbol = rownames(matrix),
      feature_type = rep("Gene Expression", nrow(matrix))
    )
  )
  invisible(path)
}

write_h5seurat_file <- function(path, matrix) {
  h5 <- hdf5r::H5File$new(path, mode = "w")
  on.exit(h5$close_all())
  h5$create_attr("active.assay", "RNA")
  write_h5_strings(h5, "cell.names", colnames(matrix))
  rna <- h5$create_group("assays")$create_group("RNA")
  write_h5_strings(rna, "features", rownames(matrix))
  write_h5_sparse_csc(rna$create_group("counts"), matrix)
  write_h5_dataframe(
    h5$create_group("meta.data"),
    index = colnames(matrix),
    columns = list(sample = rep("sampleA", ncol(matrix)))
  )
  write_h5_dataframe(
    rna$create_group("meta.data"),
    index = rownames(matrix),
    columns = list()
  )
  invisible(path)
}

write_loom_file <- function(path, matrix, transpose = FALSE) {
  h5 <- hdf5r::H5File$new(path, mode = "w")
  on.exit(h5$close_all())
  h5$create_group("attrs")
  h5$create_group("row_graphs")
  h5$create_group("col_graphs")
  layers <- h5$create_group("layers")
  row.attrs <- h5$create_group("row_attrs")
  col.attrs <- h5$create_group("col_attrs")
  h5[["attrs"]]$create_dataset("LOOM_SPEC_VERSION", robj = "3.0.0")

  stored <- if (transpose) Matrix::t(matrix) else matrix
  h5$create_dataset("matrix", robj = as.matrix(stored))
  layers$create_dataset("counts", robj = as.matrix(stored))
  write_h5_strings(row.attrs, "Gene", rownames(matrix))
  write_h5_strings(row.attrs, "Accession", paste0("ens", seq_len(nrow(matrix))))
  write_h5_strings(col.attrs, "CellID", colnames(matrix))
  write_h5_strings(col.attrs, "sample", rep("sampleA", ncol(matrix)))
  invisible(path)
}

make_io_matrix <- function() {
  cm <- Matrix::Matrix(
    c(
      1, 0, 2,
      0, 3, 0,
      5, 0, 1
    ),
    nrow = 3,
    ncol = 3,
    sparse = TRUE
  )
  rownames(cm) <- c("geneA", "geneB", "geneC")
  colnames(cm) <- c("cell1", "cell2", "cell3")
  cm
}

test_that("readCounts reads canonical 10x V3 and preserves cell names", {
  cm <- make_io_matrix()
  td <- tempfile("p2_readcounts")
  write_10x_triplet(td, cm, version = "V3")

  counts <- readCounts(td, format = "10x", verbose = FALSE)

  expect_true(inherits(counts, "dgCMatrix"))
  expect_identical(rownames(counts), rownames(cm))
  expect_identical(colnames(counts), colnames(cm))
  expect_equal(as.matrix(counts), as.matrix(cm))
  expect_true(all(c("gene_id", "gene_symbol", "feature_type") %in% colnames(attr(counts, "pagoda2.geneMeta"))))
})

test_that("readCounts autodetects renamed gzipped GEO-style 10x triplets", {
  cm <- make_io_matrix()
  td <- tempfile("p2_readcounts_geo")
  write_10x_triplet(td, cm, prefix = "GSM000_sample", version = "V3", gzip = TRUE)

  counts <- readCounts(td, format = "auto", verbose = FALSE)

  expect_identical(colnames(counts), colnames(cm))
  expect_equal(as.matrix(counts), as.matrix(cm))
})

test_that("readCounts supports explicit cell prefixes and sample metadata", {
  cm <- make_io_matrix()
  td <- tempfile("p2_readcounts_prefix")
  write_10x_triplet(td, cm, version = "V2")

  imported <- readCounts(
    td,
    format = "10x",
    version = "V2",
    cell.prefix = "sampleA",
    sample.name = "sampleA",
    return.metadata = TRUE,
    verbose = FALSE
  )

  expect_identical(colnames(imported$counts), paste0("sampleA_", colnames(cm)))
  expect_identical(as.character(imported$cellMeta$sample), rep("sampleA", ncol(cm)))
})

test_that("readCounts handles duplicate genes only when requested", {
  cm <- make_io_matrix()
  rownames(cm)[3] <- "geneA"
  td <- tempfile("p2_readcounts_dups")
  write_10x_triplet(td, cm, version = "V3")

  expect_warning(readCounts(td, verbose = FALSE), "duplicates")
  counts <- readCounts(td, make.unique.genes = TRUE, verbose = FALSE)
  expect_false(anyDuplicated(rownames(counts)) > 0)
})

test_that("readCounts validates integer counts", {
  cm <- make_io_matrix()
  cm@x[1] <- 1.5
  td <- tempfile("p2_readcounts_nonint")
  write_10x_triplet(td, cm, version = "V3")

  expect_error(readCounts(td, verbose = FALSE), "non-integer")
  expect_silent(readCounts(td, validate.integer = FALSE, verbose = FALSE))
})

test_that("readCounts requires explicit sample selection for multiple triplets", {
  cm <- make_io_matrix()
  td <- tempfile("p2_readcounts_multi")
  write_10x_triplet(td, cm, prefix = "sampleA", version = "V3")
  write_10x_triplet(td, cm, prefix = "sampleB", version = "V3")

  expect_error(readCounts(td, verbose = FALSE), "Multiple 10x triplets")
  counts <- readCounts(td, sample.pattern = "sampleB", verbose = FALSE)
  expect_equal(as.matrix(counts), as.matrix(cm))
})

test_that("readCounts autodetects CellRanger HDF5 files", {
  cm <- make_io_matrix()
  path <- tempfile(fileext = ".h5")
  write_cellranger_h5(path, cm)

  counts <- readCounts(path, format = "auto", verbose = FALSE)

  expect_identical(rownames(counts), rownames(cm))
  expect_identical(colnames(counts), colnames(cm))
  expect_equal(as.matrix(counts), as.matrix(cm))
})

test_that("readCounts autodetects loom files and reads attributes", {
  cm <- make_io_matrix()
  path <- tempfile(fileext = ".loom")
  write_loom_file(path, cm)

  imported <- readCounts(path, format = "auto", return.metadata = TRUE, verbose = FALSE)

  expect_true(inherits(imported$counts, "dgCMatrix"))
  expect_identical(rownames(imported$counts), rownames(cm))
  expect_identical(colnames(imported$counts), colnames(cm))
  expect_equal(as.matrix(imported$counts), as.matrix(cm))
  expect_identical(as.character(imported$cellMeta$sample), rep("sampleA", ncol(cm)))
  expect_true(all(c("Gene", "Accession", "gene_id", "gene_symbol") %in% colnames(imported$geneMeta)))
})

test_that("readCounts reads loom layers and transposed loom matrices", {
  cm <- make_io_matrix()[1:2, , drop = FALSE]
  path <- tempfile(fileext = ".loom")
  write_loom_file(path, cm, transpose = TRUE)

  counts <- readCounts(path, format = "loom", layer = "counts", chunk.size = 1L, verbose = FALSE)

  expect_identical(rownames(counts), rownames(cm))
  expect_identical(colnames(counts), colnames(cm))
  expect_equal(as.matrix(counts), as.matrix(cm))
})

test_that("readCounts autodetects h5ad files without reticulate", {
  cm <- make_io_matrix()
  path <- tempfile(fileext = ".h5ad")
  write_h5ad_file(path, cm)

  imported <- readCounts(path, format = "auto", return.metadata = TRUE, verbose = FALSE)

  expect_identical(rownames(imported$counts), rownames(cm))
  expect_identical(colnames(imported$counts), colnames(cm))
  expect_equal(as.matrix(imported$counts), as.matrix(cm))
  expect_identical(as.character(imported$cellMeta$sample), rep("sampleA", ncol(cm)))
  expect_true(all(c("gene_id", "gene_symbol", "feature_type") %in% colnames(imported$geneMeta)))
})

test_that("readCounts autodetects h5Seurat files without Seurat", {
  cm <- make_io_matrix()
  path <- tempfile(fileext = ".h5Seurat")
  write_h5seurat_file(path, cm)

  counts <- readCounts(path, format = "auto", verbose = FALSE)

  expect_identical(rownames(counts), rownames(cm))
  expect_identical(colnames(counts), colnames(cm))
  expect_equal(as.matrix(counts), as.matrix(cm))
})

test_that("Pagoda2$from constructs objects from 10x paths and records metadata", {
  cm <- make_io_matrix()
  td <- tempfile("p2_from")
  write_10x_triplet(td, cm, version = "V3")

  p2 <- Pagoda2$from(
    td,
    reader.args = list(sample.name = "sampleA"),
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = FALSE,
    trim = 0
  )

  expect_true(inherits(p2, "Pagoda2"))
  expect_identical(rownames(p2$cellMeta), colnames(cm))
  expect_identical(as.character(p2$cellMeta$sample), rep("sampleA", ncol(cm)))
  expect_true(all(c("gene_id", "gene_symbol") %in% colnames(p2$geneMeta)))
})

test_that("specific Pagoda2 file constructors call fixed format readers", {
  cm <- make_io_matrix()
  path <- tempfile(fileext = ".h5ad")
  write_h5ad_file(path, cm)

  p2 <- Pagoda2$fromAnnData(
    path,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = FALSE,
    trim = 0
  )

  expect_true(inherits(p2, "Pagoda2"))
  expect_identical(rownames(p2$cellMeta), colnames(cm))
  expect_identical(as.character(p2$cellMeta$sample), rep("sampleA", ncol(cm)))
})

test_that("Pagoda2$fromLoom constructs objects from loom files", {
  cm <- make_io_matrix()
  path <- tempfile(fileext = ".loom")
  write_loom_file(path, cm)

  p2 <- Pagoda2$fromLoom(
    path,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = FALSE,
    trim = 0
  )

  expect_true(inherits(p2, "Pagoda2"))
  expect_identical(rownames(p2$cellMeta), colnames(cm))
  expect_identical(rownames(p2$geneMeta), rownames(cm))
  expect_identical(as.character(p2$cellMeta$sample), rep("sampleA", ncol(cm)))
})

test_that("Pagoda2 as list and RDS export preserve core axes", {
  cm <- make_io_matrix()
  p2 <- Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = FALSE,
    trim = 0
  )

  out <- p2$as("list")
  expect_identical(rownames(out$counts), rownames(cm))
  expect_identical(colnames(out$counts), colnames(cm))

  path <- tempfile(fileext = ".rds")
  expect_silent(p2$export(path))
  expect_true(file.exists(path))
  expect_true(inherits(readRDS(path), "Pagoda2"))
})

test_that("Pagoda2 as Seurat uses gene-by-cell counts when Seurat is available", {
  testthat::skip_if_not_installed("Seurat")

  cm <- make_io_matrix()
  p2 <- Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = FALSE,
    trim = 0
  )

  seu <- p2$as("seurat")
  expect_true(inherits(seu, "Seurat"))
  expect_identical(colnames(seu), colnames(cm))
  expect_identical(rownames(seu), rownames(cm))
})

test_that("Pagoda2 as Seurat carries named embeddings when available", {
  testthat::skip_if_not_installed("Seurat")

  cm <- make_io_matrix()
  p2 <- Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    log.scale = FALSE,
    trim = 0
  )
  emb <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  rownames(emb) <- colnames(cm)
  colnames(emb) <- c("UMAP_1", "UMAP_2")
  p2$embeddings <- list(PCA = list(UMAP = emb))

  seu <- p2$as("seurat")

  expect_true("pca_umap" %in% names(seu@reductions))
  expect_equal(get("Embeddings", envir = asNamespace("Seurat"))(seu, "pca_umap"), emb)
})
