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
  expect_equal(Seurat::Embeddings(seu, "pca_umap"), emb)
})
