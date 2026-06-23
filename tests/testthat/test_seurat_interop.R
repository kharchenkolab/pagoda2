library(pagoda2)

# Seurat interop tests. Seurat is NOT a pagoda2 dependency (not in DESCRIPTION) by design, so this
# file is .Rbuildignore'd to keep it out of the CRAN tarball and avoid an "unstated dependencies in
# tests" check. Run it locally with Seurat/SeuratObject installed.

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

test_that("Pagoda2 as Seurat carries normalized data and feature metadata", {
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
  p2$setGeneMeta(data.frame(
    gene_id = paste0("ens", seq_len(nrow(cm))),
    gene_symbol = rownames(cm),
    row.names = rownames(cm)
  ))

  seu <- p2$as("seurat")
  data <- get("LayerData", envir = asNamespace("SeuratObject"))(seu, assay = "RNA", layer = "data")

  expect_equal(as.matrix(data), as.matrix(Matrix::t(p2$getExpressionBlock())))
  expect_true("gene_id" %in% colnames(seu[["RNA"]]@meta.data))
  expect_identical(as.character(seu[["RNA"]]@meta.data$gene_symbol), rownames(cm))
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

test_that("vs-Seurat: per-cell WNN weights rank-correlate with Seurat::FindMultiModalNeighbors (real CITE-seq)", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("RcppHNSW")
  h5 <- Sys.getenv("P21_CITESEQ_10X_H5", "/home/pkharchenko/p21/lstar/testdata/citeseq_10x/pbmc_1k_protein_v3.h5")
  skip_if(!file.exists(h5), "real 10x CITE-seq fixture not present")

  # pagoda2 WNN on real RNA+ADT
  p2 <- pagoda2:::.pagoda2_from_10x_h5_multimodal(h5, verbose = FALSE)
  suppressWarnings({
    p2$runVariance(verbose = FALSE)
    p2$runReduction(nPcs = 20, verbose = FALSE)
    p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(facet = "ADT", nPcs = 10, verbose = FALSE)
    p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), verbose = FALSE)
  })
  rna <- p2$reductions[["PCA"]]
  adtr <- p2$reductions[["ADT:PCA"]]
  cells <- rownames(rna)
  colnames(rna) <- paste0("rnapca_", seq_len(ncol(rna)))
  colnames(adtr) <- paste0("adtpca_", seq_len(ncol(adtr)))

  # Seurat WNN on the SAME reductions (fair algorithm comparison, not a preprocessing comparison)
  so <- suppressWarnings(SeuratObject::CreateSeuratObject(counts = Matrix::t(as(p2$getFacet("RNA")$rawCounts, "CsparseMatrix"))))
  so[["ADT"]] <- suppressWarnings(SeuratObject::CreateAssayObject(counts = Matrix::t(as(p2$getFacet("ADT")$rawCounts, "CsparseMatrix"))))
  so[["rnapca"]] <- SeuratObject::CreateDimReducObject(embeddings = rna[colnames(so), ], key = "rnapca_", assay = "RNA")
  so[["adtpca"]] <- SeuratObject::CreateDimReducObject(embeddings = adtr[colnames(so), ], key = "adtpca_", assay = "ADT")
  so <- suppressWarnings(Seurat::FindMultiModalNeighbors(so,
    reduction.list = list("rnapca", "adtpca"),
    dims.list = list(seq_len(ncol(rna)), seq_len(ncol(adtr))), verbose = FALSE))

  sr <- stats::setNames(so@meta.data[["RNA.weight"]], rownames(so@meta.data))
  pr <- stats::setNames(p2$cellMeta[cells, "wnn_weight_RNA"], cells)
  co <- intersect(cells, names(sr))
  rho <- suppressWarnings(stats::cor(pr[co], sr[co], method = "spearman"))
  expect_gt(rho, 0.2) # pagoda2 and Seurat WNN agree on which cells favor RNA (independent algorithms)
})
