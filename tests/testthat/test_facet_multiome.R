library(pagoda2)

## scRNA + ATAC (10x multiome) end-to-end on REAL data: native import (Gene Expression -> RNA,
## Peaks -> ATAC/tfidf), per-facet reductions (RNA PCA + ATAC LSI), and WNN integration over the two.
## Skip-gated on the fixture (as with the CITE-seq WNN test); peaks subset for CI-friendly LSI runtime.
test_that("scRNA+ATAC multiome: import -> RNA PCA + ATAC LSI -> WNN -> clustering", {
  skip_if_not_installed("RcppHNSW")
  h5 <- Sys.getenv("P21_MULTIOME_10X_H5", "/home/pkharchenko/p21/lstar/testdata/multiome_10x/pbmc_granulocyte_sorted_3k.h5")
  skip_if(!file.exists(h5), "real 10x multiome fixture not present")

  p2 <- pagoda2:::.pagoda2_from_10x_h5_multimodal(h5, verbose = FALSE)
  # native feature_type -> facet mapping
  expect_setequal(p2$listFacets(), c("RNA", "ATAC"))
  rna <- p2$getFacet("RNA"); atac <- p2$getFacet("ATAC")
  expect_identical(rna$modelType, "plain");  expect_identical(rna$featureType, "gene")
  expect_identical(atac$modelType, "tfidf"); expect_identical(atac$featureType, "peak")
  expect_identical(atac$defaultReduction, "LSI")
  expect_gt(ncol(atac$rawCounts), 1000) # tens of thousands of peaks

  suppressWarnings({
    p2$runVariance(verbose = FALSE)
    p2$runReduction(nPcs = 20, verbose = FALSE)                 # RNA -> reductions[["PCA"]]
    top.peaks <- names(sort(Matrix::colSums(atac$rawCounts), decreasing = TRUE))[seq_len(min(12000L, ncol(atac$rawCounts)))]
    p2$runReduction(facet = "ATAC", method = "lsi", genes = top.peaks, nPcs = 20, verbose = FALSE)
  })
  expect_true("PCA" %in% names(p2$reductions))
  expect_true("ATAC:LSI" %in% names(p2$reductions))            # ATAC LSI stored under the qualified key
  expect_identical(nrow(p2$reductions[["ATAC:LSI"]]), nrow(p2$reductions[["PCA"]]))

  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ATAC"), verbose = FALSE))
  wr <- p2$cellMeta$wnn_weight_RNA; wa <- p2$cellMeta$wnn_weight_ATAC
  expect_equal(wr + wa, rep(1, length(wr)), tolerance = 1e-9)   # per-cell modality weights sum to 1
  expect_true(all(wr >= 0 & wr <= 1))
  g <- p2$graphs[["WNN"]]
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), nrow(p2$reductions[["PCA"]]))
  expect_identical(attr(g, "facets"), c("RNA", "ATAC"))
  expect_identical(attr(p2$reductions[["WNN"]], "input_axes"), c("genes", "peaks")) # lstar S5 provenance

  skip_if_not_installed("leidenAlg")
  p2$runClustering(graph = "WNN", name = "wnn_leiden")
  expect_gt(length(unique(stats::na.omit(p2$cellMeta$wnn_leiden))), 1L) # real PBMC multiome -> many clusters
})
