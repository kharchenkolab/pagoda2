library(pagoda2)

lstar_lib <- Sys.getenv("P21_LSTAR_LIB", "/home/pkharchenko/p21/lstar/.Rlib")
if (dir.exists(file.path(lstar_lib, "lstar"))) {
  .libPaths(c(.libPaths(), lstar_lib))
}

test_that("pagoda2 -> lstar zarr -> pagoda2 is a fixed point on facet counts (multi-facet round-trip)", {
  skip_if_not_installed("lstar")
  set.seed(9)
  ng <- 12
  nc <- 25
  rna <- matrix(rpois(ng * nc, 3), ng, nc, dimnames = list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc))))
  p2 <- Pagoda2$new(as(Matrix::Matrix(rna, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 5
  a <- matrix(rpois(nc * np, 5) + 1L, nc, np, dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np))))
  p2$addFacet("ADT", as(Matrix::Matrix(a, sparse = TRUE), "dgCMatrix"), modelType = "clr", featureType = "protein")

  # embedding + clustering + cell metadata must also survive the round-trip (not just counts)
  umap <- matrix(rnorm(nc * 2), nc, 2, dimnames = list(p2$cells, c("d1", "d2")))
  p2$embeddings$PCA$UMAP <- umap
  lei <- factor(paste0("k", (seq_len(nc) - 1) %% 4)); names(lei) <- p2$cells
  p2$setGrouping("leiden", lei, setDefault = TRUE)
  dep <- as.numeric(Matrix::colSums(rna)); p2$cellMeta$depth <- dep

  path <- tempfile(fileext = ".lstar.zarr")
  p2$export(path, format = "lstar")
  expect_true(dir.exists(path) || file.exists(path)) # a .lstar.zarr store is a directory

  p3 <- pagoda2:::pagoda2FromLstar(path, verbose = FALSE)
  expect_setequal(p3$listFacets(), c("RNA", "ADT"))
  # counts are a fixed point through the lstar interchange
  expect_equal(as.matrix(p3$getFacet("RNA")$rawCounts), as.matrix(p2$getFacet("RNA")$rawCounts))
  expect_equal(as.matrix(p3$getFacet("ADT")$rawCounts), as.matrix(p2$getFacet("ADT")$rawCounts))
  # facet conventions survive (provenance)
  expect_identical(p3$getFacet("ADT")$modelType, "clr")
  expect_identical(p3$getFacet("ADT")$featureType, "protein")
  expect_identical(p3$cells, p2$cells)
  # embedding, clustering, and metadata survive (the lstar read_pagoda2 / fromLstar restoration)
  expect_false(is.null(p3$embeddings$PCA$UMAP))
  expect_equal(p3$embeddings$PCA$UMAP[p2$cells, ], umap[p2$cells, ])
  expect_equal(as.character(p3$cellMeta[p2$cells, "leiden"]), as.character(p2$cellMeta[p2$cells, "leiden"]))
  expect_equal(p3$cellMeta[p2$cells, "depth"], dep)
})

# A viewer-extended store carries viewer@0.1 `cache` navigators (counts_cellmajor + stats/markers/od).
# The `cache`-tagged counts_cellmajor is a raw cells x genes measure, so without the cache guard
# fromLstar would mistake it for a second RNA facet. It must be skipped.
test_that("fromLstar skips viewer@0.1 cache navigators (counts_cellmajor is not a phantom facet)", {
  skip_if_not_installed("lstar")
  set.seed(11); ng <- 14; nc <- 30
  rna <- matrix(rpois(ng * nc, 3), ng, nc, dimnames = list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc))))
  p2 <- Pagoda2$new(as(Matrix::Matrix(rna, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  p2$setGrouping("leiden", factor(paste0("k", (seq_len(nc) - 1) %% 4)), setDefault = TRUE)
  path <- tempfile(fileext = ".lstar.zarr")
  p2$export(path, format = "lstar")
  lstar::viewer_extend(path)                 # add the viewer@0.1 cache navigators (in place)
  p3 <- pagoda2:::pagoda2FromLstar(path, verbose = FALSE)
  expect_setequal(p3$listFacets(), "RNA")    # counts_cellmajor (cache) is NOT imported as a 2nd facet
  expect_equal(as.matrix(p3$getFacet("RNA")$rawCounts), as.matrix(p2$getFacet("RNA")$rawCounts))
})
