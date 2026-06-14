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
})
