library(pagoda2)

test_that("disk-backed (BPCells) facet viewColMeanVar matches its in-memory twin (§8.6 out-of-core seam)", {
  skip_if_not_installed("BPCells")
  set.seed(3)
  ng <- 12
  nc <- 40
  rna <- matrix(rpois(ng * nc, 3), ng, nc, dimnames = list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc))))
  p2 <- Pagoda2$new(as(Matrix::Matrix(rna, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 8
  a <- matrix(rpois(nc * np, 5) + 1L, nc, np, dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np))))
  am <- as(Matrix::Matrix(a, sparse = TRUE), "dgCMatrix")

  p2$addFacet("ADTmem", am, modelType = "plain", featureType = "protein") # in-memory
  p2$addFacet("ADTdisk", am, modelType = "plain", featureType = "protein", backend = "bpcells") # on-disk

  expect_identical(p2$getFacet("ADTmem")$backend, "memory")
  expect_identical(p2$getFacet("ADTdisk")$backend, "bpcells")
  expect_null(p2$getFacet("ADTdisk")$rawCounts) # data lives on disk, not in memory

  vm <- p2$viewColMeanVar(facet = "ADTmem")
  vd <- p2$viewColMeanVar(facet = "ADTdisk")
  expect_equal(vd$m, vm$m, tolerance = 1e-6) # identical normalized math, different storage
  expect_equal(vd$v, vm$v, tolerance = 1e-6)
})
