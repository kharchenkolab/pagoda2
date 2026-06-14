library(pagoda2)

# lstar is a sister package (built in its own lib); make it discoverable if present, else skip (CI-safe).
lstar_lib <- Sys.getenv("P21_LSTAR_LIB", "/home/pkharchenko/p21/lstar/.Rlib")
if (dir.exists(file.path(lstar_lib, "lstar"))) {
  .libPaths(c(.libPaths(), lstar_lib))
}

test_that("disk-backed (lstar zarr) facet viewColMeanVar matches its in-memory twin (§8.6 out-of-core seam)", {
  skip_if_not_installed("lstar")
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
  p2$addFacet("ADTdisk", am, modelType = "plain", featureType = "protein", backend = "lstar") # on-disk lstar zarr

  expect_identical(p2$getFacet("ADTmem")$backend, "memory")
  expect_identical(p2$getFacet("ADTdisk")$backend, "lstar")
  expect_true(file.exists(p2$getFacet("ADTdisk")$store)) # data lives in an lstar store on disk
  expect_null(p2$getFacet("ADTdisk")$rawCounts) # not in memory

  vm <- p2$viewColMeanVar(facet = "ADTmem")
  vd <- p2$viewColMeanVar(facet = "ADTdisk")
  expect_equal(vd$m, vm$m, tolerance = 1e-8) # identical normalized math, different storage
  expect_equal(vd$v, vm$v, tolerance = 1e-8)
})

test_that("disk-backed (lstar) getExpressionBlock + viewColSumByFac match the in-memory twin", {
  skip_if_not_installed("lstar")
  set.seed(4)
  ng <- 10
  nc <- 30
  rna <- matrix(rpois(ng * nc, 3), ng, nc, dimnames = list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc))))
  p2 <- Pagoda2$new(as(Matrix::Matrix(rna, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 6
  a <- matrix(rpois(nc * np, 4) + 1L, nc, np, dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np))))
  am <- as(Matrix::Matrix(a, sparse = TRUE), "dgCMatrix")
  p2$addFacet("ADTmem", am, modelType = "plain", featureType = "protein")
  p2$addFacet("ADTdisk", am, modelType = "plain", featureType = "protein", backend = "lstar")

  # block read (materialized analysis view) off disk vs in-memory
  bm <- as.matrix(p2$getExpressionBlock(facet = "ADTmem"))
  bd <- as.matrix(p2$getExpressionBlock(facet = "ADTdisk"))
  expect_equal(bd, bm, tolerance = 1e-8)

  # streaming grouped sums (pseudobulk) off disk vs in-memory
  grp <- factor(rep(c("a", "b"), length.out = nc))
  names(grp) <- paste0("c", seq_len(nc))
  sm <- p2$viewColSumByFac(groups = grp, facet = "ADTmem")
  sd <- p2$viewColSumByFac(groups = grp, facet = "ADTdisk")
  expect_equal(as.matrix(sd), as.matrix(sm), tolerance = 1e-8)
})
