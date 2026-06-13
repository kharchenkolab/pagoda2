library(pagoda2)

# Build a synthetic 10x CellRanger HDF5 holding two feature types (Gene Expression + Antibody Capture),
# the structure pagoda2's multimodal 10x reader splits into RNA + ADT facets.
write_synth_10x_h5 <- function(path) {
  set.seed(5)
  ng <- 10
  np <- 4
  nc <- 25
  rna <- matrix(rpois(ng * nc, 2), ng, nc)
  rna[1, ] <- rna[1, ] + 1L # gene1 expressed in every cell (no zero-depth cells)
  adt <- matrix(rpois(np * nc, 3), np, nc)
  adt[1, ] <- adt[1, ] + 1L # protein1 in every cell (ADT covers all cells)
  M <- as(Matrix::Matrix(rbind(rna, adt), sparse = TRUE), "CsparseMatrix") # features x cells
  h <- hdf5r::H5File$new(path, "w")
  g <- h$create_group("matrix")
  g[["data"]] <- as.numeric(M@x)
  g[["indices"]] <- as.integer(M@i) # 0-based row indices
  g[["indptr"]] <- as.integer(M@p)
  g[["shape"]] <- as.integer(dim(M))
  g[["barcodes"]] <- paste0("cell", seq_len(nc))
  fg <- g$create_group("features")
  fg[["id"]] <- c(paste0("ENSG", seq_len(ng)), paste0("AB", seq_len(np)))
  fg[["name"]] <- c(paste0("g", seq_len(ng)), paste0("P", seq_len(np)))
  fg[["feature_type"]] <- c(rep("Gene Expression", ng), rep("Antibody Capture", np))
  h$close_all()
  invisible(path)
}

test_that("native 10x CITE-seq H5 imports as RNA + ADT facets (no lstar required)", {
  skip_if_not_installed("hdf5r")
  path <- tempfile(fileext = ".h5")
  write_synth_10x_h5(path)
  on.exit(unlink(path))

  p2 <- pagoda2:::.pagoda2_from_10x_h5_multimodal(path, verbose = FALSE)
  expect_setequal(p2$listFacets(), c("RNA", "ADT"))
  expect_identical(length(p2$cells), 25L)
  expect_identical(ncol(p2$getFacet("RNA")$rawCounts), 10L) # genes
  adt <- p2$getFacet("ADT")
  expect_identical(ncol(adt$rawCounts), 4L) # proteins
  expect_identical(adt$modelType, "clr")
  expect_identical(adt$featureType, "protein")
  expect_identical(adt$defaultReduction, "PCA")
  expect_setequal(colnames(adt$rawCounts), c("P1", "P2", "P3", "P4"))

  # the imported ADT facet streams through the CLR kernel
  v <- p2$viewColMeanVar(facet = "ADT")
  expect_identical(nrow(v), 4L)
  expect_true(all(is.finite(v$m)) && all(is.finite(v$v)))
})
