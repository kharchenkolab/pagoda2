library(pagoda2)

# CLR (centered log-ratio over each cell's nonzero features, §6.2), reference implementation.
clr_reference <- function(counts) { # counts: cells x features (dense, all positive here)
  lx <- log1p(counts)
  out <- matrix(0, nrow(counts), ncol(counts), dimnames = dimnames(counts))
  for (i in seq_len(nrow(counts))) {
    nz <- counts[i, ] > 0
    s <- mean(lx[i, nz])
    out[i, nz] <- lx[i, nz] - s
  }
  out
}

clr_p2 <- function() {
  cm <- Matrix::Matrix(
    matrix(c(5, 0, 2, 1, 0, 4, 1, 3, 3, 0, 6, 2, 1, 2, 0, 5), nrow = 4, byrow = TRUE),
    sparse = TRUE, dimnames = list(paste0("g", 1:4), paste0("c", 1:4))
  )
  p2 <- Pagoda2$new(as(cm, "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  adt <- matrix(c(10, 2, 5, 3, 7, 4, 8, 1, 6, 4, 9, 2), nrow = 4, byrow = TRUE,
    dimnames = list(paste0("c", 1:4), c("CD3", "CD4", "CD8")))
  p2$addFacet("ADT", as(Matrix::Matrix(adt, sparse = TRUE), "dgCMatrix"),
    modelType = "clr", featureType = "protein")
  list(p2 = p2, adt = adt)
}

test_that("CLR view model materializes to the reference (float64)", {
  o <- clr_p2()
  mat <- as.matrix(o$p2$getExpressionBlock(facet = "ADT")) # materialized CLR, cells x proteins
  ref <- clr_reference(o$adt)
  expect_equal(mat, ref, tolerance = 1e-12)
})

test_that("CLR per-protein mean/var match the dense reference; thread-count invariant (C++ kernel)", {
  o <- clr_p2()
  ref <- clr_reference(o$adt)
  v1 <- o$p2$viewColMeanVar(facet = "ADT", n.cores = 1)
  v4 <- o$p2$viewColMeanVar(facet = "ADT", n.cores = 4)
  expect_identical(v1, v4) # bit-identical across thread counts (column-parallel kernel)
  popvar <- function(x) mean((x - mean(x))^2) # kernel uses population variance (/n)
  expect_equal(v1$m, as.numeric(colMeans(ref)), tolerance = 1e-10)
  expect_equal(v1$v, as.numeric(apply(ref, 2, popvar)), tolerance = 1e-10)
})

test_that("a CLR facet does not perturb the default RNA plain view", {
  o <- clr_p2()
  expect_identical(o$p2$getMatrixView("analysis", facet = "RNA")$model, "plain")
  expect_identical(o$p2$getMatrixView("analysis", facet = "ADT")$model, "clr")
})

test_that("subsetting genes keeps the CLR over the FULL feature axis (not collapsed to the subset)", {
  o <- clr_p2()
  full <- as.matrix(o$p2$getExpressionBlock(facet = "ADT"))            # all proteins
  # a single-protein request must equal that protein's column of the full CLR block, NOT log1p-centered
  # over one feature (which would be 0). This guards the marker/plotEmbedding(gene=, facet=) path.
  one <- o$p2$getExpressionBlock(genes = "CD3", facet = "ADT")[, "CD3"]
  expect_equal(unname(one), unname(full[names(one), "CD3"]), tolerance = 1e-12)
  expect_gt(stats::sd(one), 0)                                          # not all-zero
  two <- as.matrix(o$p2$getExpressionBlock(genes = c("CD4", "CD8"), facet = "ADT"))
  expect_equal(two, full[rownames(two), c("CD4", "CD8")], tolerance = 1e-12)
})
