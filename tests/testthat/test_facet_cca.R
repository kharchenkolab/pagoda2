library(pagoda2)

## RNA + ADT that SHARE a latent grouping (group 2 up in the first half of each modality's features),
## plus independent noise. A faithful CCA must recover that shared axis as the top canonical pair.
## A 3rd noise facet (HTO) is optional, for the two-block guard.
build_cca_p2 <- function(extra = FALSE) {
  set.seed(42)
  nc <- 80
  grp <- rep(1:2, each = nc / 2)
  ng <- 40
  rna <- matrix(rpois(ng * nc, 2), ng, nc)
  rna[1:20, grp == 2] <- rna[1:20, grp == 2] + rpois(20 * (nc / 2), 10) # shared signal
  dimnames(rna) <- list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc)))
  p2 <- Pagoda2$new(as(Matrix::Matrix(rna, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 12
  adt <- matrix(rpois(nc * np, 3) + 1L, nc, np)
  adt[grp == 2, 1:6] <- adt[grp == 2, 1:6] + rpois((nc / 2) * 6, 12) # SAME grouping as RNA
  dimnames(adt) <- list(paste0("c", seq_len(nc)), paste0("P", seq_len(np)))
  p2$addFacet("ADT", as(Matrix::Matrix(adt, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")
  suppressWarnings({
    p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
    p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
  })
  if (extra) {
    nh <- 8
    hto <- matrix(rpois(nc * nh, 3) + 1L, nc, nh, dimnames = list(paste0("c", seq_len(nc)), paste0("H", seq_len(nh))))
    p2$addFacet("HTO", as(Matrix::Matrix(hto, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "feature")
    suppressWarnings(p2$runVariance(facet = "HTO", use.raw.variance = TRUE, verbose = FALSE))
  }
  list(p2 = p2, grp = grp)
}

test_that("CCA recovers the shared latent: top canonical pair separates the groups", {
  fx <- build_cca_p2()
  p2 <- fx$p2
  grp <- fx$grp
  suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), method = "cca", nPcs = 5, verbose = FALSE))
  expect_true("CCA" %in% names(p2$reductions))
  z <- p2$reductions[["CCA"]]
  expect_identical(nrow(z), 80L)
  expect_true(ncol(z) >= 1L && ncol(z) <= 5L)
  cc <- attr(z, "cancor")
  expect_equal(cc, sort(cc, decreasing = TRUE)) # singular values -> monotone decreasing
  # the shared structure is the dominant cross-modality covariance -> CC1 separates the groups
  expect_lt(stats::t.test(z[, 1] ~ grp)$p.value, 1e-6)
})

test_that("CCA emits per-facet feature loadings on the right axes (lstar shared-factor-axis)", {
  fx <- build_cca_p2()
  p2 <- fx$p2
  suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), method = "cca", nPcs = 5, verbose = FALSE))
  z <- p2$reductions[["CCA"]]
  lu <- p2$getFacet("RNA")$loadings[["CCA"]]
  lv <- p2$getFacet("ADT")$loadings[["CCA"]]
  expect_equal(ncol(lu), ncol(z))
  expect_equal(ncol(lv), ncol(z))
  expect_true(all(grepl("^g", rownames(lu)))) # gene loadings
  expect_true(all(grepl("^P", rownames(lv)))) # protein loadings
})

test_that("CCA stores §4.5 named-product provenance (facets, feature axes, method, canonical assoc)", {
  fx <- build_cca_p2()
  p2 <- fx$p2
  suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), method = "cca", nPcs = 5, verbose = FALSE))
  z <- p2$reductions[["CCA"]]
  expect_identical(attr(z, "facets"), c("RNA", "ADT"))
  expect_identical(attr(z, "input_axes"), c("genes", "proteins"))
  expect_identical(attr(z, "method"), "joint:cca")
  expect_true(is.numeric(attr(z, "cancor")) && length(attr(z, "cancor")) == ncol(z))
})

test_that("CCA respects the no-shadow rule and the two-block constraint", {
  fx <- build_cca_p2(extra = TRUE)
  p2 <- fx$p2
  expect_error(suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), method = "cca", name = "PCA")), "collides")
  expect_error(
    suppressWarnings(p2$runReduction(facets = c("RNA", "ADT", "HTO"), method = "cca")),
    "two-block"
  )
})

test_that("CCA matches a direct base-R centered cross-covariance SVD (correctness)", {
  fx <- build_cca_p2()
  p2 <- fx$p2
  suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), method = "cca", nPcs = 5, var.scale = FALSE, verbose = FALSE))
  z <- p2$reductions[["CCA"]]
  k <- ncol(z)
  X1 <- as.matrix(p2$getExpressionBlock(genes = p2$getFacet("RNA")$odgenes, facet = "RNA"))
  X2 <- as.matrix(p2$getExpressionBlock(genes = p2$getFacet("ADT")$odgenes, facet = "ADT"))
  common <- intersect(rownames(X1), rownames(X2))
  X1 <- X1[common, , drop = FALSE]
  X2 <- X2[common, , drop = FALSE]
  C <- crossprod(scale(X1, scale = FALSE), scale(X2, scale = FALSE))
  sv <- svd(C)
  cc_ref <- sv$d[seq_len(k)] / (length(common) - 1L)
  expect_equal(as.numeric(attr(z, "cancor")), cc_ref, tolerance = 1e-4)
  s1 <- scale(X1, scale = FALSE) %*% sv$u[, seq_len(k), drop = FALSE]
  s2 <- scale(X2, scale = FALSE) %*% sv$v[, seq_len(k), drop = FALSE]
  zref <- (s1 + s2) / 2
  for (j in seq_len(k)) {
    expect_gt(abs(stats::cor(z[common, j], zref[, j])), 0.999) # match up to per-component sign
  }
})

test_that("the CCA reduction drives downstream runGraph + clustering (named product behaves like any reduction)", {
  skip_if_not_installed("N2R")
  fx <- build_cca_p2()
  p2 <- fx$p2
  suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), method = "cca", nPcs = 5, verbose = FALSE))
  suppressWarnings(p2$runGraph(reduction = "CCA", verbose = FALSE)) # explicit reduction -> no auto-WNN
  expect_true("CCA" %in% names(p2$graphs))
  expect_false("WNN" %in% names(p2$graphs))
  skip_if_not_installed("leidenAlg")
  p2$runClustering(graph = "CCA", name = "cca_leiden")
  expect_gt(length(unique(stats::na.omit(p2$cellMeta$cca_leiden))), 1L)
})

test_that("sparse-CCA (PMA) yields sparse feature loadings under the same named product", {
  skip_if_not_installed("PMA")
  fx <- build_cca_p2()
  p2 <- fx$p2
  suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), method = "scca", nPcs = 5, penalty = 0.3, verbose = FALSE))
  z <- p2$reductions[["CCA"]]
  expect_identical(attr(z, "method"), "joint:scca")
  lu <- p2$getFacet("RNA")$loadings[["CCA"]]
  expect_equal(ncol(lu), ncol(z))
  expect_gt(mean(lu == 0), 0) # L1 penalty zeroes some feature loadings
  # the `sparse=` flag on method="cca" routes to the same place
  p2b <- build_cca_p2()$p2
  suppressWarnings(p2b$runReduction(facets = c("RNA", "ADT"), method = "cca", sparse = TRUE, nPcs = 5, verbose = FALSE))
  expect_identical(attr(p2b$reductions[["CCA"]], "method"), "joint:scca")
})
