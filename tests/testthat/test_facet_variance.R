library(pagoda2)

var_p2 <- function() {
  set.seed(1)
  ng <- 12
  nc <- 30
  m <- matrix(rpois(ng * nc, lambda = 3), nrow = ng, ncol = nc,
    dimnames = list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc))))
  p2 <- Pagoda2$new(as(Matrix::Matrix(m, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  # a plain second facet (proteins), cells x features
  np <- 6
  a <- matrix(rpois(nc * np, lambda = 5) + 1L, nrow = nc, ncol = np,
    dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np))))
  p2$addFacet("ADT", as(Matrix::Matrix(a, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")
  p2
}

test_that("runVariance(facet=) stores varinfo/odgenes in the facet, not on RNA", {
  p2 <- var_p2()
  expect_null(p2$misc[["varinfo"]]) # RNA variance not computed yet
  p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
  vi <- p2$getFacet("ADT")$varinfo
  expect_true(is.data.frame(vi))
  expect_setequal(rownames(vi), paste0("P", 1:6))
  expect_true(length(p2$getFacet("ADT")$odgenes) >= 1)
  # RNA untouched
  expect_null(p2$misc[["varinfo"]])
  expect_null(p2$getFacet("RNA")$odgenes)
})

test_that("runVariance() default still targets RNA (== facet='RNA') and leaves ADT alone", {
  p2 <- var_p2()
  p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
  expect_true(is.data.frame(p2$misc[["varinfo"]])) # RNA varinfo stored top-level (primary delegation)
  expect_identical(p2$getFacet("RNA")$varinfo, p2$misc[["varinfo"]])
  expect_null(p2$getFacet("ADT")$varinfo) # ADT untouched
})

test_that("runReduction(facet=) stores scores under the qualified key and loadings in the facet", {
  p2 <- var_p2()
  p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
  p2$runReduction(facet = "ADT", nPcs = 3, n.odgenes = 6, var.scale = FALSE, verbose = FALSE)
  expect_true("ADT:PCA" %in% names(p2$reductions)) # qualified key, name-keyed top-level
  expect_false("PCA" %in% names(p2$reductions)) # RNA PCA not computed
  expect_identical(nrow(p2$reductions[["ADT:PCA"]]), 30L) # cells x PCs
  expect_identical(ncol(p2$reductions[["ADT:PCA"]]), 3L)
  expect_true("PCA" %in% names(p2$getFacet("ADT")$loadings)) # per-facet loadings
  expect_null(p2$misc$PCA) # primary loadings slot not polluted by a non-default facet
})

test_that("runReduction() default targets RNA, bare 'PCA' key, populates misc$PCA", {
  p2 <- var_p2()
  p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
  p2$runReduction(nPcs = 5, var.scale = FALSE, verbose = FALSE)
  expect_true("PCA" %in% names(p2$reductions)) # bare key for default facet
  expect_false(any(grepl(":", names(p2$reductions)))) # no qualified keys
  expect_false(is.null(p2$misc$PCA))
})
