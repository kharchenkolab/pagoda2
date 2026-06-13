library(pagoda2)

joint_p2 <- function() {
  set.seed(7)
  ng <- 12
  nc <- 30
  m <- matrix(rpois(ng * nc, 3), ng, nc, dimnames = list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc))))
  p2 <- Pagoda2$new(as(Matrix::Matrix(m, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 6
  a <- matrix(rpois(nc * np, 5) + 1L, nc, np, dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np))))
  p2$addFacet("ADT", as(Matrix::Matrix(a, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")
  p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
  p2$runReduction(nPcs = 5, var.scale = FALSE, verbose = FALSE) # RNA PCA -> reductions[["PCA"]]
  p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
  p2$runReduction(facet = "ADT", nPcs = 3, n.odgenes = 6, var.scale = FALSE, verbose = FALSE) # -> reductions[["ADT:PCA"]]
  p2
}

test_that("joint reduction (concat-PCA) is a named product with feature-axis provenance", {
  p2 <- joint_p2()
  suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), name = "WNN", nPcs = 3, verbose = FALSE))
  expect_true("WNN" %in% names(p2$reductions))
  w <- p2$reductions[["WNN"]]
  expect_identical(nrow(w), 30L) # over the shared cells
  expect_true(ncol(w) >= 1L && ncol(w) <= 3L)
  expect_identical(attr(w, "facets"), c("RNA", "ADT"))
  expect_identical(attr(w, "input_axes"), c("genes", "proteins")) # lstar S5 provenance (feature axes)
  expect_true(grepl("joint:concat", attr(w, "method")))
})

test_that("joint reduction respects the no-shadow rule (can't be named after a per-facet method)", {
  p2 <- joint_p2()
  expect_error(suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), name = "PCA")), "collides")
})

test_that("joint reduction errors if a facet reduction is missing", {
  p2 <- joint_p2()
  expect_error(
    suppressWarnings(p2$runReduction(facets = c("RNA", "ADT"), reductions = c("CCA", "PCA"), name = "WNN")),
    "not found"
  )
})
