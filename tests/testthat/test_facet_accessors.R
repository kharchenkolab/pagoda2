library(pagoda2)

acc_p2 <- function() {
  cm <- Matrix::Matrix(
    matrix(c(5, 0, 2, 1, 0, 4, 1, 3, 3, 0, 6, 2, 1, 2, 0, 5), nrow = 4, byrow = TRUE),
    sparse = TRUE, dimnames = list(paste0("g", 1:4), paste0("c", 1:4))
  )
  p2 <- Pagoda2$new(as(cm, "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  # a second facet with a plain model (CLR comes in Phase 2a)
  adt <- Matrix::Matrix(matrix(c(10, 2, 3, 7, 8, 1, 4, 9), nrow = 4, byrow = TRUE),
    sparse = TRUE, dimnames = list(paste0("c", 1:4), c("CD3", "CD4")))
  p2$addFacet("ADT", as(adt, "dgCMatrix"), modelType = "plain", featureType = "protein")
  p2
}

test_that("facet-aware getRawCounts returns the right facet's matrix", {
  p2 <- acc_p2()
  rna <- p2$getRawCounts()
  expect_identical(rna, p2$getRawCounts(facet = "RNA")) # default == explicit RNA
  adt <- p2$getRawCounts(facet = "ADT")
  expect_identical(dim(adt), c(4L, 2L))
  expect_identical(colnames(adt), c("CD3", "CD4"))
})

test_that("facet-aware matrix view + materialization operate per facet", {
  p2 <- acc_p2()
  expect_identical(p2$getMatrixView("analysis", facet = "ADT")$model, "plain")
  blk <- p2$getExpressionBlock(facet = "ADT")
  expect_identical(dim(blk), c(4L, 2L))
  expect_identical(colnames(blk), c("CD3", "CD4"))
  # default facet materialization unchanged vs explicit RNA
  expect_equal(as.matrix(p2$getExpressionBlock()), as.matrix(p2$getExpressionBlock(facet = "RNA")))
})

test_that("viewColMeanVar runs per facet and is thread-count invariant", {
  p2 <- acc_p2()
  v_adt <- p2$viewColMeanVar(facet = "ADT", n.cores = 1)
  expect_identical(nrow(v_adt), 2L) # two proteins
  expect_true(all(is.finite(v_adt$m)) && all(is.finite(v_adt$v)))
  # default-facet summary unchanged whether addressed bare or as RNA
  expect_identical(p2$viewColMeanVar(n.cores = 1), p2$viewColMeanVar(facet = "RNA", n.cores = 1))
  # thread-count invariance on the facet (bit-identical)
  expect_identical(p2$viewColMeanVar(facet = "ADT", n.cores = 1), p2$viewColMeanVar(facet = "ADT", n.cores = 2))
})
