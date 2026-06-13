library(pagoda2)

res_p2 <- function() {
  cm <- Matrix::Matrix(
    matrix(c(5, 0, 2, 1, 0, 4, 1, 3, 3, 0, 6, 2, 1, 2, 0, 5), nrow = 4, byrow = TRUE),
    sparse = TRUE, dimnames = list(paste0("g", 1:4), paste0("c", 1:4))
  )
  p2 <- Pagoda2$new(as(cm, "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  adt <- Matrix::Matrix(matrix(c(10, 2, 3, 7, 8, 1, 0, 9), nrow = 4, byrow = TRUE),
    sparse = TRUE, dimnames = list(paste0("c", 1:4), c("CD3", "CD4")))
  p2$addFacet("ADT", as(adt, "dgCMatrix"), modelType = "clr", featureType = "protein", defaultReduction = "PCA")
  p2
}

test_that("resolveFacet handles NULL, name, and a Facet object", {
  p2 <- res_p2()
  expect_identical(p2$resolveFacet()$name, "RNA")
  expect_true(p2$resolveFacet()$primary)
  expect_identical(p2$resolveFacet("ADT")$name, "ADT")
  f <- p2$getFacet("ADT")
  expect_identical(p2$resolveFacet(f)$name, "ADT")
  expect_error(p2$resolveFacet(c("RNA", "ADT")), "single facet")
})

test_that("reduction keys follow the default-unqualified / qualified rule (§4.5.1)", {
  p2 <- res_p2()
  rk <- pagoda2:::.pagoda2_reduction_key
  expect_identical(rk(p2, NULL), "PCA")            # default facet, default reduction -> bare
  expect_identical(rk(p2, "RNA"), "PCA")           # explicit default facet -> still bare
  expect_identical(rk(p2, "RNA", "CCA"), "CCA")    # non-default reduction on default facet -> bare name
  expect_identical(rk(p2, "ADT"), "ADT:PCA")       # non-default facet -> qualified
  expect_identical(rk(p2, "ADT", "CCA"), "ADT:CCA")
})

test_that("qualified-name parsing splits on the first colon only", {
  pq <- pagoda2:::.pagoda2_parse_qualified
  expect_identical(pq("ADT:CD3", "RNA"), list(facet = "ADT", item = "CD3"))
  expect_identical(pq("MS4A1", "RNA"), list(facet = "RNA", item = "MS4A1"))
  expect_identical(pq("ATAC:chr1:100-200", "RNA"), list(facet = "ATAC", item = "chr1:100-200"))
})

test_that("no-shadow validator forbids joint names colliding with reduction methods or containing ':'", {
  p2 <- res_p2()
  vj <- pagoda2:::.pagoda2_validate_joint_name
  expect_identical(vj(p2, "WNN"), "WNN")           # distinct name ok
  expect_identical(vj(p2, "MOFA"), "MOFA")
  expect_error(vj(p2, "PCA"), "collides")          # both facets default to PCA -> shadow
  expect_error(vj(p2, "ADT:x"), "must not contain")
})
