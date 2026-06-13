library(pagoda2)

make_facet_rna <- function() {
  cm <- Matrix::Matrix(
    c(
      5, 0, 2, 1,
      0, 4, 1, 3,
      3, 0, 6, 2,
      1, 2, 0, 5
    ),
    nrow = 4, ncol = 4, byrow = TRUE, sparse = TRUE,
    dimnames = list(paste0("c", 1:4), paste0("g", 1:4))
  )
  as(cm, "dgCMatrix")
}

make_facet_adt <- function() {
  cm <- Matrix::Matrix(
    c(
      10, 2,
      3, 7,
      8, 1,
      0, 9
    ),
    nrow = 4, ncol = 2, byrow = TRUE, sparse = TRUE,
    dimnames = list(paste0("c", 1:4), c("CD3", "CD4"))
  )
  as(cm, "dgCMatrix")
}

new_rna_p2 <- function() {
  # constructor expects gene-by-cell? setCountMatrix takes cells x genes per field doc; matrix here is cells x genes
  Pagoda2$new(
    Matrix::t(make_facet_rna()), # Pagoda2$new historically takes gene-by-cell; transpose so cells are columns
    verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE
  )
}

test_that("default facet exists and is RNA", {
  p2 <- new_rna_p2()
  expect_identical(p2$defaultFacet, "RNA")
  expect_identical(p2$listFacets(), "RNA")
  expect_true("RNA" %in% names(p2$facets))
})

test_that("default facet view delegates to top-level storage", {
  p2 <- new_rna_p2()
  f <- p2$getFacet() # default
  expect_true(inherits(f, "Pagoda2Facet"))
  expect_true(f$primary)
  expect_identical(f$rawCounts, p2$rawCounts)
  expect_identical(p2$facets$RNA$rawCounts, p2$rawCounts)
  expect_identical(f$featureMeta, p2$geneMeta)
  expect_identical(f$depth, p2$depth)
})

test_that("p2$cells equals the default facet's cell axis", {
  p2 <- new_rna_p2()
  expect_identical(p2$cells, rownames(p2$rawCounts))
})

test_that("writing through the default facet view reflects in top-level fields", {
  p2 <- new_rna_p2()
  f <- p2$getFacet()
  f$varinfo <- data.frame(gsf = 1, qv = 2, v = 3, row.names = "g1")
  expect_identical(p2$misc[["varinfo"]], f$varinfo)
  p2$misc[["odgenes"]] <- c("g1", "g2")
  expect_identical(p2$getFacet()$odgenes, c("g1", "g2"))
})

test_that("addFacet stores an ADT facet without perturbing RNA", {
  p2 <- new_rna_p2()
  rna_before <- p2$rawCounts
  p2$addFacet("ADT", make_facet_adt(), modelType = "clr", featureType = "protein")
  expect_setequal(p2$listFacets(), c("RNA", "ADT"))
  adt <- p2$getFacet("ADT")
  expect_false(adt$primary)
  expect_identical(dim(adt$rawCounts), c(4L, 2L))
  expect_identical(colnames(adt$rawCounts), c("CD3", "CD4"))
  expect_identical(adt$modelType, "clr")
  expect_identical(adt$featureType, "protein")
  expect_identical(adt$defaultReduction, "PCA")
  # RNA untouched
  expect_identical(p2$rawCounts, rna_before)
  expect_identical(p2$facets$RNA$rawCounts, rna_before)
})

test_that("writing through a non-default facet view persists", {
  p2 <- new_rna_p2()
  p2$addFacet("ADT", make_facet_adt(), modelType = "clr", featureType = "protein")
  adt <- p2$getFacet("ADT")
  adt$odgenes <- c("CD3")
  expect_identical(p2$getFacet("ADT")$odgenes, "CD3")
  expect_identical(p2$misc$facetStore$ADT$odgenes, "CD3")
})

test_that("getFacet errors on unknown facet; addFacet rejects the default name", {
  p2 <- new_rna_p2()
  expect_error(p2$getFacet("nope"), "no facet named")
  expect_error(p2$addFacet("RNA", make_facet_adt()), "default facet")
})

test_that("copy constructor and serialization preserve facets", {
  p2 <- new_rna_p2()
  p2$addFacet("ADT", make_facet_adt(), modelType = "clr", featureType = "protein")
  cp <- Pagoda2$new(p2)
  expect_setequal(cp$listFacets(), c("RNA", "ADT"))
  expect_identical(cp$getFacet("ADT")$rawCounts, p2$getFacet("ADT")$rawCounts)
  rt <- unserialize(serialize(p2, NULL))
  expect_setequal(rt$listFacets(), c("RNA", "ADT"))
  expect_identical(rt$getFacet("ADT")$modelType, "clr")
})
