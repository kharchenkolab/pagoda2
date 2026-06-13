library(pagoda2)

test_that("canonical cells axis is the union; membership masks + requireFacets complete-cases (§4.3)", {
  set.seed(8)
  ng <- 10
  rna_cells <- paste0("c", 1:20)
  rna <- matrix(rpois(ng * 20, 3), ng, 20, dimnames = list(paste0("g", seq_len(ng)), rna_cells))
  p2 <- Pagoda2$new(as(Matrix::Matrix(rna, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  adt_cells <- c(paste0("c", 5:20), "cX") # a subset of RNA cells + one extra not in RNA
  np <- 4
  a <- matrix(rpois(length(adt_cells) * np, 5) + 1L, length(adt_cells), np,
    dimnames = list(adt_cells, paste0("P", seq_len(np))))
  p2$addFacet("ADT", as(Matrix::Matrix(a, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")

  # union axis, default facet first
  expect_identical(length(p2$cells), 21L)
  expect_identical(p2$cells[1:20], rna_cells)
  expect_true("cX" %in% p2$cells)

  # membership masks over the canonical axis
  mr <- p2$getFacetMembership("RNA")
  ma <- p2$getFacetMembership("ADT")
  expect_identical(length(mr), 21L)
  expect_identical(sum(mr), 20L) # RNA covers c1..c20, not cX
  expect_false(mr[p2$cells == "cX"])
  expect_identical(sum(ma), 17L) # ADT covers c5..c20 (16) + cX
  expect_true(ma[p2$cells == "cX"])
  expect_false(ma[p2$cells == "c1"])

  # complete-cases: cells measured in BOTH facets, in canonical order
  rc <- p2$requireFacets(c("RNA", "ADT"))
  expect_setequal(rc, paste0("c", 5:20))
  expect_identical(rc, p2$cells[p2$cells %in% paste0("c", 5:20)])
})

test_that("single-facet object: canonical cells still equal the default facet's cells", {
  cm <- Matrix::Matrix(matrix(rpois(5 * 6, 3), 5, 6, dimnames = list(paste0("g", 1:5), paste0("c", 1:6))), sparse = TRUE)
  p2 <- Pagoda2$new(as(cm, "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  expect_identical(p2$cells, rownames(p2$rawCounts))
  expect_true(all(p2$getFacetMembership())) # default facet covers all canonical cells
})
