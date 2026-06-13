library(pagoda2)

mk_p2 <- function() {
  set.seed(2)
  ng <- 12
  nc <- 30
  m <- matrix(rpois(ng * nc, lambda = 3), nrow = ng, ncol = nc,
    dimnames = list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc))))
  p2 <- Pagoda2$new(as(Matrix::Matrix(m, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 6
  a <- matrix(rpois(nc * np, lambda = 5) + 1L, nrow = nc, ncol = np,
    dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np))))
  p2$addFacet("ADT", as(Matrix::Matrix(a, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")
  grp <- stats::setNames(rep(c("a", "b"), length.out = nc), paste0("c", seq_len(nc)))
  p2$setGrouping("grp", grp, setDefault = TRUE)
  p2
}

test_that("markers are facet-keyed: RNA and ADT do not collide", {
  p2 <- mk_p2()
  p2$runMarkers(grouping = "grp", verbose = FALSE) # RNA (default facet)
  p2$runMarkers(grouping = "grp", facet = "ADT", verbose = FALSE) # ADT facet
  expect_true("grp" %in% names(p2$markerResults$RNA))
  expect_true("grp" %in% names(p2$markerResults$ADT))
  expect_true("grp" %in% names(p2$diffgenes$RNA))
  expect_true("grp" %in% names(p2$diffgenes$ADT))
  expect_false("counts" %in% names(p2$diffgenes)) # no legacy sentinel key
  # RNA markers range over genes, ADT markers over proteins
  rna_feats <- unique(unlist(lapply(p2$diffgenes$RNA$grp, rownames)))
  adt_feats <- unique(unlist(lapply(p2$diffgenes$ADT$grp, rownames)))
  expect_true(length(adt_feats) == 0 || all(adt_feats %in% paste0("P", 1:6)))
  expect_true(length(rna_feats) == 0 || all(rna_feats %in% paste0("g", 1:12)))
})

test_that("legacy 'counts' read path resolves to the default facet", {
  p2 <- mk_p2()
  p2$runMarkers(grouping = "grp", verbose = FALSE)
  # getMarkerResult / resolveMarkers default (type='counts') -> default facet 'RNA'
  res <- p2$getMarkerResult("grp")
  expect_s3_class(res, "pagoda2_marker_result")
  expect_identical(p2$resolveMarkers("grp")$type, "RNA")
})
