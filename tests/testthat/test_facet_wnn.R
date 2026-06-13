library(pagoda2)

test_that("WNN produces per-cell modality weights that down-weight the uninformative modality", {
  skip_if_not_installed("FNN")
  set.seed(12)
  ng <- 40
  nc <- 60
  grp <- rep(1:2, each = nc / 2)
  base <- matrix(rpois(ng * nc, 2), ng, nc)
  base[1:20, grp == 2] <- base[1:20, grp == 2] + rpois(20 * (nc / 2), 15) # two strongly separated RNA clusters
  dimnames(base) <- list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc)))
  p2 <- Pagoda2$new(as(Matrix::Matrix(base, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 10
  adt <- matrix(rpois(nc * np, 4) + 1L, nc, np, dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np)))) # pure noise
  p2$addFacet("ADT", as(Matrix::Matrix(adt, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")
  # matched reduction dimensionality so the per-cell concentration weights compare fairly across facets
  suppressWarnings({
    p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(nPcs = 8, var.scale = FALSE, verbose = FALSE)
    p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(facet = "ADT", nPcs = 8, var.scale = FALSE, verbose = FALSE)
  })

  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), verbose = FALSE))

  # named-product joint reduction + provenance
  expect_true("WNN" %in% names(p2$reductions))
  expect_identical(attr(p2$reductions[["WNN"]], "facets"), c("RNA", "ADT"))
  expect_identical(attr(p2$reductions[["WNN"]], "method"), "wnn")

  # per-cell modality weights, shared cell measures, sum to 1
  expect_true(all(c("wnn_weight_RNA", "wnn_weight_ADT") %in% colnames(p2$cellMeta)))
  wr <- p2$cellMeta$wnn_weight_RNA
  wa <- p2$cellMeta$wnn_weight_ADT
  expect_equal(wr + wa, rep(1, nc), tolerance = 1e-9)

  # the informative modality (structured RNA) gets a higher mean weight than the noise modality (ADT)
  expect_gt(mean(wr), mean(wa))
})
