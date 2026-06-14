library(pagoda2)

## Downstream plotting/markers must pull expression from the SELECTED facet, not always RNA. RNA genes
## ("g*") and ADT proteins ("P*") have disjoint names, so a method that ignores facet= and reaches into
## RNA would error ("no genes present") on the protein markers — the regression these guard.
build_plot_p2 <- function() {
  set.seed(11)
  nc <- 80
  grp <- rep(1:2, each = nc / 2)
  ng <- 30
  rna <- matrix(rpois(ng * nc, 2), ng, nc)
  rna[1:10, grp == 2] <- rna[1:10, grp == 2] + rpois(10 * (nc / 2), 12)
  dimnames(rna) <- list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc)))
  p2 <- Pagoda2$new(as(Matrix::Matrix(rna, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 8
  adt <- matrix(rpois(nc * np, 4) + 1L, nc, np)
  adt[grp == 2, 1:4] <- adt[grp == 2, 1:4] + rpois((nc / 2) * 4, 20)
  dimnames(adt) <- list(paste0("c", seq_len(nc)), paste0("P", seq_len(np)))
  p2$addFacet("ADT", as(Matrix::Matrix(adt, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")
  p2$cellMeta$grp <- factor(paste0("grp", grp))
  suppressWarnings({
    p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
    p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
    p2$runMarkers(grouping = "grp", name = "bygrp", verbose = FALSE)              # RNA markers
    p2$runMarkers(grouping = "grp", facet = "ADT", name = "bygrp", verbose = FALSE) # ADT markers
  })
  p2
}

test_that("getTopMarkers(facet=) returns the facet's markers (proteins, not genes)", {
  p2 <- build_plot_p2()
  tm <- suppressWarnings(p2$getTopMarkers(markers = "bygrp", facet = "ADT", n.genes.per.group = 3, z.threshold = 1))
  expect_true(nrow(tm) >= 1L)
  expect_true(all(grepl("^P", tm$gene)))                 # protein names, not "g*"
  tr <- suppressWarnings(p2$getTopMarkers(markers = "bygrp", n.genes.per.group = 3, z.threshold = 1))
  expect_true(all(grepl("^g", tr$gene)))                 # RNA default still genes
})

test_that("plotMarkerDotPlot(facet=ADT) plots protein expression (not RNA)", {
  p2 <- build_plot_p2()
  g <- suppressWarnings(p2$plotMarkerDotPlot(markers = "bygrp", facet = "ADT", n.genes.per.group = 3, z.threshold = 1))
  expect_s3_class(g, "ggplot")
  gr <- suppressWarnings(p2$plotMarkerDotPlot(markers = "bygrp", n.genes.per.group = 3, z.threshold = 1)) # RNA back-compat
  expect_s3_class(gr, "ggplot")
})

test_that("plotMarkerHeatmap(facet=ADT) renders from the ADT facet", {
  p2 <- build_plot_p2()
  pdf(tempfile(fileext = ".pdf"))
  on.exit(dev.off(), add = TRUE)
  expect_error(suppressWarnings(p2$plotMarkerHeatmap(markers = "bygrp", facet = "ADT", engine = "native",
                                                     n.genes.per.group = 3, z.threshold = 1)), NA)
})

test_that("plotEmbedding(gene=, facet=) colors by a feature from the named facet", {
  skip_if_not_installed("uwot")
  skip_if_not_installed("FNN")
  p2 <- build_plot_p2()
  suppressWarnings({
    p2$runReduction(nPcs = 5, var.scale = FALSE, verbose = FALSE)
    p2$runEmbedding(reduction = "PCA", name = "umap", verbose = FALSE)
  })
  g <- p2$plotEmbedding(reduction = "PCA", embedding = "umap", gene = "P1", facet = "ADT")  # protein, not in RNA
  expect_s3_class(g, "ggplot")
  expect_error(p2$plotEmbedding(reduction = "PCA", embedding = "umap", gene = "P1"),         # RNA default lacks P1
               "isn't present")
})
