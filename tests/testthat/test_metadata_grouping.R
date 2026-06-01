library(pagoda2)

make_test_p2 <- function() {
  cm <- Matrix::Matrix(
    c(
      1, 0, 3, 0,
      0, 2, 0, 4,
      5, 0, 1, 0,
      0, 3, 0, 2,
      1, 1, 1, 1
    ),
    nrow = 5,
    ncol = 4,
    sparse = TRUE
  )
  rownames(cm) <- paste0("g", seq_len(nrow(cm)))
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  Pagoda2$new(
    cm,
    n.cores = 1,
    verbose = FALSE,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0
  )
}

test_that("cellMeta and geneMeta are initialized from count matrix axes", {
  p2 <- make_test_p2()

  expect_identical(rownames(p2$cellMeta), paste0("c", 1:4))
  expect_identical(rownames(p2$geneMeta), paste0("g", 1:5))
  expect_equal(ncol(p2$cellMeta), 0)
  expect_equal(ncol(p2$geneMeta), 0)
})

test_that("metadata setters align named inputs and preserve missing rows", {
  p2 <- make_test_p2()

  p2$setCellMeta("sample", c(c3 = "s2", c1 = "s1", c5 = "extra"))
  expect_identical(as.character(p2$cellMeta$sample), c("s1", NA, "s2", NA))

  gene.info <- data.frame(symbol = c("G2", "G1"), row.names = c("g2", "g1"))
  p2$setGeneMeta(gene.info)
  expect_identical(as.character(p2$geneMeta$symbol), c("G1", "G2", NA, NA, NA))

  expect_error(p2$setCellMeta("sample", rep("x", 4), overwrite = FALSE), "already exists")
})

test_that("groupings are stored in cellMeta and resolve through defaultGrouping", {
  p2 <- make_test_p2()
  clusters <- c(c1 = "0", c2 = "0", c3 = "1", c4 = "1")

  p2$setGrouping("leiden", clusters, setDefault = TRUE)

  expect_identical(p2$getDefaultGrouping(), "leiden")
  expect_identical(as.character(p2$getGrouping()), c("0", "0", "1", "1"))
  expect_true("leiden" %in% p2$listGroupings()$name)
  expect_true(p2$listGroupings()$is.default[p2$listGroupings()$name == "leiden"])
})

test_that("grouping resolver handles direct vectors and missing labels", {
  p2 <- make_test_p2()
  partial <- c(c1 = "A", c2 = "A", c5 = "extra")

  resolved <- p2$resolveGrouping(groups = partial)
  expect_identical(as.character(resolved), c("A", "A", NA, NA))
  expect_error(p2$resolveGrouping(groups = partial, allow.missing = FALSE), "missing values")
  expect_error(p2$resolveGrouping(grouping = "x", groups = partial), "only one")
  expect_error(p2$resolveGrouping(groups = c("A", "B")), "length 4")
})

test_that("defaultGrouping rejects continuous metadata", {
  p2 <- make_test_p2()

  p2$setCellMeta("nUMI", seq_len(4))
  expect_error(p2$setDefaultGrouping("nUMI"), "discrete cell grouping")
})

test_that("annotateClusters supports many-to-one mappings", {
  p2 <- make_test_p2()
  clusters <- c(c1 = "0", c2 = "0", c3 = "1", c4 = "2")

  p2$setGrouping("leiden", clusters, setDefault = TRUE)
  p2$annotateClusters(
    from = "leiden",
    to = "cell_type",
    map = c("0" = "T cell", "1" = "T cell", "2" = "B cell"),
    setDefault = TRUE
  )

  expect_identical(p2$getDefaultGrouping(), "cell_type")
  expect_identical(as.character(p2$getGrouping()), c("T cell", "T cell", "T cell", "B cell"))
  expect_identical(as.character(p2$getGrouping("leiden")), c("0", "0", "1", "2"))
})

test_that("annotateClusters handles unmapped source levels explicitly", {
  p2 <- make_test_p2()
  clusters <- c(c1 = "0", c2 = "0", c3 = "1", c4 = "2")
  p2$setGrouping("leiden", clusters)

  expect_error(
    p2$annotateClusters("leiden", "cell_type", c("0" = "T cell"), unmapped = "error"),
    "No annotation"
  )

  p2$annotateClusters("leiden", "cell_type", c("0" = "T cell"), unmapped = "keep")
  expect_identical(as.character(p2$getGrouping("cell_type")), c("T cell", "T cell", "1", "2"))
})

test_that("runLeiden stores labels in legacy clusters and cellMeta", {
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_test_p2()
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- rownames(p2$counts)
  p2$graphs$PCA <- g

  p2$runLeiden(name = "leiden", resolution = 1)

  expect_true("leiden" %in% names(p2$clusters$PCA))
  expect_true("leiden" %in% colnames(p2$cellMeta))
  expect_identical(p2$getDefaultGrouping(), "leiden")
  expect_identical(p2$clusterings$leiden$grouping, "leiden")
  expect_identical(p2$clusterings$leiden$graph, "PCA")
})

test_that("runMarkers records grouping provenance", {
  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)

  markers <- p2$runMarkers(name = "leiden", append.specificity.metrics = FALSE)
  marker.meta <- attr(markers, "pagoda2.marker")

  expect_true("leiden" %in% names(p2$diffgenes$counts))
  expect_identical(marker.meta$grouping, "leiden")
  expect_identical(marker.meta$group.levels, c("0", "1"))
  expect_identical(p2$history$markers$leiden$grouping, "leiden")
})

test_that("result discovery and selector resolvers use canonical defaults", {
  p2 <- make_test_p2()
  p2$reductions$PCA <- matrix(seq_len(8), nrow = 4, dimnames = list(rownames(p2$counts), paste0("PC", 1:2)))
  p2$embeddings$PCA$UMAP <- matrix(seq_len(8), nrow = 4, dimnames = list(rownames(p2$counts), paste0("UMAP", 1:2)))
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- rownames(p2$counts)
  p2$graphs$PCA <- g
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  p2$runMarkers(name = "leiden", append.specificity.metrics = FALSE)

  expect_identical(p2$resolveReduction(), "PCA")
  expect_identical(p2$resolveGraph(), "PCA")
  expect_identical(p2$resolveEmbedding()$embedding, "UMAP")
  expect_identical(p2$resolveMarkers()$name, "leiden")

  results <- p2$listResults()
  expect_true("PCA" %in% results$reductions$name)
  expect_true("PCA" %in% results$graphs$name)
  expect_true("UMAP" %in% results$embeddings$embedding)
  expect_true("leiden" %in% results$markers$name)
})

test_that("legacy DE method uses defaultGrouping when available", {
  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)

  expect_warning(
    markers <- p2$getDifferentialGenes(append.specificity.metrics = FALSE),
    "runMarkers"
  )

  expect_true("leiden" %in% names(p2$diffgenes$counts))
  expect_identical(names(markers), c("0", "1"))
})

test_that("plotEmbedding resolves defaultGrouping when available", {
  testthat::skip_if_not_installed("ggplot2")

  p2 <- make_test_p2()
  p2$embeddings$PCA$UMAP <- matrix(seq_len(8), nrow = 4, dimnames = list(rownames(p2$counts), paste0("UMAP", 1:2)))
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)

  expect_silent(plot <- p2$plotEmbedding())

  expect_s3_class(plot, "ggplot")
})

test_that("new wrappers do not emit legacy deprecation warnings", {
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- rownames(p2$counts)
  p2$graphs$PCA <- g

  expect_warning(p2$getKnnClusters(type = "PCA", name = "legacy"), "runLeiden")
  expect_silent(p2$runLeiden(name = "leiden2"))
  expect_silent(p2$runMarkers(name = "leiden", append.specificity.metrics = FALSE))
})
