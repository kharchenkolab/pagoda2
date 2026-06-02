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

test_that("metadata setters preserve flexible named inputs and resolvers align them", {
  p2 <- make_test_p2()

  p2$setCellMeta("sample", c(c3 = "s2", c1 = "s1", c5 = "extra"))
  expect_identical(rownames(p2$getCellMeta()), c("c1", "c2", "c3", "c4", "c5"))
  expect_identical(as.character(p2$getCellMeta("sample")$sample), c("s1", NA, "s2", NA, "extra"))
  expect_identical(as.character(p2$resolveCellMeta("sample")$sample), c("s1", NA, "s2", NA))
  expect_error(p2$resolveCellMeta("sample", allow.missing = FALSE), "missing values")

  gene.info <- data.frame(symbol = c("G2", "G1", "G6"), row.names = c("g2", "g1", "g6"))
  p2$setGeneMeta(gene.info)
  expect_identical(rownames(p2$getGeneMeta()), c("g1", "g2", "g3", "g4", "g5", "g6"))
  expect_identical(as.character(p2$getGeneMeta("symbol")$symbol), c("G1", "G2", NA, NA, NA, "G6"))
  expect_identical(as.character(p2$resolveGeneMeta("symbol")$symbol), c("G1", "G2", NA, NA, NA))

  expect_error(p2$setCellMeta("sample", rep("x", 4), overwrite = FALSE), "already exist")
})

test_that("groupings are stored in cellMeta and resolve through defaultGrouping", {
  p2 <- make_test_p2()
  clusters <- c(c1 = "0", c2 = "0", c3 = "1", c4 = "1")

  p2$setGrouping("leiden", clusters, setDefault = TRUE)

  expect_identical(p2$getDefaultGrouping(), "leiden")
  expect_identical(as.character(p2$getGrouping()), c("0", "0", "1", "1"))
  expect_true("leiden" %in% p2$listGroupings()$name)
  expect_true(p2$listGroupings()$is.default[p2$listGroupings()$name == "leiden"])

  p2$setCellMeta("external_label", c(c1 = "A", c2 = "A", c5 = "extra"))
  listed <- p2$listGroupings()
  expect_equal(listed$n.groups[listed$name == "external_label"], 1L)
  expect_equal(listed$n.missing[listed$name == "external_label"], 2L)
})

test_that("metadata palettes use sccore defaults without storing unless requested", {
  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)

  groups <- p2$getGrouping("leiden")
  pal <- p2$resolveFactorColors(axis = "cell", name = "leiden", values = groups)
  expected <- sccore::fac2col(groups, return.details = TRUE)$palette

  expect_identical(pal, expected)
  expect_null(p2$getPalette("leiden", axis = "cell"))

  p2$setPalette("leiden", c("0" = "#111111", "1" = "#E69F00"), axis = "cell")
  expect_identical(
    p2$resolveFactorColors(axis = "cell", name = "leiden", values = groups),
    c("0" = "#111111", "1" = "#E69F00")
  )

  p2$setGrouping("leiden", c(c1 = "0", c2 = "2", c3 = "1", c4 = "2"), overwrite = TRUE)
  pal2 <- p2$resolveFactorColors(axis = "cell", name = "leiden", values = p2$getGrouping("leiden"))

  expect_identical(pal2[c("0", "1")], c("0" = "#111111", "1" = "#E69F00"))
  expect_true("2" %in% names(pal2))
  expect_identical(p2$getPalette("leiden", axis = "cell", colors.only = TRUE), c("0" = "#111111", "1" = "#E69F00"))
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
  igraph::V(g)$name <- rownames(p2$getRawCounts())
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
  marker.result <- p2$getMarkerResult("leiden")

  expect_true("leiden" %in% names(p2$diffgenes$counts))
  expect_true("leiden" %in% names(p2$markerResults$counts))
  expect_identical(marker.result$schema, "pagoda2.marker.v1")
  expect_identical(marker.result$tables, markers)
  expect_identical(marker.meta$grouping, "leiden")
  expect_identical(marker.meta$group.levels, c("0", "1"))
  expect_identical(p2$history$markers$leiden$grouping, "leiden")
})

test_that("marker plotting methods resolve marker schema and grouping", {
  testthat::skip_if_not_installed("ggplot2")

  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  p2$runMarkers(name = "leiden", z.threshold = 0, append.specificity.metrics = FALSE)

  expect_s3_class(
    p2$plotMarkerDotPlot(n.genes.per.group = 2, z.threshold = NULL, highest.only = FALSE),
    "ggplot"
  )
})

test_that("native marker heatmap returns details and draws without ComplexHeatmap", {
  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  p2$setPalette("leiden", c("0" = "#111111", "1" = "#E69F00"), axis = "cell")
  p2$setCellMeta(
    data.frame(
      condition = c("ctrl", "ctrl", "stim", "stim"),
      score = c(1.2, 0.5, 2.5, 1.8),
      row.names = paste0("c", 1:4)
    ),
    overwrite = TRUE
  )
  p2$runMarkers(name = "leiden", z.threshold = 0, append.specificity.metrics = FALSE)

  pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  details <- p2$plotMarkerHeatmap(
    n.genes.per.group = 2,
    z.threshold = NULL,
    highest.only = FALSE,
    additional.genes = "g5",
    column.metadata = c("condition", "score"),
    labeled.gene.subset = 1,
    split = TRUE,
    split.gap = 0.5,
    averaging.window = 2,
    cluster.rows = TRUE,
    cluster.columns = TRUE,
    annotation.grobs = list(
      top = list(grid::rectGrob(gp = grid::gpar(fill = "grey90", col = NA))),
      left = list(grid::rectGrob(gp = grid::gpar(fill = "grey85", col = NA))),
      bottom = list(grid::rectGrob(gp = grid::gpar(fill = "grey80", col = NA))),
      right = list(grid::rectGrob(gp = grid::gpar(fill = "grey75", col = NA)))
    ),
    legend.columns = 2,
    return.details = TRUE
  )

  expect_identical(details$engine, "native")
  expect_true(any(details$genes == "g5"))
  expect_true(all(c("condition", "score") %in% colnames(details$column.annotation)))
  expect_equal(ncol(details$matrix), 4)
  expect_s3_class(details$spec, "pagoda2_marker_heatmap_spec")
  expect_equal(lengths(details$spec$annotation.grobs), c(top = 1L, right = 1L, bottom = 1L, left = 1L))
  expect_identical(details$spec$annotation.colors$palettes$group, c("0" = "#111111", "1" = "#E69F00"))
})

test_that("native heatmap renderer accepts generic matrix specs", {
  native_heatmap_spec <- getFromNamespace(".pagoda2_native_heatmap_spec", "pagoda2")
  draw_native_heatmap <- getFromNamespace(".pagoda2_draw_native_heatmap", "pagoda2")
  x <- matrix(
    c(0, 0.2, 0.7, 1, 0.1, 0.3, 0.8, 0.9, 0.6, 0.4, 0.2, 0),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(paste0("gene", 1:3), paste0("cell", 1:4))
  )
  column.groups <- factor(c("a", "a", "b", "b"), levels = c("a", "b"))
  names(column.groups) <- colnames(x)
  row.groups <- factor(c("m1", "m1", "m2"), levels = c("m1", "m2"))
  names(row.groups) <- rownames(x)
  column.annotation <- data.frame(
    batch = factor(c("x", "x", "y", "y"), levels = c("x", "y")),
    row.names = colnames(x)
  )

  spec <- native_heatmap_spec(
    x,
    column.groups = column.groups,
    row.groups = row.groups,
    column.annotation = column.annotation,
    column.annotation.colors = list(
      group = c(a = "#111111", b = "#999999"),
      batch = c(x = "#4477AA", y = "#CC6677")
    ),
    label.indices = c(1L, 3L),
    split = TRUE,
    show_heatmap_legend = TRUE
  )

  expect_s3_class(spec, "pagoda2_native_heatmap_spec")
  expect_false(inherits(spec, "pagoda2_marker_heatmap_spec"))
  expect_identical(spec$row.groups, spec$gene.groups)
  expect_identical(spec$row.group.colors, spec$gene.group.colors)
  expect_true(all(c("group", "batch") %in% colnames(spec$column.annotation)))

  pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  drawn <- NULL
  expect_silent(drawn <- draw_native_heatmap(spec))
  expect_identical(drawn$native.layout$column.labels$angle, 0)
})

test_that("native heatmap angles crowded column group labels", {
  native_heatmap_spec <- getFromNamespace(".pagoda2_native_heatmap_spec", "pagoda2")
  draw_native_heatmap <- getFromNamespace(".pagoda2_draw_native_heatmap", "pagoda2")
  cell.types <- c(
    "Classical monocytes",
    "Naive CD4 T cells",
    "Memory CD8 T cells",
    "Regulatory T cells",
    "Cycling NK cells",
    "Plasmacytoid dendritic cells",
    "Conventional dendritic cells",
    "Megakaryocyte progenitors"
  )
  cells.per.group <- 3L
  x <- matrix(
    runif(5 * length(cell.types) * cells.per.group),
    nrow = 5,
    dimnames = list(
      paste0("marker", seq_len(5)),
      paste0("cell", seq_len(length(cell.types) * cells.per.group))
    )
  )
  column.groups <- factor(rep(cell.types, each = cells.per.group), levels = cell.types)
  names(column.groups) <- colnames(x)
  row.groups <- factor(c("myeloid", "tcell", "tcell", "nk", "progenitor"))
  names(row.groups) <- rownames(x)
  spec <- native_heatmap_spec(
    x,
    column.groups = column.groups,
    row.groups = row.groups,
    split = TRUE,
    show_heatmap_legend = FALSE
  )

  pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  drawn <- NULL
  expect_silent(drawn <- draw_native_heatmap(spec))
  expect_gt(drawn$native.layout$column.labels$angle, 0)
  expect_gt(drawn$native.layout$column.labels$row.height.mm, 6)
  expect_gt(min(drawn$native.layout$column.labels$leader.end.mm), 2)
  expect_gt(drawn$native.layout$column.labels$label.y.mm, 3)
})

test_that("ComplexHeatmap marker heatmap returns details without drawing", {
  testthat::skip_if_not_installed("ComplexHeatmap")

  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  p2$runMarkers(name = "leiden", z.threshold = 0, append.specificity.metrics = FALSE)

  details <- p2$plotMarkerHeatmap(
    engine = "complex",
    n.genes.per.group = 2,
    z.threshold = NULL,
    highest.only = FALSE,
    return.details = TRUE
  )

  expect_s4_class(details$heatmap, "Heatmap")
  expect_equal(ncol(details$matrix), nrow(p2$getRawCounts()))
  expect_true(all(details$genes %in% colnames(p2$getRawCounts())))
})

test_that("ComplexHeatmap marker heatmap supports real plot controls", {
  testthat::skip_if_not_installed("ComplexHeatmap")

  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  p2$setCellMeta(
    "condition",
    c(c1 = "ctrl", c2 = "ctrl", c3 = "stim", c4 = "stim"),
    overwrite = TRUE
  )
  p2$runMarkers(name = "leiden", z.threshold = 0, append.specificity.metrics = FALSE)

  details <- p2$plotMarkerHeatmap(
    engine = "complex",
    n.genes.per.group = 2,
    z.threshold = NULL,
    highest.only = FALSE,
    additional.genes = "g5",
    column.metadata = "condition",
    labeled.gene.subset = 1,
    split = TRUE,
    split.gap = 0.5,
    averaging.window = 2,
    max.cells = 2,
    return.details = TRUE
  )

  expect_true(any(details$genes == "g5"))
  expect_true("condition" %in% colnames(details$column.annotation))
  expect_equal(ncol(details$matrix), 4)
  pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(ComplexHeatmap::draw(details$heatmap))
})

test_that("legacy marker heatmap engine resolves new marker grouping provenance", {
  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  p2$runMarkers(name = "leiden", z.threshold = 0, append.specificity.metrics = FALSE)

  pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(
    p2$plotMarkerHeatmap(
      markers = "leiden",
      engine = "legacy",
      z.score = NULL,
      n.genes = 2
    )
  )
})

test_that("ComplexHeatmap marker heatmap warns on large dense plot requests", {
  testthat::skip_if_not_installed("ComplexHeatmap")

  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  p2$runMarkers(name = "leiden", z.threshold = 0, append.specificity.metrics = FALSE)

  expect_warning(
    p2$plotMarkerHeatmap(
      engine = "complex",
      n.genes.per.group = 2,
      z.threshold = NULL,
      highest.only = FALSE,
      max.dense.entries = 1,
      return.details = TRUE
    ),
    "densify"
  )
})

test_that("result discovery and selector resolvers use canonical defaults", {
  p2 <- make_test_p2()
  p2$reductions$PCA <- matrix(seq_len(8), nrow = 4, dimnames = list(rownames(p2$getRawCounts()), paste0("PC", 1:2)))
  p2$embeddings$PCA$UMAP <- matrix(seq_len(8), nrow = 4, dimnames = list(rownames(p2$getRawCounts()), paste0("UMAP", 1:2)))
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- rownames(p2$getRawCounts())
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
  p2$embeddings$PCA$UMAP <- matrix(seq_len(8), nrow = 4, dimnames = list(rownames(p2$getRawCounts()), paste0("UMAP", 1:2)))
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)

  expect_silent(plot <- p2$plotEmbedding())

  expect_s3_class(plot, "ggplot")
})

test_that("new wrappers do not emit legacy deprecation warnings", {
  testthat::skip_if_not_installed("leidenAlg")

  p2 <- make_test_p2()
  p2$setGrouping("leiden", c(c1 = "0", c2 = "0", c3 = "1", c4 = "1"), setDefault = TRUE)
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- rownames(p2$getRawCounts())
  p2$graphs$PCA <- g

  expect_warning(p2$getKnnClusters(type = "PCA", name = "legacy"), "runLeiden")
  expect_silent(p2$runLeiden(name = "leiden2"))
  expect_silent(p2$runMarkers(name = "leiden", append.specificity.metrics = FALSE))
})
