# Markers And Plots

This reference covers marker calculation, marker accessors, marker selection
presets, dotplots, native heatmaps, and cautious cluster annotation.

## Marker Calculation

After the default `p2$run()`, markers are usually available for the default
grouping. Check before recomputing:

```r
marker_name <- p2$getDefaultGrouping()
stopifnot(!is.null(marker_name))

if (!marker_name %in% p2$listMarkers()$name) {
  p2$runMarkers(name = marker_name,
                upregulated.only = TRUE,
                append.auc = TRUE,
                append.specificity.metrics = TRUE,
                verbose = TRUE)
}
```

Source-verified `runMarkers()` call shape:

```r
p2$runMarkers(grouping = "leiden",
              name = "leiden",
              type = "counts",
              z.threshold = 3,
              upregulated.only = TRUE,
              append.specificity.metrics = TRUE,
              append.auc = TRUE,
              verbose = TRUE)
```

For multimodal data, `runMarkers()` takes `facet=` and stores results
facet-keyed (e.g. protein markers via `p2$runMarkers(grouping="leiden",
facet="ADT")`); `findMarkers()` is an alias. See
`references/multimodal_facets.md`.

If `grouping` is omitted, pagoda2 uses `defaultGrouping` when available:

```r
p2$runMarkers(name = p2$getDefaultGrouping(),
              upregulated.only = TRUE,
              append.auc = TRUE,
              append.specificity.metrics = TRUE,
              verbose = TRUE)
```

Use external or annotation groupings explicitly:

```r
p2$setGrouping("external_annotation", external_labels, setDefault = FALSE)
p2$runMarkers(grouping = "external_annotation",
              name = "external_annotation",
              upregulated.only = TRUE,
              append.auc = TRUE,
              append.specificity.metrics = TRUE,
              verbose = TRUE)
```

Default marker calculations should favor upregulated markers and keep AUC plus
specificity metrics. Turn those off only when performance is limiting on a
very large dataset and the user accepts less informative ranking.

## Marker Result Access

List stored marker results:

```r
p2$listMarkers()
```

Get the stored result object:

```r
marker_result <- p2$getMarkerResult("leiden")
names(marker_result)
```

Get compact top markers:

```r
top_markers <- p2$getTopMarkers(markers = "leiden",
                                n.genes.per.group = 5,
                                selection = "balanced")
print(top_markers)
```

Save a marker table:

```r
top_markers <- p2$getTopMarkers(markers = "leiden",
                                n.genes.per.group = 10,
                                selection = "balanced")
utils::write.csv(top_markers, "cluster_markers.csv", row.names = FALSE)
```

Prefer `listMarkers()`, `getMarkerResult()`, and `getTopMarkers()` over
manual slot walking.

## Marker Selection Presets

Dotplot and heatmap use the same marker-selection logic:

- `balanced`: default first-pass choice; balances precision and expression
  fraction, with AUC, effect size, and specificity as tie breakers
- `auc`: emphasizes classifier-like separation
- `precision`: emphasizes group-specific markers with minimum target
  expression-fraction support
- `effect`: emphasizes expression effect size
- custom function/list: advanced ranking

Examples:

```r
p2$getTopMarkers("leiden", selection = "balanced", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "auc", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "precision", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "effect", n.genes.per.group = 5)
```

Specificity-oriented filters:

```r
p2$getTopMarkers(
  "leiden",
  selection = "precision",
  min.expression.fraction = 0.35,
  min.precision = 0.60,
  n.genes.per.group = 5
)
```

`highest.only = TRUE` keeps markers whose expression is highest in the target
group. Keep it on for ordinary marker displays; turn it off only when shared
or broad markers are specifically desired.

## Marker Dotplot

Dotplots use color for mean expression and point size for expression fraction:

```r
p_dot <- p2$plotMarkerDotPlot(
  markers = "leiden",
  n.genes.per.group = 5,
  selection = "balanced",
  order.groups = TRUE,
  dot.scale = 8.5
)
ggplot2::ggsave("marker_dotplot.png", p_dot,
                width = 15.5, height = 10.5, units = "in", dpi = 120,
                bg = "white")
```

`order.groups = TRUE` orders plotted groups from the values shown in the
dotplot; it does not require external biology. Use `group.order` only when
the desired order is already known:

```r
# REPLACE with a group order supported by the current analysis.
known_group_order <- c()

if (length(known_group_order) > 0) {
  p2$plotMarkerDotPlot(markers = "leiden",
                       group.order = known_group_order)
}
```

If labels or dots collide, first increase figure width/height. Do not shrink
the dot scale until the figure has enough physical space.

Assess:

- whether each cluster has high-expression, high-fraction markers
- whether markers are specific or broadly expressed
- whether group ordering improves readability
- whether stress, cell-cycle, mitochondrial, or ribosomal genes dominate

## Native Marker Heatmap

Use the native heatmap engine by default. `engine = "native"` is drawn by the
shared `sccore` heatmap renderer (`sccore::heatmapSpec()` / `drawHeatmap()`), so
it needs **sccore >= 1.1.0** — guaranteed by the install step in
`installation_and_io.md`; no ComplexHeatmap required.

```r
png("marker_heatmap_native.png",
    width = 13.8, height = 9, units = "in", res = 120, bg = "white")
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  selection = "balanced",
  column.metadata = intersect(c("n_molecules", "n_genes",
                                "percent_mito", "percent_ribo"),
                              colnames(p2$cellMeta)),
  row.label.font.size = 9,
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()
```

For large datasets, cap cells per group and keep rasterization on:

```r
png("marker_heatmap_native.png",
    width = 13.8, height = 9, units = "in", res = 120, bg = "white")
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  max.cells = 500,
  use.raster = TRUE,
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()
```

The native engine draws expression, marker-origin groups, cell groups, and
optional cell metadata tracks without requiring ComplexHeatmap.

For a high-resolution PDF, do not render the native heatmap directly into
`cairo_pdf()` if the matrix is large. The native engine renders one device
pixel per matrix cell and does not expose a `raster_quality`, `raster_device`,
or max-cells-DPI knob, so direct PDF embedding can look oversmoothed at
typical zoom. Render the heatmap to a high-DPI PNG first, then embed that
raster into the PDF:

```r
png("marker_heatmap_native_300dpi.png",
    width = 13.8, height = 9, units = "in", res = 300,
    type = "cairo", bg = "white")
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()

cairo_pdf("marker_heatmap_native.pdf", width = 13.8, height = 9)
grid::grid.raster(png::readPNG("marker_heatmap_native_300dpi.png"),
                  interpolate = FALSE)
dev.off()
```

## Optional ComplexHeatmap Backend

`engine = "complex"` remains available, but it requires ComplexHeatmap at
plot time and should not be the default recommendation:

```r
if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
  p2$plotMarkerHeatmap(markers = "leiden",
                       engine = "complex",
                       n.genes.per.group = 3)
}
```

Do not install ComplexHeatmap just for a standard pagoda2.1 marker heatmap
unless the user explicitly wants that backend.

## Explicit Gene Panels

Plot known genes instead of selected markers:

```r
p_dot <- p2$plotMarkerDotPlot(
  markers = "leiden",
  genes = c("CD3D", "MS4A1", "LYZ", "NKG7"),
  grouping = "leiden"
)
```

Heatmap with additional genes:

```r
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  additional.genes = c("CD3D", "MS4A1", "LYZ")
)
```

Report missing genes if the panel was supplied by the user.

## Cluster Annotation

Annotate only after marker review. Do not copy example biological labels into
a real analysis.

```r
# REPLACE with cluster-to-cell-type assignments derived from marker review.
cluster_to_type <- c(
  # "<cluster_id>" = "<cell type>"
)

if (length(cluster_to_type) > 0) {
  p2$annotateClusters(from = "leiden",
                      to = "cell_type",
                      map = cluster_to_type,
                      unmapped = "keep",
                      setDefault = TRUE,
                      overwrite = TRUE)
}
```

Many-to-one mappings are normal. If all clusters must be annotated, use
`unmapped = "error"` only after building a complete map from marker evidence.

External annotations that do not map to clusters should be stored directly as
a grouping:

```r
p2$setGrouping("external_annotation", external_labels, setDefault = FALSE)
```

## Marker Report

Report:

- marker result name and grouping
- whether markers were recomputed or reused
- ranking mode used: balanced, auc, precision, effect, or custom
- top marker genes per cluster
- clusters with weak, broad, or QC-dominated markers
- dotplot and heatmap filenames
- any annotation map applied and any clusters left unmapped
