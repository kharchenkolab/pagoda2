# Markers And Plots

This page covers marker gene calculation and the main marker visualizations:
dotplots and native marker heatmaps.

## Marker Calculation

Run markers after QC, filtering, PCA/UMAP, and clustering look plausible:

```r
p2$runMarkers(
  grouping = "leiden",
  name = "leiden",
  upregulated.only = TRUE,
  append.auc = TRUE,
  append.specificity.metrics = TRUE,
  verbose = TRUE
)
```

If `grouping` is omitted, methods use `defaultGrouping` when available:

```r
p2$getDefaultGrouping()
p2$runMarkers(name = "leiden")
```

Use an explicit grouping for external labels:

```r
p2$setGrouping("external_annotation", external_labels, setDefault = FALSE)
p2$runMarkers(grouping = "external_annotation", name = "external_annotation")
```

Default marker analysis should favor upregulated markers. AUC and specificity
metrics are useful for ranking crisp marker genes, so keep
`append.auc = TRUE` and `append.specificity.metrics = TRUE` unless there is a
performance reason to skip them.

## Marker Result Access

List stored marker results:

```r
p2$listMarkers()
```

Get the full marker result metadata:

```r
marker_result <- p2$getMarkerResult("leiden")
names(marker_result)
```

Get selected top markers as a compact table:

```r
top_markers <- p2$getTopMarkers(
  markers = "leiden",
  n.genes.per.group = 5,
  selection = "balanced"
)
head(top_markers)
```

Write a full marker table when the user needs a persistent entity:

```r
marker_tables <- p2$markerResults$counts$leiden$tables
marker_df <- do.call(rbind, lapply(names(marker_tables), function(group) {
  x <- marker_tables[[group]]
  if (is.null(x) || !nrow(x)) return(NULL)
  x$group <- group
  x
}))
utils::write.csv(marker_df, "cluster_markers.csv", row.names = FALSE)
```

Some older objects may store marker tables under older slots. Prefer
`listMarkers()`, `getMarkerResult()`, and `getTopMarkers()` when available.

## Marker Selection Presets

Modern marker plots share marker selection logic. The main `selection` options
are:

- `balanced`: default; balances precision and expression fraction, then uses
  AUC/effect statistics as tie breakers
- `auc`: ranks by AUC, then Z and specificity metrics
- `precision`: favors markers that are highly specific to the target group and
  applies a default minimum expression fraction
- `effect`: ranks by effect size
- a custom function/list for advanced cases

Examples:

```r
p2$getTopMarkers("leiden", selection = "balanced", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "auc", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "precision", n.genes.per.group = 5)
```

Common filters:

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
group. This is usually desirable for clean marker displays. Set it to `FALSE`
only when broad or shared markers are scientifically important.

## Dotplot

Dotplots summarize selected markers with color for mean expression and point
size for expression fraction:

```r
p_dot <- p2$plotMarkerDotPlot(
  markers = "leiden",
  n.genes.per.group = 5,
  selection = "balanced",
  order.groups = TRUE,
  dot.scale = 7
)
```

`order.groups = TRUE` orders the y-axis using the groups represented by the
selected markers, which often makes marker blocks follow a rough diagonal.
Override with `group.order` when a biological order is known:

```r
p2$plotMarkerDotPlot(
  markers = "leiden",
  group.order = c("T cells", "B cells", "monocytes", "NK cells")
)
```

Figure sizing matters. If labels or dots collide, increase width/height before
changing marker selection:

```r
ggplot2::ggsave("marker_dotplot.png", p_dot,
                width = 16, height = 10, units = "in", dpi = 120,
                bg = "white")
```

Assess:

- whether each group has high-expression, high-fraction marker genes
- whether markers are specific or broadly expressed
- whether groups are ordered coherently
- whether selected markers are dominated by QC, stress, ribosomal,
  mitochondrial, or cell-cycle genes

## Native Marker Heatmap

The native engine is the default for this recipe. It avoids a heavy optional
heatmap dependency and supports group labels, row labels, metadata tracks,
legends, and rasterization:

```r
png("marker_heatmap_native.png",
    width = 13.5, height = 8.1, units = "in", res = 120, bg = "white")
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  selection = "balanced",
  column.metadata = intersect(
    c("n_molecules", "n_genes", "percent_mito", "percent_ribo"),
    colnames(p2$getCellMeta())
  ),
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()
```

For large datasets, avoid densifying too much expression data:

```r
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  max.cells = 500,
  use.raster = TRUE
)
```

Use `return.details = TRUE` when debugging marker selection, group order, or
metadata tracks:

```r
details <- p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  return.details = TRUE,
  native.newpage = FALSE
)
names(details)
```

## Complex And Legacy Engines

`engine = "complex"` is optional when `ComplexHeatmap` is installed. Use it
only when the user needs its specific annotation features. `engine = "legacy"`
exists for comparison with older pagoda2 behavior and should not be the default
in new workflows.

## Plotting Custom Gene Lists

Use explicit `genes` when the user supplies a list or when marker ranking is
not the goal. Current marker plot methods still need a marker result name for
context, so pass `markers` explicitly:

```r
p2$plotMarkerDotPlot(
  markers = "leiden",
  genes = c("CD3D", "MS4A1", "LYZ"),
  grouping = "leiden"
)
p2$plotMarkerHeatmap(
  markers = "leiden",
  genes = c("CD3D", "MS4A1", "LYZ"),
  grouping = "leiden"
)
```

For additional genes on top of selected markers:

```r
p2$plotMarkerHeatmap(
  markers = "leiden",
  additional.genes = c("MALAT1", "PPBP")
)
```

## Annotation Discipline

Do not annotate clusters from a single marker name. Present marker evidence,
possible cell types, and uncertainty. If the user approves or the marker
evidence is clear, store annotations as groupings:

```r
p2$annotateClusters(from = "leiden", to = "cell_type", map = cluster_to_type)
p2$plotEmbedding(grouping = "cell_type", mark.groups = TRUE)
```
