# Markers And Plots

This reference covers marker calculation, marker selection, dotplots, native
marker heatmaps, and cautious cluster annotation.

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

Defaults should favor upregulated markers. AUC and specificity metrics support
clean marker ranking, so keep them on unless a very large dataset makes marker
calculation too slow.

After the default `p2$run()` call, markers are usually already available for
the default grouping. Check before recomputing:

```r
marker_name <- p2$getDefaultGrouping()
if (!marker_name %in% p2$listMarkers()$name) {
  p2$runMarkers(name = marker_name, verbose = TRUE)
}
```

## Marker Result Access

List marker results:

```r
p2$listMarkers()
```

Get the stored result:

```r
marker_result <- p2$getMarkerResult("leiden")
names(marker_result)
```

Get compact top markers:

```r
p2$getTopMarkers(markers = "leiden",
                 n.genes.per.group = 5,
                 selection = "balanced")
```

Write a full marker table:

```r
marker_result <- p2$getMarkerResult("leiden")
marker_df <- do.call(rbind, lapply(names(marker_result$tables), function(group) {
  x <- marker_result$tables[[group]]
  if (is.null(x) || !nrow(x)) return(NULL)
  x$group <- group
  x
}))
utils::write.csv(marker_df, "cluster_markers.csv", row.names = FALSE)
```

Prefer `listMarkers()`, `getMarkerResult()`, and `getTopMarkers()` over manual
slot walking.

## Marker Selection Presets

Dotplot and heatmap use the same marker-selection logic:

- `balanced`: default; balances precision and expression fraction, with AUC,
  effect size, and specificity as tie breakers
- `auc`: emphasizes classifier-like separation
- `precision`: emphasizes group-specific markers with a default minimum
  expression-fraction requirement
- `effect`: emphasizes expression effect size
- custom function/list: advanced ranking

Examples:

```r
p2$getTopMarkers("leiden", selection = "balanced", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "auc", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "precision", n.genes.per.group = 5)
p2$getTopMarkers("leiden", selection = "effect", n.genes.per.group = 5)
```

Common specificity filters:

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

## Dotplot

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

`order.groups = TRUE` orders the y-axis based on the plotted values, not on
external biology. Use `group.order` when the desired order is known. Derive
the vector from the current plotted groups or an approved biological ordering:

```r
# REPLACE with the group order supported by the current analysis.
known_group_order <- c()

p2$plotMarkerDotPlot(
  markers = "leiden",
  group.order = known_group_order
)
```

If labels or dots collide, first increase figure width/height. Do not shrink
the dot scale until the figure has enough space.

For beginner-facing output, save the dotplot as a wide figure. The defaults
are tuned for readability, but marker labels and large dots need physical
space:

```r
ggplot2::ggsave("marker_dotplot.png", p_dot,
                width = 15.5, height = 10.5, units = "in", dpi = 120,
                bg = "white")
```

Assess:

- whether each group has high-expression, high-fraction markers
- whether markers are specific or broadly expressed
- whether the ordering improves readability
- whether stress, cell-cycle, MT, or ribosomal genes dominate

## Native Marker Heatmap

Use the native engine by default:

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
                              colnames(p2$getCellMeta())),
  row.label.font.size = 9,
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()
```

For large datasets, cap cells and keep rasterization on:

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

Use `return.details = TRUE` only for debugging or method development:

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

`engine = "complex"` uses `ComplexHeatmap` when installed. It is useful for
specialized annotation layouts, but it is a heavy optional dependency and
should not be required for standard ABA analysis.

`engine = "legacy"` exists for comparison with older pagoda2 behavior. Do not
use it as the default in new analyses.

## Explicit Gene Lists

Use `genes` when the user supplies a panel or when marker ranking is not the
goal:

```r
p2$plotMarkerDotPlot(
  markers = "leiden",
  genes = c("CD3D", "MS4A1", "LYZ"),
  grouping = "leiden"
)
```

```r
png("marker_heatmap_native.png",
    width = 10, height = 7, units = "in", res = 120, bg = "white")
p2$plotMarkerHeatmap(
  markers = "leiden",
  genes = c("CD3D", "MS4A1", "LYZ"),
  grouping = "leiden",
  engine = "native"
)
dev.off()
```

Add genes to selected markers with:

```r
p2$plotMarkerHeatmap(
  markers = "leiden",
  additional.genes = c("MALAT1", "PPBP"),
  engine = "native"
)
```

## Annotation Discipline

Do not annotate clusters from a single marker name. Build a cluster-to-label
map from marker review and leave uncertain clusters unmapped:

```r
# REPLACE with labels supported by marker review.
cluster_to_type <- c()

if (length(cluster_to_type) > 0) {
  p2$annotateClusters(from = "leiden",
                      to = "cell_type",
                      map = cluster_to_type,
                      unmapped = "keep",
                      setDefault = TRUE)
  p2$plotEmbedding(grouping = "cell_type", mark.groups = TRUE)
}
```

Many-to-one mappings are normal: several Leiden clusters may map to one cell
type. External annotations that do not map cleanly to clusters should be
stored directly with `setGrouping()` and kept separate from cluster-derived
annotation.

Report marker evidence, confidence, unresolved clusters, and any disagreement
between marker-derived labels and external labels.
