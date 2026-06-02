# Markers And Plots

## Marker Calculation

Run markers after QC, filtering, PCA/UMAP, and clustering are plausible:

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

Default marker analysis should favor upregulated markers. AUC and specificity
metrics help rank crisp marker genes.

## Marker Selection

Modern marker plots share marker selection logic. Common defaults:

```r
selection = "balanced"
n.genes.per.group = 5
```

Use `selection = "auc"` or stricter precision/specificity thresholds when the
balanced markers are too diffuse.

## Dotplot

Show:

```r
p_dot <- p2$plotMarkerDotPlot(
  markers = "leiden",
  n.genes.per.group = 5,
  selection = "balanced",
  order.groups = TRUE,
  dot.scale = 7
)
```

Assess:

- whether each cluster has high-expression, high-fraction marker genes
- whether marker dots follow an approximate diagonal after group ordering
- whether many markers are expressed broadly across clusters
- whether the plot is too dense for the selected figure size

## Native Marker Heatmap

Show:

```r
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  selection = "balanced",
  column.metadata = intersect(c("n_molecules", "n_genes", "percent_mito", "percent_ribo"),
                              colnames(p2$getCellMeta())),
  split = TRUE,
  show_heatmap_legend = TRUE
)
```

The native engine avoids requiring `ComplexHeatmap` and is the default for this
recipe. `engine = "complex"` is optional when `ComplexHeatmap` is installed.

Assess:

- whether marker blocks are clean
- whether QC tracks explain suspicious clusters
- whether group labels and legends remain legible
- whether marker signal is dominated by stress, MT, ribosomal, or cell-cycle
  genes

## Marker Table

Write markers as a persistent entity:

```r
marker_tables <- p2$markerResults$leiden$markers
marker_df <- if (is.data.frame(marker_tables)) {
  marker_tables
} else {
  do.call(rbind, lapply(names(marker_tables), function(group) {
    x <- marker_tables[[group]]
    if (is.null(x) || !nrow(x)) return(NULL)
    x$group <- group
    x
  }))
}
utils::write.csv(marker_df, "cluster_markers.csv")
```

Do not annotate clusters from marker names alone. Present marker evidence and
uncertainty explicitly.
