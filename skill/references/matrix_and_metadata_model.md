# Matrix And Metadata Model

This page describes the object model agents should respect when reading,
processing, plotting, and exporting pagoda2.1 objects.

## R6 Object Access

`Pagoda2` is an R6 class. Use `$` methods and fields:

```r
p2 <- Pagoda2$from("sample_dir", format = "10x")
p2$runQC()
p2$filterData()
p2$run(steps = c("variance", "pca", "graph", "umap", "leiden"))
```

Avoid inventing S3 or S4-style wrappers in recipes unless a conversion method
explicitly returns an S4 object.

## Matrix Orientation

Inside pagoda2.1, raw and normalized view matrices are cell-by-gene:

```r
dim(p2$getRawCounts())           # cells x genes
dim(p2$getExpressionBlock())     # cells x genes
```

External formats often use gene-by-cell. Check orientation when comparing to
lower-level readers or other ecosystems:

```r
counts_gxc <- readCounts("sample_dir", format = "10x")
counts_cxg <- p2$getRawCounts()
```

If a downstream function expects genes by cells, request that orientation:

```r
p2$getRawCounts(orientation = "gene_by_cell")
p2$getExpressionBlock(genes = c("CD3D", "LYZ"), orientation = "gene_by_cell")
```

## Raw Counts And Normalized Views

Key storage:

- `p2$rawCounts`: canonical sparse raw count matrix, cell-by-gene
- `p2$matrixViews$analysis`: normalized expression recipe
- `p2$counts`: removed legacy normalized slot; do not restore or rely on it
- `p2$misc$rawCounts`: legacy raw-count alias kept for transition only

Preferred accessors:

```r
p2$getRawCounts()
p2$getExpressionBlock(genes = c("CD3D", "LYZ"))
p2$materializeView(cells = some_cells, genes = marker_genes)
p2$viewColMeanVar()
p2$viewColSumByFac(grouping = "leiden")
```

Do not materialize the full normalized matrix unless the dataset is known to be
small and the downstream consumer requires it. Subset cells and genes first:

```r
marker_expr <- p2$getExpressionBlock(
  cells = names(p2$getGrouping("leiden"))[p2$getGrouping("leiden") == "0"],
  genes = c("CD3D", "IL7R", "LYZ")
)
```

Use summary helpers for inspection rather than creating extra large variables:

```r
p2$describeMatrices()
p2$validateMatrices()
```

## Metadata Storage

`cellMeta` and `geneMeta` are flexible metadata stores. They may contain extra
rows or partial coverage, which is useful when importing annotations from
related objects.

Add a complete vector:

```r
p2$setCellMeta("donor", donor_vector)
p2$setGeneMeta("biotype", gene_biotype_vector)
```

Add a data frame with names:

```r
p2$setCellMeta(data.frame(
  predicted_type = predicted_type,
  row.names = names(predicted_type)
))
```

Retrieve flexible metadata as stored:

```r
p2$getCellMeta()
p2$getGeneMeta(c("analysis_pass", "feature_type"))
```

By default, `getCellMeta()` and `getGeneMeta()` do not coerce to the current
axis. This preserves flexible metadata during analysis.

## Resolved Metadata

Resolve metadata when a method, plot, or export needs values aligned to the
current cells or genes:

```r
cell_meta <- p2$resolveCellMeta(columns = c("sample", "qc_pass"))
gene_meta <- p2$resolveGeneMeta(columns = "analysis_pass")
```

Resolution rules:

- named metadata aligns by cell or gene names
- missing resolved values become `NA` unless `allow.missing = FALSE`
- extra stored rows are ignored during resolution
- unnamed vectors must match the current axis exactly
- duplicate row names should error

Use strict resolution before operations that cannot tolerate missing labels:

```r
p2$resolveCellMeta("cell_type", allow.missing = FALSE)
```

Foreign exports such as h5ad require exact `obs` and `var` dimensions, so
export paths resolve metadata before writing.

## Groupings

Groupings are discrete `cellMeta` columns. They are used for plotting,
clustering labels, marker tests, and summaries:

```r
p2$setGrouping("leiden", labels, setDefault = TRUE)
p2$getGrouping()
p2$getGrouping("leiden")
p2$listGroupings()
p2$setDefaultGrouping("cell_type")
```

`defaultGrouping` is a pointer to a metadata column, not a separate active
identity vector. If `grouping` is omitted in marker and plotting methods, they
usually use `defaultGrouping`.

You may also pass group vectors directly:

```r
p2$runMarkers(groups = external_labels, name = "external_labels")
p2$plotEmbedding(groups = external_labels)
```

Direct vectors are useful for one-off comparisons, but store important labels
with `setGrouping()` so later plots and exports can reuse them.

## Annotation

Use `annotateClusters()` for cluster-to-label mapping:

```r
p2$annotateClusters(
  from = "leiden",
  to = "cell_type",
  map = c("0" = "T cells", "1" = "T cells", "2" = "monocytes"),
  setDefault = TRUE
)
```

Many-to-one mappings are normal. For example, multiple Leiden clusters may map
to the same cell type. Use `unmapped = "keep"` to preserve source labels for
unannotated clusters, or `unmapped = "error"` when every source cluster must be
explicitly annotated:

```r
p2$annotateClusters(
  from = "leiden",
  to = "cell_type",
  map = cluster_to_type,
  unmapped = "error"
)
```

External annotations that do not map to clusters should be stored directly:

```r
p2$setGrouping("external_annotation", external_labels, setDefault = TRUE)
```

## Factor Colors

Use central factor color resolution instead of ad hoc plot-local palettes. The
same factor should look consistent in UMAPs, dotplots, heatmaps, and metadata
tracks:

```r
p2$resolveFactorColors(
  axis = "cell",
  name = "leiden",
  values = p2$getGrouping("leiden")
)
```

By default, colors are resolved consistently without forcing every generated
palette into the object. Store a palette only when the user has chosen a fixed
mapping that should persist:

```r
p2$setPalette("cell_type", c("T cells" = "#1f77b4", "monocytes" = "#d62728"))
```

If a plot accepts explicit `group.colors`, use it only for the current figure or
when the user requested a custom color scheme.
