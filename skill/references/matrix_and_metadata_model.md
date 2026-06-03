# Matrix And Metadata Model

This reference explains the pagoda2.1 object model that agents should respect
when reading, processing, plotting, and exporting data.

## R6 Access Pattern

`Pagoda2` is an R6 class. Use `$` methods and fields:

```r
p2 <- Pagoda2$from("/path/to/sample_directory",
                   format = "10x",
                   reader.args = list(sample.name = "sample_01"))
p2$runQC(verbose = TRUE)
p2$filterData(verbose = TRUE)
p2$run(steps = c("variance", "pca", "graph", "embedding", "leiden"),
       plots = "none",
       verbose = TRUE)
```

Do not invent S3/S4 wrappers in a recipe unless a conversion method explicitly
returns that class.

## Matrix Orientation

Inside pagoda2.1, matrices are cell-by-gene:

```r
dim(p2$getRawCounts())        # cells x genes
dim(p2$getExpressionBlock())  # cells x genes for the requested block
```

External readers can differ. `readCounts()` returns gene-by-cell:

```r
dim(readCounts("/path/to/sample_directory", format = "10x"))
dim(p2$getRawCounts())
```

Request gene-by-cell orientation only when a downstream function needs it:

```r
p2$getRawCounts(orientation = "gene_by_cell")
p2$getExpressionBlock(genes = c("CD3D", "LYZ"),
                      orientation = "gene_by_cell")
```

## Raw Counts Versus Normalized Views

Pagoda2.1 avoids keeping a full duplicated normalized matrix by default.
Instead it stores:

- canonical sparse raw counts in cell-by-gene orientation
- matrix-view parameters for normalized expression
- analysis masks and model results needed to materialize blocks quickly

Preferred accessors:

```r
p2$getRawCounts()
p2$getExpressionBlock(genes = c("CD3D", "LYZ"))
p2$materializeView(cells = c("AAACCCAAGAAACACT-1"),
                   genes = c("CD3D", "LYZ"))
p2$viewColMeanVar()
p2$viewColSumByFac(grouping = "leiden")
```

Avoid materializing full normalized expression for large datasets. Subset cells
and genes first:

```r
p2$getExpressionBlock(
  cells = names(p2$getGrouping("leiden"))[p2$getGrouping("leiden") == "0"],
  genes = c("CD3D", "IL7R", "LYZ")
)
```

Do not rely on `p2$counts`. The legacy full normalized slot was removed to
reduce memory footprint. Use `getRawCounts()` for counts and
`getExpressionBlock()` or matrix-view helpers for normalized expression.

Use summary helpers when developing or debugging, not as required ceremony in
user recipes:

```r
p2$describeMatrices()
p2$validateMatrices()
```

## Flexible Metadata

`cellMeta` and `geneMeta` can store flexible metadata. A metadata table may
cover only some cells/genes or include extra rows from a related object.

Set a named vector:

```r
p2$setCellMeta("predicted_type", predicted_type)
p2$setGeneMeta("biotype", gene_biotype)
```

Set a data frame whose row names are cell or gene names:

```r
p2$setCellMeta(data.frame(
  predicted_type = predicted_type,
  row.names = names(predicted_type)
))
```

Retrieve stored metadata without forcing it onto the current axis:

```r
p2$getCellMeta()
p2$getCellMeta(c("sample", "qc_pass"))
p2$getGeneMeta(c("analysis_pass", "feature_type"))
```

`getCellMeta()` and `getGeneMeta()` default to unresolved flexible metadata.
This is intentional.

## Resolved Metadata

Resolve metadata when a method, plot, or export needs one value per current
cell or gene:

```r
p2$resolveCellMeta(columns = c("sample", "qc_pass"))
p2$resolveGeneMeta(columns = "analysis_pass")
```

Resolution rules:

- named metadata aligns by cell or gene names
- missing values become `NA` unless `allow.missing = FALSE`
- extra stored rows are ignored during resolution
- unnamed vectors must match the current axis exactly
- duplicate row names should error

Use strict resolution when missing labels are invalid:

```r
p2$resolveCellMeta("cell_type", allow.missing = FALSE)
```

Foreign formats such as h5ad require exact `obs` and `var` dimensions. Export
resolves metadata before writing instead of dropping useful partial columns.

## Groupings

Groupings are discrete cell metadata columns used for clustering labels,
plotting, marker tests, and summaries:

```r
p2$setGrouping("leiden", labels, setDefault = TRUE)
p2$getGrouping()
p2$getGrouping("leiden")
p2$listGroupings()
p2$setDefaultGrouping("cell_type")
```

`defaultGrouping` is a pointer to a `cellMeta` column, not a separate identity
slot. Methods use it when `grouping` is omitted.

Direct group vectors are allowed for one-off work:

```r
p2$runMarkers(groups = external_labels, name = "external_labels")
p2$plotEmbedding(groups = external_labels)
```

Store important groupings with `setGrouping()` so later plots, markers, and
exports can reuse them.

## Cluster Annotation

Use `annotateClusters()` for cluster-to-label mappings. Build the map from the
current marker evidence rather than copying example labels:

```r
# REPLACE with cluster-to-cell-type assignments derived from marker review.
cluster_to_type <- c()

if (length(cluster_to_type) > 0) {
  p2$annotateClusters(from = "leiden",
                      to = "cell_type",
                      map = cluster_to_type,
                      unmapped = "keep",
                      setDefault = TRUE)
}
```

Many-to-one mappings are expected. If every cluster must be annotated, use
strict `unmapped = "error"` after building the mapping from marker evidence:

```r
# REPLACE with a complete map derived from marker review.
cluster_to_type <- c()

if (length(cluster_to_type) > 0) {
  p2$annotateClusters(from = "leiden",
                      to = "cell_type",
                      map = cluster_to_type,
                      unmapped = "error",
                      setDefault = TRUE)
}
```

External annotations that do not correspond to clusters should be stored
directly:

```r
p2$setGrouping("external_annotation", external_labels, setDefault = TRUE)
```

## Factor Colors

Use pagoda2/sccore color resolution rather than ad hoc palettes. The same
factor should look consistent across embeddings, dotplots, heatmaps, and
metadata tracks:

```r
p2$resolveFactorColors(axis = "cell",
                       name = "leiden",
                       values = p2$getGrouping("leiden"))
```

By default, pagoda2 can resolve colors consistently without storing every
generated palette in the object. Store a palette only when the user requests a
fixed mapping:

```r
# REPLACE with a user-approved mapping for existing factor levels.
cell_type_colors <- c()

if (length(cell_type_colors) > 0) {
  p2$setPalette("cell_type",
                colors = cell_type_colors,
                axis = "cell")
}
```

Use explicit plot-local colors only for a single figure or when the user asks
for a custom color scheme.
