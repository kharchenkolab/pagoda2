# Matrix And Metadata Model

## Matrix Orientation

Inside pagoda2.1, matrices are cell-by-gene:

```r
p2$getRawCounts()
p2$getExpressionBlock()
```

External formats often use gene-by-cell. Always check orientation when reading,
exporting, or comparing to Seurat/AnnData.

## Raw Counts And Views

Key storage:

- `p2$rawCounts`: canonical sparse raw count matrix, cell-by-gene
- `p2$matrixViews$analysis`: lightweight normalized expression recipe
- `p2$counts`: removed legacy normalized slot; do not restore it
- `p2$misc$rawCounts`: legacy alias to raw counts

Preferred accessors:

```r
p2$getRawCounts()
p2$getExpressionBlock(genes = c("Cd3d", "Lyz"))
p2$getExpressionBlock(orientation = "gene_by_cell")
p2$viewColMeanVar()
p2$viewColSumByFac(grouping = "leiden")
```

Avoid materializing the full normalized matrix. Subset cells and genes first.

## Flexible Metadata

`cellMeta` and `geneMeta` are flexible stores. They may have extra rows or only
partial coverage. Resolve them at use time:

```r
p2$resolveCellMeta(columns = c("sample", "qc_pass"))
p2$resolveGeneMeta(columns = "analysis_pass")
```

Resolution rules:

- named metadata aligns by cell or gene names
- missing resolved values become `NA` unless disallowed
- extra stored rows are ignored during resolution
- unnamed vectors must match the current axis exactly
- duplicate row names should error

## Groupings

Groupings are discrete `cellMeta` columns:

```r
p2$setGrouping("leiden", labels, setDefault = TRUE)
p2$getGrouping()
p2$getGrouping("leiden")
p2$listGroupings()
p2$setDefaultGrouping("cell_type")
```

`defaultGrouping` is a pointer to a metadata column, not an active identity
vector.

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

Many-to-one mappings are normal. External labels that do not map to clusters
should be stored directly with `setGrouping()`.

## Factor Colors

Use central color resolution rather than plot-local palettes:

```r
p2$resolveFactorColors(axis = "cell", name = "leiden", values = p2$getGrouping())
```

The same factor should look consistent in UMAPs, dotplots, heatmaps, and
metadata tracks.
