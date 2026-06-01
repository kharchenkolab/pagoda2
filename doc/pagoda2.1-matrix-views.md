# Pagoda2.1 Matrix Storage And Views

This note describes the current pagoda2.1 direction for single-dataset matrix
handling. The goal is to keep one canonical raw count matrix and derive
normalized analysis matrices from lightweight view recipes.

## Matrix Roles

- `p2$rawCounts`: canonical filtered raw count matrix in cell-by-gene
  orientation. Values should remain integer-like.
- `p2$matrixViews$analysis`: recipe for the normalized analysis matrix. The
  current recipe records model type, depth, depth scale, log scaling,
  batch-correction factors, and winsorization caps/depths when used.
- `p2$counts`: removed legacy normalized matrix slot. Accessing or assigning it
  errors with guidance to use view accessors instead.
- `p2$misc$rawCounts`: legacy alias to `p2$rawCounts`.

The preferred accessors are:

```r
p2$getRawCounts()
p2$getExpressionBlock(genes = c("Cd3d", "Lyz"))
p2$getExpressionBlock(genes = odgenes, scale.variance = TRUE)
p2$viewColMeanVar()
p2$viewColSumByFac(grouping = "leiden")
```

`getExpressionBlock()` materializes only the requested cells and genes. Use it
at plotting, PCA, export, or marker boundaries where an actual sparse matrix is
needed. Use the `viewCol*()` methods when summary calculations can stream over
the raw matrix without allocating the normalized matrix.

## Workflow Pattern

```r
cm <- readCounts("filtered_feature_bc_matrix")
p2 <- Pagoda2$new(cm, log.scale = TRUE, trim = 10, n.cores = 8)
p2$run(skip = "markers", profile = "pipeline", plots = "none")
p2$runMarkers(grouping = "leiden", append.auc = TRUE)
p2$plotMarkerDotPlot(markers = "leiden")
p2$plotMarkerHeatmap(markers = "leiden")
```

The workflow-facing path uses matrix views by default; no full normalized
matrix is retained after construction.

## Import And Export

Inputs should be treated as raw counts unless the format explicitly identifies a
different layer. `Pagoda2$new()` stores the filtered raw matrix in `rawCounts`
and records the analysis view recipe during normalization.

For h5ad export:

- `X` defaults to the normalized analysis view.
- `layers/counts` stores raw counts.
- `obs` and `var` metadata are resolved onto exact AnnData axes before writing.

For `p2$as("list")`, `p2$as("sce")`, and `p2$as("seurat")`, counts are exported
as gene-by-cell raw counts, while normalized data is materialized from the
analysis view when requested.

## Current Limitations

The `linearObs` normalization model is not supported without a stored
normalized matrix yet. It now fails early rather than creating a view that
cannot be materialized.

The p2app export path is postponed while the app layer is refactored for matrix
views.
