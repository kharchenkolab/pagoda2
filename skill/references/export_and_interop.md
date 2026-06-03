# Export And Interoperability

This reference covers native persistence, h5ad export, in-memory conversions,
metadata alignment, and round-trip checks.

## Native Persistence

Save pagoda2 objects with standard R serialization:

```r
saveRDS(p2, "pagoda2_processed.rds")
p2 <- readRDS("pagoda2_processed.rds")
stopifnot(identical(p2$apiVersion, "2.1"))
```

Use RDS for pagoda2-native continuation because it preserves raw counts,
matrix views, metadata, reductions, graphs, embeddings, groupings, markers,
thread settings, history, and `apiVersion`.

`p2$export("file.rds")` can also write RDS, but `saveRDS()` is the clearest
R-native command.

## h5ad Export

Export h5ad for AnnData/scanpy-compatible downstream work:

```r
p2$export("pagoda2_processed.h5ad", format = "h5ad", overwrite = TRUE)
```

Current h5ad semantics:

- `X`: normalized analysis expression by default
- `layers/counts`: raw counts
- `obs`: resolved cell metadata on the exact exported cell axis
- `var`: resolved gene metadata on the exact exported gene axis
- `obsm`: reductions and embeddings where available

Use raw counts in `X` only when the receiving workflow expects it:

```r
p2$export("pagoda2_counts_x.h5ad",
          format = "h5ad",
          x = "counts",
          overwrite = TRUE)
```

For most scanpy-oriented interchange, keep normalized `X` plus
`layers/counts`.

Pagoda2 writes h5ad directly; do not require scanpy, reticulate, Seurat, or
SeuratDisk just to export this format.

## Metadata Alignment On Export

Pagoda2 metadata can be flexible internally. Foreign formats require exact
axis dimensions, so export resolves metadata before writing:

```r
p2$resolveCellMeta(cells = rownames(p2$getRawCounts()))
p2$resolveGeneMeta(genes = colnames(p2$getRawCounts()))
```

Partial metadata maps by names and leaves unresolved cells/genes missing:

```r
p2$setCellMeta("manual_label", partial_labels)
resolved <- p2$resolveCellMeta("manual_label")
cat(sprintf("%d cells have missing manual labels\n",
            sum(is.na(resolved$manual_label))))
```

This is better than dropping a useful metadata column just because it does not
cover every cell internally.

AnnData does allow missing values in `obs`/`var`, but dimensions must match
exactly. Therefore the export rule is: resolve by names first, keep missing
values where names do not map, and fail only when the matrix axes themselves
are inconsistent.

## In-Memory Conversion

List conversion is lightweight:

```r
as_list <- p2$as("list")
names(as_list)
```

The list contains counts, optional normalized expression, metadata,
reductions, embeddings, graphs, and markers in ordinary R containers.

SingleCellExperiment conversion is optional:

```r
if (requireNamespace("SingleCellExperiment", quietly = TRUE)) {
  sce <- p2$as("sce")
}
```

Seurat conversion is optional:

```r
if (requireNamespace("Seurat", quietly = TRUE)) {
  seurat_obj <- p2$as("seurat")
}
```

Do not install Seurat or SingleCellExperiment just to run pagoda2. They are
optional conversion targets.

## Read-Back Checks

When export code changes, test a small round trip:

```r
p2$export("tmp_pagoda2.h5ad", format = "h5ad", overwrite = TRUE)
imported <- readCounts("tmp_pagoda2.h5ad",
                       format = "h5ad",
                       layer = "counts",
                       return.metadata = TRUE)
dim(imported$counts)
str(imported$cellMeta)
str(imported$geneMeta)
```

Remember orientation: `readCounts()` returns gene-by-cell; `p2$getRawCounts()`
returns cell-by-gene.

For source-code changes, compare a small fixture's raw counts against
`layers/counts` and normalized expression against `X` within numerical
tolerance.

## Export Report

Report:

- native RDS filename
- h5ad filename, if written
- cells and genes exported
- whether raw counts and normalized expression were both included
- metadata/grouping columns included in `obs`
- gene metadata columns included in `var`
- missing metadata values introduced by resolution
- optional conversion targets created, if any

## Current Boundaries

Current pagoda2.1 interop:

- read: 10x triplets, CellRanger HDF5, h5ad, h5Seurat, loom
- write: RDS and h5ad
- convert in memory: list, SingleCellExperiment if installed, Seurat if
  installed

Do not promise export to h5Seurat, loom, zarr, parquet, or every readable
format unless the implementation exists.
