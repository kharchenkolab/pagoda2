# Export And Interoperability

This reference covers native persistence, h5ad export, in-memory conversions,
metadata alignment, and read-back checks.

## Native Persistence

Use standard R serialization for pagoda2-native continuation:

```r
saveRDS(p2, "pagoda2_processed.rds")
p2 <- readRDS("pagoda2_processed.rds")
stopifnot(identical(p2$apiVersion, "2.2"))
```

RDS preserves raw counts, matrix views, metadata, reductions, graphs,
embeddings, groupings, markers, thread settings, history, and `apiVersion`.

`p2$export("pagoda2_processed.rds", format = "rds")` is also implemented,
but `saveRDS()` is the clearest R-native command.

## h5ad Export

Export h5ad for AnnData/scanpy-compatible downstream work:

```r
p2$export("pagoda2_processed.h5ad",
          format = "h5ad",
          overwrite = TRUE)
```

For multimodal objects, `format = "lstar"` writes a portable lstar Zarr store
(a `cells` axis + one feature axis per facet + per-facet raw `counts`, with
facet/recipe provenance); `pagoda2:::pagoda2FromLstar(path)` reads it back. The
lstar Zarr store also doubles as the out-of-core disk backing for facets. See
`references/multimodal_facets.md`.

Current h5ad defaults:

- `X`: normalized analysis expression
- `layers/counts`: raw counts
- `obs`: resolved cell metadata on the exact exported cell axis
- `var`: resolved gene metadata on the exact exported gene axis
- `obsm`: reductions and embeddings where available

Use raw counts in `X` only when the receiving workflow expects that:

```r
p2$export("pagoda2_counts_x.h5ad",
          format = "h5ad",
          x = "counts",
          overwrite = TRUE)
```

Control whether counts, reductions, and embeddings are written:

```r
p2$export("pagoda2_no_obsm.h5ad",
          format = "h5ad",
          include.reductions = FALSE,
          include.embeddings = FALSE,
          overwrite = TRUE)
```

Do not require scanpy, reticulate, Seurat, or SeuratDisk just to export h5ad.
Pagoda2 writes h5ad directly through hdf5r.

## Metadata Alignment On Export

Pagoda2 metadata can be flexible internally. Foreign formats require exact
axis dimensions, so h5ad export resolves metadata before writing.

Preview resolved cell metadata:

```r
cell_meta <- p2$resolveCellMeta(cells = rownames(p2$getRawCounts()))
gene_meta <- p2$resolveGeneMeta(genes = colnames(p2$getRawCounts()))
cat(sprintf("obs: %d rows; var: %d rows\n",
            nrow(cell_meta), nrow(gene_meta)))
```

Partial metadata maps by names and leaves unresolved cells or genes as `NA`:

```r
p2$setCellMeta("manual_label", partial_labels)
resolved <- p2$resolveCellMeta("manual_label")
cat(sprintf("%d cells have missing manual labels\n",
            sum(is.na(resolved$manual_label))))
```

AnnData allows missing values in `obs`/`var`, but dimensions must match the
matrix axes. The export rule is: resolve by names first, preserve missing
values where names do not map, and fail only when matrix axes are inconsistent.

## In-Memory Conversion

List conversion is lightweight and has no optional dependency:

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

Do not install Seurat or SingleCellExperiment just to run pagoda2. Install
them only when the user explicitly needs those conversion targets.

## Read-Back Checks

When export behavior changes or the user requests a check, read back counts
from h5ad:

```r
p2$export("tmp_pagoda2.h5ad", format = "h5ad", overwrite = TRUE)

imported <- readCounts("tmp_pagoda2.h5ad",
                       format = "h5ad",
                       layer = "counts",
                       return.metadata = TRUE)

stopifnot(inherits(imported$counts, "dgCMatrix"))
stopifnot(all(abs(imported$counts@x - round(imported$counts@x)) < 1e-8))
cat(sprintf("Read back %d genes x %d cells\n",
            nrow(imported$counts), ncol(imported$counts)))
```

Remember orientation:

- `readCounts()` returns gene-by-cell.
- `p2$getRawCounts()` returns cell-by-gene.

For source-code changes, compare a small fixture's raw counts against
`layers/counts` and normalized expression against `X` within numerical
tolerance.

## Export Boundaries

Current pagoda2.1 interop:

- read: 10x triplets, CellRanger HDF5, h5ad, h5Seurat, loom
- write: RDS and h5ad
- convert in memory: list, SingleCellExperiment if installed, Seurat if
  installed

Do not promise export to h5Seurat, loom, zarr, parquet, or every readable
format unless the implementation exists.

## Export Report

Report:

- native RDS filename
- h5ad filename, if written
- cells and genes exported
- whether `X` is normalized or counts
- whether raw counts were included in `layers/counts`
- metadata/grouping columns included in `obs`
- gene metadata columns included in `var`
- missing metadata values introduced by resolution
- optional conversion targets created, if any
