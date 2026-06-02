# Export And Interoperability

This page covers native persistence, h5ad export, in-memory conversions, and
round-trip checks.

## Native Persistence

Always save the pagoda2 object with standard R serialization:

```r
saveRDS(p2, "pagoda2_processed.rds")
p2 <- readRDS("pagoda2_processed.rds")
```

Use RDS for pagoda2-native continuation because it preserves raw counts,
matrix views, metadata, reductions, graphs, embeddings, groupings, markers, and
history.

`p2$export("file.rds")` also writes RDS, but ordinary `saveRDS()` is the
clearest R-native command.

## h5ad Export

Export h5ad for AnnData/scanpy or any strict-axis consumer:

```r
p2$export("pagoda2_processed.h5ad", format = "h5ad", overwrite = TRUE)
```

Current h5ad semantics:

- `X`: normalized analysis expression
- `layers/counts`: raw counts
- `obs`: resolved cell metadata on the exact exported cell axis
- `var`: resolved gene metadata on the exact exported gene axis
- `obsm`: reductions and embeddings where available

Pagoda2 metadata can be flexible internally, but h5ad metadata must match
matrix dimensions. Export resolves metadata first, maps values by names, and
leaves missing values as `NA` where the target format permits.

Use `x = "counts"` when the target should put raw counts in `X`:

```r
p2$export("pagoda2_counts_x.h5ad", format = "h5ad", x = "counts", overwrite = TRUE)
```

Use the default normalized `X` plus `layers/counts` for most scanpy-oriented
interchange.

## In-Memory Conversion

List conversion is lightweight and has no optional package dependency:

```r
as_list <- p2$as("list")
names(as_list)
```

The list contains gene-by-cell `counts`, optional normalized expression,
metadata, reductions, embeddings, graphs, and markers. Use it when a downstream
R function needs pieces of a pagoda2 object without requiring a specific object
class.

Optional SingleCellExperiment conversion:

```r
if (requireNamespace("SingleCellExperiment", quietly = TRUE)) {
  sce <- p2$as("sce")
}
```

Optional Seurat conversion:

```r
if (requireNamespace("Seurat", quietly = TRUE)) {
  seurat_obj <- p2$as("seurat")
}
```

Do not install Seurat or SingleCellExperiment unless the downstream task really
needs the in-memory object. These are optional conversion targets, not pagoda2
runtime requirements.

## Metadata Alignment On Export

Before strict-axis export or conversion, metadata is resolved:

```r
cell_meta <- p2$resolveCellMeta(cells = rownames(p2$getRawCounts()))
gene_meta <- p2$resolveGeneMeta(genes = colnames(p2$getRawCounts()))
```

If an imported annotation only covers some cells, those cells map by name and
the rest become missing:

```r
p2$setCellMeta("manual_label", partial_labels)
resolved <- p2$resolveCellMeta("manual_label")
sum(is.na(resolved$manual_label))
```

For h5ad and object conversions, this is preferable to dropping a useful
metadata column just because it had partial coverage internally.

## Round-Trip Sanity

When export code changes, test read-back on a fixture or small dataset:

```r
p2$export("tmp.h5ad", format = "h5ad", overwrite = TRUE)
imported <- readCounts("tmp.h5ad", format = "h5ad", layer = "counts",
                       return.metadata = TRUE)
dim(imported$counts)
str(imported$cellMeta)
str(imported$geneMeta)
```

For deeper validation, compare raw counts against `layers/counts` and
normalized values against `X` using a small fixture. Be careful about
orientation: `readCounts()` returns gene-by-cell while `p2$getRawCounts()`
returns cell-by-gene.

## Export Report

Report:

- path of native RDS
- path of h5ad, if written
- cells and genes in exported matrices
- whether raw counts and normalized expression were both included
- metadata/grouping columns included in `obs`
- any missing metadata values introduced by resolution
- optional conversion targets created, if any

## Current Boundaries

Current pagoda2.1 interop is intentionally focused:

- read: 10x triplets, CellRanger HDF5, h5ad, h5Seurat, loom
- write: RDS and h5ad
- convert in memory: list, SingleCellExperiment if installed, Seurat if
  installed

Do not promise export to every readable format unless the implementation exists.
