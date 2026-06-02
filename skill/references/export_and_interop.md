# Export And Interoperability

## Native Persistence

Always save the pagoda2 object with standard R serialization:

```r
saveRDS(p2, "pagoda2_processed.rds")
p2 <- readRDS("pagoda2_processed.rds")
```

Use RDS for pagoda2-native continuation.

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
matrix dimensions. Export should resolve metadata first, then write.

## In-Memory Conversion

List conversion:

```r
as_list <- p2$as("list")
```

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
needs the in-memory object.

## Export Report

Report:

- path of native RDS
- path of h5ad, if written
- cells and genes in exported matrices
- whether raw counts and normalized expression were both included
- metadata/grouping columns included in `obs`
- any missing metadata values introduced by resolution

## Round-Trip Sanity

When export code changes, test read-back:

```r
p2$export("tmp.h5ad", format = "h5ad", overwrite = TRUE)
imported <- readCounts("tmp.h5ad", format = "h5ad", return.metadata = TRUE)
dim(imported$counts)
```

For deeper validation, compare raw counts against `layers/counts` and normalized
values against `X` using a small fixture.
