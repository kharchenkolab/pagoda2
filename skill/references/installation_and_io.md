# Installation And I/O

## Install From GitHub Devel

The recipe assumes pagoda2.1 from the GitHub `devel` branch:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
remotes::install_github("kharchenkolab/sccore", ref = "devel")
remotes::install_github("kharchenkolab/pagoda2", ref = "devel")
```

For local development inside this repository:

```sh
R CMD INSTALL ../sccore
R CMD INSTALL --no-byte-compile .
```

Do not install heavy ecosystem packages unless a specific optional conversion
needs them. Pagoda2 reads h5ad, h5Seurat, CellRanger HDF5, and loom directly
with its own readers.

## Supported Input Shapes

Autodetect:

```r
p2 <- Pagoda2$from(path, sample.name = "sample_01", verbose = TRUE)
```

Specific readers:

```r
p2 <- Pagoda2$from10x(path_to_triplet_dir)
p2 <- Pagoda2$from10xH5("filtered_feature_bc_matrix.h5")
p2 <- Pagoda2$fromAnnData("sample.h5ad")
p2 <- Pagoda2$fromH5Seurat("sample.h5seurat")
p2 <- Pagoda2$fromLoom("sample.loom")
```

Lower-level reader:

```r
counts <- readCounts(path, format = "auto")
imported <- readCounts(path, format = "auto", return.metadata = TRUE)
```

## Layer Choice

Counts should be raw integer-like values. For formats with multiple matrices,
choose the raw count layer explicitly:

```r
p2 <- Pagoda2$fromAnnData("sample.h5ad", reader.args = list(layer = "counts"))
p2 <- Pagoda2$fromH5Seurat("sample.h5seurat", reader.args = list(layer = "counts"))
p2 <- Pagoda2$fromLoom("sample.loom", reader.args = list(layer = "counts"))
```

If `integer_like` is false after loading, stop and check the selected layer.

## 10x Triplet Flexibility

Pagoda2 can detect standard 10x files and many renamed triplet attachments:

- matrix: `matrix.mtx`, `*.matrix.mtx.gz`, `*.mtx.gz`
- barcodes: `barcodes.tsv`, `*.barcodes.tsv.gz`
- genes/features: `features.tsv`, `genes.tsv`, renamed equivalents

When a directory contains multiple triplets, use `sample.pattern` in
`reader.args` to select one.

## Load Sanity Report

Always report:

```r
counts <- p2$getRawCounts()
data.frame(
  cells = nrow(counts),
  genes = ncol(counts),
  nonzero = Matrix::nnzero(counts),
  sparsity = 1 - Matrix::nnzero(counts) / prod(dim(counts)),
  integer_like = all(abs(counts@x - round(counts@x)) < 1e-8)
)
```

Also check for duplicate or missing names before analysis.
