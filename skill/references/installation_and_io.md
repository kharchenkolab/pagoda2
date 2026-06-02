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

For arbitrary filenames, specify the triplet explicitly. Relative file paths are
resolved against the directory passed to `Pagoda2$from()`:

```r
p2 <- Pagoda2$from(
  "sample_dir",
  format = "10x",
  reader.args = list(
    files = list(
      matrix = "custom_matrix_name.mtx.gz",
      barcodes = "custom_cells.tsv.gz",
      features = "custom_genes.tsv.gz"
    )
  )
)
```

Equivalent direct arguments are also supported:

```r
p2 <- Pagoda2$from(
  "sample_dir",
  format = "10x",
  reader.args = list(
    matrix.file = "custom_matrix_name.mtx.gz",
    barcodes.file = "custom_cells.tsv.gz",
    features.file = "custom_genes.tsv.gz"
  )
)
```

For V2-style 10x annotations, use `genes = ...` in the `files` list or
`genes.file = ...` with `version = "V2"`. When a directory contains multiple
detectable triplets and you do not specify files explicitly, use
`sample.pattern` in `reader.args` to select one.

## Load Sanity Report

Always report:

```r
raw_dim <- dim(p2$getRawCounts())
raw_nnz <- Matrix::nnzero(p2$getRawCounts())
data.frame(
  cells = raw_dim[1],
  genes = raw_dim[2],
  nonzero = raw_nnz,
  sparsity = 1 - raw_nnz / prod(raw_dim),
  integer_like = all(abs(p2$getRawCounts()@x - round(p2$getRawCounts()@x)) < 1e-8)
)
```

Also check for duplicate or missing names before analysis.
