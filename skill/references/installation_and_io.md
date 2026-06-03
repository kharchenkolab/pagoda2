# Installation And I/O

This reference covers installation and input loading for pagoda2.1. The core
rule is simple: load raw integer-like counts, state exactly which file or layer
was used, and route reader-specific arguments through `reader.args`.

## Install From GitHub Devel

Pagoda2.1 is currently a development branch. Install from GitHub `devel`:

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("hdf5r", quietly = TRUE)) {
  install.packages("hdf5r")
}
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
if (!requireNamespace("R.utils", quietly = TRUE)) {
  install.packages("R.utils")
}

remotes::install_github("kharchenkolab/pagoda2", ref = "devel",
                        dependencies = TRUE, upgrade = "never")
```

For local development inside a pagoda2 checkout:

```sh
R CMD INSTALL --no-byte-compile .
```

Do not run the package test suite during user analysis or routine installation.
Run focused tests only when editing pagoda2 source.

## Constructor Pattern

Use `Pagoda2$from()` when the input path should be auto-detected or when the
format is passed explicitly:

```r
p2 <- Pagoda2$from(
  "/path/to/input",
  format = "auto",
  reader.args = list(sample.name = "sample_01"),
  verbose = FALSE
)
```

Use a specific constructor when that makes the script clearer:

```r
p2 <- Pagoda2$from10x("/path/to/10x_triplet_directory",
                      reader.args = list(sample.name = "sample_01"),
                      verbose = FALSE)
p2 <- Pagoda2$from10xH5("/path/to/filtered_feature_bc_matrix.h5",
                        reader.args = list(sample.name = "sample_01"),
                        verbose = FALSE)
p2 <- Pagoda2$fromAnnData("/path/to/sample.h5ad",
                          reader.args = list(layer = "counts",
                                             sample.name = "sample_01"),
                          verbose = FALSE)
p2 <- Pagoda2$fromH5Seurat("/path/to/sample.h5seurat",
                           reader.args = list(assay = "RNA", layer = "counts",
                                              sample.name = "sample_01"),
                           verbose = FALSE)
p2 <- Pagoda2$fromLoom("/path/to/sample.loom",
                       reader.args = list(layer = "counts",
                                          sample.name = "sample_01"),
                       verbose = FALSE)
```

Routing matters:

- `sample.name`, `sample.pattern`, `matrix.file`, `barcodes.file`,
  `features.file`, `genes.file`, `files`, `layer`, `assay`, `gene.id`,
  `feature.type`, and `cell.prefix` are reader arguments. Put them in
  `reader.args = list(...)`.
- `n.cores`, `threads`, and `verbose` are constructor/runtime arguments. Put
  them at top level.

Example with both:

```r
p2 <- Pagoda2$fromAnnData(
  "/path/to/sample.h5ad",
  reader.args = list(layer = "counts", gene.id = "symbol",
                     sample.name = "donor_A"),
  threads = list(total = 8, sgd = 1),
  verbose = FALSE
)
```

## Direct Reader Use

Use `readCounts()` directly when a downstream function needs only a sparse
matrix or when imported metadata should be inspected before constructing a
pagoda2 object:

```r
stopifnot(inherits(readCounts("/path/to/sample.h5ad",
                              format = "h5ad",
                              layer = "counts"), "dgCMatrix"))

imported <- readCounts("/path/to/sample.h5ad",
                       format = "h5ad",
                       layer = "counts",
                       return.metadata = TRUE)
str(imported$cellMeta)
str(imported$geneMeta)
```

Important orientation difference:

- `readCounts()` returns a gene-by-cell sparse count matrix.
- `Pagoda2$from*()` stores raw counts as cell-by-gene in the object.

## Format Autodetection

Autodetection handles common cases:

```r
p2 <- Pagoda2$from("/path/to/filtered_feature_bc_matrix",
                   reader.args = list(sample.name = "sample_01"))
p2 <- Pagoda2$from("/path/to/sample.h5ad",
                   reader.args = list(layer = "counts",
                                      sample.name = "sample_01"))
p2 <- Pagoda2$from("/path/to/sample.h5seurat",
                   reader.args = list(assay = "RNA", layer = "counts",
                                      sample.name = "sample_01"))
p2 <- Pagoda2$from("/path/to/sample.loom",
                   reader.args = list(layer = "counts",
                                      sample.name = "sample_01"))
```

Use explicit `format` when the extension is unusual, when a `.h5` file could be
CellRanger HDF5 or another HDF5 layout, or when an input directory contains
multiple candidate triplets:

```r
p2 <- Pagoda2$from("/path/to/matrix_files", format = "10x",
                   reader.args = list(sample.name = "sample_01"))
p2 <- Pagoda2$from("/path/to/cellranger_output.h5", format = "10x_h5",
                   reader.args = list(sample.name = "sample_01"))
p2 <- Pagoda2$from("/path/to/custom_extension.dat", format = "h5ad",
                   reader.args = list(layer = "counts",
                                      sample.name = "sample_01"))
```

Supported format names include `auto`, `10x`, `10x_h5`, `h5ad`, `h5seurat`,
and `loom`.

## 10x Matrix Market Triplets

A 10x triplet has:

- a Matrix Market sparse matrix, usually `matrix.mtx` or `matrix.mtx.gz`
- a barcode file, usually `barcodes.tsv` or `barcodes.tsv.gz`
- a feature/gene file, usually `features.tsv`, `features.tsv.gz`, `genes.tsv`,
  or `genes.tsv.gz`

Vignette-style local folder with one downloaded triplet:

```r
p2 <- Pagoda2$from10x("data", verbose = FALSE)
cat(sprintf("Loaded %d cells x %d genes\n",
            nrow(p2$getRawCounts()), ncol(p2$getRawCounts())))
```

Standard CellRanger directory:

```r
p2 <- Pagoda2$from10x(
  "/path/to/filtered_feature_bc_matrix",
  reader.args = list(sample.name = "sample_01"),
  verbose = FALSE
)
```

10x V3 feature files usually have gene id, symbol, and feature type columns.
10x V2 gene files usually have two columns. Let pagoda2 infer this by default
or specify it when needed:

```r
p2 <- Pagoda2$from10x("/path/to/10x_v3",
                      reader.args = list(version = "V3",
                                         sample.name = "sample_01"))
p2 <- Pagoda2$from10x("/path/to/10x_v2",
                      reader.args = list(version = "V2",
                                         sample.name = "sample_01"))
```

Choose gene row names explicitly if symbols are duplicated or unstable:

```r
p2 <- Pagoda2$from10x("/path/to/10x_directory",
                      reader.args = list(gene.id = "symbol",
                                         sample.name = "sample_01"))
p2 <- Pagoda2$from10x("/path/to/10x_directory",
                      reader.args = list(gene.id = "id",
                                         sample.name = "sample_01"))
```

For multi-feature 10x files, restrict to RNA:

```r
p2 <- Pagoda2$from10x(
  "/path/to/filtered_feature_bc_matrix",
  reader.args = list(feature.type = "Gene Expression",
                     sample.name = "sample_01")
)
```

Use `cell.prefix` if later sample combination would otherwise duplicate
barcodes:

```r
p2 <- Pagoda2$from10x(
  "/path/to/filtered_feature_bc_matrix",
  reader.args = list(cell.prefix = "donor_A",
                     sample.name = "donor_A")
)
```

## Explicit 10x Triplet Files

Use explicit triplet filenames when GEO/SRA/web attachments have custom names.
This is usually clearer than relying on patterns, symlinks, or renaming.
Relative file names are resolved against the directory passed as `path`.

```r
p2 <- Pagoda2$from10x(
  "/path/to/geo_triplet_directory",
  reader.args = list(
    sample.name = "GSM5746259",
    files = list(
      matrix = "GSM5746259_MGI0369_1_SLAB-145-0.matrix.mtx.gz",
      barcodes = "GSM5746259_MGI0369_1_SLAB-145-0.barcodes.tsv.gz",
      features = "GSM5746259_MGI0369_1_SLAB-145-0.features.tsv.gz"
    )
  ),
  verbose = FALSE
)
```

Equivalent explicit-argument form:

```r
p2 <- Pagoda2$from(
  "/path/to/geo_triplet_directory",
  format = "10x",
  reader.args = list(
    sample.name = "GSM5746259",
    matrix.file = "GSM5746259_MGI0369_1_SLAB-145-0.matrix.mtx.gz",
    barcodes.file = "GSM5746259_MGI0369_1_SLAB-145-0.barcodes.tsv.gz",
    features.file = "GSM5746259_MGI0369_1_SLAB-145-0.features.tsv.gz"
  ),
  verbose = FALSE
)
```

For V2-style gene files:

```r
p2 <- Pagoda2$from10x(
  "/path/to/geo_triplet_directory",
  reader.args = list(
    version = "V2",
    sample.name = "sample_01",
    files = list(
      matrix = "counts.mtx.gz",
      barcodes = "cells.tsv.gz",
      genes = "genes.tsv.gz"
    )
  ),
  verbose = FALSE
)
```

If the target triplet is known, explicit files are preferred. If a directory
contains multiple detectable triplets and explicit files are not yet known,
use `sample.pattern` as a fallback:

```r
p2 <- Pagoda2$from10x(
  "/path/to/geo_raw_directory",
  reader.args = list(sample.pattern = "GSM5746259",
                     sample.name = "GSM5746259"),
  verbose = FALSE
)
```

## HDF5-Based Readers

CellRanger HDF5:

```r
p2 <- Pagoda2$from10xH5(
  "/path/to/filtered_feature_bc_matrix.h5",
  reader.args = list(feature.type = "Gene Expression",
                     genome = NULL,
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

AnnData h5ad:

```r
p2 <- Pagoda2$fromAnnData(
  "/path/to/sample.h5ad",
  reader.args = list(layer = "counts",
                     use.raw = FALSE,
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

Use `layer = NULL` to read `X` only when `X` is known to be raw counts.
Use `use.raw = TRUE` only when the h5ad `raw` group contains the intended raw
count matrix.

h5Seurat:

```r
p2 <- Pagoda2$fromH5Seurat(
  "/path/to/sample.h5seurat",
  reader.args = list(assay = "RNA",
                     layer = "counts",
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

loom:

```r
p2 <- Pagoda2$fromLoom(
  "/path/to/sample.loom",
  reader.args = list(layer = "counts",
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

For all HDF5 formats, stop if the chosen matrix is not integer-like:

```r
stopifnot(all(abs(p2$getRawCounts()@x - round(p2$getRawCounts()@x)) < 1e-8))
```

If the check fails, the object may have been built from normalized expression
instead of counts. Reload from the count layer rather than trying to repair the
matrix after import.

## Import Report

Report these items after loading:

- constructor and format used
- sample name recorded in metadata
- exact 10x triplet filenames or HDF5 layer/assay used
- dimensions as cells by genes from `dim(p2$getRawCounts())`
- raw count class, usually `dgCMatrix`
- whether nonzero entries are integer-like
- any gene-name decision, such as symbol versus stable ID or unique-name repair

If the integer-like check fails, do not continue with QC. Choose the raw count
layer or ask the user which layer contains counts.
