# Installation And I/O

This reference explains how to install pagoda2.1 and how to load count data.
The main rule is: load raw integer-like counts, preserve cell and gene names,
and pass explicit file or layer arguments whenever guessing would be ambiguous.

## Install From GitHub Devel

The pagoda2.1 workflow assumes the GitHub `devel` branch:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
remotes::install_github("kharchenkolab/pagoda2", ref = "devel", dependencies = TRUE)
```

For local development inside a pagoda2 checkout:

```sh
R CMD INSTALL --no-byte-compile .
```

Do not run the package test suite as part of user installation. Run focused
tests only when editing package code.

Pagoda2.1 reads h5ad, h5Seurat, CellRanger HDF5, and loom with lean internal
readers. Do not install Seurat, SeuratDisk, reticulate, scanpy, or loomR just
to read those formats. Seurat and SingleCellExperiment are optional only for
in-memory conversion targets.

## Constructor Overview

Use `Pagoda2$from()` when the input is a path and either the format can be
inferred or you want to pass `format` explicitly:

```r
p2 <- Pagoda2$from("/path/to/input",
                   reader.args = list(sample.name = "sample_01"))
p2 <- Pagoda2$from("/path/to/input", format = "10x",
                   reader.args = list(sample.name = "sample_01"))
```

Use a format-specific constructor when the source format should be obvious in
the analysis script:

```r
p2 <- Pagoda2$from10x("/path/to/10x_triplet_dir")
p2 <- Pagoda2$from10xH5("/path/to/filtered_feature_bc_matrix.h5")
p2 <- Pagoda2$fromAnnData("/path/to/sample.h5ad")
p2 <- Pagoda2$fromH5Seurat("/path/to/sample.h5seurat")
p2 <- Pagoda2$fromLoom("/path/to/sample.loom")
```

All constructors call `readCounts()` internally, then build an R6 `Pagoda2`
object. Put reader options such as `sample.name`, `layer`, `gene.id`, and
explicit 10x files in `reader.args`. Put object construction options such as
`n.cores` or `verbose` outside it:

```r
p2 <- Pagoda2$fromAnnData(
  "sample.h5ad",
  reader.args = list(layer = "counts", gene.id = "symbol",
                     sample.name = "donor_A"),
  n.cores = 8,
  verbose = FALSE
)
```

Use `readCounts()` directly only when you want a sparse count matrix or want to
inspect imported metadata before constructing an object:

```r
counts <- readCounts("sample.h5ad", format = "h5ad", layer = "counts")
imported <- readCounts(
  "sample.h5ad",
  format = "h5ad",
  layer = "counts",
  return.metadata = TRUE
)
str(imported$cellMeta)
str(imported$geneMeta)
```

Important orientation rule: `readCounts()` returns a gene-by-cell sparse matrix.
`Pagoda2$from()` stores raw counts internally as cell-by-gene.

## Choosing Formats

Autodetection is convenient for routine paths:

```r
p2 <- Pagoda2$from("filtered_feature_bc_matrix")
p2 <- Pagoda2$from("sample.h5ad")
p2 <- Pagoda2$from("sample.h5seurat")
p2 <- Pagoda2$from("sample.loom")
```

Explicit `format` is safer when an extension is nonstandard or a directory
contains multiple candidate files:

```r
p2 <- Pagoda2$from("matrix_files", format = "10x")
p2 <- Pagoda2$from("cellranger_output.h5", format = "10x_h5")
p2 <- Pagoda2$from("custom_extension.dat", format = "h5ad")
```

Use `sample.name` to record sample identity in cell metadata:

```r
p2 <- Pagoda2$from("sample_dir", format = "10x",
                   reader.args = list(sample.name = "donor_A"))
p2$getCellMeta("sample")
```

Use `cell.prefix` when later combining samples would otherwise create duplicate
barcodes:

```r
p2 <- Pagoda2$from10x(
  "sample_dir",
  reader.args = list(cell.prefix = "donor_A")
)
```

## 10x Matrix Market Triplets

A 10x triplet has three files:

- a Matrix Market sparse count matrix, usually `matrix.mtx` or `matrix.mtx.gz`
- a barcode file, usually `barcodes.tsv` or `barcodes.tsv.gz`
- a feature/gene file, usually `features.tsv`, `features.tsv.gz`, `genes.tsv`,
  or `genes.tsv.gz`

Standard directory:

```r
p2 <- Pagoda2$from10x("filtered_feature_bc_matrix")
```

V3 feature files usually have gene id, gene symbol, and feature type columns.
V2 gene files usually have two columns. Let pagoda2 infer this with
`version = "auto"` or specify:

```r
p2_v3 <- Pagoda2$from10x("dir", reader.args = list(version = "V3"))
p2_v2 <- Pagoda2$from10x("dir", reader.args = list(version = "V2"))
```

Choose gene row names with `gene.id`:

```r
p2_symbol <- Pagoda2$from10x("dir", reader.args = list(gene.id = "symbol"))
p2_id <- Pagoda2$from10x("dir", reader.args = list(gene.id = "id"))
```

For multi-feature 10x V3 files, restrict to gene expression:

```r
p2 <- Pagoda2$from10x(
  "dir",
  reader.args = list(feature.type = "Gene Expression")
)
```

If selected gene names are duplicated, prefer stable IDs or make names unique
and report the choice:

```r
p2 <- Pagoda2$from10x("dir", reader.args = list(gene.id = "id"))
p2 <- Pagoda2$from10x("dir", reader.args = list(make.unique.genes = TRUE))
```

## Explicit 10x Triplet Files

When filenames are arbitrary, specify the three files. This is the clearest
approach for GEO/SRA/web attachments whose names no longer follow 10x
conventions.

Relative paths are resolved against the directory passed to `Pagoda2$from()`:

```r
p2 <- Pagoda2$from(
  "sample_dir",
  format = "10x",
  reader.args = list(
    files = list(
      matrix = "GSM5746259_custom_matrix.mtx.gz",
      barcodes = "GSM5746259_custom_cells.tsv.gz",
      features = "GSM5746259_custom_genes.tsv.gz"
    )
  )
)
```

The equivalent direct-argument form is useful when constructing `reader.args`
programmatically:

```r
p2 <- Pagoda2$from(
  "sample_dir",
  format = "10x",
  reader.args = list(
    matrix.file = "GSM5746259_custom_matrix.mtx.gz",
    barcodes.file = "GSM5746259_custom_cells.tsv.gz",
    features.file = "GSM5746259_custom_genes.tsv.gz"
  )
)
```

For 10x V2-style `genes.tsv` files:

```r
p2 <- Pagoda2$from(
  "sample_dir",
  format = "10x",
  reader.args = list(
    version = "V2",
    files = list(
      matrix = "counts.mtx.gz",
      barcodes = "cells.tsv.gz",
      genes = "genes.tsv.gz"
    )
  )
)
```

Absolute paths are accepted:

```r
p2 <- Pagoda2$from(
  ".",
  format = "10x",
  reader.args = list(
    matrix.file = "/data/sample/counts.anyname",
    barcodes.file = "/data/sample/cell_names.anyname",
    features.file = "/data/sample/gene_names.anyname"
  )
)
```

If a directory contains several detectable triplets and explicit files are not
available, use `sample.pattern`:

```r
p2 <- Pagoda2$from10x(
  "geo_raw_dir",
  reader.args = list(sample.pattern = "GSM5746259")
)
```

Prefer explicit files over `sample.pattern` when the target triplet is already
known.

## CellRanger HDF5

CellRanger HDF5 files are read with:

```r
p2 <- Pagoda2$from10xH5("filtered_feature_bc_matrix.h5")
```

Common options:

```r
p2 <- Pagoda2$from10xH5(
  "filtered_feature_bc_matrix.h5",
  reader.args = list(
    genome = "GRCh38",
    feature.type = "Gene Expression",
    gene.id = "symbol"
  )
)
```

Use `genome` only when the HDF5 file has multiple genome groups. Use
`feature.type` for feature-barcode matrices with RNA plus antibody or CRISPR
features.

## AnnData h5ad

AnnData may store raw counts in `X`, in `raw/X`, or in a named layer. Do not
assume `X` is raw counts.

Preferred when a counts layer exists:

```r
p2 <- Pagoda2$fromAnnData(
  "sample.h5ad",
  reader.args = list(layer = "counts", gene.id = "symbol")
)
```

Use `use.raw = TRUE` only when `raw/X` is the intended raw count matrix:

```r
p2 <- Pagoda2$fromAnnData(
  "sample.h5ad",
  reader.args = list(use.raw = TRUE, gene.id = "symbol")
)
```

If neither `layer` nor `use.raw` is supplied, the reader uses `X`. Verify
integer-likeness after loading; many scanpy workflows store normalized or log
values in `X`.

## h5Seurat

Pagoda2 reads h5Seurat directly without requiring the Seurat package:

```r
p2 <- Pagoda2$fromH5Seurat(
  "sample.h5seurat",
  reader.args = list(assay = "RNA", layer = "counts")
)
```

Use `assay` when the file has multiple assays. Use `layer = "counts"` whenever
possible; do not load normalized `data` as raw counts.

## Loom

Loom files may store matrices in the root `matrix` dataset or in named layers:

```r
p2 <- Pagoda2$fromLoom("sample.loom")
p2 <- Pagoda2$fromLoom("sample.loom", reader.args = list(layer = "counts"))
```

Dense loom matrices can be large. Tune `chunk.size` to trade memory and speed:

```r
p2 <- Pagoda2$fromLoom(
  "sample.loom",
  reader.args = list(layer = "counts", chunk.size = 2000L)
)
```

## Count Sanity Checks

After loading, check the loaded object rather than re-checking the directory
shape. Raw counts should be a sparse cell-by-gene `dgCMatrix` with integer-like
values:

```r
raw <- p2$getRawCounts()
stopifnot(inherits(raw, "dgCMatrix"))
stopifnot(all(abs(raw@x - round(raw@x)) < 1e-8))
stopifnot(!anyDuplicated(rownames(raw)))
stopifnot(!anyDuplicated(colnames(raw)))
cat(sprintf("Loaded %d cells x %d genes\n", nrow(raw), ncol(raw)))
```

If the integer-like check fails, stop and check layer selection before running
QC. Many h5ad/h5Seurat/loom files put normalized or log-transformed values in
the default matrix.

Use `describeMatrices()` only when debugging matrix/view state:

```r
p2$describeMatrices()
head(p2$getCellMeta())
head(p2$getGeneMeta())
```

## Troubleshooting I/O

Use these checks when loading fails:

- `Input path does not exist`: check the path visible to the R process.
- `No complete 10x triplet`: use explicit `files` or check that the directory
  has exactly one matrix/barcode/feature set.
- `Count matrix contains non-integer values`: a normalized/log layer was likely
  selected.
- duplicate selected gene names: use `gene.id = "id"` or
  `make.unique.genes = TRUE`, then report the choice.
- h5ad/h5Seurat/loom layer confusion: inspect available layers externally or
  with HDF5 tools, then pass the intended raw count layer explicitly.
