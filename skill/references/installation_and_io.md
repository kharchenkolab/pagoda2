# Installation And I/O

This reference covers pagoda2.1 installation and input loading. The core rule:
load raw integer-like counts, state exactly which file or layer was used, and
route reader-specific arguments through `reader.args`.

## Install From GitHub Devel

Pagoda2.1 is currently the GitHub `devel` branch. Install it before running
the recipe:

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))

for (pkg in c("remotes", "ggplot2", "hdf5r", "data.table", "R.utils",
              "uwot", "leidenAlg")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# pagoda2.1 requires sccore >= 1.1.0 (native heatmap engine); on GitHub `dev`
# until the next CRAN release. Install from source first so dependencies = TRUE
# below does not pull the older CRAN sccore.
if (!requireNamespace("sccore", quietly = TRUE) ||
    utils::packageVersion("sccore") < "1.1.0") {
  remotes::install_github("kharchenkolab/sccore", ref = "dev", upgrade = "never")
}

if (!requireNamespace("pagoda2", quietly = TRUE) ||
    !identical(get("Pagoda2", envir = asNamespace("pagoda2"))$public_fields$apiVersion, "2.2")) {
  remotes::install_github("kharchenkolab/pagoda2", ref = "devel",
                          dependencies = TRUE, upgrade = "never")
}

library(pagoda2)
stopifnot(identical(pagoda2::Pagoda2$public_fields$apiVersion, "2.2"))
stopifnot(utils::packageVersion("sccore") >= "1.1.0")
```

Do not install Seurat, SeuratDisk, scanpy, reticulate, or loomR just to read
their file formats. Pagoda2 reads h5ad, h5Seurat, and loom directly. Seurat
and SingleCellExperiment are optional only for in-memory conversion.

## Constructor Routing

Source-verified constructor shape:

```text
Pagoda2$from(x, format = NULL, reader.args = list(), ...)
Pagoda2$from10x(path, reader.args = list(), ...)
Pagoda2$from10xH5(path, reader.args = list(), ...)
Pagoda2$fromAnnData(path, reader.args = list(), ...)
Pagoda2$fromH5Seurat(path, reader.args = list(), ...)
Pagoda2$fromLoom(path, reader.args = list(), ...)
```

`reader.args` is passed to `readCounts()`. Remaining `...` are passed to
`Pagoda2$new()`. Therefore:

- `sample.name`, `sample.pattern`, `version`, `gene.id`, `feature.type`,
  `genome`, `assay`, `layer`, `use.raw`, `make.unique.genes`, `cell.prefix`,
  `matrix.file`, `barcodes.file`, `features.file`, `genes.file`, `files`,
  `validate.integer`, and `chunk.size` belong inside `reader.args`.
- `verbose`, `n.cores`, and `threads` can be top-level constructor/runtime
  arguments. Top-level `verbose` is also forwarded to the reader when the
  reader did not receive its own `verbose` value.

Correct routing:

```r
p2 <- Pagoda2$fromAnnData(
  "/path/to/sample.h5ad",
  reader.args = list(layer = "counts", gene.id = "symbol",
                     sample.name = "donor_A"),
  threads = list(total = 8, sgd = 1),
  verbose = FALSE
)
```

Do not flatten reader arguments at top level. A call that puts `layer` or
`sample.name` outside `reader.args` configures the constructor, not the
reader, and does not select the intended input layer.

## Direct Reader Use

Use `readCounts()` directly when you want to inspect an imported matrix and
metadata before constructing a pagoda2 object:

```r
imported <- readCounts(
  "/path/to/sample.h5ad",
  format = "h5ad",
  layer = "counts",
  sample.name = "sample_01",
  return.metadata = TRUE,
  verbose = TRUE
)

stopifnot(inherits(imported$counts, "dgCMatrix"))
stopifnot(all(abs(imported$counts@x - round(imported$counts@x)) < 1e-8))
str(imported$cellMeta)
str(imported$geneMeta)
```

Orientation matters:

- `readCounts()` returns gene-by-cell counts.
- `Pagoda2$from*()` stores raw counts as cell-by-gene.

## Auto-Detect Versus Specific Constructors

Use `Pagoda2$from()` when auto-detection is enough:

```r
p2 <- Pagoda2$from(
  "/path/to/filtered_feature_bc_matrix",
  format = "auto",
  reader.args = list(sample.name = "sample_01"),
  verbose = FALSE
)
```

Use a specific constructor when clarity matters:

```r
p2 <- Pagoda2$from10x("/path/to/filtered_feature_bc_matrix",
                      reader.args = list(sample.name = "sample_01"),
                      verbose = FALSE)

p2 <- Pagoda2$from10xH5("/path/to/filtered_feature_bc_matrix.h5",
                        reader.args = list(sample.name = "sample_01"),
                        verbose = FALSE)
# Multimodal 10x H5 (CITE-seq / multiome): from10xH5 reads the feature_type column
# and builds facets automatically -- Gene Expression->RNA, Antibody Capture->ADT (CLR),
# Peaks->ATAC (TF-IDF). p2$listFacets() shows them. See references/multimodal_facets.md.

p2 <- Pagoda2$fromAnnData("/path/to/sample.h5ad",
                          reader.args = list(layer = "counts",
                                             sample.name = "sample_01"),
                          verbose = FALSE)

p2 <- Pagoda2$fromH5Seurat("/path/to/sample.h5seurat",
                           reader.args = list(assay = "RNA",
                                              layer = "counts",
                                              sample.name = "sample_01"),
                           verbose = FALSE)

p2 <- Pagoda2$fromLoom("/path/to/sample.loom",
                       reader.args = list(layer = "counts",
                                          sample.name = "sample_01"),
                       verbose = FALSE)
```

Supported `readCounts(format = ...)` values are `auto`, `10x`, `10x_h5`,
`h5ad`, `h5seurat`, and `loom`.

## 10x Matrix Market Triplets

A 10x Matrix Market input consists of:

- a matrix file such as `matrix.mtx.gz`
- a barcode file such as `barcodes.tsv.gz`
- a features or genes file such as `features.tsv.gz` or `genes.tsv.gz`

One standard triplet directory:

```r
p2 <- Pagoda2$from10x(
  "/path/to/filtered_feature_bc_matrix",
  reader.args = list(sample.name = "sample_01"),
  verbose = FALSE
)
```

Directory with multiple renamed GEO triplets; select one by regex:

```r
p2 <- Pagoda2$from10x(
  "/path/to/geo_bundle",
  reader.args = list(sample.pattern = "GSM5746259",
                     sample.name = "GSM5746259"),
  verbose = FALSE
)
```

Explicit triplet filenames; use this when names are arbitrary or
auto-detection is ambiguous:

```r
p2 <- Pagoda2$from10x(
  "/path/to/geo_bundle",
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

Explicit file args can also be written separately:

```r
p2 <- Pagoda2$from10x(
  "/path/to/geo_bundle",
  reader.args = list(
    sample.name = "GSM5746259",
    matrix.file = "GSM5746259_MGI0369_1_SLAB-145-0.matrix.mtx.gz",
    barcodes.file = "GSM5746259_MGI0369_1_SLAB-145-0.barcodes.tsv.gz",
    features.file = "GSM5746259_MGI0369_1_SLAB-145-0.features.tsv.gz"
  ),
  verbose = FALSE
)
```

Feature identifier choice is case-sensitive: use `gene.id = "symbol"` or
`gene.id = "id"`, not `"SYMBOL"` or `"ENSEMBL"`.

```r
p2 <- Pagoda2$from10x(
  "/path/to/filtered_feature_bc_matrix",
  reader.args = list(gene.id = "symbol", sample.name = "sample_01"),
  verbose = FALSE
)
```

Feature-type filtering is useful for 10x multi-feature files:

```r
p2 <- Pagoda2$from10x(
  "/path/to/filtered_feature_bc_matrix",
  reader.args = list(feature.type = "Gene Expression",
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

## HDF5-Backed Formats

CellRanger HDF5:

```r
p2 <- Pagoda2$from10xH5(
  "/path/to/filtered_feature_bc_matrix.h5",
  reader.args = list(gene.id = "symbol",
                     feature.type = "Gene Expression",
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

AnnData h5ad. Choose `layer = "counts"` when counts are stored in a layer:

```r
p2 <- Pagoda2$fromAnnData(
  "/path/to/sample.h5ad",
  reader.args = list(layer = "counts",
                     gene.id = "symbol",
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

If AnnData raw counts are stored in `.raw` rather than a layer:

```r
p2 <- Pagoda2$fromAnnData(
  "/path/to/sample.h5ad",
  reader.args = list(use.raw = TRUE,
                     gene.id = "symbol",
                     sample.name = "sample_01"),
  verbose = FALSE
)
```

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
                     gene.id = "symbol",
                     sample.name = "sample_01",
                     chunk.size = 1000),
  verbose = FALSE
)
```

## Load Sanity Checks

After any constructor:

```r
stopifnot(identical(p2$apiVersion, "2.2"))
stopifnot(inherits(p2$getRawCounts(), "dgCMatrix"))
stopifnot(all(abs(p2$getRawCounts()@x - round(p2$getRawCounts()@x)) < 1e-8))

cat(sprintf("Loaded %d cells x %d genes\n",
            nrow(p2$getRawCounts()), ncol(p2$getRawCounts())))
```

If the integer-like check fails, do not proceed. Re-read the input using the
raw count layer or file. Downstream QC, variance modeling, and marker results
assume count-like input.

## I/O Report

Report:

- input format and constructor used
- selected layer, assay, feature type, and gene identifier mode when relevant
- sample name or sample pattern used
- cells, genes, and integer-like count check
- whether imported metadata was attached to `cellMeta` or `geneMeta`
- any ambiguity that required explicit file or layer selection
