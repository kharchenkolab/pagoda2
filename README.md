[![<kharchenkolab>](https://circleci.com/gh/kharchenkolab/pagoda2.svg?style=svg)](https://app.circleci.com/pipelines/github/kharchenkolab/pagoda2)

<img src="https://github.com/kharchenkolab/pagoda2/blob/devel/inst/pagoda_logo.png" align="right" height="140">

# pagoda2

- [Pagoda2.1 Development Version](#pagoda21-development-version)
- [Tutorial](#tutorial)
- [Installation](#installation)
- [Citation](#citation)

## Pagoda2.1 Development Version

**Note:** this branch is a development version of pagoda2.1. It is intended for testing the next pagoda2 API and workflow design, not as a frozen CRAN-style release.

Highlights in this development branch:

- cleaner single-dataset R6 workflow API with `Pagoda2$from()`, `p2$run()`, `p2$runQC()`, `p2$filterData()`, `p2$plotEmbedding()`, and marker plotting methods;
- support for common scRNA-seq import formats, including 10x triplets, 10x/CellRanger HDF5, AnnData h5ad, h5Seurat, and loom;
- export support for native RDS and h5ad, plus optional in-memory conversion to list, SingleCellExperiment, or Seurat objects;
- smaller memory footprint through sparse raw counts and lightweight normalized matrix views instead of storing a full duplicate normalized matrix;
- reduced dependency footprint, with heavy ecosystem packages kept optional where possible;
- agent integration through the repository-local pagoda2.1 skill in [`skill/`](skill/).

Web app support is currently disabled in this development version while the app export and frontend integration are redesigned.

Pagoda2 is an R package for analyzing large-scale single-cell RNA-seq datasets. The methods were optimized to rapidly process modern scRNAseq datasets, which are both large and sparse. The package provides methods for quality control, filtering, clustering, visualization, differential expression, cross-cutting aspects/states, and geneset/pathway overdispersion analysis.

Note that `pagoda2` is an R package developed for analyzing standalone scRNAseq datasets. For joint analysis of multiple datasets, please see the package [conos](https://github.com/kharchenkolab/conos). (The package `pagoda2` is primarily used to preprocess input datasets for conos.)

Several methods within this package were developed based on the originals implemented within [SCDE](https://hms-dbmi.github.io/scde/) and PAGODA1.

## Tutorial

The current pagoda2.1 single-dataset workflow vignette is available as a GitHub-viewable notebook:

- [Pagoda2.1 single-dataset workflow notebook](doc/pagoda2.1-single-dataset.ipynb)

Development notes:

- [Matrix storage and views](doc/pagoda2.1-matrix-views.md)
- [Notebook rendering](doc/notebook_rendering.md)
- [Agent skill and recipe](skill/)

## Installation

Install this development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("kharchenkolab/sccore", ref = "devel")
remotes::install_github("kharchenkolab/pagoda2", ref = "devel")
```

## Citation

If you find `pagoda2` useful for your publication, please cite:

```
Nikolas Barkas, Viktor Petukhov, Peter Kharchenko, Simon Steiger, 
Rasmus Rydbirk, and Evan Biederstedt (2021). pagoda2: Single Cell 
Analysis and Differential Expression. R package version 1.1.1.
```
