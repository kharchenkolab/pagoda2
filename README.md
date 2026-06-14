[![<kharchenkolab>](https://circleci.com/gh/kharchenkolab/pagoda2.svg?style=svg)](https://app.circleci.com/pipelines/github/kharchenkolab/pagoda2)

<img src="https://github.com/kharchenkolab/pagoda2/blob/devel/inst/pagoda_logo.png" align="right" height="140">

# pagoda2

- [Pagoda2.1 Development Version](#pagoda21-development-version)
- [Tutorials](#tutorials)
- [Installation](#installation)
- [Citation](#citation)

## Pagoda2.1 Development Version

**Note:** this branch is a development version of pagoda2.1. It is intended for testing the next pagoda2 API and workflow design, not as a frozen CRAN-style release.

Pagoda2 is an R package for analyzing single-cell datasets. The methods were optimized to rapidly process modern single-cell data, which is large and sparse. Pagoda2.1 keeps the **raw counts as the canonical matrix** and computes normalized "analysis views" on the fly from a small recipe, so it stays memory-lean and can run **out-of-core** off a portable on-disk store.

Pagoda2.1 is now **multimodal**: a dataset is a set of **facets** (one molecular modality each — RNA, CITE-seq protein, ATAC peaks, ...), all sharing the same cells, with cross-modality integration.

Highlights in this development branch:

- **One R6 object, a generic step API.** `Pagoda2$from...()` to import, then `p2$runQC()`, `p2$filterData()`, `p2$runVariance()`, and the generic pipeline steps `p2$runReduction()`, `p2$runGraph()`, `p2$runClustering()`, `p2$runEmbedding()` — the algorithm is always a `method=` (e.g. `method = "pca"`, `"leiden"`, `"wnn"`), so there is no `runPCA`/`runLeiden`/`runWNN`. `p2$run()` chains the standard workflow.
- **Multimodal via facets.** `p2$addFacet()` adds a modality; native 10x multimodal H5 import maps `feature_type` to facets automatically (`Gene Expression`→RNA, `Antibody Capture`→ADT/CLR, `Peaks`→ATAC/TF-IDF). Per-facet normalization view models: `plain` (RNA), `clr` (CITE-seq protein), `tfidf` (ATAC).
- **Cross-modality integration.** `runGraph(method = "wnn")` (weighted nearest neighbors), and reduction-level `runReduction(facets = ..., method = "cca" | "scca" | "concat")` — joints are stored as ordinary name-keyed products (`reductions[["WNN"]]`, `reductions[["CCA"]]`).
- **Memory-lean and disk-backed.** Sparse raw counts + lightweight view recipes instead of a duplicate normalized matrix; a facet's counts can live in a portable **lstar Zarr** store and stream in bounded memory (`backend = "lstar"`), bit-identical to in-memory.
- **Fast.** Threaded approximate-kNN (RcppHNSW) for all graph building; RSpectra truncated SVD for PCA/LSI.
- **I/O.** Imports 10x triplets, 10x/CellRanger HDF5 (incl. multimodal), AnnData h5ad, h5Seurat, and loom; exports native RDS, h5ad, and lstar Zarr; optional in-memory conversion to list, SingleCellExperiment, or Seurat.
- **Agent integration** through the repository-local pagoda2.1 skill in [`skill/`](skill/).

For joint analysis of **multiple datasets**, see [conos](https://github.com/kharchenkolab/conos) (pagoda2 preprocesses its input samples). Several methods here build on [SCDE](https://hms-dbmi.github.io/scde/) and PAGODA1. Web app support is currently disabled in this development version while the app export is redesigned.

## Tutorials

GitHub-viewable notebooks (rendered from the `.Rmd` sources in [`doc/`](doc/); see [notebook rendering](doc/notebook_rendering.md)):

- [**Single-dataset scRNA-seq workflow**](doc/pagoda2.1-single-dataset.ipynb) — import → QC → variance/HVG → PCA → graph → UMAP → Leiden → markers, on a PBMC sample.
- [**CITE-seq (RNA + protein)**](doc/pagoda2.1-citeseq.ipynb) — a 10x CITE-seq sample as RNA + ADT facets (CLR), per-facet reductions, **WNN** integration, joint clustering, and RNA + protein markers.
- [**ATAC / 10x multiome (RNA + ATAC)**](doc/pagoda2.1-multiome.ipynb) — a 10x multiome sample as RNA + ATAC facets, RNA PCA + ATAC **LSI** (TF-IDF→SVD), **WNN** integration, and joint clustering.

More notes:

- [Matrix storage and views](doc/pagoda2.1-matrix-views.md)
- [Notebook rendering](doc/notebook_rendering.md)
- [Disk-backing benchmarks (in-memory vs lstar-zarr)](benchmark/README.md)
- [Agent skill and recipe](skill/)

## Installation

Install this development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("kharchenkolab/sccore", ref = "devel")
remotes::install_github("kharchenkolab/pagoda2", ref = "devel")
```

The multimodal Zarr import/export and disk backing additionally require the `lstar` R package.

## Citation

If you find `pagoda2` useful for your publication, please cite:

```
Nikolas Barkas, Viktor Petukhov, Peter Kharchenko, Simon Steiger, 
Rasmus Rydbirk, and Evan Biederstedt (2021). pagoda2: Single Cell 
Analysis and Differential Expression. R package version 1.1.1.
```
