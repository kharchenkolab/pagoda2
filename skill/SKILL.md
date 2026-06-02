---
name: pagoda2-scrna-v2
description: Use for pagoda2.1 single-sample scRNA-seq analysis with QC, filtering, variance modeling, PCA, UMAP, Leiden clustering, marker genes, plotting, and common scRNA-seq input/output formats. Start with aba_recipe.md, then load only the reference files needed for the current decision.
---

# Pagoda2 scRNA-seq Skill

Use this skill when a user wants a pagoda2-first single-dataset scRNA-seq
workflow, or when an agent needs a persistent recipe for pagoda2.1 in an
analysis chat system.

The recipe is intentionally pagoda2-native. Do not translate the workflow into
Seurat or scanpy unless the user explicitly asks for a conversion or comparison.
Pagoda2.1 reads common input formats directly and keeps raw counts in a sparse
matrix with normalized expression represented as a lightweight view.

## Start Here

Read `aba_recipe.md` first. It gives the end-to-end workflow and the
user-facing checkpoints. Use the reference files for details and variants:

- `references/installation_and_io.md`: install from GitHub `devel`; use
  `Pagoda2$from()`, `from10x()`, `fromAnnData()`, `readCounts()`, explicit 10x
  triplet file arguments, layer selection, and count sanity checks.
- `references/qc_and_filtering.md`: use `runQC()`, `plotQC()`,
  `plotQCViolin()`, `filterCells()`, and `filterData()`; decide when to pass
  mitochondrial/ribosomal gene patterns; report filtering impact.
- `references/matrix_and_metadata_model.md`: understand raw count storage,
  normalized views, matrix orientation, flexible `cellMeta`/`geneMeta`,
  resolved metadata, groupings, annotations, and factor colors.
- `references/workflow_and_clustering.md`: run staged or full workflows with
  `run()`, `runVariance()`, `runPCA()`, `runGraph()`, `runUMAP()`, and
  `runLeiden()`; assess OD genes, PCA, graph/UMAP, and clustering.
- `references/markers_and_plots.md`: calculate markers with `runMarkers()`,
  inspect/rank with `getTopMarkers()`, and plot with `plotMarkerDotPlot()` and
  `plotMarkerHeatmap()`.
- `references/export_and_interop.md`: save RDS, export h5ad, use `as("list")`,
  `as("sce")`, and `as("seurat")`, and respect strict metadata axes in foreign
  formats.

## Minimal Usage Pattern

```r
library(pagoda2)

p2 <- Pagoda2$from("sample_dir", format = "10x", sample.name = "sample_01")
p2$runQC(verbose = TRUE)
p2$filterData(verbose = TRUE)
p2$run(steps = c("variance", "pca", "graph", "umap", "leiden"),
       plots = "none", verbose = TRUE)
p2$runMarkers(grouping = "leiden", name = "leiden", verbose = TRUE)
```

Show the user at least QC, PCA elbow, UMAP, marker dotplot, and marker heatmap.
Report count-layer sanity, QC loss, clustering settings, marker quality, and
saved/exported outputs.

## Core Invariants

- `Pagoda2` is an R6 object. Use `$` methods and fields.
- Raw counts live in `p2$rawCounts` as a sparse cell-by-gene matrix.
- Do not restore or depend on the old full normalized `$counts` slot.
- Normalized expression is represented by a matrix view and materialized only
  for selected cells/genes.
- `readCounts()` returns a gene-by-cell sparse matrix; `Pagoda2$from()` creates
  a `Pagoda2` object with cell-by-gene raw storage.
- Groupings are discrete `p2$cellMeta` columns. The default grouping is a
  pointer to one such column, not a separate active identity vector.
- `cellMeta` and `geneMeta` may be flexible internally. Resolve metadata with
  `resolveCellMeta()` or `resolveGeneMeta()` when a method/export needs exact
  alignment.
- Factor colors should resolve through pagoda2's central color handling rather
  than ad hoc plot-local palettes.

## Repository Scope

This skill lives inside the pagoda2 repository. Treat the repository root as the
working scope. Do not assume any sibling checkout exists.

## Development Checks

For ordinary analysis, installing and loading pagoda2 is enough. Do not run the
package test suite as part of user analysis.

For source edits, a developer can install from the repository root:

```sh
R CMD INSTALL --no-byte-compile .
```

Run focused tests only when changing package code:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test_io.R")'
```
