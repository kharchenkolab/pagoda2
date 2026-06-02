---
name: pagoda2-scrna-v2
description: Use for pagoda2.1 single-sample scRNA-seq analysis with QC, filtering, variance modeling, PCA, UMAP, Leiden clustering, marker genes, plotting, and common scRNA-seq input/output formats. Start with aba_recipe.md and load references only as needed.
---

# Pagoda2 scRNA-seq Skill

Use this skill when a user wants a pagoda2-first single-dataset scRNA-seq
analysis, or when an agent in the graphical chat system needs a persistent,
entity-oriented recipe for pagoda2.1.

## Start Here

Read `aba_recipe.md` first. It is the main executable recipe and includes the
required user-facing checkpoints.

Load reference files only when the task needs more detail:

- `references/installation_and_io.md`: GitHub `devel` install, local source
  install, supported readers, and format selection.
- `references/qc_and_filtering.md`: QC metrics, mitochondrial/ribosomal gene
  handling, filtering semantics, and reporting decisions.
- `references/matrix_and_metadata_model.md`: raw counts, matrix views,
  flexible metadata, groupings, and color consistency.
- `references/workflow_and_clustering.md`: `run()` steps, OD genes, PCA, graph,
  UMAP, Leiden, and clustering assessment.
- `references/markers_and_plots.md`: marker calculation, marker selection,
  dotplot, native heatmap, and plot assessment.
- `references/export_and_interop.md`: RDS, h5ad export, `as()` conversions,
  and strict foreign-format metadata axes.

## Repository Scope

This skill lives inside the pagoda2 repository. Treat the repository root as the
working scope. 

## Core Invariants

- `Pagoda2` is an R6 object.
- Raw counts live in `p2$rawCounts` as a sparse cell-by-gene matrix.
- Do not restore or depend on the old full normalized `$counts` slot.
- Normalized expression is a lightweight view, materialized only when needed.
- Groupings are discrete `p2$cellMeta` columns.
- Flexible metadata resolves at use/export time with `resolveCellMeta()` and
  `resolveGeneMeta()`.
- Factor colors should resolve through the central pagoda2/sccore color path.

## Minimal Development Checks

From the repository root:

```sh
R CMD INSTALL --no-byte-compile .
Rscript -e 'library(pagoda2); testthat::test_dir("tests/testthat", reporter = "summary")'
```
