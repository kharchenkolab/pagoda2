---
name: pagoda2-scrna-v2
description: Run a pagoda2.1 single-dataset scRNA-seq workflow from raw count import through QC, filtering, variance QC, PCA, UMAP, Leiden, marker genes, marker plots, optional annotation, and RDS/h5ad export. Also covers multimodal data (CITE-seq RNA+ADT, ATAC/multiome) via facets, with WNN/CCA integration.
when_to_use: Use for one raw single-cell dataset (RNA, or multimodal CITE-seq / ATAC / 10x multiome) when the user wants pagoda2.1 analysis, sparse memory-conscious or disk-backed (lstar-zarr) processing, common scRNA-seq file I/O, or clean QC/UMAP/marker figures. For multimodal, see references/multimodal_facets.md. Use a separate integration recipe for multi-sample integration or cross-dataset label transfer.
avoid_when: Do not use for multi-sample (cross-dataset) integration, trajectory analysis, or a Seurat/scanpy-native workflow unless the user explicitly asks to convert pagoda2 outputs. (Single-sample multimodal — CITE-seq, ATAC, multiome — IS supported here via facets.)
requires_tools: [run_r]
capabilities_needed: [R, pagoda2-devel, ggplot2, hdf5r, data.table, R.utils, uwot, leidenAlg]
keywords: [pagoda2, pagoda2.1, scRNA-seq, single cell RNA-seq, QC, filtering, variance normalization, overdispersed genes, PCA, UMAP, Leiden, markers, dotplot, heatmap, h5ad, h5Seurat, loom, 10x, CellRanger]
produces: [qc_gene_molecule.png, qc_composition_violin.png, variance_qc.png, pca_elbow.png, umap_leiden.png, marker_dotplot.png, marker_heatmap_native.png, cluster_markers.csv, pagoda2_processed.rds, pagoda2_processed.h5ad]
domain: genomics
source: "Pagoda2.1 devel workflow based on doc/pagoda2.1-single-dataset.Rmd and source-verified pagoda2.1 R6 methods."
---

# scRNA-seq single-dataset analysis with pagoda2.1

Run one pagoda2.1 analysis from raw integer counts to QC, filtering, variance
modeling, PCA, graph construction, default UMAP embedding, Leiden clustering,
marker detection, marker plots, and export. The recipe is for one dataset or
one sample. Pagoda2.1 keeps sparse raw counts as the canonical matrix and
uses lightweight normalized matrix views, so agents should use public
accessors instead of reaching into legacy matrix slots.

Pagoda2.1 can read 10x Matrix Market triplets, CellRanger/10x HDF5, AnnData
h5ad, h5Seurat, and loom. It can save native RDS objects, export h5ad, and
convert in memory to list, SingleCellExperiment, or Seurat when optional
packages are installed.

## Bundled references - load on demand

This SKILL.md is self-contained for the standard workflow. Load a reference
only when the task needs a variant, parameter detail, or troubleshooting:

- `references/installation_and_io.md` - install, reader routing, auto-detect,
  explicit renamed 10x triplets, h5ad/h5Seurat/loom layers, and direct
  `readCounts()` use.
- `references/qc_and_filtering.md` - `runQC()`, MT/ribo metrics,
  `plotQC()`, `plotQCViolin()`, `filterData()`, and analysis-gene masks.
- `references/workflow_and_clustering.md` - `p2$run()` step semantics,
  variance QC, OD genes, PCA, graph, embedding, Leiden, and thread control.
- `references/markers_and_plots.md` - `runMarkers()`, marker accessors,
  marker selection modes, dotplots, native heatmaps, and cluster annotation.
- `references/matrix_and_metadata_model.md` - raw counts versus normalized
  views, cell/gene metadata resolution, groupings, colors, and object access.
- `references/export_and_interop.md` - RDS, h5ad export, optional
  conversions, metadata alignment, and round-trip checks.
- `references/multimodal_facets.md` - **multimodal data via facets**
  (CITE-seq RNA+ADT, ATAC/multiome): `addFacet()`, CLR/TF-IDF view models,
  per-facet `runReduction()` (PCA/LSI), joint integration (WNN, CCA/sparse-CCA,
  concat-PCA), name-keyed reductions/graphs, native 10x multimodal import, and
  lstar-zarr disk backing. Load this whenever the data has >1 modality.

## Install

Install pagoda2 from GitHub `devel` before use. The full workflow also needs
ggplot2 for plots, data.table/R.utils for gzipped 10x text files, uwot for
UMAP, and leidenAlg for Leiden clustering. Pagoda2.1 also requires
**sccore >= 1.1.0** (the native marker-heatmap engine lives in sccore); that
version is on GitHub `dev` until the next CRAN release, so install it from
source first — `dependencies = TRUE` would otherwise pull the older CRAN sccore
and `plotMarkerHeatmap(engine = "native")` would fail.

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))

for (pkg in c("remotes", "ggplot2", "hdf5r", "data.table", "R.utils",
              "uwot", "leidenAlg")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# sccore >= 1.1.0 from GitHub `dev` (carries the native heatmap engine pagoda2 calls);
# upgrade = "never" keeps the pagoda2 install below from downgrading it.
if (!requireNamespace("sccore", quietly = TRUE) ||
    utils::packageVersion("sccore") < "1.1.0") {
  remotes::install_github("kharchenkolab/sccore", ref = "dev", upgrade = "never")
}

needs_pagoda2 <- !requireNamespace("pagoda2", quietly = TRUE)
if (!needs_pagoda2) {
  needs_pagoda2 <- !identical(
    get("Pagoda2", envir = asNamespace("pagoda2"))$public_fields$apiVersion,
    "2.2"
  )
}
if (needs_pagoda2) {
  remotes::install_github("kharchenkolab/pagoda2", ref = "devel",
                          dependencies = TRUE, upgrade = "never")
}

library(pagoda2)
library(ggplot2)

stopifnot(identical(pagoda2::Pagoda2$public_fields$apiVersion, "2.2"))
stopifnot(utils::packageVersion("sccore") >= "1.1.0")
```

Do not run the pagoda2 package test suite as part of user analysis or routine
installation. Run package tests only when editing pagoda2 source code.

## Decisions to surface up front

Tell the user these are the analysis-defining decisions:

1. **Input format and count layer** - use raw integer counts. For h5ad,
   h5Seurat, or loom, choose the count layer explicitly when needed.
2. **QC filtering** - inspect gene-versus-molecule QC before interpreting
   clusters. MT/ribo percentages are optional because gene naming varies.
3. **Analysis gene set and PCs** - pagoda2 records an `analysis_pass` gene
   mask, selects overdispersed genes, and computes 50 PCs by default.
4. **Graph, embedding, and clustering** - defaults are cosine graph,
   UMAP embedding, and Leiden clustering. Check cluster sizes, embedding
   coherence, and marker quality.
5. **Compute footprint** - use `n.cores` for a simple total core budget or
   `threads = list(...)` for advanced role-specific controls.
6. **Marker interpretation** - marker plots should guide annotation, but do
   not assign cell types until marker evidence supports the labels.

Show the user these figures as the analysis proceeds:

- `qc_gene_molecule.png`
- `qc_composition_violin.png`, if MT/ribo metrics exist
- `variance_qc.png`
- `pca_elbow.png`
- `umap_leiden.png`
- `marker_dotplot.png`
- `marker_heatmap_native.png`

---

## Step 1 - Load and sanity-check counts

Load raw counts into a pagoda2.1 object. Constructors call `readCounts()`
internally, then build the R6 object.

### Standard 10x triplet directory

Use this when the input directory contains one complete 10x-style Matrix
Market triplet.

```r
# sample.name, sample.pattern, layer, assay, gene.id, and explicit triplet
# filenames are readCounts() arguments. They MUST be inside reader.args.
# n.cores, threads, and verbose are constructor/runtime arguments.
p2 <- Pagoda2$from10x(
  "/path/to/sample_directory",
  reader.args = list(sample.name = "GSM5746259"),
  verbose = FALSE
)

stopifnot(identical(p2$apiVersion, "2.2"))
stopifnot(inherits(p2$getRawCounts(), "dgCMatrix"))
stopifnot(all(abs(p2$getRawCounts()@x - round(p2$getRawCounts()@x)) < 1e-8))

cat(sprintf("Loaded %d cells x %d genes\n",
            nrow(p2$getRawCounts()), ncol(p2$getRawCounts())))
```

### Variant: renamed GEO/SRA triplet files - use instead of the standard block

Use explicit filenames when web attachments do not have CellRanger names. Do
not symlink or rename files just to mimic another directory layout.

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

### Variant: other supported formats - use instead when the input is not 10x Matrix Market

Choose the constructor that matches the actual input. Count-layer choices go
inside `reader.args`.

```r
p2 <- Pagoda2$from10xH5(
  "/path/to/filtered_feature_bc_matrix.h5",
  reader.args = list(sample.name = "sample_01"),
  verbose = FALSE
)

p2 <- Pagoda2$fromAnnData(
  "/path/to/sample.h5ad",
  reader.args = list(layer = "counts", sample.name = "sample_01"),
  verbose = FALSE
)

p2 <- Pagoda2$fromH5Seurat(
  "/path/to/sample.h5seurat",
  reader.args = list(assay = "RNA", layer = "counts",
                     sample.name = "sample_01"),
  verbose = FALSE
)

p2 <- Pagoda2$fromLoom(
  "/path/to/sample.loom",
  reader.args = list(layer = "counts", sample.name = "sample_01"),
  verbose = FALSE
)
```

**Assess and report:** input format, sample ID, files or layer used, cells,
genes, and whether raw counts are integer-like. If counts are not integer-like,
stop and choose the correct raw count layer.

For reader options and edge cases, read `references/installation_and_io.md`.

---

## Step 2 - Run QC and save QC figures

Compute cell QC metrics, show the gene/molecule decision, and save MT/ribo
composition violins when the metrics are available.

```r
p2$runQC(verbose = TRUE)

qc <- p2$resolveCellMeta(c("n_molecules", "n_genes", "qc_pass"))
cat(sprintf(
  "QC: %d cells; %d pass, %d fail; median molecules %.0f; median genes %.0f\n",
  nrow(qc),
  sum(as.logical(qc$qc_pass), na.rm = TRUE),
  sum(!as.logical(qc$qc_pass), na.rm = TRUE),
  median(qc$n_molecules, na.rm = TRUE),
  median(qc$n_genes, na.rm = TRUE)
))

p_qc <- p2$plotQC()
ggsave("qc_gene_molecule.png", p_qc,
       width = 10, height = 4.5, units = "in", dpi = 120, bg = "white")
```

```r
composition_metrics <- intersect(c("percent_ribo", "percent_mito"),
                                 colnames(p2$cellMeta))
if (length(composition_metrics) > 0) {
  p_comp <- p2$plotQCViolin(metrics = composition_metrics)
  ggsave("qc_composition_violin.png", p_comp,
         width = 7.5, height = 4.5, units = "in", dpi = 120, bg = "white")
}
```

**Assess and report:** QC pass/fail counts, fail fraction, median molecules,
median detected genes, whether MT/ribo metrics were found, and whether failed
cells look like a quality tail or a coherent population worth discussing.

For QC parameters and MT/ribo handling, read `references/qc_and_filtering.md`.

---

## Step 3 - Run the default workflow and save core diagnostics

Run filtering, variance modeling, PCA, graph construction, default UMAP,
Leiden clustering, and marker detection. `p2$run()` mutates the object in
place and stores results under canonical names.

```r
p2$run(plots = "none", verbose = TRUE)

groups <- p2$getGrouping()
cat(sprintf(
  "Workflow: %d cells, %d raw genes, %d analysis genes, %d OD genes, %d clusters\n",
  nrow(p2$getRawCounts()),
  ncol(p2$getRawCounts()),
  sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass, na.rm = TRUE),
  length(p2$getOdGenes()),
  length(levels(groups))
))

print(head(sort(table(groups), decreasing = TRUE), 15))
```

Save variance normalization QC, PCA elbow, and UMAP.

```r
p_var <- p2$plotVarianceQC()
ggsave("variance_qc.png", p_var,
       width = 10, height = 4.8, units = "in", dpi = 120, bg = "white")

p_elbow <- p2$plotPCAElbow()
ggsave("pca_elbow.png", p_elbow,
       width = 7.5, height = 4.2, units = "in", dpi = 120, bg = "white")

p_umap <- p2$plotEmbedding(
  grouping = "leiden",
  mark.groups = TRUE,
  size = 0.35,
  alpha = 0.55,
  title = "Leiden clusters"
)
ggsave("umap_leiden.png", p_umap,
       width = 7.4, height = 6.2, units = "in", dpi = 120, bg = "white")
```

### Variant: skip marker detection - use instead when clustering is the only immediate goal

```r
p2$run(skip = "markers", plots = "none", verbose = TRUE)
```

### Variant: limit compute footprint - use instead when the user gives a core budget

```r
p2$run(plots = "none", verbose = TRUE, n.cores = 10)
```

**Assess and report:** cells retained after filtering, analysis genes, OD
genes, PCA elbow shape, graph/embedding method, Leiden cluster count, largest
and smallest cluster sizes, and whether UMAP structure is coherent or dominated
by QC/sample metadata.

For step overrides and thread controls, read
`references/workflow_and_clustering.md`.

---

## Step 4 - Save marker table, dotplot, and native heatmap

Use the marker result for the default grouping, usually `leiden` after the
standard run. Default marker settings favor upregulated markers with AUC and
specificity metrics.

```r
marker_name <- p2$getDefaultGrouping()
stopifnot(!is.null(marker_name))

top_markers <- p2$getTopMarkers(
  markers = marker_name,
  n.genes.per.group = 10,
  selection = "balanced"
)
utils::write.csv(top_markers, "cluster_markers.csv", row.names = FALSE)

p_dot <- p2$plotMarkerDotPlot(
  markers = marker_name,
  n.genes.per.group = 5,
  selection = "balanced",
  order.groups = TRUE,
  dot.scale = 8.5
) + ggplot2::labs(title = paste(marker_name, "marker genes"))
ggsave("marker_dotplot.png", p_dot,
       width = 15.5, height = 10.5, units = "in", dpi = 120, bg = "white")
```

Use the native heatmap engine by default. It avoids the heavy ComplexHeatmap
dependency and supports top metadata tracks.

```r
png("marker_heatmap_native.png",
    width = 13.8, height = 9, units = "in", res = 120, bg = "white")
p2$plotMarkerHeatmap(
  markers = marker_name,
  engine = "native",
  n.genes.per.group = 3,
  selection = "balanced",
  column.metadata = intersect(c("n_molecules", "n_genes",
                                "percent_mito", "percent_ribo"),
                              colnames(p2$cellMeta)),
  row.label.font.size = 9,
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()
```

**Assess and report:** marker result name, marker selection mode, strongest
markers per cluster, whether markers are cluster-specific, whether broad/QC
genes dominate, and whether dotplot/heatmap labels are readable.

For marker ranking modes and plot variants, read
`references/markers_and_plots.md`.

---

## Step 5 - Optional annotation and export

Annotate clusters only after marker review. Many-to-one cluster-to-cell-type
maps are expected; unmapped clusters can keep their Leiden label.

```r
# REPLACE with assignments derived from the marker review in Step 4.
cluster_to_type <- c(
  # "<cluster_id>" = "<cell type>"
)

if (length(cluster_to_type) > 0) {
  p2$annotateClusters(
    from = marker_name,
    to = "cell_type",
    map = cluster_to_type,
    unmapped = "keep",
    setDefault = TRUE,
    overwrite = TRUE
  )
}
```

Save a native pagoda2 object for continuation and an h5ad file for scanpy or
AnnData-compatible consumers.

```r
saveRDS(p2, "pagoda2_processed.rds")

p2$export(
  "pagoda2_processed.h5ad",
  format = "h5ad",
  overwrite = TRUE
)
```

**Assess and report:** whether annotation was added, current default grouping,
RDS filename, h5ad filename, exported cells and genes, and whether h5ad export
included raw counts and normalized expression.

For metadata resolution and export semantics, read
`references/matrix_and_metadata_model.md` and
`references/export_and_interop.md`.

---

## Multimodal data (facets): CITE-seq, ATAC, 10x multiome

When the input has more than one modality, each modality is a **facet** — a
first-class bundle of raw counts + a normalization view recipe + its own
variance/reductions. RNA is the default facet; add others with `addFacet()`.
Pipeline steps are **generic with a `method=`** — there is no `runPCA`,
`runLeiden`, `runWNN`, or `runUMAP`; the algorithm is always a `method=` of
`runReduction` / `runGraph` / `runClustering` / `runEmbedding`.

Native 10x multimodal import maps `feature_type` to facets automatically
(`Gene Expression`→RNA/plain, `Antibody Capture`→ADT/CLR, `Peaks`→ATAC/TF-IDF):

```r
p2 <- Pagoda2$from10xH5("/path/filtered_feature_bc_matrix.h5", verbose = FALSE) # RNA + ADT/ATAC facets
p2$listFacets()                                   # e.g. c("RNA","ADT") or c("RNA","ATAC")
```

Per-facet reductions, then **integrate** (joint products are name-keyed:
`reductions[["PCA"]]`, `reductions[["ADT:PCA"]]`, `reductions[["WNN"]]`):

```r
p2$runVariance();                p2$runReduction(nPcs = 30)               # RNA -> PCA
p2$runVariance(facet = "ADT", use.raw.variance = TRUE)
p2$runReduction(facet = "ADT", nPcs = 20)                                 # ADT (CLR) -> ADT:PCA
# ATAC instead: p2$runReduction(facet = "ATAC", method = "lsi")           # TF-IDF -> SVD -> ATAC:LSI

p2$runGraph(method = "wnn", facets = c("RNA", "ADT"))   # WNN: per-cell modality weights + WSNN graph
#   or a joint reduction: p2$runReduction(facets = c("RNA","ADT"), method = "cca")   # also "scca","concat"
p2$runClustering(graph = "WNN", name = "wnn_leiden")
p2$runEmbedding(reduction = "WNN", name = "umap")
```

Full API (membership masks / `requireFacets`, view models, CCA/concat options,
lstar import-export, disk backing) is in `references/multimodal_facets.md`.

---

## Final response checklist

When the analysis is complete, summarize these items in order:

- Input format, sample ID, cells, genes, and integer-count check.
- QC pass/fail counts and whether MT/ribo metrics were available.
- Cells retained, analysis genes, OD genes, PCA/UMAP/Leiden settings, and
  number of clusters.
- Figures produced: QC, optional composition violin, variance QC, PCA elbow,
  UMAP, marker dotplot, and native marker heatmap.
- Marker result name and the top marker evidence used for any annotation.
- Saved outputs: `cluster_markers.csv`, `pagoda2_processed.rds`, and
  `pagoda2_processed.h5ad`.
- Caveats: any non-integer input, missing MT/ribo metrics, suspicious QC
  structure, weak markers, small clusters, or labels left unannotated.
