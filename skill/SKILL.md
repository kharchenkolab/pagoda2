---
name: pagoda2-scrna-v2
description: Run a pagoda2.1 single-dataset scRNA-seq workflow from raw counts through QC, filtering, PCA, graph/UMAP, Leiden clustering, marker genes, marker plots, optional annotation, and RDS/h5ad export.
when_to_use: Use for one raw single-cell RNA-seq dataset when the user wants pagoda2.1 analysis, sparse memory-conscious processing, common scRNA-seq I/O, or clean QC/UMAP/marker figures. Use a separate integration recipe for multi-sample integration or cross-dataset label transfer.
avoid_when: Do not use for multi-sample integration, ATAC/multiome-specific methods, trajectory analysis, or a Seurat/scanpy-native workflow unless the user explicitly asks to convert pagoda2 outputs.
requires_tools: [run_r]
capabilities_needed: [R, pagoda2-devel, hdf5r, ggplot2]
keywords: [pagoda2, pagoda2.1, scrna-seq, single cell RNA-seq, QC, filtering, PCA, UMAP, Leiden, markers, dotplot, heatmap, h5ad, h5Seurat, loom, 10x, CellRanger]
produces: [qc_gene_molecule.png, qc_composition_violin.png, pca_elbow.png, umap_leiden.png, marker_dotplot.png, marker_heatmap_native.png, cluster_markers.csv, pagoda2_processed.rds, pagoda2_processed.h5ad]
domain: genomics
source: "Pagoda2.1 devel workflow based on doc/pagoda2.1-single-dataset.Rmd and source-verified pagoda2.1 methods."
---

# scRNA-seq single-dataset analysis with pagoda2.1

Run one pagoda2.1 analysis: load raw counts, verify that the count layer is
integer-like, run QC, filter, calculate variance/PCA/graph/UMAP/Leiden,
detect markers, draw dotplot and native marker heatmap, optionally annotate
clusters, then save a native RDS and an AnnData-compatible h5ad file.

Pagoda2.1 can read 10x Matrix Market triplets, 10x/CellRanger HDF5, AnnData
h5ad, h5Seurat, and loom without requiring SeuratDisk, reticulate, scanpy, or
loomR. It can save native RDS, export h5ad, and convert in memory to list,
SingleCellExperiment, or Seurat when optional packages are installed.

## Bundled references - load on demand

This recipe is self-contained for the standard workflow. Load a reference only
when the task needs a variant or deeper parameter detail:

- `references/installation_and_io.md` - install, reader routing, supported
  input shapes, explicit 10x triplets, h5ad/h5Seurat/loom layers.
- `references/qc_and_filtering.md` - QC metrics, MT/ribo handling,
  `filterData()`, analysis-gene masks, and reporting thresholds.
- `references/workflow_and_clustering.md` - `run()` semantics, thread
  controls, OD genes, PCA, graph diagnostics, UMAP, and Leiden resolution.
- `references/markers_and_plots.md` - marker calculation, marker selection
  presets, dotplots, native heatmaps, and annotation from marker evidence.
- `references/matrix_and_metadata_model.md` - raw counts versus normalized
  views, orientation, metadata resolution, groupings, and factor colors.
- `references/export_and_interop.md` - RDS, h5ad export, optional
  conversions, strict-axis metadata, and round-trip checks.

## Install

Install pagoda2 from GitHub `devel` before use. Re-run this block only when
the runtime lacks pagoda2.1 or when the user asks to refresh the package.

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
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
if (!requireNamespace("pagoda2", quietly = TRUE) ||
    utils::packageVersion("pagoda2") < "1.1.1") {
  remotes::install_github("kharchenkolab/pagoda2", ref = "devel",
                          dependencies = TRUE, upgrade = "never")
}

library(pagoda2)
library(ggplot2)
```

Do not run the pagoda2 package test suite as part of user analysis or
installation. Run package tests only when editing pagoda2 source code.

## Decisions to surface up front

Tell the user these are the analysis-defining decisions:

1. **Input format and count layer** - use raw integer counts. For h5ad,
   h5Seurat, or loom, choose the count layer explicitly when needed.
2. **QC filtering** - inspect the gene-versus-molecule QC figure before
   interpreting clusters. Mitochondrial and ribosomal percentages are optional
   because gene naming varies by organism and annotation.
3. **Analysis genes and PCs** - pagoda2 keeps raw genes but uses an
   `analysis_pass` gene mask, overdispersed genes, and 50 PCs by default.
4. **Graph and clustering** - the default graph uses cosine distance and
   Leiden clustering. Assess cluster sizes, UMAP coherence, and marker quality.
5. **Compute footprint** - by default pagoda2 uses a capped thread policy.
   If the user asks to limit resources, use `n.cores` or `threads`.
6. **Marker interpretation** - default marker plots favor upregulated,
   group-specific markers with AUC/specificity metrics. Do not annotate cell
   types until marker evidence supports it.

Show the user these figures as the analysis proceeds:

- `qc_gene_molecule.png`
- `qc_composition_violin.png`, if MT/ribo metrics exist
- `pca_elbow.png`
- `umap_leiden.png`
- `marker_dotplot.png`
- `marker_heatmap_native.png`

---

## Step 1 - Load and sanity-check counts

Load raw counts into a `Pagoda2` object. `Pagoda2$from*()` constructors call
`readCounts()` internally, then build the R6 object.

### Standard 10x triplet directory

Use this when the input directory contains one complete 10x-style Matrix
Market triplet.

```r
# sample.name, sample.pattern, layer, and explicit triplet filenames are
# readCounts() arguments. They MUST be inside reader.args = list(...).
# n.cores and threads are Pagoda2$new() arguments, so they stay top-level.
p2 <- Pagoda2$from10x(
  "/path/to/sample_directory",
  reader.args = list(sample.name = "GSM5746259"),
  verbose = FALSE
)

stopifnot(inherits(p2$getRawCounts(), "dgCMatrix"))
stopifnot(all(abs(p2$getRawCounts()@x - round(p2$getRawCounts()@x)) < 1e-8))
cat(sprintf("Loaded %d cells x %d genes\n",
            nrow(p2$getRawCounts()), ncol(p2$getRawCounts())))
```

### Variant: renamed GEO/SRA triplet files - use instead of the standard block

Use explicit files when web attachments have arbitrary names. Do not symlink
or rename files just to mimic CellRanger output.

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

**Assess and report:** input format, sample ID, exact files or layer used,
cells, genes, and whether raw counts are integer-like. If counts are not
integer-like, stop and choose the correct raw count layer.

For reader options and edge cases, read `references/installation_and_io.md`.

---

## Step 2 - Run QC and save QC figures

Compute cell QC metrics, show the gene/molecule decision, and save
composition violins when MT/ribo metrics are available.

```r
invisible(p2$runQC(verbose = TRUE))

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

Save composition QC only when at least one metric exists.

```r
composition_metrics <- intersect(c("percent_ribo", "percent_mito"),
                                 colnames(p2$getCellMeta()))
if (length(composition_metrics) > 0) {
  p_comp <- p2$plotQCViolin(metrics = composition_metrics)
  ggsave("qc_composition_violin.png", p_comp,
         width = 7.5, height = 4.5, units = "in", dpi = 120, bg = "white")
}
```

**Assess and report:** QC pass/fail counts, fail fraction, median molecules,
median detected genes, whether MT/ribo metrics were detected, and whether
failed cells look like a quality tail or a coherent biological population.

For QC parameters and MT/ribo handling, read
`references/qc_and_filtering.md`.

---

## Step 3 - Run the default workflow

Run filtering, variance modeling, PCA, graph construction, UMAP, Leiden, and
marker detection with pagoda2 defaults.

```r
# Step-specific arguments belong in the matching list: pca = list(...),
# graph = list(...), umap = list(...), leiden = list(...), markers = list(...).
# Example: n.odgenes is a PCA-step argument, not a variance-step argument.
invisible(p2$run(plots = "none", verbose = TRUE))

groups <- p2$getGrouping()
cat(sprintf(
  "Workflow: %d cells, %d raw genes, %d analysis genes, %d OD genes, %d clusters\n",
  nrow(p2$getRawCounts()),
  ncol(p2$getRawCounts()),
  sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass),
  length(p2$misc$odgenes),
  length(levels(groups))
))

cluster_sizes <- sort(table(groups), decreasing = TRUE)
print(utils::head(data.frame(cluster = names(cluster_sizes),
                             cells = as.integer(cluster_sizes)), 15))
```

### Variant: limit compute footprint - use instead of the run call above

Use `n.cores` for a simple total core budget. Use `threads` only when the user
asks for role-specific control.

```r
p2$run(plots = "none", verbose = TRUE, n.cores = 10)

p2$run(
  plots = "none",
  verbose = TRUE,
  threads = list(total = 10, sgd = 1)
)
```

### Variant: skip marker detection - use instead when markers will be computed later

```r
p2$run(skip = "markers", plots = "none", verbose = TRUE)

p2$runMarkers(
  grouping = p2$getDefaultGrouping(),
  name = p2$getDefaultGrouping(),
  upregulated.only = TRUE,
  append.auc = TRUE,
  append.specificity.metrics = TRUE,
  verbose = TRUE
)
```

**Assess and report:** cells retained, raw genes retained, analysis genes, OD
genes, default grouping, number of Leiden clusters, cluster sizes, and any
warning that QC-filtered cells were present before filtering.

For workflow variants, thread controls, and graph diagnostics, read
`references/workflow_and_clustering.md`.

---

## Step 4 - Inspect PCA and UMAP

Save the PCA elbow plot and UMAP colored by the default Leiden grouping.

```r
# Use p2$plotPCAElbow(); do NOT derive PCA variance manually from internals.
p_elbow <- p2$plotPCAElbow()
ggsave("pca_elbow.png", p_elbow,
       width = 7.5, height = 4.2, units = "in", dpi = 120, bg = "white")

p_umap <- p2$plotEmbedding(
  mark.groups = TRUE,
  size = 0.35,
  alpha = 0.55,
  title = "Leiden clusters"
)
ggsave("umap_leiden.png", p_umap,
       width = 7.4, height = 6.2, units = "in", dpi = 120, bg = "white")
```

Overlay additional groupings or QC metrics only when they exist.

```r
if ("sample" %in% colnames(p2$getCellMeta())) {
  p2$plotEmbedding(grouping = "sample")
}
if ("percent_mito" %in% colnames(p2$getCellMeta())) {
  mito <- p2$resolveCellMeta("percent_mito")
  p2$plotEmbedding(colors = stats::setNames(mito$percent_mito, rownames(mito)))
}
```

**Assess and report:** PCA elbow shape, whether 50 PCs looks reasonable, UMAP
cluster coherence, tiny outlying groups, and whether QC/sample metadata
appears to dominate the embedding.

For PCA, graph, UMAP, and Leiden details, read
`references/workflow_and_clustering.md`.

---

## Step 5 - Plot markers and write marker table

Use the marker result from the default grouping. Dotplot and heatmap share
marker-selection logic.

```r
marker_name <- p2$getDefaultGrouping()
if (is.null(marker_name)) {
  marker_name <- "leiden"
}
if (!marker_name %in% p2$listMarkers()$name) {
  p2$runMarkers(
    grouping = marker_name,
    name = marker_name,
    upregulated.only = TRUE,
    append.auc = TRUE,
    append.specificity.metrics = TRUE,
    verbose = TRUE
  )
}

p_dot <- p2$plotMarkerDotPlot(
  markers = marker_name,
  n.genes.per.group = 5,
  selection = "balanced",
  order.groups = TRUE,
  dot.scale = 8.5
) + ggplot2::labs(title = "Marker genes")
ggsave("marker_dotplot.png", p_dot,
       width = 15.5, height = 10.5, units = "in", dpi = 120, bg = "white")
```

Save the native marker heatmap.

```r
png("marker_heatmap_native.png",
    width = 13.8, height = 9, units = "in", res = 120, bg = "white")
p2$plotMarkerHeatmap(
  markers = marker_name,
  engine = "native",
  n.genes.per.group = 3,
  selection = "balanced",
  column.metadata = intersect(c("n_molecules", "n_genes"),
                              colnames(p2$getCellMeta())),
  row.label.font.size = 9,
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()
```

Write the marker table.

```r
marker_result <- p2$getMarkerResult(marker_name)
marker_tables <- marker_result$tables
marker_df <- do.call(rbind, lapply(names(marker_tables), function(group) {
  x <- marker_tables[[group]]
  if (is.null(x) || !nrow(x)) return(NULL)
  x$group <- group
  x
}))
utils::write.csv(marker_df, "cluster_markers.csv", row.names = FALSE)

print(utils::head(
  p2$getTopMarkers(markers = marker_name,
                   n.genes.per.group = 5,
                   selection = "balanced"),
  20
))
```

**Assess and report:** whether each cluster has crisp upregulated markers,
whether markers are broadly expressed in other clusters, whether QC/stress/MT
or ribosomal genes dominate, and whether the dotplot and heatmap support the
same interpretation.

For marker selection presets and plot variants, read
`references/markers_and_plots.md`.

---

## Step 6 - Annotate when evidence is sufficient

Store biological annotations as cell metadata groupings. Many-to-one mappings
from clusters to cell types are normal. Build the map from marker review, not
from literal example values.

```r
# REPLACE this empty map with cluster-to-cell-type assignments derived from
# Step 5 marker review. Map only clusters with confident evidence.
cell_type_map <- c()

if (length(cell_type_map) > 0) {
  p2$annotateClusters(
    from = "leiden",
    to = "cell_type",
    map = cell_type_map,
    unmapped = "keep",
    setDefault = TRUE
  )
  p2$plotEmbedding(grouping = "cell_type", mark.groups = TRUE)
}
```

Do not annotate from a single marker name. If evidence is incomplete, report
candidate labels and leave `leiden` as the default grouping.

**Assess and report:** annotation evidence, unresolved clusters, many-to-one
cluster merges, and whether external annotations disagree with Leiden
clusters.

For metadata and grouping semantics, read
`references/matrix_and_metadata_model.md`.

---

## Step 7 - Save and export

Save the native pagoda2 object and export h5ad when downstream tools may use
AnnData/scanpy-compatible objects.

```r
saveRDS(p2, "pagoda2_processed.rds")

p2$export("pagoda2_processed.h5ad", format = "h5ad", overwrite = TRUE)

cat("Saved pagoda2_processed.rds and pagoda2_processed.h5ad\n")
```

**Assess and report:** native RDS path, h5ad path, exported cell/gene counts,
whether raw counts and normalized expression are included, and any metadata
columns with missing resolved values.

For export semantics and optional conversions, read
`references/export_and_interop.md`.

## Final response checklist

Summarize:

- input format, sample ID, exact count layer/files, cells, genes, and count
  integer-likeness
- QC pass/fail counts, fail fraction, median molecules, median genes, and
  whether MT/ribo metrics were available
- post-filter cells, raw genes retained, analysis genes, OD genes, and default
  grouping
- Leiden cluster count and largest/smallest cluster sizes
- PCA elbow and UMAP quality observations
- marker quality, top marker examples, and whether dotplot/heatmap agree
- annotations stored or annotation uncertainty
- output figure/table/object filenames
- caveats: wrong count layer risk, weak markers, QC-driven clusters,
  doublets, batch effects, over-clustering, or under-clustering
