---
name: pagoda2-scrna-v2
description: Run a pagoda2.1 single-sample scRNA-seq workflow from raw counts through QC, filtering, PCA, graph/UMAP, Leiden clustering, marker genes, marker plots, annotation, and h5ad/RDS export. Use for pagoda2-native analysis of 10x triplets, CellRanger HDF5, h5ad, h5Seurat, or loom inputs.
when_to_use: Use for one raw single-cell RNA-seq dataset when the user wants a pagoda2.1 analysis, sparse memory-efficient processing, common scRNA-seq I/O, or clean QC/UMAP/marker figures. Use a separate integration recipe for multi-sample integration or cross-dataset label transfer.
avoid_when: Do not use for multi-sample integration, ATAC/multiome methods, trajectory analysis, or a Seurat/scanpy-native workflow unless the user explicitly asks to convert pagoda2 outputs.
requires_tools: [run_r]
capabilities_needed: [R, pagoda2-devel]
keywords: [pagoda2, pagoda2.1, scrna-seq, single cell RNA-seq, QC, filtering, PCA, UMAP, Leiden, markers, dotplot, heatmap, h5ad, h5Seurat, loom, 10x, CellRanger]
produces: [qc_gene_molecule.png, qc_composition_violin.png, pca_elbow.png, umap_leiden.png, marker_dotplot.png, marker_heatmap_native.png, cluster_markers.csv, pagoda2_processed.rds, pagoda2_processed.h5ad]
domain: genomics
source: "Pagoda2.1 devel workflow based on doc/pagoda2.1-single-dataset.Rmd."
---

# scRNA-seq single-sample QC + clustering with pagoda2.1

Run a pagoda2.1 analysis for one raw count dataset: load counts, sanity-check
the matrix, run QC, filter, calculate variance/PCA/graph/UMAP/Leiden, detect
markers, show dotplot and native heatmap, then save/export the object.

The standard example follows the current pagoda2.1 vignette: a PBMC GEO sample
(`GSM5746259`) stored as a 10x-style triplet in a local folder named `data`.
For other inputs, keep the same workflow and change only the loading block.
Pagoda2.1 can read 10x Matrix Market triplets, CellRanger HDF5, AnnData h5ad,
h5Seurat, and loom, and can write native RDS plus h5ad.

## Bundled references - load on demand

This SKILL.md is self-contained for the standard workflow. Load references only
when the task needs a variant or deeper parameter detail:

- `references/installation_and_io.md` - installation, `Pagoda2$from()`,
  `from10x()`, explicit renamed 10x triplets, h5ad/h5Seurat/loom readers, and
  count-layer sanity checks.
- `references/qc_and_filtering.md` - `runQC()`, `plotQC()`,
  `plotQCViolin()`, `filterData()`, MT/ribo gene handling, and filter
  reporting.
- `references/matrix_and_metadata_model.md` - raw count storage, normalized
  views, matrix orientation, flexible/resolved metadata, groupings, annotation,
  and color handling.
- `references/workflow_and_clustering.md` - `run()`, staged workflow variants,
  OD genes, PCA, graph diagnostics, UMAP, Leiden, and result registries.
- `references/markers_and_plots.md` - `runMarkers()`, marker selection
  presets, `getTopMarkers()`, dotplots, native marker heatmaps, and annotation
  discipline.
- `references/export_and_interop.md` - RDS, h5ad export, metadata alignment,
  optional `as("list")`, `as("sce")`, `as("seurat")`, and round-trip checks.

## Install

Install pagoda2 from GitHub `devel` before use. Do not install Seurat, scanpy,
reticulate, or SeuratDisk just to read/write common single-cell formats.

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("hdf5r", quietly = TRUE)) {
  install.packages("hdf5r")  # needed for h5ad, h5Seurat, loom, and 10x HDF5
}
if (!requireNamespace("pagoda2", quietly = TRUE) ||
    utils::packageVersion("pagoda2") < "1.1.1") {
  remotes::install_github("kharchenkolab/pagoda2", ref = "devel",
                          dependencies = TRUE)
}

library(pagoda2)
library(ggplot2)
```

Do not run the package test suite as part of analysis or installation. Run
tests only when editing pagoda2 source code.

## Decisions to surface up front

Tell the user these are the analysis-defining decisions:

1. **Input format and count layer** - load raw integer counts. For h5ad,
   h5Seurat, or loom, choose the count layer explicitly when needed.
2. **QC filtering** - inspect the gene-versus-molecule QC decision before
   interpreting clusters. MT/ribo percentages are optional and depend on gene
   annotation.
3. **Analysis genes** - pagoda2 keeps raw genes but uses an `analysis_pass`
   mask and overdispersed genes for PCA.
4. **PC/graph/clustering defaults** - default workflow uses 50 PCs, cosine
   graph distance, and Leiden clustering; inspect the elbow plot, cluster
   sizes, and marker coherence.
5. **Marker selection** - default marker plots use `selection = "balanced"`,
   upregulated markers, AUC, and specificity metrics. Change selection only
   when the plot is too diffuse or too strict.

Show the user these figures as the analysis proceeds:

- `qc_gene_molecule.png`
- `qc_composition_violin.png`, if MT/ribo metrics exist
- `pca_elbow.png`
- `umap_leiden.png`
- `marker_dotplot.png`
- `marker_heatmap_native.png`

---

## Step 1 - Load and sanity-check counts

Load a raw count matrix into a pagoda2 object, verify dimensions and
integer-like counts, and report the files used.

### Vignette 10x triplet folder

Use this block when the working directory has a `data` folder containing one
10x-style triplet, as in the current pagoda2.1 vignette.

```r
WORK_DIR <- getwd()
DATA_DIR <- file.path(WORK_DIR, "data")
SAMPLE_ID <- "GSM5746259"

input_files <- data.frame(
  data_dir = DATA_DIR,
  file = list.files(DATA_DIR, pattern = "\\.(mtx|tsv)(\\.gz)?$")
)
print(input_files)
stopifnot(nrow(input_files) >= 3)

p2 <- Pagoda2$from10x(
  DATA_DIR,
  reader.args = list(sample.name = SAMPLE_ID),
  verbose = FALSE
)

matrix_summary <- p2$describeMatrices()
raw_summary <- matrix_summary[matrix_summary$name == "raw", , drop = FALSE]
raw_dim <- dim(p2$getRawCounts())
load_summary <- data.frame(
  sample = SAMPLE_ID,
  cells = raw_dim[1],
  genes = raw_dim[2],
  nonzero_entries = raw_summary$nnz,
  sparsity = round(1 - raw_summary$nnz / prod(raw_dim), 4),
  integer_like = raw_summary$integer.like
)
print(load_summary)
stopifnot(isTRUE(p2$validateMatrices()))
stopifnot(isTRUE(load_summary$integer_like))
```

### Renamed GEO/SRA triplet files

Use explicit files when a download has arbitrary filenames. Do not symlink or
rename files just to mimic CellRanger output.

```r
WORK_DIR <- getwd()
DATA_DIR <- file.path(WORK_DIR, "data")
SAMPLE_ID <- "GSM5746259"

p2 <- Pagoda2$from(
  DATA_DIR,
  format = "10x",
  reader.args = list(
    sample.name = SAMPLE_ID,
    files = list(
      matrix = "GSM5746259_custom_matrix.mtx.gz",
      barcodes = "GSM5746259_custom_barcodes.tsv.gz",
      features = "GSM5746259_custom_features.tsv.gz"
    )
  ),
  verbose = FALSE
)
```

### Other supported formats

Use the specific constructor when the format is known:

```r
p2 <- Pagoda2$from10xH5(
  "filtered_feature_bc_matrix.h5",
  reader.args = list(sample.name = SAMPLE_ID)
)
p2 <- Pagoda2$fromAnnData("sample.h5ad",
                          reader.args = list(layer = "counts",
                                             sample.name = SAMPLE_ID))
p2 <- Pagoda2$fromH5Seurat("sample.h5seurat",
                           reader.args = list(assay = "RNA", layer = "counts",
                                              sample.name = SAMPLE_ID))
p2 <- Pagoda2$fromLoom("sample.loom",
                       reader.args = list(layer = "counts",
                                          sample.name = SAMPLE_ID))
```

**Assess and report:** input format, sample ID, exact files/layer used, cells,
genes, sparsity, and whether raw counts are integer-like. If counts are not
integer-like, stop and choose the correct raw count layer.

For reader options and edge cases, read `references/installation_and_io.md`.

---

## Step 2 - Run QC and save QC figures

Compute cell-level QC metrics, show the main gene/molecule plot, and show
composition violin plots when MT/ribo metrics are available.

```r
invisible(p2$runQC(verbose = TRUE))

qc <- p2$resolveCellMeta(c("n_molecules", "n_genes", "qc_pass"))
qc_summary <- data.frame(
  cells = nrow(qc),
  qc_pass = sum(as.logical(qc$qc_pass), na.rm = TRUE),
  qc_fail = sum(!as.logical(qc$qc_pass), na.rm = TRUE),
  fail_fraction = round(mean(!as.logical(qc$qc_pass), na.rm = TRUE), 4),
  median_molecules = median(qc$n_molecules, na.rm = TRUE),
  median_genes = median(qc$n_genes, na.rm = TRUE)
)
print(qc_summary)

p_qc <- p2$plotQC()
ggsave(file.path(WORK_DIR, "qc_gene_molecule.png"), p_qc,
       width = 10, height = 4.5, units = "in", dpi = 120, bg = "white")
```

Composition QC is annotation-dependent. Save the violin plot only if at least
one requested composition metric exists:

```r
qc_metrics <- intersect(c("percent_ribo", "percent_mito"),
                        colnames(p2$getCellMeta()))
if (length(qc_metrics) > 0) {
  p_comp <- p2$plotQCViolin(metrics = qc_metrics)
  ggsave(file.path(WORK_DIR, "qc_composition_violin.png"), p_comp,
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
invisible(p2$run(plots = "none", verbose = TRUE))

groups <- p2$getGrouping()
workflow_summary <- data.frame(
  cells = nrow(p2$getRawCounts()),
  raw_genes = ncol(p2$getRawCounts()),
  analysis_genes = sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass),
  od_genes = length(p2$misc$odgenes),
  default_grouping = p2$getDefaultGrouping(),
  clusters = length(levels(groups))
)
print(workflow_summary)

cluster_sizes <- sort(table(groups), decreasing = TRUE)
cluster_size_table <- data.frame(
  cluster = names(cluster_sizes),
  cells = as.integer(cluster_sizes),
  row.names = NULL
)
print(utils::head(cluster_size_table, 15))
```

To skip marker detection for speed, use this variant and run markers later:

```r
p2$run(skip = "markers", plots = "none", verbose = TRUE)
p2$runMarkers(grouping = p2$getDefaultGrouping(), name = p2$getDefaultGrouping(),
              upregulated.only = TRUE, append.auc = TRUE,
              append.specificity.metrics = TRUE, verbose = TRUE)
```

**Assess and report:** cells retained, raw genes retained, analysis genes, OD
genes, default grouping, number of Leiden clusters, cluster sizes, and any
warning that QC-filtered cells were present before filtering.

For workflow variants and graph diagnostics, read
`references/workflow_and_clustering.md`.

---

## Step 4 - Inspect PCA and UMAP

Save the PCA elbow plot and UMAP colored by the default Leiden grouping.

```r
p_elbow <- p2$plotPCAElbow()
ggsave(file.path(WORK_DIR, "pca_elbow.png"), p_elbow,
       width = 7.5, height = 4.2, units = "in", dpi = 120, bg = "white")

p_umap <- p2$plotEmbedding(
  mark.groups = TRUE,
  size = 0.35,
  alpha = 0.55,
  title = paste(SAMPLE_ID, "Leiden clusters")
)
ggsave(file.path(WORK_DIR, "umap_leiden.png"), p_umap,
       width = 7.4, height = 6.2, units = "in", dpi = 120, bg = "white")
```

Overlay additional groupings or QC metrics only when they exist:

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
cluster coherence, tiny outlying groups, and whether QC/sample metadata appears
to dominate the embedding.

For PCA/graph/UMAP details, read `references/workflow_and_clustering.md`.

---

## Step 5 - Plot markers and write marker table

Use the marker result from the default grouping. Dotplot and heatmap should use
the same marker-selection logic.

```r
marker_name <- p2$getDefaultGrouping()
if (is.null(marker_name)) {
  marker_name <- "leiden"
}
if (!marker_name %in% p2$listMarkers()$name) {
  p2$runMarkers(grouping = marker_name, name = marker_name,
                upregulated.only = TRUE, append.auc = TRUE,
                append.specificity.metrics = TRUE, verbose = TRUE)
}

p_dot <- p2$plotMarkerDotPlot(
  markers = marker_name,
  n.genes.per.group = 5,
  selection = "balanced",
  order.groups = TRUE,
  dot.scale = 8.5
) + ggplot2::labs(title = "Leiden marker genes")
ggsave(file.path(WORK_DIR, "marker_dotplot.png"), p_dot,
       width = 15, height = 10.5, units = "in", dpi = 120, bg = "white")
```

Save the native marker heatmap:

```r
png(file.path(WORK_DIR, "marker_heatmap_native.png"),
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

Write the marker table:

```r
marker_result <- p2$getMarkerResult(marker_name)
marker_tables <- marker_result$tables
marker_df <- do.call(rbind, lapply(names(marker_tables), function(group) {
  x <- marker_tables[[group]]
  if (is.null(x) || !nrow(x)) return(NULL)
  x$group <- group
  x
}))
utils::write.csv(marker_df, file.path(WORK_DIR, "cluster_markers.csv"),
                 row.names = FALSE)

top_marker_summary <- p2$getTopMarkers(
  markers = marker_name,
  n.genes.per.group = 5,
  selection = "balanced"
)
print(utils::head(top_marker_summary, 20))
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
from clusters to cell types are normal.

```r
cluster_to_type <- c(
  "1" = "T cells",
  "2" = "monocytes"
)

p2$annotateClusters(
  from = "leiden",
  to = "cell_type",
  map = cluster_to_type,
  unmapped = "keep",
  setDefault = TRUE
)

p2$plotEmbedding(grouping = "cell_type", mark.groups = TRUE)
```

Do not annotate from a single marker name. If evidence is incomplete, report
candidate labels and leave `leiden` as the default grouping.

**Assess and report:** annotation evidence, unresolved clusters, many-to-one
cluster merges, and whether external annotations disagree with Leiden clusters.

For metadata and grouping semantics, read
`references/matrix_and_metadata_model.md`.

---

## Step 7 - Save and export

Save the native pagoda2 object and export h5ad when downstream tools may use
AnnData/scanpy-compatible objects.

```r
saveRDS(p2, file.path(WORK_DIR, "pagoda2_processed.rds"))

p2$export(
  file.path(WORK_DIR, "pagoda2_processed.h5ad"),
  format = "h5ad",
  overwrite = TRUE
)

export_summary <- data.frame(
  file = c("pagoda2_processed.rds", "pagoda2_processed.h5ad"),
  path = file.path(WORK_DIR, c("pagoda2_processed.rds",
                               "pagoda2_processed.h5ad")),
  exists = file.exists(file.path(WORK_DIR, c("pagoda2_processed.rds",
                                             "pagoda2_processed.h5ad")))
)
print(export_summary)
```

**Assess and report:** native RDS path, h5ad path, exported cell/gene counts,
whether raw counts and normalized expression are included, and any metadata
columns with missing resolved values.

For export semantics and optional conversions, read
`references/export_and_interop.md`.

## Final response checklist

Summarize:

- input format, sample ID, exact count layer/files, cells, genes, sparsity,
  and count integer-likeness
- QC pass/fail counts, fail fraction, median molecules, median genes, and
  whether MT/ribo metrics were available
- post-filter cells, raw genes retained, analysis genes, OD genes, and default
  grouping
- Leiden cluster count and largest/smallest cluster sizes
- PCA elbow and UMAP quality observations
- marker quality, top marker examples, and whether dotplot/heatmap agree
- annotations stored or annotation uncertainty
- output figure/table/object paths
- caveats: wrong count layer risk, weak markers, QC-driven clusters,
  doublets, batch effects, over-clustering, or under-clustering
