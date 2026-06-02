---
name: pagoda2-scrna-v2
description: Basic single-sample scRNA-seq QC + clustering + markers recipe with pagoda2.1. Install pagoda2 from the GitHub devel branch before use. Supports common scRNA-seq inputs and h5ad/native exports.
when_to_use: Use for any single-sample scRNA-seq count dataset when the user wants pagoda2, an R-based sparse workflow, or flexible I/O across 10x triplets, CellRanger HDF5, AnnData h5ad, h5Seurat, loom, RDS, Seurat, or SingleCellExperiment.
requires_tools: [run_r]
capabilities_needed: [pagoda2-devel]
install_from_github_devel: remotes::install_github("kharchenkolab/pagoda2", ref = "devel")
keywords: [pagoda2, scRNA-seq, QC, UMAP, Leiden, marker genes, dotplot, heatmap, h5ad, 10x, h5Seurat, loom]
produces: [qc_gene_molecule.png, qc_composition_violin.png, pca_elbow.png, umap_leiden.png, marker_dotplot.png, marker_heatmap_native.png, cluster_markers.csv, pagoda2_processed.rds, pagoda2_processed.h5ad]
domain: genomics
source: "Pagoda2.1 devel workflow recipe for internal graphical chat analysis with persistent entities."
---

# scRNA-seq single-sample QC + clustering with pagoda2.1

Generic single-sample recipe - works for **any tissue, any organism** when the
input is a raw count matrix. Pagoda2.1 is especially useful when sparse
performance matters and when the workflow needs to read or write common
single-cell formats.

Pagoda2.1 can read 10x Matrix Market triplets, 10x/CellRanger HDF5, AnnData
`.h5ad`, h5Seurat, and loom. It can save native RDS objects, export h5ad, and
convert to list, SingleCellExperiment, or Seurat objects when optional packages
are installed.

Install from GitHub `devel` before use:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
remotes::install_github("kharchenkolab/sccore", ref = "devel")
remotes::install_github("kharchenkolab/pagoda2", ref = "devel")
```

Then:

```r
library(pagoda2)
library(ggplot2)
```

For detailed install and I/O choices, read
`references/installation_and_io.md`.

---

## Decisions To Surface Up Front

Tell the user these are the analysis-defining decisions:

1. **Input format and layer** - use raw integer counts. In h5ad, h5Seurat, or
   loom, choose the count layer explicitly when needed.
2. **QC filtering** - inspect pagoda2's gene-vs-molecule QC decision before
   filtering. Mitochondrial and ribosomal percentages are optional and
   gene-annotation dependent.
3. **Analysis genes** - pagoda2 keeps raw genes but uses an `analysis_pass` gene
   mask and overdispersed genes for PCA.
4. **PC count** - the default workflow computes 50 PCs; inspect the elbow plot.
5. **Leiden resolution** - cluster count and marker coherence determine whether
   the clustering is useful.

Show the user these figures as the analysis proceeds:

- `qc_gene_molecule.png`
- `qc_composition_violin.png`, if MT/ribo genes are identifiable
- `pca_elbow.png`
- `umap_leiden.png`
- `marker_dotplot.png`
- `marker_heatmap_native.png`

---

## Step 1 - Load And Sanity-Check Counts

```r
WORK_DIR <- "/path/to/workdir"
INPUT <- "/path/to/counts_or_single_cell_file"
SAMPLE_ID <- "sample_01"
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

p2 <- Pagoda2$from(INPUT, sample.name = SAMPLE_ID, verbose = TRUE)
```

Use a specific constructor when format guessing is not desirable:

```r
p2 <- Pagoda2$from10x("/path/to/filtered_feature_bc_matrix")
p2 <- Pagoda2$from10xH5("/path/to/filtered_feature_bc_matrix.h5")
p2 <- Pagoda2$fromAnnData("/path/to/sample.h5ad")
p2 <- Pagoda2$fromH5Seurat("/path/to/sample.h5seurat")
p2 <- Pagoda2$fromLoom("/path/to/sample.loom")
```

If a file has multiple layers:

```r
p2 <- Pagoda2$fromAnnData(
  "/path/to/sample.h5ad",
  reader.args = list(layer = "counts")
)
```

Check the count matrix:

```r
counts <- p2$getRawCounts()
load_summary <- data.frame(
  sample = SAMPLE_ID,
  cells = nrow(counts),
  genes = ncol(counts),
  nonzero = Matrix::nnzero(counts),
  sparsity = 1 - Matrix::nnzero(counts) / prod(dim(counts)),
  integer_like = all(abs(counts@x - round(counts@x)) < 1e-8)
)
print(load_summary)
stopifnot(inherits(counts, "dgCMatrix"))
stopifnot(load_summary$integer_like)
```

**Report:** input format, cells, genes, sparsity, and whether counts are
integer-like. If counts are not integer-like, stop and check whether the wrong
layer or a normalized matrix was loaded.

---

## Step 2 - QC And Filtering

```r
p2$runQC(verbose = TRUE)

qc <- p2$resolveCellMeta(c(
  "n_molecules",
  "n_genes",
  "qc_pass",
  "qc_gene_molecule_outlier",
  "qc_size_outlier"
))

qc_summary <- data.frame(
  cells = nrow(qc),
  qc_pass = sum(as.logical(qc$qc_pass), na.rm = TRUE),
  qc_fail = sum(!as.logical(qc$qc_pass), na.rm = TRUE),
  fail_fraction = mean(!as.logical(qc$qc_pass), na.rm = TRUE),
  median_molecules = median(qc$n_molecules, na.rm = TRUE),
  median_genes = median(qc$n_genes, na.rm = TRUE)
)
print(qc_summary)
```

Show the main QC figure:

```r
p_qc <- p2$plotQC()
ggsave(file.path(WORK_DIR, "qc_gene_molecule.png"), p_qc,
       width = 10, height = 4.5, units = "in", dpi = 120, bg = "white")
```

If mitochondrial/ribosomal genes are identifiable:

```r
p2$runQC(overwrite = TRUE, mt.pattern = "^MT-", ribo.pattern = "^RP[SL]")
p_comp <- p2$plotQCViolin()
ggsave(file.path(WORK_DIR, "qc_composition_violin.png"), p_comp,
       width = 7, height = 4.5, units = "in", dpi = 120, bg = "white")
```

Filter with the standard pre-analysis gate:

```r
before <- c(cells = nrow(p2$getRawCounts()), genes = ncol(p2$getRawCounts()))
p2$filterData(verbose = TRUE)
after <- c(
  cells = nrow(p2$getRawCounts()),
  raw_genes = ncol(p2$getRawCounts()),
  analysis_genes = sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass)
)
print(before)
print(after)
```

**Assess and report:** QC failure rate, why cells failed, whether the failed set
looks like a quality tail or possible biology, cells removed, raw genes
retained, and analysis genes selected. For details, read
`references/qc_and_filtering.md`.

---

## Step 3 - Run The Core Workflow

Run through clustering first:

```r
p2$run(
  steps = c("variance", "pca", "graph", "umap", "leiden"),
  plots = "none",
  verbose = TRUE,
  pca = list(nPcs = 50, n.odgenes = 3000)
)
```

Show PCA and UMAP:

```r
p_elbow <- p2$plotPCAElbow()
ggsave(file.path(WORK_DIR, "pca_elbow.png"), p_elbow,
       width = 7, height = 4, units = "in", dpi = 120, bg = "white")

p_umap <- p2$plotEmbedding(
  grouping = "leiden",
  mark.groups = TRUE,
  size = 0.35,
  alpha = 0.55,
  title = paste(SAMPLE_ID, "Leiden clusters")
)
ggsave(file.path(WORK_DIR, "umap_leiden.png"), p_umap,
       width = 7, height = 6, units = "in", dpi = 120, bg = "white")

print(sort(table(p2$getGrouping("leiden")), decreasing = TRUE))
```

**Assess and report:** OD gene count, PC elbow shape, cluster count, cluster
sizes, spatial coherence, tiny clusters, and any available sample/batch
overlays. For details, read `references/workflow_and_clustering.md`.

---

## Step 4 - Marker Genes And Plots

```r
markers <- p2$runMarkers(
  grouping = "leiden",
  name = "leiden",
  upregulated.only = TRUE,
  append.auc = TRUE,
  append.specificity.metrics = TRUE,
  verbose = TRUE
)
```

Write markers:

```r
marker_tables <- p2$markerResults$leiden$markers
marker_df <- if (is.data.frame(marker_tables)) {
  marker_tables
} else {
  do.call(rbind, lapply(names(marker_tables), function(group) {
    x <- marker_tables[[group]]
    if (is.null(x) || !nrow(x)) return(NULL)
    x$group <- group
    x
  }))
}
utils::write.csv(marker_df, file.path(WORK_DIR, "cluster_markers.csv"))
```

Show marker summaries:

```r
p_dot <- p2$plotMarkerDotPlot(
  markers = "leiden",
  n.genes.per.group = 5,
  selection = "balanced",
  order.groups = TRUE,
  dot.scale = 7
) + ggtitle("Leiden marker genes")
ggsave(file.path(WORK_DIR, "marker_dotplot.png"), p_dot,
       width = 16, height = 10, units = "in", dpi = 120, bg = "white")

png(file.path(WORK_DIR, "marker_heatmap_native.png"),
    width = 13.5, height = 8.1, units = "in", res = 120, bg = "white")
p2$plotMarkerHeatmap(
  markers = "leiden",
  engine = "native",
  n.genes.per.group = 3,
  selection = "balanced",
  column.metadata = intersect(c("n_molecules", "n_genes", "percent_mito", "percent_ribo"),
                              colnames(p2$getCellMeta())),
  split = TRUE,
  show_heatmap_legend = TRUE
)
dev.off()
```

**Assess and report:** whether markers are crisp, specific, biologically
coherent, or dominated by QC/stress/cell-cycle genes. Do not over-annotate from
marker names alone. For details, read `references/markers_and_plots.md`.

---

## Step 5 - Optional Annotation

Cluster annotations are ordinary cell metadata. Many-to-one mappings are normal:

```r
p2$annotateClusters(
  from = "leiden",
  to = "cell_type",
  map = c("0" = "T cells", "1" = "T cells", "2" = "monocytes"),
  setDefault = TRUE
)
```

External labels can be stored directly:

```r
p2$setGrouping("external_annotation", external_labels, setDefault = TRUE)
```

**Assess:** annotations may merge, split, or disagree with clusters. Report the
evidence and uncertainty.

---

## Step 6 - Save And Export

Always save the native object:

```r
saveRDS(p2, file.path(WORK_DIR, "pagoda2_processed.rds"))
```

Export h5ad when downstream work may use AnnData/scanpy:

```r
p2$export(
  file.path(WORK_DIR, "pagoda2_processed.h5ad"),
  format = "h5ad",
  overwrite = TRUE
)
```

Optional in-memory conversions:

```r
as_list <- p2$as("list")
if (requireNamespace("SingleCellExperiment", quietly = TRUE)) sce <- p2$as("sce")
if (requireNamespace("Seurat", quietly = TRUE)) seurat_obj <- p2$as("seurat")
```

For details, read `references/export_and_interop.md`.

---

## Final Response Checklist

Summarize:

- input format, sample ID, and count-layer sanity
- raw cells/genes and sparsity
- QC failure rate and filtering impact
- post-filter cells, raw genes retained, and analysis genes
- OD genes, PCs used, graph/UMAP/clustering settings if non-default
- Leiden cluster count and cluster sizes
- marker quality and biological plausibility
- figures shown to the user
- saved and exported files
- caveats: MT/ribo detection, doublets, batch effects, weak markers,
  over-clustering, or under-clustering
