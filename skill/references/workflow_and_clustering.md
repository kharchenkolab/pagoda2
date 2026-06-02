# Workflow And Clustering

This page covers the single-dataset processing thread after data import:
variance modeling, PCA, graph construction, UMAP, Leiden clustering, and result
inspection.

## Default Workflow Shape

The full default workflow is:

```text
qc -> filter -> variance -> pca -> graph -> umap -> leiden -> markers
```

For beginner-facing notebooks, make this explicit but avoid forcing the user to
specify every default:

```r
p2$run(verbose = TRUE, plots = "none")
```

To skip marker genes:

```r
p2$run(skip = "markers", verbose = TRUE, plots = "none")
```

To run through clustering first and calculate markers later:

```r
p2$run(
  steps = c("variance", "pca", "graph", "umap", "leiden"),
  plots = "none",
  verbose = TRUE
)
```

With `dependencies = "auto"` (the default), earlier required steps are included
and skipped if already complete. For example, after `runQC()` and
`filterData()`, the staged call above will not recompute them unless
`overwrite = TRUE` or explicit parameters require it.

## Profiles And Plots

`run()` supports lightweight plotting modes:

```r
p2$run(profile = "interactive")       # default plot behavior for interactive use
p2$run(profile = "pipeline", plots = "none")
p2$run(profile = "report", plots = "collect")
```

For agent workflows, prefer `plots = "none"` during computation and call plot
methods explicitly so each figure can be saved, shown, and assessed.

## Step Arguments

Pass step-specific arguments as lists. Good defaults should handle most
datasets, but these are common overrides:

```r
p2$run(
  steps = c("variance", "pca", "graph", "umap", "leiden"),
  pca = list(nPcs = 50, n.odgenes = 3000),
  graph = list(k = 30, distance = "cosine", weight.type = "1m"),
  leiden = list(resolution = 1)
)
```

When reporting results, mention only non-default parameters unless the user
asked for a full provenance table.

## Overdispersed Genes

Variance modeling identifies overdispersed genes for PCA. Usually call it
through `run()`, but a staged script can call:

```r
p2$runVariance(verbose = TRUE)
```

Inspect gene counts:

```r
od_gene_count <- length(p2$misc$odgenes)
analysis_gene_count <- sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass)
data.frame(analysis_gene_count, od_gene_count)
```

Unexpectedly low analysis or OD gene counts can indicate that import filtered
genes too early, gene names were duplicated, or `filterData()` thresholds were
too strict.

## PCA

Run PCA through `run()` or directly:

```r
p2$runPCA(nPcs = 50, n.odgenes = 3000, verbose = TRUE)
```

Show the elbow plot:

```r
p_elbow <- p2$plotPCAElbow()
ggplot2::ggsave("pca_elbow.png", p_elbow,
                width = 7, height = 4, units = "in", dpi = 120,
                bg = "white")
```

Assess:

- whether per-PC variance drops smoothly
- whether the cumulative curve supports the chosen PC range
- whether 50 PCs is clearly excessive or insufficient
- whether there is a very sharp first-PC effect that should be checked against
  QC or batch metadata

## Graph Construction

Build the graph through `run()` or directly:

```r
p2$runGraph(reduction = "PCA", k = 30, distance = "cosine", weight.type = "1m")
p2$listGraphs()
```

The default graph namespace is normally the reduction name, such as `PCA`.
Check graph size and weighted degree if embeddings or clusters look unusual:

```r
p2$listGraphs()
graph <- p2$graphs$PCA
summary(igraph::degree(graph))
if (igraph::is_weighted(graph)) summary(igraph::strength(graph))
```

Use this diagnostic when comparing old and new workflows, or when UMAP shapes
look compressed or fragmented.

## UMAP

Run UMAP through `run()` or directly:

```r
p2$runUMAP(reduction = "PCA", name = "UMAP")
p_umap <- p2$plotEmbedding(grouping = "leiden", mark.groups = TRUE)
```

Save with enough room for labels:

```r
ggplot2::ggsave("umap_leiden.png", p_umap,
                width = 7, height = 6, units = "in", dpi = 120,
                bg = "white")
```

Overlay relevant metadata when available:

```r
p2$plotEmbedding(grouping = "sample")
p2$plotEmbedding(grouping = "batch")
p2$plotEmbedding(grouping = "percent_mito")
```

Assess whether clusters are spatially coherent, whether QC metrics dominate the
embedding, and whether tiny islands are likely low-quality cells or doublets.

## Leiden Clustering

Run Leiden through `run()` or directly:

```r
p2$runLeiden(name = "leiden", setDefault = TRUE)
sort(table(p2$getGrouping("leiden")), decreasing = TRUE)
p2$listGroupings()
```

`runLeiden()` stores clusters as a `cellMeta` grouping and usually sets
`defaultGrouping` to that column. Marker methods and plotting methods can then
use the default grouping:

```r
p2$getDefaultGrouping()
p2$runMarkers(name = "leiden")
```

Use `name` when creating alternative clustering results:

```r
p2$runLeiden(name = "leiden_r15", resolution = 1.5, setDefault = FALSE)
p2$plotEmbedding(grouping = "leiden_r15")
```

## Result Registry

Use result listing helpers instead of manually searching object slots:

```r
p2$listReductions()
p2$listGraphs()
p2$listEmbeddings()
p2$listGroupings()
p2$listMarkers()
p2$listResults()
```

These are useful for agent state tracking and for explaining what has already
been computed.

## Re-Running Steps

Use `overwrite = TRUE` when intentionally replacing an existing result:

```r
p2$run(
  steps = c("pca", "graph", "umap", "leiden"),
  overwrite = TRUE,
  pca = list(nPcs = 40)
)
```

Filtering after downstream results invalidates them. If a QC or filtering
decision changes after PCA/graph/UMAP/Leiden/markers, restart from a fresh
object or call filtering with `force = TRUE`, then rerun downstream steps.

## Reporting Checklist

Report:

- analysis gene count and OD gene count
- PCs used and any notable elbow behavior
- graph settings if non-default
- UMAP embedding name and grouping shown
- Leiden cluster count and cluster sizes
- small clusters, outlier islands, or QC/batch dominated regions
- any rerun/overwrite decisions
