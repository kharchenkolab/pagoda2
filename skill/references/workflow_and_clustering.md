# Workflow And Clustering

## Default Workflow

The full default workflow is:

```text
qc -> filter -> variance -> pca -> graph -> umap -> leiden -> markers
```

For staged analysis, run through clustering first:

```r
p2$run(
  steps = c("variance", "pca", "graph", "umap", "leiden"),
  plots = "none",
  verbose = TRUE,
  pca = list(nPcs = 50, n.odgenes = 3000)
)
```

Canonical names:

- reduction: `PCA`
- graph: `PCA`
- embedding: `UMAP`
- grouping: `leiden`
- marker result: `leiden`

## Overdispersed Genes

Variance modeling identifies overdispersed genes for PCA:

```r
length(p2$misc$odgenes)
head(p2$misc$odgenes)
```

Compare OD gene count to:

```r
sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass)
```

Large unexpected differences can indicate earlier filtering or gene naming
issues.

## PCA Elbow

Show:

```r
p2$plotPCAElbow()
```

Assess:

- whether per-PC variance drops smoothly
- whether the cumulative curve supports the chosen PC range
- whether 50 PCs is clearly excessive or insufficient

## UMAP And Leiden

Show:

```r
p2$plotEmbedding(grouping = "leiden", mark.groups = TRUE)
sort(table(p2$getGrouping("leiden")), decreasing = TRUE)
```

Assess:

- number of clusters
- smallest clusters
- whether clusters are spatially coherent
- whether clusters are over-fragmented
- whether there are islands likely to be low-quality cells or doublets
- whether sample, donor, batch, or condition metadata dominate the embedding

If metadata exists, overlay it:

```r
p2$plotEmbedding(grouping = "sample")
p2$plotEmbedding(grouping = "batch")
```

## Parameter Changes

Use simple step argument lists:

```r
p2$run(
  steps = c("pca", "graph", "umap", "leiden"),
  pca = list(nPcs = 40, n.odgenes = 3000),
  graph = list(k = 50, distance = "cosine"),
  leiden = list(resolution = 1.5)
)
```

Report non-default parameters in the final answer.
