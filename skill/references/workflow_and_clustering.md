# Workflow And Clustering

This reference covers the single-dataset processing thread after import:
QC/filtering, variance modeling, PCA, graph construction, embeddings, Leiden,
thread control, and result inspection.

## Default Workflow

The canonical workflow is:

```text
qc -> filter -> variance -> pca -> graph -> embedding -> leiden -> markers
```

Run it with defaults:

```r
p2$run(plots = "none", verbose = TRUE)
```

Skip markers when the user wants clustering first:

```r
p2$run(skip = "markers", plots = "none", verbose = TRUE)
```

Run through clustering only:

```r
p2$run(
  steps = c("variance", "pca", "graph", "embedding", "leiden"),
  plots = "none",
  verbose = TRUE
)
```

With default `dependencies = "auto"`, missing upstream steps are added.
Existing canonical results are reused unless `overwrite = TRUE`.

`p2$run()` mutates the object in place. Use public result helpers rather than
guessing internal paths:

```r
p2$listResults()
p2$listReductions()
p2$listGraphs()
p2$listEmbeddings()
p2$listGroupings()
p2$listMarkers()
```

## Step-Specific Arguments

Route overrides into the matching step list:

```r
p2$run(
  plots = "none",
  verbose = TRUE,
  pca = list(nPcs = 50, n.odgenes = 3000),
  graph = list(k = 30, distance = "cosine", weight.type = "1m"),
  embedding = list(method = "UMAP"),
  leiden = list(resolution = 1),
  markers = list(upregulated.only = TRUE,
                 append.auc = TRUE,
                 append.specificity.metrics = TRUE)
)
```

`n.odgenes` belongs to the PCA step, not the variance step. Variance modeling
selects and scores overdispersed genes; PCA decides how many of those genes to
use.

## Thread Control

Use `n.cores` for a simple per-call total budget:

```r
p2$run(plots = "none", verbose = TRUE, n.cores = 10)
p2$runEmbedding(n.cores = 10)
p2$runMarkers(n.cores = 10)
```

Use `threads` for advanced role-specific control:

```r
p2$runEmbedding(threads = list(total = 10, sgd = 1))
p2$runMarkers(threads = list(total = 10, r.workers = 6))
p2$runReduction(method = "pca", threads = list(total = 10, blas = 4))
```

Supported thread roles:

- `total`: total method budget
- `r.workers`: forked R workers for marker-style parallel loops
- `native`: C++/OpenMP workers (incl. the threaded hnswlib/RcppHNSW kNN backend)
- `sgd`: UMAP stochastic-gradient workers
- `blas`: BLAS/LAPACK threads where controllable

Threading note: streaming reductions (variance, cluster pseudobulk) and kNN
graph building parallelize well; PCA (`runReduction(method="pca")`) is
irlba/sparse-matvec-bound and barely speeds up with more cores.

Set object defaults when later calls should share the same policy:

```r
p2$setCores(10)
p2$setThreads(total = 10, sgd = 1)
p2$describeThreads(method = "runEmbedding")
```

Use one of `n.cores` or `threads` in a single call. Prefer `n.cores` unless
the user explicitly asks for role-level control.

## Variance Modeling And OD Genes

Run variance modeling through the default workflow or directly:

```r
p2$runVariance(verbose = TRUE)
```

Plot diagnostics separately through the new plotting API:

```r
p_var <- p2$plotVarianceQC()
ggplot2::ggsave("variance_qc.png", p_var,
                width = 10, height = 4.8, units = "in", dpi = 120,
                bg = "white")
```

The plot shows the mean-variance fit and adjusted variance after
normalization. Overdispersed genes are highlighted. If the fit looks odd or
the OD gene count is unexpectedly low, check input layer selection and
filtering thresholds before interpreting PCA.

Inspect counts:

```r
cat(sprintf("%d analysis genes; %d OD genes\n",
            sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass, na.rm = TRUE),
            length(p2$getOdGenes())))
```

## PCA (and the generic reduction API)

PCA is a `method=` of the generic `runReduction()` step — there is no `runPCA`
verb (removed in 2.2; same for `runLeiden`/`runWNN`/`runUMAP`). Default PCA is
50 components and 3000 OD genes; `method` defaults to the facet's
`defaultReduction` (PCA for RNA, LSI for ATAC):

```r
p2$runReduction(nPcs = 50, n.odgenes = 3000, verbose = TRUE)   # = method "pca" on the default facet
p2$runReduction(facet = "ADT", nPcs = 20)                      # per-facet -> reductions[["ADT:PCA"]]
p2$runReduction(facet = "ATAC", method = "lsi")                # tfidf -> SVD -> reductions[["ATAC:LSI"]]
p2$runReduction(facets = c("RNA","ADT"), method = "cca")       # joint integration -> reductions[["CCA"]]
```

For multimodal facets, joint integration, and WNN, see
`references/multimodal_facets.md`.

Save the built-in elbow plot:

```r
p_elbow <- p2$plotPCAElbow()
ggplot2::ggsave("pca_elbow.png", p_elbow,
                width = 7.5, height = 4.2, units = "in", dpi = 120,
                bg = "white")
```

Assess:

- per-PC percent total variance explained
- cumulative variance curve shape
- whether 50 PCs appears excessive or insufficient
- whether early PCs may reflect QC, sample, batch, or library-size effects

Use `plotPCAElbow()` instead of deriving variance from internal PCA slots.

## Graph Construction

The default graph is built from PCA with cosine distance and `weight.type =
"1m"`:

```r
p2$runGraph(reduction = "PCA",
            k = 30,
            distance = "cosine",
            weight.type = "1m")
p2$listGraphs()
```

If embedding shapes look compressed, fragmented, or unexpectedly different
from a comparison workflow, inspect graph degree and weighted degree:

```r
graph <- p2$graphs$PCA
summary(igraph::degree(graph))
if (igraph::is_weighted(graph)) {
  summary(igraph::strength(graph))
}
```

Report graph settings whenever they differ from defaults or when diagnosing
embedding/clustering differences.

## Embeddings

`runEmbedding()` is the generic embedding API. Do not use method-specific
wrappers in new pagoda2.1 workflows. UMAP is the default method:

```r
p2$runEmbedding(reduction = "PCA", method = "UMAP", name = "UMAP")
```

When `distance = NULL`, pagoda2 uses method-aware defaults:

- UMAP, UMAP_graph, largeVis, and FR use cosine
- tSNE uses L2 to avoid the heavy dense cosine tSNE distance path

Plot the default UMAP:

```r
p_umap <- p2$plotEmbedding(grouping = "leiden",
                           mark.groups = TRUE,
                           size = 0.35,
                           alpha = 0.55)
ggplot2::ggsave("umap_leiden.png", p_umap,
                width = 7.4, height = 6.2, units = "in", dpi = 120,
                bg = "white")
```

Generate tSNE through the same API:

```r
p2$runEmbedding(reduction = "PCA",
                method = "tSNE",
                name = "tSNE")
p_tsne <- p2$plotEmbedding(embedding = "tSNE",
                           grouping = "leiden",
                           mark.groups = TRUE)
```

Overlay metadata when relevant:

```r
p2$plotEmbedding(grouping = "sample")
p2$plotEmbedding(grouping = "batch")
```

For numeric metadata, pass a named vector as `colors`:

```r
mito <- p2$resolveCellMeta("percent_mito")
p2$plotEmbedding(colors = stats::setNames(mito$percent_mito, rownames(mito)))
```

Assess cluster coherence, outlying islands, and whether QC or sample metadata
dominates the embedding.

## Clustering (Leiden)

Clustering is a `method=` of the generic `runClustering()` step (`leiden` is the
default and only built-in method; `runLeiden()` remains as a thin legacy alias).
Rerun clustering only:

```r
p2$runClustering(method = "leiden",
                 graph = "PCA",          # or graph = "WNN" for a joint multimodal graph
                 name = "leiden",
                 resolution = 1,
                 setDefault = TRUE,
                 overwrite = TRUE)
```

`runClustering()` stores the grouping as cell metadata and can make it the
default grouping. Inspect available groupings:

```r
p2$listGroupings()
table(p2$getGrouping("leiden"))
```

If clusters are too coarse or too fragmented, rerun Leiden with a different
`resolution`, then rerun markers for the new grouping.

## Workflow Report

Report:

- requested workflow steps and any skipped steps
- cells retained after filtering
- analysis genes and OD genes
- PCA dimensions and elbow assessment
- graph settings: reduction, k, distance, weight type
- embedding method and distance default used
- Leiden resolution and cluster count
- largest and smallest cluster sizes
- any warning that QC suggested filtering before downstream analysis
