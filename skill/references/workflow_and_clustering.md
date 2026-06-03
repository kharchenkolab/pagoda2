# Workflow And Clustering

This reference covers the single-dataset processing thread after import:
filtering, variance modeling, PCA, graph construction, embeddings, Leiden
clustering, thread control, and result inspection.

## Default Workflow

The default workflow is:

```text
qc -> filter -> variance -> pca -> graph -> embedding -> leiden -> markers
```

Run it with defaults unless the user has a reason to override:

```r
p2$run(plots = "none", verbose = TRUE)
```

Skip marker genes when the user wants clustering first:

```r
p2$run(skip = "markers", plots = "none", verbose = TRUE)
```

Run a staged subset:

```r
p2$run(
  steps = c("variance", "pca", "graph", "embedding", "leiden"),
  plots = "none",
  verbose = TRUE
)
```

With `dependencies = "auto"`, required earlier steps are included and existing
results are reused unless `overwrite = TRUE`.

`p2$run()` mutates the object in place and stores results in the object
registries: QC/gene masks in metadata, variance state in `p2$misc`,
reductions in `p2$reductions`, graphs in `p2$graphs`, embeddings in
`p2$embeddings`, Leiden groupings in `p2$cellMeta`, and markers in the marker
registry. Use listing helpers below instead of guessing internal paths.

## Step-Specific Arguments

Pass step overrides in the matching list:

```r
p2$run(
  plots = "none",
  verbose = TRUE,
  pca = list(nPcs = 50, n.odgenes = 3000),
  graph = list(k = 30, distance = "cosine", weight.type = "1m"),
  embedding = list(method = "UMAP"),
  leiden = list(resolution = 1)
)
```

Do not route PCA arguments into `variance`. For example, `n.odgenes` belongs
to the PCA step in current pagoda2.1.

## Thread Control

Use `n.cores` for the simple total core budget:

```r
p2$run(plots = "none", verbose = TRUE, n.cores = 10)
p2$runEmbedding(n.cores = 10)
p2$runMarkers(n.cores = 10)
```

Use `threads` only for advanced role-specific control:

```r
p2$runEmbedding(threads = list(total = 10, sgd = 1))
p2$runMarkers(threads = list(total = 10, r.workers = 6))
p2$runPCA(threads = list(total = 10, blas = 4))
```

Supported roles:

- `total`: user budget for the method
- `r.workers`: forked R workers, used by marker-style parallel loops
- `native`: C++/OpenMP/N2R-style native workers
- `sgd`: UMAP stochastic-gradient workers
- `blas`: BLAS/LAPACK threads where controllable

Set object defaults when all later calls should share the same policy:

```r
p2$setCores(10)
p2$setThreads(total = 10, sgd = 1)
p2$describeThreads(method = "runEmbedding")
```

Environment or option-level controls are useful for a whole session:

```r
options(pagoda2.threads = list(total = 10, sgd = 1))
options(pagoda2.n.cores = 10)
```

Use one of `n.cores` or `threads`, not both, in the same call.

## Variance And OD Genes

Run variance modeling through `run()` or directly:

```r
p2$runVariance(verbose = TRUE)
```

PCA selects overdispersed genes by default:

```r
p2$runPCA(nPcs = 50, n.odgenes = 3000, verbose = TRUE)
```

Inspect the gene counts:

```r
cat(sprintf("%d analysis genes; %d OD genes\n",
            sum(p2$resolveGeneMeta("analysis_pass")$analysis_pass),
            length(p2$getOdGenes())))
```

If the OD gene count is unexpectedly low, check that input loading did not
filter genes early and that `filterData()` thresholds are not too strict.

## PCA Assessment

Save the built-in elbow plot:

```r
p_elbow <- p2$plotPCAElbow()
ggplot2::ggsave("pca_elbow.png", p_elbow,
                width = 7.5, height = 4.2, units = "in", dpi = 120,
                bg = "white")
```

Assess:

- percent total variance explained by early PCs
- cumulative variance curve shape
- whether 50 PCs is too many or too few
- whether the first PCs may reflect QC, batch, or library-size effects

Use `plotPCAElbow()` rather than deriving PCA variance manually from internal
slots.

## Graph Construction

Run graph construction through `run()` or directly:

```r
p2$runGraph(reduction = "PCA",
            k = 30,
            distance = "cosine",
            weight.type = "1m")
p2$listGraphs()
```

If UMAP shapes look compressed, fragmented, or different from a comparison
workflow, inspect graph degree and weighted degree:

```r
graph <- p2$graphs$PCA
summary(igraph::degree(graph))
if (igraph::is_weighted(graph)) {
  summary(igraph::strength(graph))
}
```

Report graph settings when they differ from defaults or when diagnosing
embedding/clustering differences.

## Embeddings

`runEmbedding()` is the generic embedding API. Do not use method-specific
embedding wrappers in new pagoda2.1 workflows. When `distance = NULL`, it uses
method defaults: cosine for UMAP, UMAP_graph, largeVis, and FR; L2 for tSNE.
The default method is UMAP:

```r
p2$runEmbedding(reduction = "PCA", method = "UMAP", name = "UMAP")
p_umap <- p2$plotEmbedding(grouping = "leiden",
                           mark.groups = TRUE,
                           size = 0.35,
                           alpha = 0.55)
ggplot2::ggsave("umap_leiden.png", p_umap,
                width = 7.4, height = 6.2, units = "in", dpi = 120,
                bg = "white")
```

Overlay metadata when relevant:

```r
p2$plotEmbedding(grouping = "sample")
p2$plotEmbedding(grouping = "batch")
```

For numeric metadata, pass a named vector as colors:

```r
mito <- p2$resolveCellMeta("percent_mito")
p2$plotEmbedding(colors = stats::setNames(mito$percent_mito, rownames(mito)))
```

Assess cluster coherence, outlying islands, and whether QC or sample metadata
dominates the embedding. If a method is not specified, report that the default
UMAP embedding was used.

Generate tSNE through the same API:

```r
p2$runEmbedding(reduction = "PCA",
                method = "tSNE",
                name = "tSNE",
                perplexity = 50)
p2$plotEmbedding(embedding = "tSNE", grouping = "leiden")
```

This uses `distance = "L2"` by default. Use the L2 default unless the user has
a specific reason to compare with a cosine-distance tSNE. To force cosine
tSNE, pass it explicitly and expect a dense cell-cell distance matrix:

```r
p2$runEmbedding(reduction = "PCA",
                method = "tSNE",
                name = "tSNE_cosine",
                distance = "cosine",
                perplexity = 50)
```

## Leiden Clustering

Run Leiden through `run()` or directly:

```r
p2$runLeiden(name = "leiden", setDefault = TRUE)
sort(table(p2$getGrouping("leiden")), decreasing = TRUE)
```

The Leiden result is stored as a cell metadata grouping. `setDefault = TRUE`
sets the default grouping pointer, which marker and plotting methods use when
`grouping` is omitted:

```r
p2$getDefaultGrouping()
p2$runMarkers(name = p2$getDefaultGrouping())
```

Create alternative clusterings with different names:

```r
p2$runLeiden(name = "leiden_r15", resolution = 1.5, setDefault = FALSE)
p2$plotEmbedding(grouping = "leiden_r15")
```

## Result Registry

Use listing helpers for agent state tracking:

```r
p2$listReductions()
p2$listGraphs()
p2$listEmbeddings()
p2$listGroupings()
p2$listMarkers()
p2$listResults()
```

These help avoid guessing where a result lives inside the R6 object.

## Re-Running Steps

Use `overwrite = TRUE` only when intentionally replacing a result:

```r
p2$run(
  steps = c("pca", "graph", "embedding", "leiden"),
  overwrite = TRUE,
  pca = list(nPcs = 40),
  plots = "none",
  verbose = TRUE
)
```

Changing filtering after downstream results invalidates PCA, graph, embeddings,
Leiden, and markers. Use a fresh object when possible. If forced filtering is
necessary, rerun downstream steps after `filterData(force = TRUE)`.

## Reporting Checklist

Report:

- cells and genes after filtering
- analysis gene count and OD gene count
- PCs used and elbow-plot interpretation
- graph settings and graph diagnostics if checked
- embedding method/name, resolved distance when relevant, and grouping shown
- Leiden cluster count and cluster sizes
- thread controls if non-default
- rerun or overwrite decisions
