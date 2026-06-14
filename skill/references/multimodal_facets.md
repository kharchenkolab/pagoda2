# Multimodal data with pagoda2.1 facets (CITE-seq, ATAC, 10x multiome)

pagoda2.1 (`apiVersion "2.2"`) handles multiple molecular modalities of the
**same cells** through **facets**. A facet is a first-class bundle: raw counts +
a normalization *view recipe* + its own variance model, overdispersed genes, and
reductions/loadings. RNA is the default facet (`p2$defaultFacet == "RNA"`); other
facets live in `misc$facetStore`. Cells share one canonical **union** axis
(`p2$cells`); a facet covering only some cells carries a membership mask.

Two rules govern the whole API:

1. **Steps are generic with a `method=`.** There is no `runPCA`, `runLeiden`,
   `runWNN`, or `runUMAP`. The algorithm is always a `method=` of `runReduction`,
   `runGraph`, `runClustering`, or `runEmbedding`.
2. **Cell-space results are name-keyed; feature-space results are facet-keyed.**
   Reductions/graphs/embeddings are stored top-level under one `:`-qualified rule
   (`PCA`, `ADT:PCA`, `WNN`, `PCA:UMAP`); the default facet is unqualified, joint
   products are bare (`WNN`, `CCA`, `concatPCA`). Markers/DE are facet-keyed.

---

## Adding a facet

```r
p2$addFacet(
  name            = "ADT",          # facet key
  countMatrix     = adt_counts,     # cells x features (dgCMatrix); cells need not all overlap RNA
  modelType       = "clr",          # view model: "plain" (RNA) | "clr" (ADT) | "tfidf" (ATAC)
  featureType     = "protein",      # "gene" | "protein" | "peak" | ... -> lstar feature-axis name
  defaultReduction= "PCA",          # "PCA" (RNA/ADT) | "LSI" (ATAC)
  backend         = "memory"        # "memory" | "lstar" (disk-backed zarr; see "Disk backing")
)
```

Facet introspection:

```r
p2$listFacets()                  # character vector of facet names
p2$getFacet("ADT")               # a Pagoda2Facet view (rawCounts, varinfo, odgenes, loadings, ...)
p2$resolveFacet(NULL)            # the default facet (RNA); resolveFacet("ADT") -> ADT
```

### View models (normalization recipes, computed on the fly — never stored twice)

| modelType | modality | transform |
|---|---|---|
| `plain` | RNA | depth-normalize → optional winsorize → `log1p` |
| `clr`   | ADT/CITE-seq | centered-log-ratio across proteins per cell |
| `tfidf` | ATAC/peaks | term-frequency × inverse-document-frequency (feeds LSI) |

The C++ view kernel is thread-count-invariant and bit-reproducible (float64
accumulation over the sparse storage); each facet stores only raw counts + a
small recipe regardless of modality.

---

## Per-facet analysis (every accessor/step takes `facet=`)

```r
p2$runVariance()                                   # RNA (default facet)
p2$runVariance(facet = "ADT", use.raw.variance = TRUE)
p2$runReduction(nPcs = 30)                         # RNA  -> reductions[["PCA"]]
p2$runReduction(facet = "ADT", nPcs = 20)          # ADT  -> reductions[["ADT:PCA"]]
p2$runReduction(facet = "ATAC", method = "lsi")    # ATAC -> reductions[["ATAC:LSI"]] (tfidf->SVD->drop comp1)

p2$getRawCounts(facet = "ADT")                     # facet raw counts (cells x features)
p2$getMatrixView("analysis", facet = "ADT")        # the CLR view recipe
p2$getExpressionBlock(genes = od, facet = "ADT")   # materialized view block
p2$runMarkers(grouping = "leiden", facet = "ADT")  # facet-keyed: markerResults[["ADT"]][["leiden"]]
```

Per-facet **loadings** stay in the facet (`p2$getFacet("ADT")$loadings[["PCA"]]`);
reduction **scores** go top-level (`p2$reductions[["ADT:PCA"]]`).

**Visualizing a facet.** The downstream plot/marker methods take `facet=` and draw that facet's own
(correctly normalized) expression:

```r
p2$plotEmbedding(reduction = "WNN", gene = "CD3_TotalSeqB", facet = "ADT")  # color by a protein / peak
p2$getTopMarkers(markers = "leiden", facet = "ADT")                          # the facet's marker table
p2$plotMarkerDotPlot(markers = "leiden", facet = "ADT")                      # facet markers + expression
p2$plotMarkerHeatmap(markers = "leiden", facet = "ADT")
```

`getExpressionBlock(genes=, facet=)` computes the CLR/TF-IDF view over the **full** feature axis before
subsetting columns (CLR is per-cell across all features; subsetting first would collapse a single feature
to 0), so single-feature/marker expression is correct. For CITE-seq protein, surface markers are detected
in ~every cell, so a per-cluster CLR heatmap reads better than a dot plot's size dimension.

### The default / resolution rule (single-facet stays bare; joints can't shadow)

- A bare reduction name (`reductions[["PCA"]]`) is the default facet's.
- A per-facet non-default reduction is qualified: `reductions[["ADT:PCA"]]`.
- A joint/integration product gets its own distinct bare name (`WNN`, `CCA`,
  `concatPCA`) — it can **never** be named after a per-facet method (the
  no-shadow validator errors if you try `name = "PCA"`).

### Union axis, membership masks, complete-cases

```r
p2$cells                                  # canonical UNION of all facets' cells
p2$getFacetMembership("ADT")              # logical mask over p2$cells (1:1 with an lstar partial-coverage index)
common <- p2$requireFacets(c("RNA","ADT"))# cells present in ALL listed facets (intersection / complete cases)
```

The default integration cell set is **`common`** (the intersection).

---

## Facet integration (a step with pluggable `method=`s; joints are named products)

Integration is the default behavior of the graph/reduction step when >1 facet is
present. Every method produces the same byproducts over the shared cells: a
name-keyed joint reduction and/or graph, optional per-cell weights, with
provenance `{facets, input_axes, method}` recorded as attributes.

### WNN — graph-level, per-cell modality weights (the shipped default)

```r
p2$runGraph(method = "wnn", facets = c("RNA", "ADT"))
p2$cellMeta$wnn_weight_RNA      # per-cell modality weights (sum to 1 across facets)
p2$cellMeta$wnn_weight_ADT
p2$graphs[["WNN"]]              # weighted SNN (igraph) over the common cells
p2$reductions[["WNN"]]          # per-cell-weighted joint reduction (for embedding)
```

Faithful WNN (Hao et al., *Integrated analysis of multimodal single-cell data*,
Cell 2021;184(13):3573-3587, https://doi.org/10.1016/j.cell.2021.04.048): per-facet
kNN, a per-cell bandwidth kernel, within- vs cross-modality predictive affinity →
ratio-normalized weights (dimensionality-robust; validated vs
`Seurat::FindMultiModalNeighbors`). `runGraph()` with no args **auto-integrates**
via WNN when ≥2 facets are reduction-ready; a single facet falls through to a plain kNN.

### CCA / sparse-CCA — reduction-level (per-facet feature loadings)

```r
p2$runReduction(facets = c("RNA","ADT"), method = "cca")              # -> reductions[["CCA"]]
p2$runReduction(facets = c("RNA","ADT"), method = "scca", penalty=0.3)# sparse (PMA; a Suggests dep)
p2$getFacet("RNA")$loadings[["CCA"]]    # genes    x k canonical loadings
p2$getFacet("ADT")$loadings[["CCA"]]    # proteins x k
attr(p2$reductions[["CCA"]], "cancor")  # canonical associations
```

Vertical two-block canonical correlation over the shared cells (centered
cross-covariance SVD; `method="cca"` dense via irlba, `"scca"` L1-sparse via
`PMA::CCA`). Emits joint scores + per-facet feature loadings.

### concat-PCA — reduction-level fallback

```r
p2$runReduction(facets = c("RNA","ADT"), method = "concat")   # -> reductions[["concatPCA"]]
```

Scales each facet's reduction to unit norm, concatenates, re-PCAs.

### Downstream on a joint product

```r
p2$runClustering(graph = "WNN", name = "wnn_leiden")     # leiden on the WSNN graph
p2$runEmbedding(reduction = "WNN", name = "umap")        # UMAP on the joint reduction
```

### Method menu (status)

| level | method | status |
|---|---|---|
| `runGraph` | `wnn` | **shipped** (default for ≥2 facets) |
| `runReduction` | `concat`, `cca`, `scca` | **shipped** |
| `runReduction` | LIGER/iNMF, MOFA+ | scoped, deferred (`misc/integration_methods_scoping.md`) |
| `runGraph` | SNF | scoped, deferred (O(n²), small-n niche) |
| reduction | Schema | scoped, deferred (Python backend) |
| — | MOFA | **not supported** |

---

## Import: native 10x multimodal + lstar

```r
# 10x multimodal H5: feature_type -> facets automatically
#   Gene Expression -> RNA/plain/genes/PCA ; Antibody Capture -> ADT/clr/proteins/PCA
#   Peaks | Chromatin Accessibility -> ATAC/tfidf/peaks/LSI
p2 <- Pagoda2$from10xH5("/path/filtered_feature_bc_matrix.h5", verbose = FALSE)

# lstar-mediated round-trip (lstar R is a required dep for the multimodal zarr path)
p2$export("sample.lstar.zarr", format = "lstar", overwrite = TRUE)
p2 <- pagoda2:::pagoda2FromLstar("sample.lstar.zarr")    # cells + per-facet feature axes + counts
```

A facet exports as one feature axis (genes/proteins/peaks) + one raw `counts`
measure, with `{facet, model, featureType, defaultReduction}` provenance and
joint products carrying `input_axes` (lstar shared-factor-axis shape). Partial
coverage exports as a typed lstar `index`.

---

## Disk backing (out-of-core) — lstar-zarr

A facet's raw counts can live in a chunked lstar Zarr store instead of an
in-memory `dgCMatrix`:

```r
p2$addFacet("RNA", counts, backend = "lstar", backend.dir = "/path/store.lstar.zarr")
```

The view kernels stream gene blocks off disk and apply the recipe in bounded
memory; `getExpressionBlock` / `viewColMeanVar` / `viewColSumByFac` have disk
paths that are **bit-identical** to in-memory and thread-invariant (validated on
245k-cell Tabula Muris Senis; `benchmark/RESULTS.md`, `benchmark/gate_zarr.R`).
The disk backing is lstar-zarr only (no BPCells).

---

## Performance / threading notes

- **kNN** (the cost of all graph building) runs on a **threaded RcppHNSW**
  (hnswlib) backend — ~8× faster at 8 cores than the old N2R path, with higher
  recall (`benchmark/KNN_BACKEND.md`).
- **Streaming reductions** (variance/HVG, cluster pseudobulk) thread ~linearly
  (`n.cores`); **PCA** is irlba/sparse-matvec-bound and does **not** thread much
  — adding cores barely helps it.
- View kernels and the lstar fused reducers are **bit-reproducible across thread
  counts**; the approximate kNN step is not (inherent to parallel hnswlib).
