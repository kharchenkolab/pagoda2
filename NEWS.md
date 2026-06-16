# pagoda2 2.0.0

Major release: `pagoda2` is rebuilt around an R6 `Pagoda2` class with a
multimodal "facet" design. This is a clean break from the 1.x procedural API.

## Highlights

* **Generic R6 pipeline.** The algorithm is chosen by `method=`: `runReduction()`
  (PCA/LSI/CCA), `runGraph()` (kNN/WNN), `runClustering()` (Leiden),
  `runEmbedding()` (UMAP), `runMarkers()`. `run()` chains the standard sequence.
* **Multimodal facets.** Each modality (RNA, ADT/CITE-seq protein, ATAC) is a
  facet with its own view model (plain / CLR / TF-IDF). Verbs take `facet=` for
  one modality or `facets=` to integrate (e.g. WNN).
* **Faster internals.** Threaded RcppHNSW kNN; centered RSpectra operator for
  PCA/LSI (irlba fallback). Optional disk-backed (lstar/Zarr) facets.
* **Import constructors.** `Pagoda2$from()` (matrix or path), `from10x()`,
  `from10xH5()`, `fromAnnData()` (`.h5ad`), `fromH5Seurat()`, `fromLoom()` and
  `fromLstar()` (lstar/Zarr) read directly via HDF5/zarr (no Python). The file
  readers warn when a source lacks feature names and only positional `1..n`
  indices can be recovered (e.g. an `.h5seurat` written from a Seurat v5 object
  by a SeuratObject&nbsp;<&nbsp;5 SeuratDisk).

## Breaking changes

* The legacy normalized `$counts` slot was removed; use `getRawCounts()` /
  `getExpressionBlock()`.
* Removed procedural entry points `basicP2proc()`, `extendedP2proc()`,
  `tp2c.view.pathways()`, `plotOneWithValues()`. See `?\`pagoda2-deprecated\``
  for the full replacement map.
* Several methods were renamed to the generic verbs (e.g. `adjustVariance` →
  `runVariance`, `makeKnnGraph` → `runGraph`, `getKnnClusters` → `runLeiden`,
  `calculatePcaReduction` → `runReduction`, `getEmbedding` → `runEmbedding`,
  `getDifferentialGenes` → `runMarkers`). The old names still work but warn.
