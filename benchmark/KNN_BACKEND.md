# kNN backend reevaluation: N2R 1.0.5 vs RcppHNSW

**Question:** the nearest-neighbor step is the dominant cost of *every* pagoda2 graph (`makeKnnGraph`,
`makeGeneKnnGraph`, WNN's per-facet kNN). Is the current backend (`N2R::Knn`) pulling its weight?

**Trigger:** profiling WNN at scale showed the kNN, not the R-level graph assembly, is ~88% of runtime,
and that it did **not** speed up with more threads — contradicting the assumption that the bottleneck was
R-side sparse/igraph assembly.

## WNN phase split (20k cells, RNA 20-dim + ADT 10-dim, k=20)

| phase | 1 core | 8 cores |
|----|----|----|
| **kNN (N2R)** | **14.0s** | **14.1s** |
| knn_row_stats | 0.19s | 0.18s |
| incidence + ratio (matmuls) | 0.09s | 0.07s |
| graph assembly (kernel × weight, symmetrize) | 0.32s | 0.28s |
| igraph build | 0.23s | 0.19s |
| **total** | **16.0s** | **15.9s** |

The R-level assembly is ~5% of runtime and threading it would be wasted effort. The kNN is the cost, and
N2R does not thread.

## The N2R 1.0.5 bug: the query is serial (20k × 20-dim, k=20, vs FNN exact ground truth)

| backend | setting | recall@20 | time (8 threads) |
|----|----|----|----|
| **N2R** | ef_mult=50 (pagoda2 default) | 0.950 | 9.9s |
| N2R | ef_mult=200 | 0.950 | 24.0s |
| **RcppHNSW** | M=16, ef=50 | 0.981 | 1.29s |
| **RcppHNSW** | M=16, ef=200 | **1.000** | 1.12s |

The bug is **`nThreads` is a no-op** — flat 1→8 cores. The N2R source confirms it: in `src/n2knn.cpp`
the query loop's `#pragma omp parallel for num_threads(nThreads)` is **commented out**, so every
`SearchById` runs serially (libgomp is linked and the *build* takes `nThreads`, but the query dominates).
Raising `ef_search_multiplier` only makes the serial query slower (24s) for no recall change.

N2R's **recall is fine (~0.95)** — it is *not* a quality problem. (An earlier draft of this note reported
0.357; that was a measurement error: N2R returns the matrix as `(neighbor, query)`, so a query's
neighbors are a **column**, and the bad number came from reading rows — the reverse-neighbors of an
asymmetric kNN. Read correctly, N2R recall is 0.95.)

RcppHNSW (same hnswlib algorithm) reaches equal-or-better recall (0.98 at ef=50, 1.0 at ef=200) at ~8×
the speed **and threads cleanly** — and unlike N2R you can trade `ef` for recall. (Recall measured on L2;
the cosine/angular path behaves the same — exact-cosine == exact-L2 on L2-normalized rows, verified in
`tests/testthat/test_knn_backend.R`.)

### Orientation audit (was N2R read on the wrong axis anywhere?)

N2R returns `(neighbor, query)` (a query's neighbors are a **column**); `.pagoda2_knn_sparse` returns
`(query, neighbor)` (row i = cell i's own neighbors), and its N2R-fallback transposes to match. Audit:

- **`makeKnnGraph`** (the long-standing main path): **not affected** — it symmetrizes `sxn <- (xn +
  t(xn))/2` before building an undirected graph, so the orientation washes out (same edges, same weights).
- **conos** (`R/conos.R` self-graph + `crossKnn`): **not affected** — edges feed an undirected joint
  graph, so orientation washes out; conos also calls N2R with `nThreads=1`, so the threading bug never
  bit it either.
- **`makeGeneKnnGraph`**: builds a directed `from=neighbor,to=query` edge list with no symmetrization at
  that step; downstream gene-network use treats it as undirected, so this is cosmetic (from/to swap).
- **WNN** (pagoda2.1): the WSNN graph is symmetrized (orientation-neutral), but the per-cell modality
  *weights* were computed per-row — i.e. on reverse-neighbors — under the old N2R-direct path, whereas the
  function's own contract/comment and its FNN small-n branch used `(query, neighbor)`. The new backend
  makes all paths `(query, neighbor)`, so WNN now weights each cell by *its own* neighbors (the intended
  Hao-2021 orientation). Effect is small (the WNN validation suite passes either way), but it is now
  correct and consistent across backends.

## The swap: `.pagoda2_knn_sparse` (RcppHNSW preferred, N2R fallback)

A single internal backend (`R/workflow.R`) returns the same sparse `n × n` distance matrix the old code
consumed; it prefers RcppHNSW (threaded, high-recall) and falls back to N2R when RcppHNSW is absent.
Wired into `makeKnnGraph`, `makeGeneKnnGraph`, and the WNN per-facet kNN. `RcppHNSW` added to `Imports`.

### WNN end-to-end after the swap (`runGraph(method="wnn")`, RNA+ADT)

| cells | 1 core | 8 cores | old N2R (8 core) | speedup |
|----|----|----|----|----|
| 8,000 | 3.27s | **0.80s** | ~6.95s | **8.7×** |
| 20,000 | 8.21s | **1.95s** | ~15.9s | **8.2×** |
| 50,000 | 23.47s | **6.30s** | ~flat (no scaling) | — |

1→8 cores now yields ~4× scaling (8k 4.1×, 20k 4.2×, 50k 3.7×), at equal-or-better recall (0.95 → ~1.0)
— much faster graphs across all of pagoda2, not just WNN.

> Note: multi-threaded HNSW index construction is not bit-reproducible across thread counts (inherent to
> parallel hnswlib, and equally true of N2R). kNN is an approximate step; the view kernels' bit-exact
> thread-invariance contract (`benchmark/RESULTS.md`) is unaffected.

## Sister ecosystem: N2R fixed (locally)

Root cause of N2R's non-threading: in `n2`, `HnswSearch` holds a **single shared `visited_list_`**
(`include/n2/hnsw_search_impl.h:103`), so concurrent `SearchById`/`SearchByVector` on one `Hnsw` corrupt
each other — which is why the query-loop pragma in `src/n2knn.cpp` was deliberately commented out
(naively uncommenting it tanks recall: 0.95 → 0.16 at 8 threads). The correct fix uses n2's built-in
`BatchSearchByIds`/`BatchSearchByVectors`, which run a **per-thread searcher pool** (one `visited_list`
each). Patched locally in `~/p21/N2R/src/n2knn.cpp` (gated `nThreads > 1`; serial single-searcher path at
`nThreads <= 1`, fork-safe): recall preserved at **0.95 and identical across thread counts**, ~2.4× at 8
cores. Even threaded, n2 (4.0s) stays ~5× slower than RcppHNSW (0.8s) — so pagoda2 keeps RcppHNSW; the
N2R patch is for the wider ecosystem (conos uses `nThreads=1`, so unaffected regardless). **Not pushed.**

## Reproduce
`/tmp` scratch scripts in the working session, or: build two facets, `runGraph(method="wnn",
n.cores=c(1,8))`, and compare `.pagoda2_knn_sparse` recall to `FNN::get.knnx`. Tests:
`tests/testthat/test_knn_backend.R`.
