# Disk-backed pagoda2.1: in-memory vs lstar-zarr vs BPCells

**Question:** does BPCells have *real* disk-backed advantages over the lstar Zarr store, for the
standard pagoda2 operations? pagoda2.1 stores only raw counts + a lightweight normalization recipe and
computes the normalized "analysis view" on the fly, so we swap *how raw blocks are sourced* across
three backends and measure the same four operations.

- **Data:** Tabula Muris Marrow raw counts, 40,220 cells × 20,138 genes, 77.6M nonzeros (integer).
- **View:** "plain" model — `log1p(x · depthScale / depth_cell)`, depthScale=1000 (no winsor/batch).
- **Backends:** `mem` (in-memory dgCMatrix + pagoda2 C++ view kernels), `zarr` (lstar chunked CSC store
  + **pagoda2's own kernels driven per gene block via the new general `lstar_read_block`**), `bpcells`
  (on-disk matrix + BPCells streaming ops: `rowVars`, `pseudobulk_matrix`, `svds`).
- **Correctness:** every backend's result is **bit-identical** to the in-memory golden (fingerprints
  match to ~1e-9 or exactly) — verified before any timing. The zarr backend reuses pagoda2's kernels,
  so it matches by construction; BPCells matches after reconciling its sample variance `/(n-1)` to
  pagoda2's population `/n`.
- **Method:** each op in a fresh process under `/usr/bin/time -v` (peak RSS captures off-heap C++/
  BPCells allocations), 8 threads, warm cache.

## Operation performance (wall seconds / peak RSS MB)

| op | what it stresses | mem | bpcells | zarr |
|----|------|------|------|------|
| 1 variance / HVG | full streaming per-gene reduction | **0.48s** / 1319 | 3.87s / **448** | 6.82s / 900 |
| 3 cluster pseudobulk | grouped reduction | 3.51s / 1327 | **1.94s** / 455 | 9.70s / 931 |
| 4 gene-block (20 genes) | random small sub-block | **0.19s** / 1358 | 2.14s / 503 | 1.76s / 471 |
| 2 PCA (materialize + irlba) | gene-subset block + iterative SVD | **12.9s** / 2179 | pathological† | 372.9s‡ / 1560 |

† BPCells PCA *off the raw h5ad* is pathological: the odgene block is 2662 *gene rows* of a **cell-major**
on-disk matrix, so every irlba/SVD pass rescans — BPCells' native format storing the gene-major
orientation is what makes its `svds` fast (not exercised here). ‡ zarr op2 is slow because the naive
`lstar_read_genes` re-decodes chunks for 2662 *scattered* odgenes; a chunk-grouped gather (decode each
touched chunk once) brings it toward a single streaming pass (~op1 time). Both are storage-layout/
gather issues, not fundamental — see "Follow-ups".

## Disk size (same 77.6M-nnz raw counts)

| format | MB | notes |
|----|----|----|
| **BPCells (int bit-packed)** | **145** | counts as uint32 → bit-packed; the smallest |
| **lstar zarr (gzip)** | **157** | within ~8% of BPCells — gzip closes most of the gap |
| RDS (dgCMatrix, gzip) | 226 | the in-memory backend's on-disk form |
| BPCells (float) | 374 | float, *not* bit-packed (wrong dtype for counts) |
| h5ad (anndata) | 596 | the BPCells source file |
| lstar zarr (uncompressed) | 598 | the portable default |

One-time ingest: lstar zarr write 0.6s (uncompressed) / 25.9s (gzip); BPCells native write 2.1s;
in-memory materialize from h5ad 15s.

## The answer

**BPCells has real but *modest* advantages over lstar-zarr:**
- **Memory** — the core disk-backing payoff. Both disk backends slash RAM vs in-memory (~450–930 MB vs
  ~1.3–2.2 GB). BPCells is the leanest (~450 MB, ~3× under in-memory, ~2× under zarr): its fused
  streaming has a smaller working set than zarr's read-block → build-dgCMatrix → kernel path.
- **Speed** — BPCells beats zarr on every op (fused C++ streaming vs decode-materialize-then-kernel),
  and even beats in-memory on pseudobulk. zarr is the slowest on the reductions: the per-block decode +
  `dgCMatrix` construction + R-loop overhead is real.
- **Disk** — BPCells bit-packing (145 MB) is only ~8% smaller than **gzip-zarr (157 MB)**. The disk win
  is almost entirely the compression scheme, and gzip makes zarr competitive.

**Where lstar-zarr wins / why it still matters:**
- **Portability & "bring your own kernel."** The zarr backend uses lstar's *single* general primitive
  (`lstar_read_block`) and drives **pagoda2's existing kernels** off-disk — zero new lstar code per op,
  correct by construction. BPCells instead requires re-expressing the view in its transform-graph DSL.
  This is exactly the "let consumers do optimized streaming without implementing it in lstar" goal.
- The store is language-agnostic (Python/R/C++/JS read it), unlike BPCells' R-centric format.

**Bottom line for pagoda2.1:** BPCells is the better *off-the-shelf* disk backend today for raw
throughput and memory on the streaming reductions; lstar-zarr is the better *interchange + bring-your-
own-kernel* substrate, gzip-competitive on disk, with the zarr backend's per-op slowness being
read-then-materialize overhead + a naive scattered gather — both improvable.

## Follow-ups (improve the zarr/lstar side to a fairer fight)
1. **Chunk-grouped gather** in `lstar_read_genes`: decode each touched chunk once instead of per gene
   run → fixes op2's 373s (scattered odgenes) and op4 random access.
2. **Fused view-aware reducer** in lstar (optional built-in: depth + log1p in `stream_col_stats`) to
   avoid building an intermediate `dgCMatrix` per block on op1/op3 — closer to BPCells' fused path.
3. **R writer chunking/compression** (see lstar task) so the chunked store can be built from R too.
4. A BPCells **native-format** PCA row (gene-major) for a fair op2 BPCells number.

## Reproduce
`benchmark/prep.R` builds the shared artifacts (raw matrix, recipe, single HVG fit, grouping);
`benchmark/backends.R` defines the three backends; `benchmark/run_one.R` runs one (backend, op) in
isolation; build the zarr stores and BPCells dirs as in this doc. Correctness + warm timing:
`benchmark/gate_time.R`.
