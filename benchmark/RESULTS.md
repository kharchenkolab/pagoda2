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
| 1 variance / HVG | full streaming per-gene reduction | **0.27s** / 1319 | 3.97s / **448** | 1.60s / 570 |
| 3 cluster pseudobulk | grouped reduction | **0.29s** / 1327 | 1.81s / 454 | 1.35s / 573 |
| 4 gene-block (20 genes) | random small sub-block | **0.19s** / 1358 | 2.14s / 503 | 1.76s / 471 |
| 2 PCA (materialize + irlba) | gene-subset block + iterative SVD | **12.9s** / 2179 | pathological† | 15.8s‡ / **1004** |

> **Note — these op1/op3 numbers are *after* the optimization pass** (see "What the first numbers were
> measuring", below). The original run had mem op3 **3.51s** (single-threaded), zarr op1 **6.82s** and
> op3 **9.70s** (per-block `dgCMatrix` + a non-OpenMP R build). Four fixes — fork-safe threading of
> pagoda2's `colSumByFacView`; OpenMP in lstar's R build (its kernels were silently serial); bulk file
> reads; and a **fused depth-view streaming reducer** in lstar (`stream_col_stats(depth=)` /
> `lstar_stream_col_sum_by_group`, applying the plain view + reducing in one threaded C++ pass, no
> per-block `dgCMatrix`, no C++→R marshalling) — moved zarr from slowest to **beating BPCells on both
> reductions**, with every fingerprint still exactly matching the in-memory golden.

† BPCells PCA *off the raw h5ad* is pathological: the odgene block is 2662 *gene rows* of a **cell-major**
on-disk matrix, so every irlba/SVD pass rescans — BPCells' native format storing the gene-major
orientation is what makes its `svds` fast (not exercised here). ‡ zarr op2 was **372.9s** with the
first (naive, per-run) gather, which re-decoded chunks once per scattered odgene; the **decode-once
gather** now shipped (`lstar_read_csc_cols`, an ascending chunk sweep) cut the 2662-odgene read from
~370s to **2.5s**, bringing the whole PCA to **15.8s / 1004 MB** — competitive with in-memory and at
2× less RAM. (BPCells op2 stays pathological here because it needs its native gene-major format.)

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

## What the first numbers were measuring (and why they flipped)

The first run made BPCells look like it had a real engineering edge on the reductions. Chasing the
mechanism (`bpcells-vs-zarr`, profiling) showed most of that edge was *our* glue and config, not BPCells:
1. **pagoda2's `colSumByFacView` was single-threaded** (its OpenMP pragma was commented out to avoid a
   fork/`mclapply` deadlock) → mem op3 lost to BPCells. Fix: fork-safe `if(ncores>1)` guarded pragma.
2. **lstar's R build had no OpenMP flags** → every `stream_col_stats`/kernel ran serial regardless of
   `n_threads` (this, not "IO-bound," was the "flat threading"). Fix: `$(SHLIB_OPENMP_*)` in Makevars.
3. **The zarr backend built a `dgCMatrix` per block and marshalled each block C++→R** (~155M f4→f8
   conversions / ~930 MB of R vectors per pass) — *that*, not the read or the kernel, was the cost (the
   kernel alone is 0.4 s). Fix: a **fused depth-view reducer** in lstar (`stream_col_stats(depth=)` /
   `lstar_stream_col_sum_by_group`) — apply the plain view + reduce in one threaded C++ pass, return only
   the small result, no per-block matrix, no marshalling.
4. **`read_bytes` read files byte-by-byte** — fixed to a bulk read (helps cold/large reads).

After those four, every fingerprint still matches the golden, and the reduction picture inverts.

## The answer (after the fixes)

**On the streaming reductions, lstar-zarr now *beats* BPCells** (op1 1.60 vs 3.97 s; op3 1.35 vs 1.81 s)
at comparable RAM (~570 vs ~450 MB), and **in-memory wins outright** once pagoda2's op3 kernel is
threaded (0.27–0.29 s). BPCells' apparent reduction advantage was the fused-streaming *pattern*, not
something intrinsic to BPCells — once lstar gained an equivalent fused reducer, the portable zarr store
matched and passed it.

Where each still leads:
- **Memory** — BPCells remains the leanest (~450 MB), but the gap to zarr (~570 MB) is now small; both
  are ~2–3× under in-memory.
- **Disk** — BPCells bit-packed **145 MB** vs gzip-zarr **157 MB** (~8%). BPCells' delta+zigzag/FOR
  bit-packing is the one place it has a genuine, not-yet-matched structural edge (decode speed + size);
  see `bpcells-vs-zarr` notes on lifting a BP128 codec into zarr.
- **Gene-subset / PCA** — both falter off the "wrong" orientation; zarr's PCA is competitive (15.8 vs
  12.9 s) after the decode-once gather; BPCells needs its native gene-major format here.

**Where lstar-zarr wins structurally:** it's language-agnostic (Python/R/C++/JS read the same store),
and the fused reducers are *general lstar primitives* a consumer drives — pagoda2 added zero per-op C++.

**Bottom line for pagoda2.1:** with the fused reducers, **lstar-zarr is competitive-to-better than
BPCells on the standard operations on a portable store** — there's no throughput reason to take on the
BPCells dependency for these. BPCells' one durable edge is its bit-packed codec (size + decode speed),
which is worth *lifting into zarr as a portable codec* rather than depending on the package for.

## Follow-ups (improve the zarr/lstar side to a fairer fight)
1. ~~**Chunk-grouped gather** in `lstar_read_genes`~~ — **done** (`lstar_read_csc_cols`, decode each
   touched chunk once): zarr PCA 372.9s → 15.8s.
2. ~~**R writer chunking/compression**~~ — **done** (`lstar_write(chunk_elems=, compression="gzip")`):
   the chunked store can now be built from R, no Python writer needed.
3. **Fused view-aware reducer** in lstar (optional built-in: depth + log1p in `stream_col_stats`) to
   avoid building an intermediate `dgCMatrix` per block on op1/op3 — closer to BPCells' fused path.
4. A BPCells **native-format** PCA row (gene-major) for a fair op2 BPCells number.

## Reproduce
`benchmark/prep.R` builds the shared artifacts (raw matrix, recipe, single HVG fit, grouping);
`benchmark/backends.R` defines the three backends; `benchmark/run_one.R` runs one (backend, op) in
isolation; build the zarr stores and BPCells dirs as in this doc. Correctness + warm timing:
`benchmark/gate_time.R`.
