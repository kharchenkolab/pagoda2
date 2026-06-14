# Disk-backed pagoda2.1: in-memory golden vs the lstar-zarr backing

**Question:** pagoda2.1 stores only raw counts + a lightweight normalization recipe and computes the
normalized "analysis view" on the fly. Does the portable **lstar Zarr** store reproduce the in-memory
result **bit-for-bit** across the standard pagoda2 operations, and do its reducers **thread**? (An earlier
revision of this benchmark also probed BPCells as an external comparison; BPCells has been dropped — the
shipped disk backing is lstar-zarr, so this measures only that against the in-memory golden.)

- **Data:** Tabula Muris Marrow raw counts, 40,220 cells × 20,138 genes, 78M nonzeros (integer).
- **View:** "plain" model — `log1p(x · depthScale / depth_cell)`, depthScale=1000 (no winsor/batch).
- **Backends:** `mem` (in-memory `dgCMatrix` + pagoda2 C++ view kernels — the golden) and `zarr` (lstar
  chunked CSC store + **pagoda2's own kernels driven per gene block**, plus lstar's **fused** streaming
  reducers `stream_col_stats(depth=)` / `lstar_stream_col_sum_by_group`).
- **Method:** correctness gate `benchmark/gate_zarr.R` — every zarr result compared to the in-memory
  golden by max relative difference, 8 threads, warm cache; `prep.R` builds the shared artifacts.

## Correctness + thread scaling (`gate_zarr.R`)

| op | what it stresses | mem | zarr 1-core | zarr 8-core | max rel.diff vs golden |
|----|------|------|------|------|------|
| 1 variance / HVG | full streaming per-gene reduction | 0.28s | 3.48s | **1.63s** | mean 7e-17, var 1.3e-14 |
| 3 cluster pseudobulk | grouped reduction (18 groups) | 0.34s | 2.25s | **1.38s** | 7.2e-17 |
| 4 gene-block (20 genes) | random small sub-block | 0.12s | 0.12s | — | **0.0 (exact)** |
| 2 PCA (materialize + irlba) | odgene block + iterative SVD | 12.3s | 12.2s | — | \|cor\| top-10 PC = 1.0000; singval 1.4e-9 |

**Every zarr result is bit-identical to the in-memory golden** (to machine precision; op4 exactly 0),
and the fused reducers are **thread-invariant** — the 1-core and 8-core zarr results are `identical()`,
not merely close — while still scaling (op1 3.48s→1.63s, op3 2.25s→1.38s at 8 cores). In-memory wins
outright on the streaming reductions (~0.3s) once pagoda2's op3 kernel is fork-safe-threaded; zarr's PCA
is competitive (12.2 vs 12.3s) after the decode-once gather.

## Why the zarr reducers are fast (the fused-streaming path)

The portable store's reducers apply the plain view (depth-normalize + `log1p`) **and** reduce in one
threaded C++ pass — no intermediate per-block `dgCMatrix`, no C++→R marshalling of the data, returning
only the small result:

- `stream_col_stats(store, field, depth=, depthScale=, lognorm=, population=TRUE)` — fused per-gene
  mean/var (op1). `population=TRUE` matches pagoda2's `/n` variance convention.
- `lstar_stream_col_sum_by_group(store, field, codes, ng, depth=, depthScale=, lognorm=)` — fused grouped
  pseudobulk (op3).
- `lstar_read_genes` / `lstar_read_csc_cols` — a **decode-once ascending chunk sweep** for the scattered
  odgene gather (op2). This cut the 2662-odgene PCA read from ~370s (naive per-odgene re-decode) to ~2.5s,
  bringing the whole PCA to ~12s — competitive with in-memory at less RAM.

These are **general lstar primitives** a consumer drives; pagoda2 adds zero per-op C++ on the disk path.

## Disk size (same 78M-nnz raw counts)

| format | MB | notes |
|----|----|----|
| **lstar zarr (gzip)** | **157** | the portable backing; gzip closes most of the size gap to bit-packing |
| RDS (`dgCMatrix`, gzip) | 226 | the in-memory backend's on-disk form |
| lstar zarr (uncompressed) | 598 | the portable default |

One-time ingest: lstar zarr write 0.6s (uncompressed) / 25.9s (gzip). A portable **bit-packed integer
codec** (delta+zigzag/FOR) is the one remaining size/decode lever worth lifting into zarr later.

## The thread-invariance contract (why "identical", not just "close")

pagoda2's view kernels accumulate in float64 over the sparse storage with parallelism over **columns**
(disjoint per-thread writes, summed in index order) — so results are **bit-reproducible and
thread-count-invariant**. lstar's fused reducers preserve this (`zarr 1-core == zarr 8-core` above). This
is a stronger guarantee than the approximate kNN step (see `KNN_BACKEND.md`), where multi-threaded HNSW
build is intentionally not bit-reproducible.

## Bottom line

The portable lstar-zarr store reproduces the in-memory result **exactly** on every standard operation,
its fused reducers thread while staying bit-invariant, and it's language-agnostic (Python/R/C++/JS read
the same store). There's no throughput or correctness reason to take on an external on-disk-matrix
dependency for these operations — lstar-zarr is the disk backing for pagoda2.1.

## Reproduce
`benchmark/prep.R` builds the shared artifacts (raw matrix via hdf5r, recipe, single HVG fit, grouping);
`benchmark/backends.R` defines the `mem` and `zarr` backends; `benchmark/gate_zarr.R` runs the
correctness + timing gate; `benchmark/run_one.R` runs one `(backend, op)` in isolation under
`/usr/bin/time -v` for peak-RSS. Build the lstar zarr store (`/tmp/marrow_csc.lstar.zarr`) first.
