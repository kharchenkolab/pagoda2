# Performance Checks

This directory contains local performance scripts that are not part of the
regular testthat suite.

Run the GSM5746259 benchmark from the `pagoda2` repository root after installing
local `sccore` and `pagoda2` sources:

```sh
Rscript tests/perf/benchmark_gsm5746259.R
```

By default the script expects the shared p21 data checkout:

```sh
../tests/data/GSE192391/GSM5746259_MGI0369_1_SLAB-145-0
```

Override the data path or core count with:

```sh
P21_GSM5746259_DIR=/path/to/GSM5746259 P21_BENCH_CORES=8 \
  Rscript tests/perf/benchmark_gsm5746259.R
```

Current Phase 4 observations:

- The canonical GSM5746259 workflow no longer emits the Matrix deprecation
  warning from triplet-to-column sparse coercion.
- The changed 10x reader and dense constructor coercions are covered by tests
  that compare exact matrix values and dimnames.
- Legacy `read.10x.matrices()` still prefixes cell names with the dataset name.
  Keep that behavior until the Phase 5 I/O rewrite, where 10x and other formats
  should get explicit cell-name and sample-name policies.
- `sccore::dotPlotData()` aggregates marker expression by group with sparse
  column sums instead of expanding a per-cell/per-gene long table.
- `plotMarkerHeatmap()` still densifies for plotting, but only after marker gene
  and cell selection. It warns when the selected gene-by-cell matrix exceeds
  `max.dense.entries`.
