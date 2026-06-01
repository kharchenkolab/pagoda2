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

To exercise the view-backed workflow after construction, run:

```sh
P21_BENCH_DROP_COUNTS=true Rscript tests/perf/benchmark_gsm5746259.R
```

Current raw/view observations:

- `p2$rawCounts` is the canonical filtered raw count matrix.
- `p2$matrixViews$analysis` records the normalized analysis recipe.
- `p2$counts` is still populated as a legacy normalized matrix, but the
  canonical workflow-facing path is tested with `p2$counts <- NULL`.
- h5ad export writes normalized analysis values to `X` and raw counts to
  `layers/counts`.
- `sccore::dotPlotData()` aggregates marker expression by group with sparse
  column sums instead of expanding a per-cell/per-gene long table.
- `plotMarkerHeatmap()` still densifies for plotting, but only after marker gene
  and cell selection. It warns when the selected gene-by-cell matrix exceeds
  `max.dense.entries`.
