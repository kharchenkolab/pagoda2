# Pagoda2.1 Agent Guide

This repository is being refactored toward pagoda2.1. Work should preserve
pagoda2's sparse-matrix performance while making the single-dataset workflow
cleaner and more deterministic.

## Local Repository Layout

The expected p21 checkout has sibling repositories:

- `pagoda2`
- `sccore`
- `conos`

Use the current source branches when testing changes. Install local sources
after changes that affect package code:

```sh
R CMD INSTALL ../sccore
R CMD INSTALL .
```

Install `conos` only when the change touches multi-dataset integration.

## Development Loop

Use an OODA loop for each phase:

1. Observe current tests, workflow behavior, and performance-sensitive code.
2. Orient against the API and matrix-storage plan.
3. Decide whether the plan needs revision.
4. Act with the smallest coherent implementation slice, then test and commit.

Keep commits short and unsigned.

## Tests

Run focused tests first, then the full suite:

```sh
Rscript -e 'library(testthat); testthat::test_file("tests/testthat/test_matrix_views.R", reporter="summary")'
Rscript -e 'library(testthat); testthat::test_file("tests/testthat/test_io.R", reporter="summary")'
Rscript -e 'library(testthat); testthat::test_local(reporter="summary")'
```

The test suite may create `tests/testthat/Rplots.pdf`; remove it before
committing.

## Performance Rules

- Keep raw counts sparse and integer-like.
- Do not densify unless the user-facing output requires a dense matrix, such as
  heatmap rendering.
- Prefer `getExpressionBlock()` for selected materialization.
- Prefer `viewColMeanVar()` and `viewColSumByFac()` for streaming summaries.
- Avoid storing full duplicate raw and normalized matrices in new code paths.
- If a method still needs normalized expression, route it through a matrix view.

Run the GSM5746259 benchmark when changing core workflow or plotting
performance:

```sh
Rscript tests/perf/benchmark_gsm5746259.R
```

## Matrix Semantics

- `p2$rawCounts`: canonical filtered raw count matrix, cell by gene.
- `p2$matrixViews$analysis`: normalized analysis view recipe.
- `p2$counts`: removed legacy normalized matrix slot; use
  `p2$getExpressionBlock()` for normalized expression.
- h5ad export writes normalized analysis values to `X` and raw counts to
  `layers/counts`.

When adding import/export paths, validate orientation with round-trip tests and
resolve pagoda2's flexible metadata onto exact foreign-format axes at the
boundary.
