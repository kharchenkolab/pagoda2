# QC And Filtering

## What `runQC()` Does

`p2$runQC()` records cell-level metrics in `p2$cellMeta`, including:

- `n_molecules`
- `n_genes`
- `qc_gene_molecule_residual`
- `qc_gene_molecule_outlier`
- `qc_size_outlier`
- `qc_pass`

The main QC model evaluates detected genes versus molecules. Cells that are too
small, too large, or outliers from the gene/molecule trend can fail QC.

## MT And Ribo Metrics

Mitochondrial and ribosomal percentages are optional because gene naming varies
by organism and annotation source.

Human-style symbols:

```r
p2$runQC(overwrite = TRUE, mt.pattern = "^MT-", ribo.pattern = "^RP[SL]")
```

Mouse-style symbols:

```r
p2$runQC(overwrite = TRUE, mt.pattern = "^mt-", ribo.pattern = "^Rp[sl]")
```

When symbols are not useful, pass explicit gene vectors:

```r
p2$runQC(overwrite = TRUE, mt.genes = mt_genes, ribo.genes = ribo_genes)
```

If MT/ribo detection fails, do not create a false interpretation. Report that
composition QC could not be assessed from the available gene annotation.

## Figures To Show

Main QC:

```r
p_qc <- p2$plotQC()
```

Composition QC, only when meaningful:

```r
p_comp <- p2$plotQCViolin()
```

## Filtering

Use `filterData()` as the standard pre-analysis gate:

```r
p2$filterData(verbose = TRUE)
```

It can run QC if needed, filters cells by `qc_pass`, computes gene coverage
after cell filtering, and stores the gene analysis mask in:

```r
p2$geneMeta$analysis_pass
```

It should not drop raw genes simply because they are not selected for analysis.
Raw genes remain available through `p2$getRawCounts()`.

## Assessment Rules

Report these points:

- fraction of cells failing QC
- whether failures are low-depth, high-depth, gene/molecule outliers, high MT,
  or another pattern
- whether the failed set looks like a quality tail or a possible biological
  population
- cells removed by filtering
- raw genes retained
- genes passing the analysis mask

If more than roughly 20 percent of cells fail, or the failed cells form a
coherent biological-looking population, pause and discuss threshold changes
before proceeding.
