# QC And Filtering

Use QC to decide which cells should enter downstream analysis and which genes
should be eligible for variance modeling. Pagoda2 keeps raw genes available;
gene filtering creates an analysis mask rather than deleting every low-coverage
gene from the raw matrix.

## Recommended Pattern

For most single-sample datasets:

```r
p2$runQC(verbose = TRUE)
p_qc <- p2$plotQC()
p2$filterData(verbose = TRUE)
```

This creates cell metrics, shows the gene-versus-molecule decision, removes
cells failing `qc_pass`, and records a gene `analysis_pass` mask.

If the user calls `filterCells()` or `filterData()` before `runQC()`, those
methods can run QC automatically:

```r
p2$filterData(verbose = TRUE)
```

Use this shorthand for routine work, but still show `plotQC()` before or
immediately after filtering so the user sees what happened.

## What `runQC()` Records

`p2$runQC()` records cell-level metrics in `p2$cellMeta`:

```r
p2$runQC(verbose = TRUE)
p2$getCellMeta(c("n_molecules", "n_genes", "qc_pass"))
```

Typical columns include:

- `n_molecules`: total molecules per cell
- `n_genes`: detected genes per cell
- `qc_gene_molecule_residual`: residual from the gene/molecule trend
- `qc_gene_molecule_outlier`: trend outlier flag
- `qc_size_outlier`: low-depth or high-depth flag
- `qc_pass`: final QC pass/fail flag
- `percent_mito` and `percent_ribo`, when matching genes are found

Use `method = "metrics"` only when you want counts and detected-gene metrics
without the full gene/molecule filter:

```r
p2$runQC(method = "metrics", verbose = TRUE)
```

## QC Defaults And Overrides

The default molecule thresholds are intentionally reasonable rather than
requiring users to specify them for every dataset:

```r
p2$runQC(min.molecules = 500, max.molecules = 5e4)
```

Constructor legacy filter arguments are treated as defaults and can be
overridden at QC/filter time. If a dataset needs a different lower cutoff, set
it where QC is run:

```r
p2$runQC(min.molecules = 1000, verbose = TRUE)
p2$filterData(verbose = TRUE)
```

or in one step:

```r
p2$filterData(min.molecules = 1000, verbose = TRUE)
```

## MT And Ribo Metrics

Mitochondrial and ribosomal percentages are optional because gene naming varies
by organism and annotation source. Pagoda2 can infer common human and mouse
patterns, but agents should report whether the match worked.

Human-style symbols:

```r
p2$runQC(overwrite = TRUE, mt.pattern = "^MT-", ribo.pattern = "^RP[SL]")
```

Mouse-style symbols:

```r
p2$runQC(overwrite = TRUE, mt.pattern = "^mt-", ribo.pattern = "^Rp[sl]")
```

Explicit gene sets are safest when annotations are nonstandard:

```r
p2$runQC(overwrite = TRUE, mt.genes = mt_genes, ribo.genes = ribo_genes)
```

If MT/ribo detection fails, do not invent a composition interpretation. Report
that those metrics could not be assessed from the available gene annotation.

## QC Figures

Main gene/molecule QC:

```r
p_qc <- p2$plotQC()
ggplot2::ggsave("qc_gene_molecule.png", p_qc,
                width = 10, height = 4.5, units = "in", dpi = 120,
                bg = "white")
```

`plotQC()` includes the molecule histogram panel, the gene/molecule panel, and
the threshold/fit overlays when available.

Composition violin plot, only when metrics exist:

```r
p_comp <- p2$plotQCViolin(
  metrics = c("percent_ribo", "percent_mito"),
  thresholds = c(percent_mito = 20)
)
ggplot2::ggsave("qc_composition_violin.png", p_comp,
                width = 7, height = 4.5, units = "in", dpi = 120,
                bg = "white")
```

Thresholds shown on violin plots are visual guides unless the analysis code
explicitly uses them for filtering.

## Cell Filtering

Use `filterData()` as the standard pre-analysis gate:

```r
p2$filterData(verbose = TRUE)
```

Use `filterCells()` for a cell-only operation:

```r
p2$filterCells(verbose = TRUE)
```

Use explicit cells only when the user has decided on a custom selection:

```r
keep_cells <- rownames(p2$getRawCounts())[p2$getRawCounts()[, "MALAT1"] > 0]
p2$filterCells(cells = keep_cells, force = TRUE)
```

Filtering after reductions, graphs, embeddings, clusterings, or markers would
invalidate downstream results. Pagoda2 errors unless `force = TRUE`; when using
`force = TRUE`, rerun downstream steps.

## Gene Analysis Mask

`filterData()` computes gene coverage after cell filtering and stores a gene
mask:

```r
p2$filterData(min.cells.per.gene = 5, verbose = TRUE)
gene_qc <- p2$resolveGeneMeta(c("n_cells_detected", "n_molecules", "analysis_pass"))
sum(gene_qc$analysis_pass)
```

Raw genes remain in the raw count matrix. Downstream analysis methods use the
`analysis_pass` mask and overdispersed genes to avoid letting very sparse genes
drive PCA.

Use explicit genes when a curated panel must be retained:

```r
p2$filterData(keep.genes = c("CD3D", "MS4A1", "LYZ"), verbose = TRUE)
```

## Reporting Checklist

Report these points to the user:

- initial cell and gene counts
- fraction of cells failing QC
- whether failures are low-depth, high-depth, gene/molecule outliers, high MT,
  high ribosomal content, or another pattern
- whether failed cells look like a quality tail or a possible biological
  population
- cells removed by filtering
- raw genes retained
- genes passing the analysis mask

If more than roughly 20 percent of cells fail, or the failed cells form a
coherent biological-looking population, pause and discuss thresholds before
continuing.
