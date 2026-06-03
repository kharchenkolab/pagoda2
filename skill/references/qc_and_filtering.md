# QC And Filtering

This reference covers cell QC, gene eligibility, and how the standard
`filterData()` gate prepares a single dataset for variance modeling and PCA.

## Recommended Pattern

For routine single-sample analysis:

```r
invisible(p2$runQC(verbose = TRUE))
p_qc <- p2$plotQC()
ggplot2::ggsave("qc_gene_molecule.png", p_qc,
                width = 10, height = 4.5, units = "in", dpi = 120,
                bg = "white")
p2$filterData(verbose = TRUE)
```

`runQC()` records metrics and a `qc_pass` flag. `plotQC()` shows the
gene-versus-molecule decision. `filterData()` removes cells failing QC and
records a gene-level `analysis_pass` mask.

Keep `runQC(verbose = TRUE)` for agent-facing analyses because it prints a
succinct QC summary without flooding the session with per-cell output. Use
`verbose = FALSE` in notebooks when the next cell prints a clean summary table.

If the user skips `runQC()` and calls `filterData()`, pagoda2 can run QC as a
dependency:

```r
p2$filterData(verbose = TRUE)
```

Still save or show `plotQC()` before interpreting the downstream analysis.

## Cell Metrics Created By `runQC()`

Standard call:

```r
p2$runQC(method = "gene_molecule", verbose = TRUE)
```

Common cell metadata columns:

- `n_molecules`: total molecules per cell
- `n_genes`: detected genes per cell
- `qc_gene_molecule_residual`: residual from the gene/molecule trend
- `qc_gene_molecule_outlier`: gene/molecule trend outlier flag
- `qc_size_outlier`: low-depth or high-depth flag
- `qc_pass`: final QC pass/fail flag
- `percent_mito`: mitochondrial percentage, when matching genes are found
- `percent_ribo`: ribosomal percentage, when matching genes are found

Inspect aligned metrics with `resolveCellMeta()`:

```r
qc <- p2$resolveCellMeta(c("n_molecules", "n_genes", "qc_pass"))
cat(sprintf("%d of %d cells pass QC\n",
            sum(as.logical(qc$qc_pass), na.rm = TRUE), nrow(qc)))
```

Use metrics-only mode when the task needs counts/detected genes without the
gene-versus-molecule outlier model:

```r
p2$runQC(method = "metrics", verbose = TRUE)
```

## Thresholds And Defaults

Defaults are chosen so most users do not need to specify them. Override only
when the QC plot or the biological context argues for it:

```r
p2$runQC(min.molecules = 500, max.molecules = 50000, verbose = TRUE)
```

If a constructor captured legacy filter defaults, those become defaults for
the first QC/filtering decision. Explicit values in `runQC()` or
`filterData()` take precedence:

```r
p2$runQC(min.molecules = 1000, max.molecules = 60000, verbose = TRUE)
p2$filterData(verbose = TRUE)
```

Equivalent one-step gate:

```r
p2$filterData(min.molecules = 1000, max.molecules = 60000, verbose = TRUE)
```

## Mitochondrial And Ribosomal Metrics

MT/ribo metrics are optional because gene naming differs by organism and
annotation. Let pagoda2 infer common names first:

```r
p2$runQC(infer.qc.genes = TRUE, verbose = TRUE)
```

Use explicit patterns when species and gene-name style are known:

```r
p2$runQC(overwrite = TRUE,
         mt.pattern = "^MT-",
         ribo.pattern = "^RP[SL]",
         verbose = TRUE)
```

Mouse-style symbols:

```r
p2$runQC(overwrite = TRUE,
         mt.pattern = "^mt-",
         ribo.pattern = "^Rp[sl]",
         verbose = TRUE)
```

Use explicit gene sets when annotation is nonstandard:

```r
p2$runQC(overwrite = TRUE,
         mt.genes = c("MT-ND1", "MT-ND2", "MT-CO1"),
         ribo.genes = c("RPL3", "RPL4", "RPS3"),
         verbose = TRUE)
```

Do not invent MT/ribo interpretation when matching genes are absent. Report
that the metric could not be assessed from the available gene names.

## QC Figures

Gene/molecule QC:

```r
p_qc <- p2$plotQC()
ggplot2::ggsave("qc_gene_molecule.png", p_qc,
                width = 10, height = 4.5, units = "in", dpi = 120,
                bg = "white")
```

`plotQC()` can calculate missing QC metrics automatically, but standard
workflows should call `runQC()` first so the agent can report pass/fail counts
before plotting.

Composition violin plot, only when those columns exist:

```r
composition_metrics <- intersect(c("percent_ribo", "percent_mito"),
                                 colnames(p2$getCellMeta()))
if (length(composition_metrics) > 0) {
  p_comp <- p2$plotQCViolin(metrics = composition_metrics,
                            thresholds = c(percent_mito = 20))
  ggplot2::ggsave("qc_composition_violin.png", p_comp,
                  width = 7.5, height = 4.5, units = "in", dpi = 120,
                  bg = "white")
}
```

Thresholds drawn on violin plots are visual guides unless the analysis
explicitly uses those values in filtering.

## Cell Filtering

Standard gate:

```r
p2$filterData(cells = TRUE,
              genes = TRUE,
              pass.column = "qc_pass",
              min.cells.per.gene = 5,
              verbose = TRUE)
```

Cell-only gate:

```r
p2$filterCells(pass.column = "qc_pass", verbose = TRUE)
```

Custom cell list:

```r
p2$filterCells(cells = c("AAACCCAAGAAACACT-1", "AAACCCAAGAAACCAT-1"),
               force = TRUE,
               verbose = TRUE)
```

Use `force = TRUE` only when the user knowingly invalidates existing PCA,
graphs, embeddings, clusterings, or markers. After forced filtering, rerun all
downstream steps.

## Gene Analysis Mask

`filterData()` does not delete every low-coverage gene from the raw count
matrix. It records which genes are eligible for analysis:

```r
p2$filterData(min.cells.per.gene = 5,
              min.molecules.per.gene = 0,
              verbose = TRUE)
gene_qc <- p2$resolveGeneMeta(c("n_cells_detected", "n_molecules",
                                "analysis_pass"))
cat(sprintf("%d genes pass the analysis mask\n",
            sum(gene_qc$analysis_pass, na.rm = TRUE)))
```

Keep a curated panel eligible even if coverage is low:

```r
p2$filterData(keep.genes = c("CD3D", "MS4A1", "LYZ"), verbose = TRUE)
```

Downstream PCA and marker code should use analysis genes by default. Raw genes
remain retrievable for targeted expression checks and export.

## QC Interpretation

Report:

- starting cell and gene counts
- cells passing and failing QC
- low-depth, high-depth, and gene/molecule outlier patterns
- whether MT/ribo metrics were detected and whether they look extreme
- whether failures look like a quality tail or a coherent biological group
- cells retained after filtering
- raw genes retained
- genes passing `analysis_pass`

Pause before downstream analysis if more than roughly 20 percent of cells fail,
if high-MT/high-ribo cells form a coherent cluster-like population, or if the
QC thresholds are visibly cutting through the center of the distribution.
