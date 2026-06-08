# QC And Filtering

This reference covers `runQC()`, QC plots, optional mitochondrial/ribosomal
metrics, and `filterData()`. The standard pattern is: compute QC, show the
QC figure, then filter and set the gene analysis mask.

## Standard Pattern

```r
p2$runQC(verbose = TRUE)

p_qc <- p2$plotQC()
ggplot2::ggsave("qc_gene_molecule.png", p_qc,
                width = 10, height = 4.5, units = "in", dpi = 120,
                bg = "white")

p2$filterData(verbose = TRUE)
```

`runQC()` writes QC columns into `p2$cellMeta`. `filterData()` removes cells
that fail `qc_pass` and records a gene-level `analysis_pass` mask in
`p2$geneMeta`. Both mutate the R6 object in place.

## Cell QC Metrics

Source-verified call shape:

```r
p2$runQC(method = "gene_molecule",
         min.molecules = 500,
         max.molecules = 50000,
         verbose = TRUE)
```

Common columns after `runQC()`:

- `n_molecules`: total molecules per cell
- `n_genes`: detected genes per cell
- `qc_gene_molecule_residual`: residual from the gene/molecule trend
- `qc_gene_molecule_outlier`: outlier flag for the trend
- `qc_size_outlier`: low-depth or high-depth flag
- `qc_pass`: final pass/fail decision
- `percent_mito`: mitochondrial percentage when matching genes exist
- `percent_ribo`: ribosomal percentage when matching genes exist

Report aligned metrics with `resolveCellMeta()`:

```r
qc <- p2$resolveCellMeta(c("n_molecules", "n_genes", "qc_pass"))
cat(sprintf("%d of %d cells pass QC\n",
            sum(as.logical(qc$qc_pass), na.rm = TRUE), nrow(qc)))
cat(sprintf("Median molecules %.0f; median genes %.0f\n",
            median(qc$n_molecules, na.rm = TRUE),
            median(qc$n_genes, na.rm = TRUE)))
```

Use metrics-only mode when the task needs molecule/detected-gene counts but
not the gene-versus-molecule outlier model:

```r
p2$runQC(method = "metrics", verbose = TRUE)
```

## MT And Ribosomal Metrics

MT/ribo metrics are optional. Let pagoda2 try common prefixes first:

```r
p2$runQC(infer.qc.genes = TRUE, verbose = TRUE)
```

Use explicit patterns when organism and naming style are known:

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

Use explicit gene sets when symbols are nonstandard:

```r
p2$runQC(overwrite = TRUE,
         mt.genes = c("MT-ND1", "MT-ND2", "MT-CO1"),
         ribo.genes = c("RPL3", "RPL4", "RPS3"),
         verbose = TRUE)
```

Do not invent an MT/ribo interpretation if matching genes are absent. Report
that the metric could not be assessed from the available gene names.

## QC Figures

Gene/molecule QC:

```r
p_qc <- p2$plotQC()
ggplot2::ggsave("qc_gene_molecule.png", p_qc,
                width = 10, height = 4.5, units = "in", dpi = 120,
                bg = "white")
```

`plotQC()` can run missing QC automatically, but agent workflows should call
`runQC()` first so the pass/fail counts can be reported before plotting.

Composition violin plot:

```r
composition_metrics <- intersect(c("percent_ribo", "percent_mito"),
                                 colnames(p2$cellMeta))
if (length(composition_metrics) > 0) {
  p_comp <- p2$plotQCViolin(metrics = composition_metrics)
  ggplot2::ggsave("qc_composition_violin.png", p_comp,
                  width = 7.5, height = 4.5, units = "in", dpi = 120,
                  bg = "white")
}
```

Visual threshold lines are optional guides. Draw them only when the user has
specified thresholds or a project standard exists:

```r
p_comp <- p2$plotQCViolin(metrics = c("percent_mito"),
                          thresholds = c(percent_mito = 20))
```

Do not treat a visual threshold as a filtering rule unless the analysis
explicitly applies that rule.

## Cell And Gene Filtering

Default preparation:

```r
p2$filterData(cells = TRUE,
              genes = TRUE,
              pass.column = "qc_pass",
              min.cells.per.gene = 5,
              min.molecules.per.gene = 0,
              verbose = TRUE)
```

If `qc_pass` is missing, `filterData()` can call `runQC()` through `...`:

```r
p2$filterData(min.molecules = 1000,
              max.molecules = 60000,
              verbose = TRUE)
```

Cell-only filtering:

```r
p2$filterCells(pass.column = "qc_pass", verbose = TRUE)
```

Custom cell subset:

```r
p2$filterCells(cells = c("AAACCCAAGAAACACT-1", "AAACCCAAGAAACCAT-1"),
               force = TRUE,
               verbose = TRUE)
```

Use `force = TRUE` only when the user knowingly invalidates downstream PCA,
graphs, embeddings, clusters, or markers. After forced filtering, rerun
downstream steps.

## Analysis-Gene Mask

`filterData()` keeps raw counts but records which genes should be used for
analysis:

```r
p2$filterData(min.cells.per.gene = 5,
              min.molecules.per.gene = 0,
              verbose = TRUE)

gene_qc <- p2$resolveGeneMeta(c("n_cells_detected", "n_molecules",
                                "analysis_pass"))
cat(sprintf("%d genes pass the analysis mask\n",
            sum(gene_qc$analysis_pass, na.rm = TRUE)))
```

Keep a curated panel eligible regardless of coverage:

```r
p2$filterData(keep.genes = c("CD3D", "MS4A1", "LYZ"),
              verbose = TRUE)
```

Downstream PCA uses analysis genes by default. Raw genes remain available for
explicit queries and export.

## Interaction With `p2$run()`

The default workflow includes both QC and filtering:

```r
p2$run(plots = "none", verbose = TRUE)
```

To run QC but not filter immediately:

```r
p2$run(steps = "qc", plots = "none", verbose = TRUE)
```

If QC finds failed cells and filtering is not part of the requested workflow,
`p2$run()` warns so the agent can show `p2$plotQC()` and ask whether filtering
is intended.

Step-specific QC/filter overrides go in the matching list:

```r
p2$run(
  plots = "none",
  verbose = TRUE,
  qc = list(min.molecules = 1000, max.molecules = 60000),
  filter = list(min.cells.per.gene = 10)
)
```

## QC Report

Report:

- total cells before filtering
- cells passing and failing `qc_pass`
- median molecules and detected genes
- whether MT/ribo metrics were available
- any threshold overrides used
- cells retained after filtering
- genes passing `analysis_pass`
- any forced filtering that invalidated downstream results
