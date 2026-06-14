# Matrix And Metadata Model

This reference explains the pagoda2.1 object model that agents should respect
when reading, processing, plotting, and exporting data.

## R6 Object Access

`Pagoda2` is an R6 class. Use `$` fields and methods:

```r
p2 <- Pagoda2$from("/path/to/sample_directory",
                   format = "10x",
                   reader.args = list(sample.name = "sample_01"),
                   verbose = FALSE)
stopifnot(identical(p2$apiVersion, "2.2"))

p2$runQC(verbose = TRUE)
p2$filterData(verbose = TRUE)
p2$run(steps = c("variance", "pca", "graph", "embedding", "leiden"),
       plots = "none",
       verbose = TRUE)
```

Do not invent S3/S4 wrappers in a recipe unless a conversion method explicitly
returns that class.

For class-level checks before loading data:

```r
stopifnot(identical(pagoda2::Pagoda2$public_fields$apiVersion, "2.2"))
```

External packages should prefer `p2$apiVersion` and public accessors such as
`getRawCounts()` and `getExpressionBlock()` over probing internal fields.

## Matrix Orientation

Inside pagoda2.1, matrices are cell-by-gene:

```r
dim(p2$getRawCounts())        # cells x genes
dim(p2$getExpressionBlock())  # cells x genes
```

Direct reader output differs:

```r
imported <- readCounts("/path/to/sample_directory",
                       format = "10x",
                       return.metadata = TRUE)
dim(imported$counts)          # genes x cells
```

Request gene-by-cell orientation only when a downstream function needs it:

```r
p2$getRawCounts(orientation = "gene_by_cell")
p2$getExpressionBlock(genes = c("CD3D", "LYZ"),
                      orientation = "gene_by_cell")
```

## Raw Counts Versus Normalized Views

Pagoda2.1 avoids storing a full duplicate normalized matrix by default.
Instead it stores:

- canonical sparse raw counts in cell-by-gene orientation
- matrix-view parameters for normalized expression
- QC and analysis masks
- variance-model results needed to scale expression on demand

The normalized view is a *recipe* (`view$model`) applied on the fly, not a
second matrix. Models: `plain` (RNA: depth-normalize → log1p), `clr` (ADT:
centered-log-ratio), `tfidf` (ATAC: TF-IDF). Each modality is a **facet** with
its own raw counts + recipe; the default facet is RNA, and every accessor below
takes a `facet=` argument (`p2$getRawCounts(facet="ADT")`,
`p2$getMatrixView("analysis", facet="ADT")`). See
`references/multimodal_facets.md` for the full facet model.

Preferred accessors:

```r
p2$getRawCounts()
p2$getRawCounts(cells = c("AAACCCAAGAAACACT-1"),
                genes = c("CD3D", "LYZ"))

p2$getExpressionBlock(genes = c("CD3D", "LYZ"))
p2$getExpressionBlock(cells = names(p2$getGrouping())[p2$getGrouping() == "0"],
                      genes = c("CD3D", "IL7R", "LYZ"))

p2$viewColMeanVar()
p2$viewColSumByFac(grouping = "leiden")
```

Avoid materializing full normalized expression for large datasets. Subset
cells and genes first.

Do not rely on `p2$counts`. The legacy full normalized slot was removed to
reduce memory footprint. Use `getRawCounts()` for counts and
`getExpressionBlock()` or matrix-view helpers for normalized expression.

Debug helpers are useful during development but should not become required
ceremony in user recipes:

```r
p2$describeMatrices()
p2$validateMatrices()
```

## Cell And Gene Metadata

`cellMeta` and `geneMeta` can hold flexible metadata. A metadata table may
cover only some cells/genes or include extra rows from a related object.

Set a named vector:

```r
p2$setCellMeta("predicted_type", predicted_type)
p2$setGeneMeta("biotype", gene_biotype)
```

Set a data frame whose row names are cell or gene names:

```r
p2$setCellMeta(data.frame(
  predicted_type = predicted_type,
  row.names = names(predicted_type)
))
```

Retrieve stored metadata without forcing it onto the current axis:

```r
p2$getCellMeta()
p2$getCellMeta(c("sample", "qc_pass"))
p2$getGeneMeta(c("analysis_pass", "feature_type"))
```

`getCellMeta()` and `getGeneMeta()` default to `resolved = FALSE`. This is
intentional; pagoda2 users can carry flexible metadata until a method needs it
aligned to the current matrix.

## Resolved Metadata

Resolve metadata when a plot, marker calculation, export, or strict report
needs one value per current cell or gene:

```r
p2$resolveCellMeta(columns = c("sample", "qc_pass"))
p2$resolveGeneMeta(columns = "analysis_pass")
```

Resolution rules:

- named metadata aligns by cell or gene names
- missing values become `NA` unless `allow.missing = FALSE`
- extra stored rows are ignored during resolution
- unnamed vectors must match the current axis exactly
- duplicate row names should error

Strict resolution:

```r
p2$resolveCellMeta("cell_type", allow.missing = FALSE)
```

Foreign formats such as h5ad require exact `obs` and `var` dimensions. Export
resolves metadata onto the exported axes before writing instead of dropping
partially mapped useful metadata.

## Groupings And `defaultGrouping`

Groupings are discrete cell metadata columns used for clustering labels,
plotting, marker tests, and summaries:

```r
p2$setGrouping("leiden", labels, setDefault = TRUE)
p2$getGrouping()
p2$getGrouping("leiden")
p2$listGroupings()
p2$setDefaultGrouping("cell_type")
```

`defaultGrouping` is a pointer to a `cellMeta` column, not a separate identity
slot. Methods use it when `grouping` is omitted:

```r
p2$plotEmbedding()
p2$runMarkers(name = p2$getDefaultGrouping())
p2$plotMarkerDotPlot()
```

Direct group vectors are allowed for one-off work:

```r
p2$runMarkers(groups = external_labels, name = "external_labels")
p2$plotEmbedding(groups = external_labels)
```

Store important groupings with `setGrouping()` so later plots, markers, and
exports can reuse them.

## Cluster Annotation

Use `annotateClusters()` for cluster-to-label mappings. Build the map from the
current marker evidence, not from a template:

```r
# REPLACE with cluster-to-cell-type assignments derived from marker review.
cluster_to_type <- c(
  # "<cluster_id>" = "<cell type>"
)

if (length(cluster_to_type) > 0) {
  p2$annotateClusters(from = "leiden",
                      to = "cell_type",
                      map = cluster_to_type,
                      unmapped = "keep",
                      setDefault = TRUE,
                      overwrite = TRUE)
}
```

Many-to-one mappings are expected. External annotations that do not correspond
to clusters should be stored directly:

```r
p2$setGrouping("external_annotation", external_labels, setDefault = FALSE)
```

## Factor Colors And Plot Theme

Pagoda2 plot methods use package-level factor color behavior by default and
allow object/call-level ggplot theme overrides.

Set an object-level ggplot theme:

```r
p2$setPlotTheme(themePagoda2(base_size = 11))
```

Override a single plot:

```r
p2$plotPCAElbow(plot.theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")))
```

For factor colors, prefer the defaults unless a project requires a fixed
palette. If a fixed palette is needed, pass named colors to the plotting
method that accepts them, such as marker heatmap `group.colors`.

## Metadata Report

Report:

- raw count matrix dimensions and orientation used
- whether normalized expression was materialized or queried as a subset
- metadata columns added or resolved
- missing values introduced by resolution
- current `defaultGrouping`
- stored groupings available for plots and markers
- any annotation map applied and any unmapped clusters
