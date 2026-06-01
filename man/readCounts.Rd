\name{readCounts}
\alias{readCounts}
\title{Read Count Matrices}
\usage{
readCounts(
  path,
  format = c("auto", "10x", "10x_h5", "h5ad", "h5seurat", "loom"),
  version = c("auto", "V3", "V2"),
  gene.id = c("symbol", "id"),
  feature.type = NULL,
  genome = NULL,
  assay = NULL,
  layer = NULL,
  use.raw = FALSE,
  make.unique.genes = FALSE,
  cell.prefix = NULL,
  sample.name = NULL,
  sample.pattern = NULL,
  validate.integer = TRUE,
  return.metadata = FALSE,
  chunk.size = 1000L,
  verbose = TRUE
)
}
\arguments{
\item{path}{Directory containing a 10x triplet, or a supported count file.}

\item{format}{Input format. \code{auto} detects 10x triplet directories, h5ad,
h5Seurat, CellRanger HDF5, and loom files.}

\item{version}{10x feature file version: \code{auto}, \code{V3}, or \code{V2}.}

\item{gene.id}{Which feature column to use as matrix row names: \code{symbol} or \code{id}.}

\item{feature.type}{Optional 10x V3 feature type to retain.}

\item{genome}{Optional CellRanger HDF5 genome group.}

\item{assay}{Optional h5Seurat assay.}

\item{layer}{Optional h5ad, h5Seurat, or loom layer.}

\item{use.raw}{Whether h5ad input should read raw/X when no layer is supplied.}

\item{chunk.size}{Row chunk size when reading dense HDF5 matrices such as loom.}

\item{make.unique.genes}{Whether to make duplicated selected gene names unique.}

\item{cell.prefix}{Optional string to prefix to cell barcodes.}

\item{sample.name}{Optional sample name recorded in cell metadata.}

\item{sample.pattern}{Optional regex used to select one triplet from a directory with several renamed triplets.}

\item{validate.integer}{Whether to reject non-integer count values.}

\item{return.metadata}{Whether to return a list with counts, cellMeta, geneMeta, and files.}

\item{verbose}{Whether to report detected files.}
}
\value{
A gene-by-cell sparse count matrix, or a list when \code{return.metadata} is \code{TRUE}.
}
\description{
Read count matrices with explicit format and naming policies. Supported inputs
include 10x Matrix Market triplets, 10x/CellRanger HDF5, AnnData h5ad,
h5Seurat, and loom files.
}
