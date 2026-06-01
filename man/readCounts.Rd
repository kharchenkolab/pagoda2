\name{readCounts}
\alias{readCounts}
\title{Read Count Matrices}
\usage{
readCounts(
  path,
  format = c("auto", "10x"),
  version = c("auto", "V3", "V2"),
  gene.id = c("symbol", "id"),
  feature.type = NULL,
  make.unique.genes = FALSE,
  cell.prefix = NULL,
  sample.name = NULL,
  sample.pattern = NULL,
  validate.integer = TRUE,
  return.metadata = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{path}{Directory containing a 10x triplet.}

\item{format}{Input format. Currently \code{auto} and \code{10x} resolve to the 10x reader.}

\item{version}{10x feature file version: \code{auto}, \code{V3}, or \code{V2}.}

\item{gene.id}{Which feature column to use as matrix row names: \code{symbol} or \code{id}.}

\item{feature.type}{Optional 10x V3 feature type to retain.}

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
Read count matrices with explicit format and naming policies. The initial
implementation supports 10x Matrix Market triplets with canonical or renamed
GEO-style filenames.
}
