% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p1_liftover.R
\name{pagoda.reduce.redundancy}
\alias{pagoda.reduce.redundancy}
\title{Collapse aspects driven by similar patterns (i.e. separate the same sets of cells)
Examines PC loading vectors underlying the identified aspects and clusters aspects based on score correlation. Clusters of aspects driven by the same patterns are determined based on the distance.threshold.}
\usage{
pagoda.reduce.redundancy(
  tamr,
  distance.threshold = 0.2,
  cluster.method = "complete",
  distance = NULL,
  weighted.correlation = TRUE,
  plot = FALSE,
  top = Inf,
  trim = 0,
  abs = FALSE,
  ...
)
}
\arguments{
\item{tamr}{Combined pathways that show similar expression patterns, output of pagoda.reduce.loading.redundancy()}

\item{distance.threshold}{numeric Similarity threshold for grouping interdependent aspects (default=0.2)}

\item{cluster.method}{character One of the standard clustering methods to be used (default="complete")}

\item{distance}{distance matrix (default=NULL)}

\item{weighted.correlation}{boolean Whether to use a weighted correlation in determining the similarity of patterns (default=TRUE)}

\item{plot}{boolean Whether to show plot (default=FALSE)}

\item{top}{bololean Restrict output to the top N aspects of heterogeneity (default=Inf, i.e. no restriction)}

\item{trim}{numeric Winsorization trim to use prior to determining the top aspects (default=0)}

\item{abs}{boolean Whether to use absolute correlation (default=FALSE)}

\item{...}{additional arguments are passed to the pagoda.view.aspects() method during plotting}
}
\value{
List structure analogous to that returned by pagoda.top.aspects(), but with addition of a $cnam element containing a list of aspects summarized by each row of the new (reduced) $xv and $xvw
}
\description{
Collapse aspects driven by similar patterns (i.e. separate the same sets of cells)
Examines PC loading vectors underlying the identified aspects and clusters aspects based on score correlation. Clusters of aspects driven by the same patterns are determined based on the distance.threshold.
}
