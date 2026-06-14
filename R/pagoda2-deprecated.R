#' Removed and deprecated pagoda2 functions
#'
#' pagoda2.1 replaced the pagoda1-era procedural API with the \code{\link{Pagoda2}}
#' R6 class and its generic pipeline verbs (\code{runReduction}, \code{runGraph},
#' \code{runClustering}, \code{runEmbedding}, \code{runMarkers}; or the all-in-one
#' \code{run}). This page maps the removed standalone functions and the renamed
#' \code{Pagoda2} methods to what to call instead.
#'
#' Removed standalone functions:
#'
#' \tabular{ll}{
#'   \strong{Removed} \tab \strong{Use instead} \cr
#'   \code{basicP2proc(cd)} \tab \code{p2 <- Pagoda2$new(cd); p2$run()} \cr
#'   \code{extendedP2proc(p2, organism)} \tab \code{p2$testPathwayOverdispersion(setenv = go.env)} \cr
#'   \code{p2.generate.go(p2, organism)} \tab build a gene-set environment yourself (e.g. via \code{AnnotationDbi}/an org.*.eg.db) and pass it as \code{setenv} \cr
#'   \code{tp2c.view.pathways(...)} \tab \code{p2$testPathwayOverdispersion()} with \code{p2$plotMarkerHeatmap()} / \code{p2$plotEmbedding()} \cr
#'   \code{plotOneWithValues(p2, values)} \tab \code{p2$plotEmbedding(colors = values)} \cr
#' }
#'
#' Renamed \code{Pagoda2} methods (the old names still work but emit a deprecation
#' message; the algorithm is selected by the generic verb's \code{method=} argument):
#'
#' \tabular{ll}{
#'   \strong{Deprecated method} \tab \strong{Use instead} \cr
#'   \code{adjustVariance()} \tab \code{runVariance()} \cr
#'   \code{calculatePcaReduction()} \tab \code{runReduction()} \cr
#'   \code{makeKnnGraph()} \tab \code{runGraph()} \cr
#'   \code{getKnnClusters()} \tab \code{runLeiden()} (a \code{runClustering(method = "leiden")}) \cr
#'   \code{getEmbedding()} \tab \code{runEmbedding()} \cr
#'   \code{getDifferentialGenes()} \tab \code{runMarkers()} \cr
#' }
#'
#' @seealso \code{\link{Pagoda2}}
#' @name pagoda2-deprecated
NULL
