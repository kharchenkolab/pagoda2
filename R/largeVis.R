## 'largeVis' moveover routine
## copied from https://github.com/elbamos/largeVis

#' @importFrom stats runif
NULL

#' Rescale the weights in an edge matrix to match a given perplexity.
#' From 'largeVis', <https://github.com/elbamos/largeVis>
#'
#' @param x An edgematrix, either an `edgematrix` object or a sparse matrix.
#' @param threads numeric The maximum number of threads to spawn (default=NULL). Determined automatically if NULL (default=NULL)
#' @param perplexity numeric Given perplexity (default=50)
#' @return A \code{list} with the following components: \describe{
#'    \item{'dist'}{An [N,K] matrix of the distances to the nearest neighbors.}
#'    \item{'id'}{An [N,K] matrix of the node indexes of the neartest neighbors.  Note that this matrix is 1-indexed,
#'    unlike most other matrices in this package.}
#'    \item{'k'}{The number of nearest neighbors.}
#'  }
#' @export
buildWijMatrix <- function(x, threads = NULL, perplexity = 50) {
  is <- rep(0:(ncol(x) - 1), diff(x@p))
  wij <- referenceWij(is, x@i, x@x^2, as.integer(threads), perplexity)
  return(wij)
}

#' Project a distance matrix into a lower-dimensional space. (from elbamos/largeVis)
#'
#' Takes as input a sparse matrix of the edge weights connecting each node to its nearest neighbors, and outputs
#' a matrix of coordinates embedding the inputs in a lower-dimensional space.
#'
#' The algorithm attempts to estimate a \code{dim}-dimensional embedding using stochastic gradient descent and
#' negative sampling.
#'
#' The objective function is: \deqn{ O = \sum_{(i,j)\in E} w_{ij} (\log f(||p(e_{ij} = 1||) + \sum_{k=1}^{M} E_{jk~P_{n}(j)} \gamma \log(1 - f(||p(e_{ij_k} - 1||)))}
#' where \eqn{f()} is a probabilistic function relating the distance between two points in the low-dimensional projection space,
#' and the probability that they are nearest neighbors.
#'
#' The default probabilistic function is \eqn{1 / (1 + \alpha \dot ||x||^2)}. If \eqn{\alpha} is set to zero,
#' an alternative probabilistic function, \eqn{1 / (1 + \exp(x^2))} will be used instead.
#'
#' Note that the input matrix should be symmetric.  If any columns in the matrix are empty, the function will fail.
#'
#' @param wij A symmetric sparse matrix of edge weights, in C-compressed format, as created with the \code{Matrix} package.
#' @param dim numeric The number of dimensions for the projection space (default=2)
#' @param sgd_batches numeric The number of edges to process during SGD (default=NULL). Defaults to a value set based on the size of the dataset. If the parameter given is
#' between \code{0} and \code{1}, the default value will be multiplied by the parameter.
#' @param M numeric (largeVis) The number of negative edges to sample for each positive edge (default=5).
#' @param gamma numeric (largeVis) The strength of the force pushing non-neighbor nodes apart (default=7).
#' @param alpha numeric (largeVis) The hyperparameter in the distance function (default=1). The default distance function, \eqn{1 / (1 + \alpha \dot ||y_i - y_j||^2)}.  
#' The function relates the distance between points in the low-dimensional projection to the likelihood that the two points are nearest neighbors. Increasing \eqn{\alpha} tends
#' to push nodes and their neighbors closer together; decreasing \eqn{\alpha} produces a broader distribution. Setting \eqn{\alpha} to zero
#' enables the alternative distance function. \eqn{\alpha} below zero is meaningless.
#' @param rho (largeVis) numeric Initial learning rate (default=1)
#' @param coords An initialized coordinate matrix (default=NULL)
#' @param useDegree boolean Whether to use vertex degree to determine weights in negative sampling (if TRUE) or the sum of the vertex's edges (if FALSE) (default=FALSE)
#' @param momentum If not NULL, SGD with momentum is used, with this multiplier, which must be between 0 and 1 (default=NULL). Note that
#' momentum can drastically speed-up training time, at the cost of additional memory consumed.
#' @param seed numeric Random seed to be passed to the C++ functions (default=NULL). Sampled from hardware entropy pool if \code{NULL} (the default).
#' Note that if the seed is not \code{NULL} (the default), the maximum number of threads will be set to 1 in phases of the algorithm
#' that would otherwise be non-deterministic.
#' @param threads numeric The maximum number of threads to spawn (default=NULL). Determined automatically if \code{NULL} (the default).
#' @param verbose boolean Verbosity (default=getOption("verbose", TRUE))
#'
#' @note If specified, \code{seed} is passed to the C++ and used to initialize the random number generator. This will not, however, be
#' sufficient to ensure reproducible results, because the initial coordinate matrix is generated using the \code{R} random number generator.
#' To ensure reproducibility, call \code{\link[base]{set.seed}} before calling this function, or pass it a pre-allocated coordinate matrix.
#'
#' @note The original paper called for weights in negative sampling to be calculated according to the degree of each vertex, the number of edges
#' connecting to the vertex. The reference implementation, however, uses the sum of the weights of the edges to each vertex. In experiments, the
#' difference was imperceptible with small (MNIST-size) datasets, but the results seems aesthetically preferrable using degree. The default
#' is to use the edge weights, consistent with the reference implementation.
#'
#' @return A dense [N,D] matrix of the coordinates projecting the w_ij matrix into the lower-dimensional space.
#' @examples
#' \donttest{
#' data(CO2)
#' CO2$Plant <- as.integer(CO2$Plant)
#' CO2$Type <- as.integer(CO2$Type)
#' CO2$Treatment <- as.integer(CO2$Treatment)
#' co <- scale(as.matrix(CO2))
#' # Very small datasets often produce a warning regarding the alias table.  This is safely ignored.
#' suppressWarnings(vis <- largeVis(t(co), K = 20, sgd_batches = 1, threads = 2))
#' suppressWarnings(coords <- projectKNNs(vis$wij, threads = 2))
#' plot(t(coords))
#' }
#' @export
projectKNNs <- function(wij, # symmetric sparse matrix
                        dim = 2, # dimension of the projection space
                        sgd_batches = NULL,
                        M = 5,
                        gamma = 7,
                        alpha = 1,
                        rho = 1,
                        coords = NULL,
                        useDegree = FALSE,
                        momentum = NULL,
                        seed = NULL,
                        threads = NULL,
                        verbose = getOption("verbose", TRUE)) {

  if (alpha < 0) stop("alpha < 0 is meaningless")
  N <-  (length(wij@p) - 1)
  js <- rep(0:(N - 1), diff(wij@p))
  if (any(is.na(js))) stop("NAs in the index vector.")
  is <- wij@i

  ##############################################
  # Initialize coordinate matrix
  ##############################################
  if (is.null(coords)){
    coords <- matrix((runif(N * dim) - 0.5) / dim * 0.0001, nrow = dim)
  }

  if (is.null(sgd_batches)) {
    sgd_batches <- sgdBatches(N, length(wij@x / 2))
  } else if (sgd_batches < 0) {
    stop("sgd batches must be > 0")
  } else if (sgd_batches < 1) {
    sgd_batches = sgd_batches * sgdBatches(N, length(wij@x / 2))
  }

  if (!is.null(threads)) {
    threads <- as.integer(threads)
  }
  if (!is.null(momentum)) {
    momentum <- as.double(momentum)
  }

  #################################################
  # SGD
  #################################################
  if (verbose) message("Estimating embeddings.\n")
  coords <- sgd(coords,
                targets_i = is,
                sources_j = js,
                ps = wij@p,
                weights = wij@x,
                alpha = as.double(alpha),
                gamma = as.double(gamma),
                M = as.integer(M),
                rho = as.double(rho),
                n_samples = sgd_batches,
                momentum = momentum,
                useDegree = as.logical(useDegree),
                seed = seed,
                threads = threads,
                verbose = as.logical(verbose))

  return(coords)
}

#' Calculate the default number of batches for a given number of vertices and edges.
#' The formula used is the one used by the 'largeVis' reference implementation.  This is substantially less than the recommendation \eqn{E * 10000} in the original paper.
#'
#' @param N Number of vertices
#' @param E Number of edges (default = 150*N/2)
#' @return The recommended number of sgd batches.
#'
#' @examples
#' # Observe that increasing K has no effect on processing time
#' \donttest{
#' N <- 70000 # MNIST
#' K <- 10:250
#' plot(K, sgdBatches(rep(N, length(K)), N * K / 2))
#'
#' # Observe that processing time scales linarly with N
#' N <- c(seq(from = 1, to = 10000, by = 100), seq(from = 10000, to = 10000000, by = 1000))
#' plot(N, sgdBatches(N))
#' }
#' @export
sgdBatches <- function(N, E = 150 * N / 2) {
	ifelse(N < 10000, 2000 * E, ifelse(N < 1000000, 1000000 * (9000 * (N - 10000) / (1000000 - 10000) + 1000), N * 10000))
}