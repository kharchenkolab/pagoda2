## Functions from 'PAGODA1', 'SCDE' 
## <https://www.bioconductor.org/packages/release/bioc/html/scde.html>


#' Collapse aspects driven by the same combinations of genes.
#' (Aspects are some pattern across cells e.g. sequencing depth, 
#' or PC corresponding to an undesired process such as ribosomal pathway variation.)
#' Examines PC loading vectors underlying the identified aspects and clusters of aspects based
#' on a product of loading and score correlation (raised to corr.power). 
#' Clusters of aspects driven by the same genes are determined based 
#' on the parameter "distance.threshold".
#'
#' @param tam output of pagoda.top.aspects(), i.e. a list structure containing the following items:
    #' xv: a matrix of normalized aspect patterns (rows: significant aspects, columns: cells)
    #' xvw: corresponding weight matrix 
    #' gw: set of genes driving the significant aspects 
    #' df: text table with the significance testing results 
#' @param pwpca output of pagoda.pathway.wPCA(), i.e. a list of weighted PCA info for each valid gene set
#' @param clpca output of pagoda.gene.clusters() (optional) (default=NULL). The output of pagoda.gene.clusters() is 
    #' a list structure containing the following fields:
    #' clusters: alist of genes in each cluster values
    #' xf: extreme value distribution fit for the standardized lambda1 of a randomly generated pattern
    #' tci: index of a top cluster in each random iteration
    #' cl.goc: weighted PCA info for each real gene cluster
    #' varm: standardized lambda1 values for each randomly generated matrix cluster
    #' clvlm: a linear model describing dependency of the cluster lambda1 on a Tracy-Widom lambda1 expectation
#' @param plot boolean Whether to plot the resulting clustering (default=FALSE)
#' @param cluster.method string One of the standard clustering methods to be used (default="complete") 
#' @param distance.threshold numeric Similarity threshold for grouping interdependent aspects (default=0.01)
#' @param corr.power numeric Power to which the product of loading and score correlation is raised (default=4)
#' @param abs boolean Whether to use absolute correlation (default=TRUE)
#' @param n.cores numeric Number of cores to use during processing (default=1)
#' @param ... additional arguments are passed to the pagoda.view.aspects() method during plotting
#' @return a list structure analogous to that returned by pagoda.top.aspects(), but with addition of a $cnam element containing a list of aspects summarized by each row of the new (reduced) $xv and $xvw
#'
#' @export
pagoda.reduce.loading.redundancy <- function(tam, pwpca, clpca = NULL, plot = FALSE, cluster.method = "complete", distance.threshold = 0.01, corr.power = 4, abs = TRUE, n.cores = 1, ...) {
  pclc <- pathway.pc.correlation.distance(c(pwpca, clpca$cl.goc), tam$xv, target.ndf = 100, n.cores = n.cores)
  cda <- cor(t(tam$xv))
  if(abs) {
    cda <- abs(cda)
  } else {
    cda[cda<0] <- 0
  }
  cda <- as.dist(1-cda)
  cc <- (1-sqrt((1-pclc)*(1-cda)))^corr.power

  y <- fastcluster::hclust(cc, method = cluster.method)
  ##y <- stats::hclust(cc, method = cluster.method)

  ct <- cutree(y, h = distance.threshold)
  ctf <- factor(ct, levels = sort(unique(ct)))
  xvl <- collapse.aspect.clusters(tam$xv, tam$xvw, ct, pick.top = FALSE, scale = TRUE)

  if(plot) {
    sc <- sample(colors(), length(levels(ctf)), replace = TRUE)
    view.aspects(tam$xv, row.clustering = y, row.cols = sc[as.integer(ctf)], ...)
  }

  # collapsed names
  if(!is.null(tam$cnam)) { # already has collapsed names
    cnam <- tapply(rownames(tam$xv), ctf, function(xn) unlist(tam$cnam[xn]))
  } else {
    cnam <- tapply(rownames(tam$xv), ctf, I)
  }
  names(cnam) <- rownames(xvl$d)
  tam$xv <- xvl$d
  tam$xvw <- xvl$w
  tam$cnam <- cnam
  return(tam)
}



#' Collapse aspect patterns into clusters
#'
#' @param d matrix of normalized aspect patterns (rows: significant aspects, columns: cells), normally the output $xv in 'tamr', the combined pathways that show similar expression patterns
#' @param dw corresponding weight matrix to parameter 'd'
#' @param ct clusters, the output of fastcluster::hclust()
#' @param scale boolean Whether to scale aspects (default=TRUE)
#' @param pick.top boolean Whether to pick top aspects (default=FALSE)
#' @return list of clusters from matrix of normalized aspect patterns and clusters from the corresponding weight matrix
#'
#' @export 
collapse.aspect.clusters <- function(d, dw, ct, scale = TRUE, pick.top = FALSE) {
  if (!requireNamespace("pcaMethods", quietly = TRUE)) {
    stop("Package \"pcaMethods\" needed for this function to work. Please install it with `BiocManager::install('pcaMethods')`.", call. = FALSE)
  }

  xvm <- do.call(rbind, tapply(seq_len(nrow(d)), factor(ct, levels = sort(unique(ct))), function(ii) {
    if(length(ii) == 1) return(d[ii, ])
    if(pick.top) {
      return(d[ii[which.max(apply(d[ii, ], 1, var))], ])
    }
    xp <- pcaMethods::pca(t(d[ii, ]), nPcs = 1, center = TRUE, scale = "none")
    xv <- pcaMethods::scores(xp)[, 1]
    if(sum(abs(diff(xv))) > 0 && cor(xv, colMeans(d[ii, ]*abs(pcaMethods::loadings(xp)[, 1])))<0) { xv <- -1*xv }
    #set scale at top pathway?
    if(sum(abs(diff(xv))) > 0) {
      if(scale) {
        xv <- xv*sqrt(max(apply(d[ii, ], 1, var)))/sqrt(var(xv))
      }
      if(sum(abs(xv)) == 0) { xv <- abs(rnorm(length(xv), sd = 1e-6)) }
    } else {
      xv <- abs(rnorm(length(xv), sd = 1e-6))
    }
    #xv <- xv/sqrt(length(ii))
    xv
  }))
  rownames(xvm) <- unlist(tapply(seq_len(nrow(d)), factor(ct, levels = sort(unique(ct))), function(ii) {
    if(length(ii) == 1) return(rownames(d)[ii])
    return(rownames(d)[ii[which.max(apply(d[ii, ], 1, var))]])
  }))

  xvmw <- do.call(rbind, tapply(seq_len(nrow(d)), factor(ct, levels = sort(unique(ct))), function(ii) {
    w <- colSums(dw[ii, , drop = FALSE]*apply(d[ii, , drop = FALSE], 1, sd))
    w <- w/sum(w)
  }))

  return(list(d = xvm, w = xvmw))
}



#' Collapse aspects driven by similar patterns (i.e. separate the same sets of cells)
#' Examines PC loading vectors underlying the identified aspects and clusters aspects based on score correlation. Clusters of aspects driven by the same patterns are determined based on the distance.threshold.
#'
#' @param tamr Combined pathways that show similar expression patterns, output of pagoda.reduce.loading.redundancy()
#' @param distance.threshold numeric Similarity threshold for grouping interdependent aspects (default=0.2)
#' @param cluster.method character One of the standard clustering methods to be used (default="complete") 
#' @param distance distance matrix (default=NULL)
#' @param weighted.correlation boolean Whether to use a weighted correlation in determining the similarity of patterns (default=TRUE)
#' @param plot boolean Whether to show plot (default=FALSE)
#' @param top bololean Restrict output to the top N aspects of heterogeneity (default=Inf, i.e. no restriction)
#' @param trim numeric Winsorization trim to use prior to determining the top aspects (default=0)
#' @param abs boolean Whether to use absolute correlation (default=FALSE)
#' @param ... additional arguments are passed to the pagoda.view.aspects() method during plotting
#' @return List structure analogous to that returned by pagoda.top.aspects(), but with addition of a $cnam element containing a list of aspects summarized by each row of the new (reduced) $xv and $xvw
#'
#' @export 
pagoda.reduce.redundancy <- function(tamr, distance.threshold=0.2, cluster.method="complete", 
  distance=NULL, weighted.correlation=TRUE, plot=FALSE, top=Inf, trim=0, abs=FALSE, ...) {
  if(is.null(distance)) {
    if(weighted.correlation) {
      #distance <- .Call("matWCorr", t(tamr$xv), t(tamr$xvw), PACKAGE = "pagoda2")
      distance <- matWCorr(t(tamr$xv), t(tamr$xvw))
      rownames(distance) <- colnames(distance) <- rownames(tamr$xv)
      if(abs) {
        distance <- stats::as.dist(1-abs(distance), upper = TRUE)
      } else {
        distance <- stats::as.dist(1-distance, upper = TRUE)
      }
    } else {
      if(abs) {
        distance <- stats::as.dist(1-abs(cor(t(tamr$xv))))
      } else {
        distance <- stats::as.dist(1-cor(t(tamr$xv)))
      }
    }
  }

  y <- fastcluster::hclust(distance, method = cluster.method)
  ##y <- stats::hclust(distance, method = cluster.method)

  ct <- cutree(y, h = distance.threshold)
  ctf <- factor(ct, levels = sort(unique(ct)))
  xvl <- collapse.aspect.clusters(tamr$xv, tamr$xvw, ct, pick.top = FALSE, scale = TRUE)

  if(plot) {
    sc <- sample(colors(), length(levels(ctf)), replace = TRUE)
    view.aspects(tamr$xv, row.clustering = y, row.cols = sc[as.integer(ctf)], ...)
  }

  # collapsed names
  if(!is.null(tamr$cnam)) { # already has collapsed names
    cnam <- tapply(rownames(tamr$xv), ctf, function(xn) unlist(tamr$cnam[xn]))
  } else {
    cnam <- tapply(rownames(tamr$xv), ctf, I)
  }
  names(cnam) <- rownames(xvl$d)

  if(trim > 0) { xvl$d <- winsorize.matrix(xvl$d, trim) } # trim prior to determining the top sets

  rcmvar <- apply(xvl$d, 1, var)
  vi <- order(rcmvar, decreasing = TRUE)[1:min(length(rcmvar), top)]

  tamr2 <- tamr
  tamr2$xv <- xvl$d[vi, ]
  tamr2$xvw <- xvl$w[vi, ]
  tamr2$cnam <- cnam[vi]
  return(tamr2)
}


#' Sets the ncol(mat)*trim top outliers in each row to the next lowest value same for the lowest outliers
#'
#' @param mat Numeric matrix
#' @param trim numeric Fraction of outliers (on each side) that should be Winsorized, or (if the value is  >= 1) the number of outliers to be trimmed on each side
#' @return Winsorized matrix
#'
#' @examples
#' set.seed(0)
#' mat <- matrix( c(rnorm(5*10,mean=0,sd=1), rnorm(5*10,mean=5,sd=1)), 10, 10)  # random matrix
#' mat[1,1] <- 1000  # make outlier
#' range(mat)  # look at range of values
#' win.mat <- winsorize.matrix(mat, 0.1)
#' range(win.mat)  # note outliers removed
#'
#' @export
winsorize.matrix <- function(mat, trim) {
  if (trim  >  0.5) { 
    trim <- trim/ncol(mat)  
  }
  #wm <- .Call("winsorizeMatrix", mat, trim, PACKAGE = "pagoda2")
  wm <- winsorizeMatrix(mat, trim)
  rownames(wm) <- rownames(mat)
  colnames(wm) <- colnames(mat)
  return(wm)
}

#' Calculate correlation distance between PC magnitudes given a number of target dimensions
#'
#' @param pcc weighted PC magnitudes e.g. scde::pagoda.pathway.wPCA() gives the weighted PC magnitudes for each gene provided; 
    #' e.g. scde::pagoda.gene.clusters() gives the weighted PC magnitudes for de novo gene sets identified by clustering on expression
#' @param xv a matrix of normalized aspect patterns (rows: significant aspects, columns: cells)
#' @param n.cores numeric Number of cores to use (default=1)
#' @param target.ndf numeric Target dimensions (default=NULL)
#' @return correlation distance matrix, akin to stats dist
#' @export
pathway.pc.correlation.distance <- function(pcc, xv, n.cores=1, target.ndf=NULL) {
  # all relevant gene names
  rotn <- unique(unlist(lapply(pcc[gsub("^#PC\\d+# ", "", rownames(xv))], function(d) rownames(d$xp$rotation))))
  # prepare an ordered (in terms of genes) and centered version of each component
  pl <- lapply(rownames(xv), function(nam) {
    pnam <- gsub("^#PC\\d+# ", "", nam)
    pn <- as.integer(gsub("^#PC(\\d+)# .*", "\\1", nam))
    rt <- pcc[[pnam]]$xp$rotation[, pn]
    # order names/values according to increasing name match index
    mi <- match(names(rt), rotn)
    mo <- order(mi, decreasing = FALSE)
    rt <- as.numeric(rt)-mean(rt)
    return(list(i = mi[mo], v = rt[mo]))
  })

  x <- plSemicompleteCor2(pl)

  if (!is.null(target.ndf)) {
    r <- x$r[upper.tri(x$r)]
    n <- x$n[upper.tri(x$n)]
    suppressWarnings(tv <- r*sqrt((n-2)/(1-r^2)))
    z <- pt(tv, df = n-2, lower.tail = FALSE, log.p = TRUE)
    nr <- qt(z, df = target.ndf-2, lower.tail = FALSE, log.p = TRUE)
    nr <- nr/sqrt(target.ndf-2+nr^2)
    nr[is.nan(nr)] <- r[is.nan(nr)]

    cr <- x$r
    cr[upper.tri(cr)] <- nr
    cr[lower.tri(cr)] <- t(cr)[lower.tri(cr)]
  } else {
    cr <- x$r
  }

  rownames(cr) <- colnames(cr) <- rownames(xv)
  d <- stats::as.dist(1-abs(cr))
  d[d<0] <- 0
  return(d)
}
