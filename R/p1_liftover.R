# Functions from pagoda1

#' @importFrom pcaMethods pca
#' @importFrom pcaMethods scores
NULL

##' Collapse aspects driven by the same combinations of genes
##'
##' Examines PC loading vectors underlying the identified aspects and clusters aspects based
##' on a product of loading and score correlation (raised to corr.power). Clusters of aspects
##' driven by the same genes are determined based on the distance.threshold and collapsed.
##'
##' @param tam output of pagoda.top.aspects()
##' @param pwpca output of pagoda.pathway.wPCA()
##' @param clpca output of pagoda.gene.clusters() (optional)
##' @param plot whether to plot the resulting clustering
##' @param cluster.method one of the standard clustering methods to be used (fastcluster::hclust is used if available or stats::hclust)
##' @param distance.threshold similarity threshold for grouping interdependent aspects
##' @param corr.power power to which the product of loading and score correlation is raised
##' @param abs Boolean of whether to use absolute correlation
##' @param n.cores number of cores to use during processing
##' @param ... additional arguments are passed to the pagoda.view.aspects() method during plotting
##'
##' @return a list structure analogous to that returned by pagoda.top.aspects(), but with addition of a $cnam element containing a list of aspects summarized by each row of the new (reduced) $xv and $xvw
##'
##' @examples
##' data(pollen)
##' cd <- clean.counts(pollen)
##' \donttest{
##' knn <- knn.error.models(cd, k=ncol(cd)/4, n.cores=10, min.count.threshold=2, min.nonfailed=5, max.model.plots=10)
##' varinfo <- pagoda.varnorm(knn, counts = cd, trim = 3/ncol(cd), max.adj.var = 5, n.cores = 1, plot = FALSE)
##' pwpca <- pagoda.pathway.wPCA(varinfo, go.env, n.components=1, n.cores=10, n.internal.shuffles=50)
##' tam <- pagoda.top.aspects(pwpca, return.table = TRUE, plot=FALSE, z.score=1.96)  # top aspects based on GO only
##' tamr <- pagoda.reduce.loading.redundancy(tam, pwpca)
##' }
##'
##' @export
pagoda.reduce.loading.redundancy <- function(tam, pwpca, clpca = NULL, plot = FALSE, cluster.method = "complete", distance.threshold = 0.01, corr.power = 4, n.cores = 1, abs = TRUE, ...) {
  pclc <- pathway.pc.correlation.distance(c(pwpca, clpca$cl.goc), tam$xv, target.ndf = 100, n.cores = n.cores)
  cda <- cor(t(tam$xv))
  if(abs) {
    cda <- abs(cda)
  } else {
    cda[cda<0] <- 0
  }
  cda <- as.dist(1-cda)
  cc <- (1-sqrt((1-pclc)*(1-cda)))^corr.power

  if(is.element("fastcluster", installed.packages()[, 1])) {
    y <- fastcluster::hclust(cc, method = cluster.method)
  } else {
    y <- stats::hclust(cc, method = cluster.method)
  }
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



##' @export
collapse.aspect.clusters <- function(d, dw, ct, scale = TRUE, pick.top = FALSE) {
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



##' Collapse aspects driven by similar patterns (i.e. separate the same sets of cells)
##'
##' Examines PC loading vectors underlying the identified aspects and clusters aspects based on score correlation. Clusters of aspects driven by the same patterns are determined based on the distance.threshold.
##'
##' @param tamr output of pagoda.reduce.loading.redundancy()
##' @param distance.threshold similarity threshold for grouping interdependent aspects
##' @param cluster.method one of the standard clustering methods to be used (fastcluster::hclust is used if available or stats::hclust)
##' @param distance distance matrix
##' @param weighted.correlation Boolean of whether to use a weighted correlation in determining the similarity of patterns
##' @param plot Boolean of whether to show plot
##' @param top Restrict output to the top n aspects of heterogeneity
##' @param trim Winsorization trim to use prior to determining the top aspects
##' @param abs Boolean of whether to use absolute correlation
##' @param ... additional arguments are passed to the pagoda.view.aspects() method during plotting
##'
##' @return a list structure analogous to that returned by pagoda.top.aspects(), but with addition of a $cnam element containing a list of aspects summarized by each row of the new (reduced) $xv and $xvw
##'
##' @examples
##' data(pollen)
##' cd <- clean.counts(pollen)
##' \donttest{
##' knn <- knn.error.models(cd, k=ncol(cd)/4, n.cores=10, min.count.threshold=2, min.nonfailed=5, max.model.plots=10)
##' varinfo <- pagoda.varnorm(knn, counts = cd, trim = 3/ncol(cd), max.adj.var = 5, n.cores = 1, plot = FALSE)
##' pwpca <- pagoda.pathway.wPCA(varinfo, go.env, n.components=1, n.cores=10, n.internal.shuffles=50)
##' tam <- pagoda.top.aspects(pwpca, return.table = TRUE, plot=FALSE, z.score=1.96)  # top aspects based on GO only
##' tamr <- pagoda.reduce.loading.redundancy(tam, pwpca)
##' tamr2 <- pagoda.reduce.redundancy(tamr, distance.threshold = 0.9, plot = TRUE, labRow = NA, labCol = NA, box = TRUE, margins = c(0.5, 0.5), trim = 0)
##' }
##'
##' @export
pagoda.reduce.redundancy <- function(tamr, distance.threshold = 0.2, cluster.method = "complete", distance = NULL, weighted.correlation = TRUE, plot = FALSE, top = Inf, trim = 0, abs = FALSE, ...) {
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
  if(is.element("fastcluster", installed.packages()[, 1])) {
    y <- fastcluster::hclust(distance, method = cluster.method)
  } else {
    y <- stats::hclust(distance, method = cluster.method)
  }

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

##' Winsorize matrix
##'
##' Sets the ncol(mat)*trim top outliers in each row to the next lowest value same for the lowest outliers
##'
##' @param mat matrix
##' @param trim fraction of outliers (on each side) that should be Winsorized, or (if the value is  >= 1) the number of outliers to be trimmed on each side
##'
##' @return Winsorized matrix
##'
##' @examples
##' set.seed(0)
##' mat <- matrix( c(rnorm(5*10,mean=0,sd=1), rnorm(5*10,mean=5,sd=1)), 10, 10)  # random matrix
##' mat[1,1] <- 1000  # make outlier
##' range(mat)  # look at range of values
##' win.mat <- winsorize.matrix(mat, 0.1)
##' range(win.mat)  # note outliers removed
##'
##' @export
winsorize.matrix <- function(mat, trim) {
  if(trim  >  0.5) { trim <- trim/ncol(mat)  }
  #wm <- .Call("winsorizeMatrix", mat, trim, PACKAGE = "pagoda2")
  wm <- winsorizeMatrix(mat, trim)
  rownames(wm) <- rownames(mat)
  colnames(wm) <- colnames(mat)
  return(wm)
}


##' @export
pathway.pc.correlation.distance <- function(pcc, xv, n.cores = 1, target.ndf = NULL) {
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

  #x <- .Call("plSemicompleteCor2", pl, PACKAGE = "pagoda2")
  x <- plSemicompleteCor2(pl)

  if(!is.null(target.ndf)) {
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
  d

}
