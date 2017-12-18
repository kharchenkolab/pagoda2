#### Cluster Matching Functions ####

#' Classify cells in a pagoda2 application given an annotated pagoda2 object
#' @description Given a reference pagoda2 application an a set of labels for each
#' cell, classify cells in a new application. This function works by placing both
#' datasets onto a common GSVD and identifying mutual nearest neighbours. Only cells
#' with mutual nearest neighbours are classified.
#' @param referenceP2 pagoda2 object to use as reference for the annotation
#' @param r2 pagoda2 object to the annotation of
#' @param referenceP2lables a factor providing labels for every cell in reference P2
#' @param var.scale perform variance scaling (according to p2 variance estimate)
#' @param k number of nearest neighbours to look for (before filtering for mutual)
#' @param log.scale transform to log scale
#' @param center perform centering
#' @param verbose print progress messages
#' @param extra.info return extra information from the run, this is currently the gsvd decomposition object
#' @export identifyCellsGSVDMNN
identifyCellsGSVDMNN <- function(referenceP2, r2, referenceP2labels,
                                 var.scale = T, k = 30, log.scale = T,
                                 center =T, verbose = T,
                                 extra.info = F) {
    require('plyr')
    require('geigen')

    ## Check referenceP2 and r2 objects
    if( !(class(referenceP2) == 'Pagoda2' & class(r2) == 'Pagoda2')) {
        stop('Both referenceP2 and r2 have to be objects of type Pagoda2');
    }

    ## Add check for referenceP2labels
    if (verbose) cat('Preparing data... ');

    ## For the moment use the same k for both datasets
    k1 <- k
    k2 <- k

    ## Get overdispersed genes, or some other relevant geneset
    odgenes <- union(referenceP2$misc$odgenes, r2$misc$odgenes)
    odgenes <- intersect(odgenes, colnames(referenceP2$counts))
    odgenes <- intersect(odgenes, colnames(r2$counts))

    ## Get matrices
    x1 <- referenceP2$counts[,odgenes]
    x2 <- r2$counts[,odgenes]

    ## Optionally variance scale
    if (var.scale) {
        x1 <- sweep(x1, 2, referenceP2$misc$varinfo[odgenes,]$gsf, FUN='*')
        x2 <- sweep(x2, 2, r2$misc$varinfo[odgenes,]$gsf, FUN='*')
    }

    ## Optionally log scale
    pseudocount <- 1e-6;
    if (log.scale) {
        x1 <- log10(x1 + pseudocount);
        x2 <- log10(x2 + pseudocount);
    }

    ## Remove mean and convert to full matrix
    if (center) {
        x1.colMean <- Matrix::colMeans(x1)
        x2.colMean <- Matrix::colMeans(x2)
        x1 <- sweep(x1, 2, x1.colMean, FUN='-')
        x2 <- sweep(x2, 2, x2.colMean, FUN='-')
    }

    ## Convert to full matrix
    x1 <- as.matrix(x1);
    x2 <- as.matrix(x2);
    if (verbose) cat('done\n');

    if (verbose) cat('Performing GSVD... ');
    ## Perform gsvd on the two matrices
    o <- gsvd(x1,x2)
    if (verbose) cat('done\n');

    ## Calculate rotated matrices
    if (verbose) cat('Calculating PCs... ');
    # x1.rot <- o$U %*% gsvd.D1(o) %*% gsvd.R(o)
    # x2.rot <- o$V %*% gsvd.D2(o) %*% gsvd.R(o)
    # This is faster
    x1.rot <- x1 %*% o$Q
    x2.rot <- x2 %*% o$Q

    rownames(x1.rot) <- rownames(x1)
    rownames(x2.rot) <- rownames(x2)
    if (verbose) cat('done\n');

    ## In this space assign cells by mutual NNs
    if (verbose) {cat('Finding MNNs... ');}
    mnnres <- pagoda2:::interNN(x1.rot, x2.rot, k1, k2, 2, verbose=F)
    if (verbose) cat('done\n');

    if (verbose) cat('Summarising... ');
    ## Put the labels on
    mnnres$clustereferenceP2 <- referenceP2labels[rownames(x1.rot)[mnnres$mA.id]]

    ## Summarize on a per cell basis
    summarize.cell <- ddply(mnnres, .(mB.id), function(x) {
        t <- table(x$clustereferenceP2)
        data.frame(match = names(t)[which.max(t)])
    })

    ## Put cell names back and remove index column
    summarize.cell$cell.id <-  rownames(x2.rot)[summarize.cell$mB.id]
    summarize.cell <- summarize.cell[,c('cell.id','match')]

    if (verbose) cat('done\n');

    res <- list();
    res$result = summarize.cell;

    ## Optionally return extra information
    if (extra.info) {
        res$extra.info <- list();
        res$extra.info$gsvd <- o;
    }

    ## Return value
    res
}

#' Classify cells in a pagoda2 application given a set of annotated reference applications
#' @description This function is similar to identifyCellsGSVDMNN() but it can
#' take as input a list of annotated pagoda2 objects. It then classifies cells
#' against all applications and summarised the results.
#' @param referencesets input datasets (p2 objects in a names list)
#' @param annotset p2 dataset to be annotated
#' @param clustersOrigin a names factor with cluster assignments for referenceset cells
#' @return a named factor with labells for annotset
#' @export identifyCellsGSVDMNNmulti
identifyCellsGSVDMNNmulti <- function(referencesets, annotset, clustersOrig) {
    require(dplyr)
    d1 <- lapply(seq_along(referencesets), function(i) {
        cat(paste0('Processing reference set ', names(referencesets)[i]), '\n')
        curset <- referencesets[[i]]
        cl1 <- clustersOrig[rownames(curset$counts)]
        ident <- identifyCellsGSVDMNN(curset, annotset, cl1, verbose=T, extra.info=F)
        ident$result$match <- as.character(ident$result$match)
        ident$result
    })
    cat('Summarizing ... ');
    d2 <- do.call(rbind, d1)
    d3 <- ddply(d2, .(cell.id), function(x) {data.frame(cell.id=x$cell.id[1], match=Mode(x$match))})

    d4 <- as.factor(d3$match)
    names(d4) <- as.character(d3$cell.id)
    cat('done\n');

    d4
}

#' Obtain joint clustering between multiple pagoda2 applications
#' @description obtain joint clustering between multiple pagoda2 applications
#' by identifying mutual nearest neighbours pairwise and constructing
#' a network on which traditional cluster identification algorithms are used to identify clusters
#' @param r.n a named list of pagoda2 apps
#' @param k number of KNN to look for
#' @param community.detection.method one of the igraph package community detection methods (or compatible fn)
#' @param min.group.size minimum group size to keep after matching
#' @param ncomps number of components to use
#' @return a named (by cell name) factor of groups
#' @export getJointClustering
getJointClustering <- function(r.n, k=30, community.detection.method = walktrap.community,
                                 min.group.size = 10,ncomps=100,n.cores=30, return.details=F,mnnres=NULL,n.odgenes=NULL) {
    require('gtools')
    #require('pbapply')
    require('igraph')
    require(parallel)
  xl <- NULL;
  if(is.null(mnnres)) {
    ## Get all non-redundant pair of apps
    nms <- names(r.n)
    combs <- combinations(n = length(nms), r = 2, v = nms, repeats.allowed =F)
    ## Convert to list for lapply
    combsl <- split(t(combs), rep(1:nrow(combs), each=ncol(combs)))
    
    ## get MNN pairs from all possible app pairs
    
    cat('Running GSVD for application pairs ...')
    xl <- mclapply(combsl, function(x) {
      cat('.')
      getMNNforP2pair(r.n[[x[1]]], r.n[[x[2]]], k = k, verbose =F, ncomps = ncomps, n.odgenes=n.odgenes,skipMNN=T);
    },mc.cores=n.cores);
    names(xl) <- unlist(lapply(combsl,paste,collapse='.vs.'))
    cat("done\n")
    
    # run mNN separatly as it can't deal with multithreading
    cat('Calculating MNN for application pairs ...')
    mnnres <- lapply(xl,function(x) {
      mnnres <- pagoda2:::interNN(x$x1.rot,x$x2.rot,k,k,2,verbose=F)
      mnnres$mA.lab <- rownames(x$x1.rot)[mnnres$mA.id]
      mnnres$mB.lab <- rownames(x$x2.rot)[mnnres$mB.id]
      cat('.')
      return(mnnres)
    })
    cat("done\n")
  }
    ## Merge the results into a edge table
    mnnres.all <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab')]

    ## Make a graph with the MNNs
    el <- matrix(c(mnnres.all$mA.lab, mnnres.all$mB.lab), ncol=2)
    g  <- graph_from_edgelist(el, directed =FALSE)

    ## Do community detection on this graph
    cat('Detecting clusters ...');
    cls <- community.detection.method(g)
    cat('done\n')
    ## Extract groups from this graph
    cls.mem <- membership(cls)
    cls.groups <- as.character(cls.mem)
    names(cls.groups) <- names(cls.mem)

    ## Filter groups
    lvls.keep <- names(which(table(cls.groups)  > min.group.size))
    cls.groups[! as.character(cls.groups) %in% as.character(lvls.keep)] <- NA
    cls.groups <- as.factor(cls.groups)

    if(return.details) {
      return(list(groups=cls.groups,mnnres=mnnres,xl=xl))
    } else {
      return(cls.groups)
    }
}


cpcaJC <- function(r.n, k=30, k.self=0, k.self.weight=1,community.detection.method = multilevel.community, var.scale =TRUE, min.group.size = 10,ncomps=100, n.odgenes=1000,n.cores=30,return.details=F,verbose=T,neighborhood.average=FALSE,neighborhood.average.k=10,xcp=NULL, ...) {
  require(parallel)
  require(cpca)
  require(Matrix)
  require(abind)
  require(igraph)
  
  k1 <- k2 <- k
  
  if(is.null(xcp)) {
    xcp <- quickCPCA(r.n,k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=verbose,var.scale=var.scale)
  } 
  odgenes <- rownames(xcp$CPC)  
  # common variance scaling
  if (var.scale) {
    cgsf <- do.call(cbind,lapply(r.n,function(x) x$misc$varinfo[odgenes,]$gsf))
    cgsf <- exp(rowMeans(log(cgsf)))
  }
  
  # determine common centering
  cat('centering ...')
  cproj <- lapply(r.n,function(r) {
    x <- r$counts[,odgenes];
    if(var.scale) {
      x@x <- x@x*rep(cgsf,diff(x@p))
    }
    x
  })
  ncells <- unlist(lapply(cproj,nrow));
  centering <- colSums(do.call(rbind,lapply(cproj,colMeans))*ncells)/sum(ncells)
  cat(' done\n')
  
  cat('projecting ...')
  cpproj <- mclapply(cproj,function(x) {
    x <- t(as.matrix(t(x))-centering)
    x %*% xcp$CPC
  },mc.cores=n.cores)
  cat(' done\n')

    ## Get all non-redundant pair of apps
  comb <- combn(names(r.n),2)
  cat('mNN pairs ')
  mnnres <- lapply(1:ncol(comb),function(i) {
    n1 <- comb[1,i]; n2 <- comb[2,i]
    cat(".")
    mnnres <- pagoda2:::interNN(cpproj[[n1]], cpproj[[n2]], k1, k2, 2, verbose=F,neighbourhoodAverage=neighborhood.average,neighbourAvgKA=neighborhood.average.k,neighbourAvgKB=neighborhood.average.k,TRUE)
    mnnres$mA.lab <- rownames(cpproj[[n1]])[mnnres$mA.id]
    mnnres$mB.lab <- rownames(cpproj[[n2]])[mnnres$mB.id]
    mnnres
  })
  cat(' done\n')
  
  ## Merge the results into a edge table
  el <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab')]
  el$w <- 1
  
  # append some local edges
  if(k.self>0) {
    cat('kNN pairs ')
    x <- data.frame(do.call(rbind,lapply(cpproj,function(x) {
      xk <- pagoda2:::hnswKnn2(x,k.self,n.cores,verbose=F)
      xk <- xk[xk$s!=xk$e,]
      cat(".")
      cbind("mA.lab"=rownames(x)[xk$s+1],"mB.lab"=rownames(x)[xk$e+1])
    })),stringsAsFactors = F)
    x$w <- k.self.weight
    cat(' done\n')
    el <- rbind(el,x)
  }
  
  g  <- graph_from_edgelist(as.matrix(el[,c(1,2)]), directed =FALSE)
  E(g)$weight <- el[,3]
  
  ## Do community detection on this graph
  cat('detecting clusters ...');
  cls <- community.detection.method(g, ...)
  cat('done\n')
  ## Extract groups from this graph
  cls.mem <- membership(cls)
  cls.groups <- as.character(cls.mem)
  names(cls.groups) <- names(cls.mem)
  
  ## Filter groups
  lvls.keep <- names(which(table(cls.groups)  > min.group.size))
  cls.groups[! as.character(cls.groups) %in% as.character(lvls.keep)] <- NA
  cls.groups <- as.factor(cls.groups)
  
  if(return.details) {
    return(list(groups=cls.groups,cpproj=cpproj,xcp=xcp,g=g))
  } else {
    cls.groups
  }
  
}

quickCCA <- function(r.n,k=30,ncomps=100,n.odgenes=NULL,var.scale=T,verbose=T,cgsf=NULL) {
  require(RGCCA)
  require(Matrix)
  
  if(length(r.n)!=2) stop("quickCCA supports only pair alignment")
  
  # select a common set of genes
  if(is.null(cgsf)) {
    if(is.null(n.odgenes)) {
      odgenes <- table(unlist(lapply(r.n,function(x) x$misc$odgenes)))
    } else {
      odgenes <- table(unlist(lapply(r.n,function(x) rownames(x$misc$varinfo)[(order(x$misc$varinfo$lp,decreasing=F)[1:min(ncol(x$counts),n.odgenes)])])))
    }
    odgenes <- odgenes[names(odgenes) %in% Reduce(intersect,lapply(r.n,function(x) colnames(x$counts)))]
    odgenes <- names(odgenes)[1:min(length(odgenes),n.odgenes)]
  } else {
    odgenes <- names(cgsf)
  }
  
  # common variance scaling
  if (var.scale) {
    if(is.null(cgsf)) {
      cgsf <- do.call(cbind,lapply(r.n,function(x) x$misc$varinfo[odgenes,]$gsf))
      cgsf <- exp(rowMeans(log(cgsf)))
    }
  }

  # determine common centering
  cproj <- lapply(r.n,function(r) {
    x <- r$counts[,odgenes];
    if(var.scale) {
      x@x <- x@x*rep(cgsf,diff(x@p))
    }
    x
  })
  ncells <- unlist(lapply(cproj,nrow));
  centering <- colSums(do.call(rbind,lapply(cproj,colMeans))*ncells)/sum(ncells)

  cproj <- lapply(cproj,function(x) {
    x <- t(as.matrix(t(x))-centering)
  })
  
  #x <- matrix(rnorm(150), 50, 3)
  #y <- matrix(rnorm(250), 50, 5)
  #z <- rgcca(A=list(x,y),C=matrix(c(0,1,1,0),2,2),tau=c(1,1),ncomp=c(2,2))
  #str(z)
  
  z <- rgcca(A=list(t(cproj[[1]]),t(cproj[[2]])),C=matrix(c(0,1,1,0),2,2),tau=c(0.1,0.1),ncomp=c(ncomps,ncomps),scale=FALSE,verbose=verbose)
  #z$Ys <- lapply(z$Y,function(x) t(t(x)/sqrt(colSums(x^2))))
  #z$P <- list(cproj[[1]] %*% z$Ys[[1]],cproj[[2]] %*% z$Ys[[2]])
  return(z);
}


ccaJCp <- function(r.n, k=30, k.self=0, k.self.weight=1,community.detection.method = multilevel.community, var.scale =TRUE, min.group.size = 10,ncomps=100, n.odgenes=1000, n.cores=30, return.details=F,xl=NULL,neighborhood.average=FALSE,neighborhood.average.k=10,verbose=TRUE, ...) {
  require(parallel)
  require(Matrix)
  require(igraph)

  cis <- combn(names(r.n),2)
  if(is.null(xl)) {
    cat('pairwise CCA ')
    xl <- pagoda2:::papply(1:ncol(cis), function(i) {
      xcp <- quickCCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=ifelse(n.cores==1,verbose,FALSE),var.scale=var.scale)
      cat('.')
      xcp
    },n.cores=n.cores);
    names(xl) <- apply(cis,2,paste,collapse='.vs.');
    cat(" done\n")
  } else {
    # match for all the pairs
    mi <- rep(NA,ncol(cis));
    cat('matched ',sum(!is.na(mi)),' out of ',length(mi),' CCA results ... ')
    mi[which(!is.na(match(apply(cis,2,paste,collapse='.vs.'),names(xl))))] <- T;
    mi[which(!is.na(match(apply(cis[c(2,1),],2,paste,collapse='.vs.'),names(xl))))] <- T;
    if(any(is.na(mi))) {
      cat('running ',sum(is.na(mi)),' additional CCAs ')
    }
    xl2 <- pagoda2:::papply(which(is.na(mi)), function(i) {
      xcp <- quickCCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=ifelse(n.cores==1,verbose,FALSE),var.scale=var.scale)
      cat('.')
      xcp
    },n.cores=n.cores);
    names(xl2) <- apply(cis[,which(is.na(mi)),drop=F],2,paste,collapse='.vs.');
    xl <- c(xl,xl2);
    cat(" done\n");
  }
  
  # run mNN separatly as it can't deal with multithreading
  cat('mNN ')
  mnnres <- lapply(1:ncol(cis), function(i) {
    cat(".")
    mnnres <- pagoda2:::interNN(xl[[i]]$a[[1]],xl[[i]]$a[[2]], k, k, 2, verbose=F,neighbourhoodAverage=neighborhood.average,neighbourAvgKA=neighborhood.average.k,neighbourAvgKB=neighborhood.average.k,TRUE)
    #mnnres <- pagoda2:::interNN(cpproj[[n1]], cpproj[[n2]], k1, k2, 2, verbose=F,neighborhood.average,neighborhood.average.k,neighborhood.average.k,TRUE)
    mnnres$mA.lab <- rownames(xl[[i]]$a[[1]])[mnnres$mA.id]
    mnnres$mB.lab <- rownames(xl[[i]]$a[[2]])[mnnres$mB.id]
    mnnres
  })
  cat("done\n")
  ## Merge the results into a edge table
  el <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab','dist')]
  colnames(el) <- c("mA.lab","mB.lab","w")
  
  # append some local edges
  if(k.self>0) {
    cat('kNN pairs ')
    x <- data.frame(do.call(rbind,lapply(r.n,function(x) {
      xk <- pagoda2:::hnswKnn2(x$reductions$PCA,k.self,n.cores,verbose=F)
      xk <- xk[xk$s!=xk$e,]
      cat(".")
      cbind("mA.lab"=rownames(x$reductions$PCA)[xk$s+1],"mB.lab"=rownames(x$reductions$PCA)[xk$e+1])
    })),stringsAsFactors = F)
    x$w <- k.self.weight
    cat(' done\n')
    el <- rbind(el,x)
  }
  
  g  <- graph_from_edgelist(as.matrix(el[,c(1,2)]), directed =FALSE)
  E(g)$weight <- el[,3]
  
  ## Do community detection on this graph
  cat('detecting clusters ...');
  cls <- community.detection.method(g, ...)
  cat('done\n')
  ## Extract groups from this graph
  cls.mem <- membership(cls)
  cls.groups <- as.character(cls.mem)
  names(cls.groups) <- names(cls.mem)
  
  ## Filter groups
  lvls.keep <- names(which(table(cls.groups)  > min.group.size))
  cls.groups[! as.character(cls.groups) %in% as.character(lvls.keep)] <- NA
  cls.groups <- as.factor(cls.groups)
  
  if(return.details) {
    return(list(groups=cls.groups,xl=xl,cls=cls,g=g))
  } else {
    cls.groups
  }
  
}

quickJNMF <- function(r.n, k = 30, ncomps =100, n.odgenes=NULL, var.scale=T, verbose =T, cgsf=NULL, maxiter=1000) {
    require(Matrix)
    require(Rjnmf)
    if(length(r.n)!=2) stop('quickJNMF only supports pair alignment')
    
    ## select a common set of genes
    if(is.null(cgsf)) {
      if(is.null(n.odgenes)) {
        odgenes <- table(unlist(lapply(r.n,function(x) x$misc$odgenes)))
      } else {
        odgenes <- table(unlist(lapply(r.n,function(x) rownames(x$misc$varinfo)[(order(x$misc$varinfo$lp,decreasing=F)[1:min(ncol(x$counts),n.odgenes)])])))
      }
      odgenes <- odgenes[names(odgenes) %in% Reduce(intersect,lapply(r.n,function(x) colnames(x$counts)))]
      odgenes <- names(odgenes)[1:min(length(odgenes),n.odgenes)]
    } else {
      odgenes <- names(cgsf)
    }
    ## common variance scaling
    if (var.scale) {
      if(is.null(cgsf)) {
        cgsf <- do.call(cbind,lapply(r.n,function(x) x$misc$varinfo[odgenes,]$gsf))
        cgsf <- exp(rowMeans(log(cgsf)))
      }
    }

    cproj <- lapply(r.n,function(r) {
      x <- r$counts[,odgenes];
      if(var.scale) {
        x@x <- x@x*rep(cgsf,diff(x@p))
      }
      x
    })

    ## cproj <- lapply(r.n,function(r) {
    ##   x <- r$misc$rawCounts[,odgenes];
    ##   if(var.scale) {
    ##     x@x <- x@x*rep(cgsf,diff(x@p))
    ##   }
    ##   x
    ## })
    
     ## cproj <- lapply(r.n,function(r) {
     ##  x <- r$misc$rawCounts[,odgenes];
     ##  if(var.scale) {
     ##    x@x <- log10(x@x*rep(cgsf,diff(x@p))+1)
     ##  }
     ##  x
     ## })
    
    #ncells <- unlist(lapply(cproj,nrow));
    #centering <- colSums(do.call(rbind,lapply(cproj,colMeans))*ncells)/sum(ncells)

    ## cproj <- lapply(cproj,function(x) {
    ##     x <- t(as.matrix(t(x))-centering)
    ## })

    ## # Make sure all values are > 0
    ## cproj <- lapply(cproj, function(x) {
    ##     x <- x - min(x) + 1e-6
    ## })
    cproj <- lapply(cproj,function(x) as.matrix(x))
    
    z <- Rjnmf(t(cproj[[1]]),t(cproj[[2]]),k=ncomps, alpha=0.5,lambda=0.5, epsilon=0.001, maxiter=maxiter, verbose=F, seed=12345)

    rot1 <- cproj[[1]] %*% z$W
    rot2 <- cproj[[2]] %*% z$W

    list(rot1=rot1, rot2=rot2,z=z,cgsf=cgsf)

}

quickCPCA <- function(r.n,k=30,ncomps=100,n.odgenes=NULL,var.scale=T,verbose=T,cgsf=NULL,neighborhood.average=FALSE,n.cores=30) {
  require(parallel)
  require(cpca)
  require(Matrix)
  
  # select a common set of genes
  if(is.null(cgsf)) {
    if(is.null(n.odgenes)) {
      odgenes <- table(unlist(lapply(r.n,function(x) x$misc$odgenes)))
    } else {
      odgenes <- table(unlist(lapply(r.n,function(x) rownames(x$misc$varinfo)[(order(x$misc$varinfo$lp,decreasing=F)[1:min(ncol(x$counts),n.odgenes)])])))
    }
    odgenes <- odgenes[names(odgenes) %in% Reduce(intersect,lapply(r.n,function(x) colnames(x$counts)))]
    odgenes <- names(odgenes)[1:min(length(odgenes),n.odgenes)]
  } else {
    odgenes <- names(cgsf)
  }
  
  # common variance scaling
  if (var.scale) {
    if(is.null(cgsf)) {
      cgsf <- do.call(cbind,lapply(r.n,function(x) x$misc$varinfo[odgenes,]$gsf))
      cgsf <- exp(rowMeans(log(cgsf)))
    }
  }

  
  if(verbose) cat('calculating covariances for',length(r.n),' datasets ...')
  
  sparse.cov <- function(x){
    n <- nrow(x)
    cMeans <- Matrix::colMeans(x)
    covmat <- (as.matrix(Matrix::crossprod(x)) - n*Matrix::tcrossprod(cMeans))/(n-1)
  }
  
  covl <- lapply(r.n,function(r) {
    x <- r$counts[,odgenes];
    if(var.scale) {
      x@x <- x@x*rep(cgsf,diff(x@p))
    }
    if(neighborhood.average) {
      xk <- r$misc$edgeMat$quickCPCA;
      x <- t(xk) %*% x
    }

    sparse.cov(x)
  })
  if(verbose) cat(' done\n')
  
  ncells <- unlist(lapply(r.n,function(x) nrow(x$counts)));
  if(verbose) cat('common PCs ...')
  xcp <- cpca(covl,ncells,ncomp=ncomps)
  #system.time(xcp <- cpca:::cpca_stepwise_base(covl,ncells,k=ncomps))
  #xcp <- cpc(abind(covl,along=3),k=ncomps)
  rownames(xcp$CPC) <- odgenes;
  #xcp$rot <- xcp$CPC*cgsf;
  if(verbose) cat(' done\n')
  return(xcp);
}

cpcaJCp <- function(r.n, k=30, k.self=0, k.self.weight=1,community.detection.method = multilevel.community, reduction.method='CPCA', var.scale =TRUE, min.group.size = 10,ncomps=100, n.odgenes=1000, n.cores=30, return.details=F,xl=NULL,neighborhood.average=FALSE,neighborhood.average.k=10,groups=NULL, ...) {
  require(parallel)
  require(cpca)
  require(Matrix)
  require(abind)
  require(igraph)
  k1 <- k2 <- k;

  
  cis <- combn(names(r.n),2)
  if(is.null(xl)) {
    cat('pairwise',reduction.method)
    xl <- pagoda2:::papply(1:ncol(cis), function(i) {
      if(reduction.method=='CPCA') {
        xcp <- quickCPCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
      } else if(reduction.method=='JNMF') {
        xcp <- quickJNMF(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
      }
      cat('.')
      xcp
    },n.cores=n.cores);
    names(xl) <- apply(cis,2,paste,collapse='.vs.');
    cat("done\n")
  } else {
    # match for all the pairs
    mi <- rep(NA,ncol(cis));
    nm <- match(apply(cis,2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    nm <- match(apply(cis[c(2,1),,drop=F],2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    cat('matched',sum(!is.na(mi)),'out of',length(mi),'CPCA pairs ... ')
    if(any(is.na(mi))) {
      cat('running',sum(is.na(mi)),'additional CPCAs ')
      xl2 <- pagoda2:::papply(which(is.na(mi)), function(i) {
        if(reduction.method=='CPCA') {
          xcp <- quickCPCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
        } else if(reduction.method=='JNMF') {
          xcp <- quickJNMF(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
        }
        #xcp <- quickCPCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
        cat('.')
        xcp
      },n.cores=n.cores);
      names(xl2) <- apply(cis[,which(is.na(mi)),drop=F],2,paste,collapse='.vs.');
      xl <- c(xl,xl2);
    }
    # re-do the match and order
    mi <- rep(NA,ncol(cis));
    nm <- match(apply(cis,2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    nm <- match(apply(cis[c(2,1),,drop=F],2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    if(any(is.na(mi))) { stop("unable to get complete set of CPCA results") }
    xl <- xl[mi]
    cat(" done\n");
  }
  
  # run mNN separatly as it can't deal with multithreading
  cat('mNN ')
  mnnres <- lapply(1:ncol(cis), function(i) {
    r.ns <- r.n[cis[,i]]
    if(!is.null(xl[[i]]$rot1)) {
      # JNMF
      #system.time(yn <- pagoda2:::crossNN(x,x,k,2,2.0,verbose,n.cores))
      mnnres <- pagoda2:::interNN(xl[[i]]$rot1, xl[[i]]$rot2, k, k, 2, verbose=F,neighbourhoodAverage=neighborhood.average,neighbourAvgKA=neighborhood.average.k,neighbourAvgKB=neighborhood.average.k,TRUE)
      mnnres$mA.lab <- rownames(xl[[i]]$rot1)[mnnres$mA.id]
      mnnres$mB.lab <- rownames(xl[[i]]$rot2)[mnnres$mB.id]
    } else {
      if(!is.null(xl[[i]]$CPC)) {
        # CPCA
        rot <- xl[[i]]$CPC;
        odgenes <- rownames(xl[[i]]$CPC)
      } else if(!is.null(xl[[i]]$o$Q)) {
        # GSVD
        rot <- xl[[i]]$o$Q;
        odgenes <- rownames(rot) <- colnames(xl[[i]]$o$A);
      } else {
        stop("unknown reduction provided")
      }
      if (var.scale) {
        cgsf <- do.call(cbind,lapply(r.ns,function(x) x$misc$varinfo[odgenes,]$gsf))
        cgsf <- exp(rowMeans(log(cgsf)))
      }
      # create matrices, adjust variance
      cproj <- lapply(r.ns,function(r) {
        x <- r$counts[,odgenes];
        if(var.scale) {
          x@x <- x@x*rep(cgsf,diff(x@p))
        }
        x
      })
      ncells <- unlist(lapply(cproj,nrow));
      centering <- colSums(do.call(rbind,lapply(cproj,colMeans))*ncells)/sum(ncells)
      cpproj <- lapply(cproj,function(x) {
        x <- t(as.matrix(t(x))-centering)
        x %*% rot;
      })
      n1 <- cis[1,i]; n2 <- cis[2,i]
      cat(".")
      mnnres <- pagoda2:::interNN(cpproj[[n1]], cpproj[[n2]], k, k, 2, verbose=F,neighbourhoodAverage=neighborhood.average,neighbourAvgKA=neighborhood.average.k,neighbourAvgKB=neighborhood.average.k,TRUE)
      #mnnres <- pagoda2:::interNN(cpproj[[n1]], cpproj[[n2]], k1, k2, 2, verbose=F,neighborhood.average,neighborhood.average.k,neighborhood.average.k,TRUE)
      mnnres$mA.lab <- rownames(cpproj[[n1]])[mnnres$mA.id]
      mnnres$mB.lab <- rownames(cpproj[[n2]])[mnnres$mB.id]
    }
    mnnres
  })
  cat("done\n")
  ## Merge the results into a edge table
  #el <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab')]
  el <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab','dist')]
  colnames(el) <- c("mA.lab","mB.lab","w")
    

  el$w <- 1
  
  # append some local edges
  if(k.self>0) {
    cat('kNN pairs ')
    x <- data.frame(do.call(rbind,lapply(r.n,function(x) {
      xk <- pagoda2:::hnswKnn2(x$reductions$PCA,k.self,n.cores,verbose=F)
      xk <- xk[xk$s!=xk$e,]
      cat(".")
      cbind("mA.lab"=rownames(x$reductions$PCA)[xk$s+1],"mB.lab"=rownames(x$reductions$PCA)[xk$e+1])
    })),stringsAsFactors = F)
    x$w <- k.self.weight
    cat(' done\n')
    el <- rbind(el,x)
  }
  
  g  <- graph_from_edgelist(as.matrix(el[,c(1,2)]), directed =FALSE)
  E(g)$weight <- el[,3]
  
  ## Do community detection on this graph
  cat('detecting clusters ...');
  cls <- community.detection.method(g, ...)
  cat('done\n')
  ## Extract groups from this graph
  cls.mem <- membership(cls)
  cls.groups <- as.character(cls.mem)
  names(cls.groups) <- names(cls.mem)
  
  ## Filter groups
  lvls.keep <- names(which(table(cls.groups)  > min.group.size))
  cls.groups[! as.character(cls.groups) %in% as.character(lvls.keep)] <- NA
  cls.groups <- as.factor(cls.groups)
  
  if(return.details) {
    return(list(groups=cls.groups,xl=xl,cls=cls,g=g))
  } else {
    cls.groups
  }
  
}


cpcaJCp2 <- function(r.n, k=30, k.self=0, k.self.weight=1,community.detection.method = multilevel.community, reduction.method='CPCA', var.scale =TRUE, min.group.size = 10,ncomps=100, n.odgenes=1000, n.cores=30, return.details=F,xl=NULL,neighborhood.average=FALSE,neighborhood.average.k=5,groups=NULL, ...) {
  require(parallel)
  require(cpca)
  require(Matrix)
  require(abind)
  require(igraph)
  k1 <- k2 <- k;

  if(neighborhood.average) {
    cat("neighborhood averaging ")
    r.n <- lapply(r.n,function(r) {
      xk <- pagoda2:::crossNN(r$reductions$PCA,r$reductions$PCA,neighborhood.average.k,2,2.0,FALSE,n.cores)
      xk@x <- pmax(1-xk@x,0);
      diag(xk) <- 1;
      xk <- t(t(xk)/colSums(xk))
      colnames(xk) <- rownames(xk) <- rownames(r$reductions$PCA)
      r$misc$edgeMat$quickCPCA <- xk;
      cat(".")
      r
    })
    cat(" done\n")
  }
    

  cis <- combn(names(r.n),2)
  if(is.null(xl)) {
    cat('pairwise',reduction.method,' ')
    xl <- pagoda2:::papply(1:ncol(cis), function(i) {
      if(reduction.method=='CPCA') {
        xcp <- quickCPCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale,neighborhood.average=neighborhood.average)
      } else if(reduction.method=='JNMF') {
        xcp <- quickJNMF(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
      }
      cat('.')
      xcp
    },n.cores=n.cores);
    names(xl) <- apply(cis,2,paste,collapse='.vs.');
    cat("done\n")
  } else {
    # match for all the pairs
    mi <- rep(NA,ncol(cis));
    nm <- match(apply(cis,2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    nm <- match(apply(cis[c(2,1),,drop=F],2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    cat('matched',sum(!is.na(mi)),'out of',length(mi),'CPCA pairs ... ')
    if(any(is.na(mi))) {
      cat('running',sum(is.na(mi)),'additional CPCAs ')
      xl2 <- pagoda2:::papply(which(is.na(mi)), function(i) {
        if(reduction.method=='CPCA') {
          xcp <- quickCPCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale,neighborhood.average=neighborhood.average)
        } else if(reduction.method=='JNMF') {
          xcp <- quickJNMF(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
        }
        #xcp <- quickCPCA(r.n[cis[,i]],k=k,ncomps=ncomps,n.odgenes=n.odgenes,verbose=FALSE,var.scale=var.scale)
        cat('.')
        xcp
      },n.cores=n.cores);
      names(xl2) <- apply(cis[,which(is.na(mi)),drop=F],2,paste,collapse='.vs.');
      xl <- c(xl,xl2);
    }
    # re-do the match and order
    mi <- rep(NA,ncol(cis));
    nm <- match(apply(cis,2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    nm <- match(apply(cis[c(2,1),,drop=F],2,paste,collapse='.vs.'),names(xl));
    mi[which(!is.na(nm))] <- na.omit(nm);
    if(any(is.na(mi))) { stop("unable to get complete set of CPCA results") }
    xl <- xl[mi]
    cat(" done\n");
  }
  
  # run mNN separatly as it can't deal with multithreading
  cat('mNN ')
  mnnres <- lapply(1:ncol(cis), function(i) {
    r.ns <- r.n[cis[,i]]
    if(!is.null(xl[[i]]$rot1)) {
      # JNMF
      
      #mnnres <- pagoda2:::interNN(xl[[i]]$rot1, xl[[i]]$rot2, k, k, 2, verbose=F,neighbourhoodAverage=neighborhood.average,neighbourAvgKA=neighborhood.average.k,neighbourAvgKB=neighborhood.average.k,TRUE)
      #mnnres$mA.lab <- rownames(xl[[i]]$rot1)[mnnres$mA.id]
      #mnnres$mB.lab <- rownames(xl[[i]]$rot2)[mnnres$mB.id]

      n12 <- pagoda2:::crossNN(xl[[i]]$rot1,xl[[i]]$rot2,k,2,2.0,FALSE,n.cores)
      n21 <- pagoda2:::crossNN(xl[[i]]$rot2,xl[[i]]$rot1,k,2,2.0,FALSE,n.cores)
      mnn <- drop0(n21*t(n12))
      mnn <- as(n21*t(n12),'dgTMatrix')
      return(data.frame('mA.lab'=rownames(cpproj[[n1]])[mnn@i+1],'mB.lab'=rownames(cpproj[[n2]])[mnn@j+1],'w'=pmax(1-mnn@x,0),stringsAsFactors=F))
    } else {
      if(!is.null(xl[[i]]$CPC)) {
        # CPCA
        rot <- xl[[i]]$CPC;
        odgenes <- rownames(xl[[i]]$CPC)
      } else if(!is.null(xl[[i]]$o$Q)) {
        # GSVD
        rot <- xl[[i]]$o$Q;
        odgenes <- rownames(rot) <- colnames(xl[[i]]$o$A);
      } else {
        stop("unknown reduction provided")
      }
      if (var.scale) {
        cgsf <- do.call(cbind,lapply(r.ns,function(x) x$misc$varinfo[odgenes,]$gsf))
        cgsf <- exp(rowMeans(log(cgsf)))
      }
      # create matrices, adjust variance
      cproj <- lapply(r.ns,function(r) {
        x <- r$counts[,odgenes];
        if(var.scale) {
          x@x <- x@x*rep(cgsf,diff(x@p))
        }
        if(neighborhood.average) {
          xk <- r$misc$edgeMat$quickCPCA;
          x <- t(xk) %*% x
        }
        x
      })
      ncells <- unlist(lapply(cproj,nrow));
      centering <- colSums(do.call(rbind,lapply(cproj,colMeans))*ncells)/sum(ncells)
      cpproj <- lapply(cproj,function(x) {
        x <- t(as.matrix(t(x))-centering)
        x %*% rot;
      })
      n1 <- cis[1,i]; n2 <- cis[2,i]
      cat(".")

      #mnnres <- pagoda2:::interNN(cpproj[[n1]], cpproj[[n2]], k, k, 2, verbose=F,neighbourhoodAverage=neighborhood.average,neighbourAvgKA=neighborhood.average.k,neighbourAvgKB=neighborhood.average.k,TRUE)
      #mnnres <- pagoda2:::interNN(cpproj[[n1]], cpproj[[n2]], k1, k2, 2, verbose=F,neighborhood.average,neighborhood.average.k,neighborhood.average.k,TRUE)
      #mnnres$mA.lab <- rownames(cpproj[[n1]])[mnnres$mA.id]
      #mnnres$mB.lab <- rownames(cpproj[[n2]])[mnnres$mB.id]
      
      n12 <- pagoda2:::crossNN(cpproj[[n1]],cpproj[[n2]],k,2,2.0,FALSE,n.cores)
      n21 <- pagoda2:::crossNN(cpproj[[n2]],cpproj[[n1]],k,2,2.0,FALSE,n.cores)
      #colnames(n12) <- rownames(n21) <- rownames(cpproj[[n1]])
      #colnames(n21) <- rownames(n12) <- rownames(cpproj[[n2]])
      mnn <- drop0(n21*t(n12))
      mnn <- as(n21*t(n12),'dgTMatrix')
      return(data.frame('mA.lab'=rownames(cpproj[[n1]])[mnn@i+1],'mB.lab'=rownames(cpproj[[n2]])[mnn@j+1],'w'=pmax(1-mnn@x,0),stringsAsFactors=F))
      
    }
    mnnres
  })
  cat("done\n")
  ## Merge the results into a edge table
  #el <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab')]
  #el <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab','dist')]
  #colnames(el) <- c("mA.lab","mB.lab","w")
  el <- do.call(rbind,mnnres)

  #el$w <- 1
  
  # append some local edges
  if(k.self>0) {
    cat('kNN pairs ')
    x <- data.frame(do.call(rbind,lapply(r.n,function(x) {
      xk <- pagoda2:::crossNN(x$reductions$PCA,x$reductions$PCA,k.self,2,2.0,FALSE,n.cores)
      diag(xk) <- 0;
      xk <- as(xk,'dgTMatrix')
      cat(".")
      return(data.frame('mA.lab'=rownames(x$reductions$PCA)[xk@i+1],'mB.lab'=rownames(x$reductions$PCA)[xk@j+1],'w'=pmax(1-xk@x,0),stringsAsFactors=F))
    })),stringsAsFactors = F)
    x$w <- k.self.weight
    cat(' done\n')
    el <- rbind(el,x)
  }
  
  g  <- graph_from_edgelist(as.matrix(el[,c(1,2)]), directed =FALSE)
  E(g)$weight <- el[,3]
  
  ## Do community detection on this graph
  cat('detecting clusters ...');
  cls <- community.detection.method(g, ...)
  cat('done\n')
  ## Extract groups from this graph
  cls.mem <- membership(cls)
  cls.groups <- as.character(cls.mem)
  names(cls.groups) <- names(cls.mem)
  
  ## Filter groups
  lvls.keep <- names(which(table(cls.groups)  > min.group.size))
  cls.groups[! as.character(cls.groups) %in% as.character(lvls.keep)] <- NA
  cls.groups <- as.factor(cls.groups)
  
  if(return.details) {
    return(list(groups=cls.groups,xl=xl,cls=cls,g=g))
  } else {
    cls.groups
  }
  
}


#' Side by side plot jointly called clusters from getJointClustering()
#' @param r.n the named list of the pagoda2 jointly called clusters
#' @param cl the clusters factor as returned by getJointClustering()
#' @return NULL
#' @export plotJointClustering
plotJointClustering <- function(r.n, cl, alpha =0.3, main=NULL) {
    require('RColorBrewer')
    require('scales')

    l <- length(r.n)
    if (l < 4) {
        par(mfrow=c(1,l));
    } else {
        plot.size <- ceiling(sqrt(l))
        par(mfrow=c(plot.size,plot.size))
    }

    pal <- rainbow(nlevels(cl), v = 0.8)
    pal <- alpha(pal, alpha)
    lapply(r.n, function(r) {
        plot(r$embeddings$PCA$tSNE, col = pal[cl[rownames(r$embeddings$PCA$tSNE)]], pch=20, cex=2,main=main)
    });

    invisible(NULL);
}

#' This function identifies marker genes that support joint clustering
#' among different pagoda2 application
#' @description This function performs differential expression between clusters
#' identified with getJointClustering() and looks for genes that are consistently
#' differentially regulated between different application
#' @param applist a list of Pagoda2 object
#' @param js a factor with joint clusters as returned by getJointClustering()
#' @return a list of markers for each cluster
#' @export getJointClusterMarkerGenes
getJointClusterMarkerGenes <- function(applist, jc) {
    t <- lapply(applist, function(x) {x$getDifferentialGenes(groups=jc)})

    res <- list()
    for (lvl in levels(jc)) {
        t2 <- lapply(t, function(x) {rownames(x[[lvl]])})
        res[[lvl]] <- Reduce(intersect, t2)
    }

    res
}

#' Get mutual neighbours for 2 pagoda2 objects
#' @description Given two pagoda2 objects, place the objects on a common space
#' of variation using GSVD and identify pairs of mutual neighbours, that are then
#' returned as a data.frame
#' @param r1 first pagoda object
#' @param r2 second pagoda object
#' @param var.scale option to scale for variance
#' @param k number of nearest neighbours
#' @param log.scale work on logarithmic scale - note, this does not take into account log scale option of the pagoda2
#' @param center center the data
#' @param verbose verbosity level
#' @param ncomps number of components form the gsvd to use
#' @return a data frame with all the pairs of the mutual neighbours
#' @export getMNNforP2pair
getMNNforP2pair <- function(r1, r2, var.scale = T , n.odgenes=NULL, k = 30, log.scale=F, center=T, verbose =T, ncomps = 100, plot.projection = F, return.details = T, skipMNN=F) {
    require('plyr')
    require('geigen')

    ## Check r1 and r2 objects
    if( !(class(r1) == 'Pagoda2' & class(r2) == 'Pagoda2')) {
        stop('Both r1 and r2 have to be objects of type Pagoda2');
    }

    ## Add check for r1labels

    if (verbose) cat('Preparing data... ');

    ## For the moment use the same k for both datasets
    k1 <- k
    k2 <- k

    ## Get overdispersed genes, or some other relevant geneset
    if(is.null(n.odgenes)) {
      odgenes <- union(r1$misc$odgenes, r2$misc$odgenes)
    } else {
      odgenes <- unique(rownames(r1$misc[['varinfo']])[(order(r1$misc[['varinfo']]$lp,decreasing=F)[1:min(ncol(r1$counts),n.odgenes)])],
                        rownames(r2$misc[['varinfo']])[(order(r2$misc[['varinfo']]$lp,decreasing=F)[1:min(ncol(r2$counts),n.odgenes)])])
    }
    odgenes <- intersect(odgenes, intersect(colnames(r1$counts),colnames(r2$counts)))

    ## Get matrices
    x1 <- r1$counts[,odgenes]
    x2 <- r2$counts[,odgenes]

    ## Optionally variance scale
    if (var.scale) {
      # need to use a common scaling ... can add dataset size or variance weighting
      cgsf <- sqrt(r1$misc$varinfo[odgenes,]$gsf * r2$misc$varinfo[odgenes,]$gsf); 
      x1@x <- x1@x*rep(cgsf,diff(x1@p))
      x2@x <- x2@x*rep(cgsf,diff(x2@p))
    }

    ## Optionally log scale
    if (log.scale) {
        x1@x <- log10(x1@x + 1);
        x2@x <- log10(x2@x + 1);
    }

    ## Remove mean, jointly
    if (center) {
        x1.colMean <- Matrix::colMeans(x1)
        x2.colMean <- Matrix::colMeans(x2)
        means <- (x1.colMean + x2.colMean) /2
        x1@x <- x1@x - rep(means,diff(x1@p))
        x2@x <- x2@x - rep(means,diff(x2@p))
    }

    ## Convert to full matrix
    x1 <- as.matrix(x1);
    x2 <- as.matrix(x2);

    if (verbose) cat('done (',ncol(x1),'genes)\n');

    if (verbose) cat('Performing GSVD... ');
    ## Perform gsvd on the two matrices
    o <- gsvd(x1,x2)
    if (verbose) cat('done\n');

    ## Calculate rotated matrices
    if (verbose) cat('Calculating PCs... ');
    #x1.rot <- o$U %*% gsvd.D1(o) %*% gsvd.R(o)
    #x2.rot <- o$V %*% gsvd.D2(o) %*% gsvd.R(o)

    # This is the same thing but faster
    x1.rot <- x1 %*% o$Q
    x2.rot <- x2 %*% o$Q
    if (verbose) cat('done\n');

    # If ncomps is specified take only those from each dataset
    if (!is.null(ncomps)) {
        D1v <- apply(x1.rot,2,var)
        D2v <- apply(x2.rot,2,var)
        D1v <- D1v/sum(D1v)
        D2v <- D2v/sum(D2v)
        if (ncomps > length(D1v)) {
          warn('More components than available were specified');
          ncomps <- length(D1v)
        }
        comps.keep <- order(D1v+D2v,decreasing=T)[1:ncomps]
        
        # D1 <- gsvd.D1(o)
        # D2 <- gsvd.D2(o)
        # 
        # D1d <- diag(D1);
        # D2d <- diag(D2);
        # 
        # if (ncomps > length(D1d)) {
        #     warn('More components than available were specified');
        #     ncomps <- length(D1d);
        # }
        # 
        # D1.cutoff <- sort(D1d,decr=T)[ncomps]
        # D2.cutoff <- sort(D2d,decr=T)[ncomps]
        # 
        # comps.keep <- c(which(D1d > D1.cutoff), which(D2d > D2.cutoff))

        if (plot.projection) {
            if (comps.keep <= 5) {
            plot(as.data.frame(rbind(x1.rot,x2.rot)[,comps.keep]),
                 col=c(rep('red',dim(x1.rot)[1]),rep('blue',dim(x2.rot)[1])),
                 pch=16,cex=0.2)
            } else {
                warn('plot.projection set to TRUE and comps.keep > 5: Can\'t plot that many components')
            }
        }

        x1.rot <- x1.rot[,comps.keep]
        x2.rot <- x2.rot[,comps.keep]
    }

    if(skipMNN) {return(list(o=o,x1.rot=x1.rot,x2.rot=x2.rot)) }
    
    ## In this space assign cells by mutual NNs
    if (verbose) {cat('Finding MNNs... ');}
    mnnres <- pagoda2:::interNN(x1.rot,x2.rot,k1,k2,2,verbose=F)
    if (verbose) cat('done\n');

    mnnres$mA.lab <- rownames(r1$counts)[mnnres$mA.id]
    mnnres$mB.lab <- rownames(r2$counts)[mnnres$mB.id]
    
    if(return.details) {
      return(list(mnnres=mnnres,o=o,x1.rot=x1.rot,x2.rot=x2.rot))
    } else {
      return(mnnres)
    }
}
