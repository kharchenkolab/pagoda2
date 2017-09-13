
#' @export getEmbeddingPlotWithClusters
getEmbeddingPlotWithClusters <- function(embedding, clusters) {
  dft1 <- as.data.frame(embedding)
  dft1$cluster <- clusters[rownames(dft1)]
  p1 <- ggplot(dft1, aes(x=V1, y=V2, color=cluster)) + geom_point() + theme_bw()

  p1
}

#' @export getClusterMatch
getClusterMatch <- function(r1, r2, cls1, cls2, var.scale=F, k = 100, log.scale=T, center = T) {
  require('plyr')

  if( !(class(r1) == 'Pagoda2' & class(r2) == 'Pagoda2')) {
    stop('Both r1 and r2 have to be objects of type Pagoda2');
  }

  ## Get union of odgenes present in both
  odgenes <- union(r1$misc$odgenes, r2$misc$odgenes)
  odgenes <- intersect(odgenes, colnames(r1$counts))
  odgenes <- intersect(odgenes, colnames(r2$counts))

  ## Get matrices
  x1 <- r1$counts[,odgenes]
  x2 <- r2$counts[,odgenes]

  ## Remove mean and convert to full matrix
  if (center) {

    colmean <- (Matrix::colMeans(x1) + Matrix::colMeans(x2))

    x1 <- sweep(x1, 2, colmean, FUN='-')
    x2 <- sweep(x2, 2, colmean, FUN='-')
  }


  if (var.scale) {
    x1 <- sweep(x1, 2, r1$misc$varinfo[odgenes,]$gsf, FUN='*')
    x2 <- sweep(x2, 2, r2$misc$varinfo[odgenes,]$gsf, FUN='*')
  }

  pseudocount <- 1e-6;
  if (log.scale) {
    x1 <- log10(x1 + pseudocount);
    x2 <- log10(x2 + pseudocount);
  }

  x1 <- as.matrix(x1);
  x2 <- as.matrix(x2);


  ## Find mutual neighbours using constant k
  res <- pagoda2:::mutualNN(x1,x2, k, k)

  ## Annotate results with cluster labels
  res$cluster1 <- cls1[rownames(x1)[res$mA.id]]
  res$cluster2 <- cls2[rownames(x2)[res$mB.id]]

  ## Total number of cells in each cluster for normalisation
  clusterCountsA <- table(cls1[rownames(x1)])
  clusterCountsB <- table(cls2[rownames(x2)])

  ## Calculate percent of cells connected
  resSummary <- ddply(res, .(cluster1, cluster2), function(x) {
    c(uA=length(unique(x$mA.id)), uB=length(unique(x$mB.id)))
  }, .drop=F)

  ## Per cluster counts
  resSummary$count1 <- as.numeric(clusterCountsA[resSummary$cluster1])
  resSummary$count2 <- as.numeric(clusterCountsB[resSummary$cluster2])

  ## Calculate percentages
  resSummary$pc1 <- resSummary$uA / resSummary$count1;
  resSummary$pc2 <- resSummary$uB / resSummary$count2;

  resSummary$pc1[is.na(resSummary$pc1)] <- 0;
  resSummary$pc2[is.na(resSummary$pc2)] <- 0;

  list(full=res, summarised = resSummary)
}

#' @export plotClusterMatch
plotClusterMatch <- function(clusterMatch, cutoff = 0.25, plot =T ) {
  sumData <- clusterMatch$summarised

  ## All connections
  p0 <- ggplot(sumData) +
    geom_segment(aes(y=cluster1, yend=cluster2, x=0, xend=100, color=cluster1, size=pc1),alpha=0.25) +
    geom_segment(aes(y=cluster1, yend=cluster2, x=0, xend=100, color=cluster2, size=pc2),alpha=0.25) +
    theme_bw() + scale_size_continuous(range = c(0,5)) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    ggtitle('All connections') + scale_x_continuous(name='') + scale_y_discrete(name='')

  p0

  ## Connections filtered for pc1
  a <- sumData
  a[a$pc1 < cutoff,]$pc1 <- 0
  p1 <- ggplot(a, aes(y=cluster1, yend=cluster2, x=0, xend=100, color=cluster1, size=pc1)) +
    geom_segment(alpha=0.5) + theme_bw() + scale_size_continuous(range = c(0,5)) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    ggtitle('Filtered for Clusters 1') + scale_x_continuous(name='') + scale_y_discrete(name='')

  ## Connections filtered for pc2
  b <- sumData
  b[b$pc2 < cutoff,]$pc2 <- 0
  p2 <- ggplot(b, aes(y=cluster1, yend=cluster2, x=0, xend=100, color=cluster2, size=pc2)) +
    geom_segment(alpha=0.5) + theme_bw() + theme_bw() + scale_size_continuous(range = c(0,5)) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    ggtitle('Filtered for Cluster 2') + scale_x_continuous(name='') + scale_y_discrete(name='')

  ## Connections filtered for pc1 and pc2
  c <- sumData
  c[c$pc1 < cutoff,]$pc1 <- 0
  c[c$pc1 < cutoff,]$pc2 <- 0
  c[c$pc2 < cutoff,]$pc1 <- 0
  c[c$pc2 < cutoff,]$pc2 <- 0
  p3 <- ggplot(c, aes(y=cluster1, yend=cluster2, x=0, xend=100, color=cluster1, size=(pc1+pc2)/2)) +
    geom_segment(alpha=0.5) + theme_bw() + theme_bw() + scale_size_continuous(range = c(0,5)) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    ggtitle('Filtered for Clusters 1 and 2') + scale_x_continuous(name='') + scale_y_discrete(name='')

  if (plot) {
    grid.arrange(p0, p1,p2,p3,ncol=2)
  }


  invisible(list(p0,p1,p2,p3));
}

#' @export getMaximalMatch
getMaximalMatch <- function(clusterMatch) {
  require('plyr')
  sumData <- clusterMatch$summarised

  r <- ddply(sumData, .(cluster2), function(x) {
    x[which.max(x$pc2),]
  })[,c('cluster1','cluster2')]

  names(r) <- c('reference', 'matched')

  r
}


#' @export callClusterMatch
callClusterMatch <- function (clm)
{
  require(plyr)
  summarize.cell <- ddply(clm$full, .(mA.id), function(x) {
    t <- table(x$cluster2)
    data.frame(cluster = as.character(x$cluster1[1]), referenceMatch = names(t)[which.max(t)])
  })

  called.clusters <- ddply(summarize.cell, .(cluster), function(x) {
    t <- table(x$referenceMatch)
    data.frame(cluster = x$cluster[1], match = names(t)[which.max(t)])
  })

  called.clusters
}

#' @export callClusterMatchRev
callClusterMatchRev <- function (clm)
{
  require(plyr)
  summarize.cell <- ddply(clm$full, .(mB.id), function(x) {
    t <- table(x$cluster1)
    data.frame(cluster = as.character(x$cluster2[1]), referenceMatch = names(t)[which.max(t)])
  })
  called.clusters <- ddply(summarize.cell, .(cluster), function(x) {
    t <- table(x$referenceMatch)
    data.frame( cluster = names(t)[which.max(t)], match = x$cluster[1],)
  })
  called.clusters
}

#' @export makeFactorFromCalledClusterMatch
makeFactorFromCalledClusterMatch <- function(calledClusterMatch, clustersMatched)  {
  ## z is the map of clusters
  z <- as.character(calledClusterMatch$cluster)
  names(z) <- calledClusterMatch$match

  ## convert clustersMatched to character for matching
  clusMatched <- as.character(clustersMatched)
  names(clusMatched) <- names(clustersMatched)

  ## Do matching and convert back to factor
  zp <- z[clusMatched]
  names(zp) <- names(clusMatched)
  zp <- as.factor(zp)

  zp
}

makeFactorFromCalledClusterMatchRev <- function (calledClusterMatch, clustersMatched)
{
  z <- as.character(calledClusterMatch$match)
  names(z) <- calledClusterMatch$cluster

  clusMatched <- as.character(clustersMatched)
  names(clusMatched) <- names(clustersMatched)


  zp <- z[clusMatched]
  names(zp) <- names(clusMatched)
  zp <- as.factor(zp)
  zp
}


#' Identify cells in GSVD space with MNN summarisation
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
identifyCellsGSVDMNN <- function(referenceP2, r2, referenceP2labels, var.scale = T, k = 30, log.scale = T, center =T, verbose = T,
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
  x1.rot <- o$U %*% gsvd.D1(o) %*% gsvd.R(o)
  x2.rot <- o$V %*% gsvd.D2(o) %*% gsvd.R(o)
  rownames(x1.rot) <- rownames(x1)
  rownames(x2.rot) <- rownames(x2)
  if (verbose) cat('done\n');


  ## In this space assign cells by mutual NNs
  if (verbose) {cat('Finding MNNs... ');}
  mnnres <- pagoda2:::mutualNN(x1.rot,x2.rot,k1,k2,2,verbose=F)
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

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' identify cells using teh identifyCellsGSVDMNN function using multiple input
#' reference sets
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


#' Get mutual neighbours for 2 pagoda2 objects
#' @param r1 first pagoda object
#' @param r2 second pagoda object
#' @param var.scale option to scale for variance
#' @param k number of nearest neighbours
#' @param log.scale work on logarithmic scale
#' @param center center the data
#' @param verbose verbosity level
#' @return a data frame with all the pairs of the mutual neighbours
#' @export getMNNforP2pair
getMNNforP2pair <- function(r1, r2, var.scale =T , k = 30, log.scale=T, center=T, verbose =T) {
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
  odgenes <- union(r1$misc$odgenes, r2$misc$odgenes)
  odgenes <- intersect(odgenes, colnames(r1$counts))
  odgenes <- intersect(odgenes, colnames(r2$counts))

  ## Get matrices
  x1 <- r1$counts[,odgenes]
  x2 <- r2$counts[,odgenes]

  ## Optionally variance scale
  if (var.scale) {
    x1 <- sweep(x1, 2, r1$misc$varinfo[odgenes,]$gsf, FUN='*')
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
  x1.rot <- o$U %*% gsvd.D1(o) %*% gsvd.R(o)
  x2.rot <- o$V %*% gsvd.D2(o) %*% gsvd.R(o)
  rownames(x1.rot) <- rownames(x1)
  rownames(x2.rot) <- rownames(x2)
  if (verbose) cat('done\n');


  ## In this space assign cells by mutual NNs
  if (verbose) {cat('Finding MNNs... ');}
  mnnres <- pagoda2:::mutualNN(x1.rot,x2.rot,k1,k2,2,verbose=F)
  if (verbose) cat('done\n');

  mnnres$mA.lab <- rownames(r1$counts)[mnnres$mA.id]
  mnnres$mB.lab <- rownames(r2$counts)[mnnres$mB.id]

  mnnres
}

#' Obtain joint clustering between two samples by constructing MNN network
#' and calling clusters on that
#' @param r.n a named list of pagoda2 apps
#' @param k number of KNN to look for
#' @param community.detection.method one of the igraph package community detection methods (or compatible fn)
#' @param min.group.size minimum group size to keep after matching
#' @return a named (by cell name) factor of groups
#' @export get.joint.clustering
get.joint.clustering <- function(r.n, k=30, community.detection.method = walktrap.community,
                                 min.group.size = 10) {
  require('gtools')
  require('pbapply')
  require('igraph')
  ## Get all non-redundant pair of apps
  nms <- names(r.n)
  combs <- combinations(n = length(nms), r = 2, v = nms, repeats.allowed =F)
  ## Convert to list for lapply
  combsl <- split(t(combs), rep(1:nrow(combs), each=ncol(combs)))

  ## get MNN pairs from all possible app pairs

  cat('Calculating MNN for application pairs ...\n')
  mnnres <- pblapply(combsl, function(x) {
    getMNNforP2pair(r[[x[1]]], r[[x[2]]], k = k, verbose =F);
  });

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

  cls.groups
}

#' Side by side plot jointly called clusters from get.joint.clusters()
#' @param r.n the named list of the pagoda2 jointly called clusters
#' @param cl the clusters factor as returned by get.joint.clusters()
#' @return NULL
#' @export plotJointClusters
plotJointClusters <- function(r.n, cl) {
  require('RColorBrewer')

  l <- length(r.n)
  if (l < 4) {
    par(mfrow=c(1,l));
  } else {
    plot.size <- ceiling(sqrt(l))
    par(mfrow=c(plot.size,plot.size))
  }

  pal <- rainbow(nlevels(cl), v = 0.8)
  lapply(r.n, function(r) {
    plot(r$embeddings$PCA$tSNE, col = pal[cl[rownames(r$embeddings$PCA$tSNE)]], pch=20, cex=2)
  });

  invisible(NULL);
}
