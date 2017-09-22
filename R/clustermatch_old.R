
#### Old Functions ####
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


