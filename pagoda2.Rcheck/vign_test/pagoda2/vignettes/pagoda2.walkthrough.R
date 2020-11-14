## -----------------------------------------------------------------------------
library(Matrix)
library(pagoda2)
library(igraph)

## -----------------------------------------------------------------------------
cm <- readRDS(file.path(find.package('pagoda2'), 'extdata', 'sample_BM1.rds'))
dim(cm)

## -----------------------------------------------------------------------------
cm[1:3, 1:3]

## -----------------------------------------------------------------------------
str(cm)

## -----------------------------------------------------------------------------
par(mfrow=c(1,2), mar = c(3.5,3.5,2.0,0.5), mgp = c(2,0.65,0), cex = 1.0)
hist(log10(colSums(cm)+1), main='molecules per cell', col='cornsilk', xlab='log10(molecules per cell)')
hist(log10(rowSums(cm)+1), main='molecules per gene', col='cornsilk', xlab='log10(molecules per gene])')

## -----------------------------------------------------------------------------
counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)

## -----------------------------------------------------------------------------
hist(log10(rowSums(counts)+1), main='Molecules per gene', xlab='molecules (log10)', col='cornsilk')
abline(v=1, lty=2, col=2)

## -----------------------------------------------------------------------------
counts <- counts[rowSums(counts)>=10, ]
dim(counts)

## -----------------------------------------------------------------------------
rownames(counts) <- make.unique(rownames(counts))
r <- Pagoda2$new(counts,log.scale=TRUE, n.cores=2)

## -----------------------------------------------------------------------------
r$adjustVariance(plot=TRUE, gam.k=10)

## -----------------------------------------------------------------------------
r$calculatePcaReduction(nPcs=50, n.odgenes=3e3)

## -----------------------------------------------------------------------------
r$makeKnnGraph(k=40, type='PCA', center=TRUE, distance='cosine')

## -----------------------------------------------------------------------------
r$getKnnClusters(method=infomap.community, type='PCA')

## -----------------------------------------------------------------------------
M <- 30
r$getEmbedding(type='PCA', embeddingType = 'largeVis', M=M, perplexity=30, gamma=1/M, alpha=1)

## -----------------------------------------------------------------------------
r$plotEmbedding(type='PCA', show.legend=FALSE, mark.clusters=TRUE, min.group.size=50, shuffle.colors=FALSE, mark.cluster.cex=1, alpha=0.1, main='clusters (largeVis)')

## -----------------------------------------------------------------------------
r$getEmbedding(type='PCA', embeddingType='tSNE', perplexity=50,verbose=FALSE)
r$plotEmbedding(type='PCA', embeddingType='tSNE', show.legend=FALSE, mark.clusters=TRUE, min.group.size=1, shuffle.colors=FALSE, mark.cluster.cex=1, alpha=0.1, main='clusters (tSNE)')

## -----------------------------------------------------------------------------
gene <-"HBB"
r$plotEmbedding(type='PCA', embeddingType='tSNE', colors=r$counts[,gene], shuffle.colors=FALSE, mark.cluster.cex=1, alpha=0.1, main=gene)

## -----------------------------------------------------------------------------
gene <-"LYZ"
r$plotEmbedding(type='PCA', embeddingType='tSNE', colors=r$counts[,gene], shuffle.colors=FALSE, mark.cluster.cex=1, alpha=0.1, main=gene)

## -----------------------------------------------------------------------------
r$getKnnClusters(method=multilevel.community, type='PCA', name='multilevel')
r$getKnnClusters(method=walktrap.community, type='PCA', name='walktrap')

## -----------------------------------------------------------------------------
str(r$clusters)

## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
r$plotEmbedding(type='PCA', embeddingType='tSNE', groups=r$clusters$PCA$community, show.legend=FALSE, mark.clusters=TRUE, min.group.size=1, shuffle.colors=FALSE, mark.cluster.cex=1, alpha=0.1, main='infomap clusters (tSNE)')
r$plotEmbedding(type='PCA',embeddingType='tSNE', clusterType='multilevel', show.legend=FALSE, mark.clusters=TRUE, min.group.size=1, shuffle.colors=FALSE, mark.cluster.cex=1, alpha=0.1, main='multlevel clusters (tSNE)')

## -----------------------------------------------------------------------------
r$getDifferentialGenes(type='PCA', verbose=TRUE, clusterType='community')

## -----------------------------------------------------------------------------
de <- r$diffgenes$PCA[[1]][['2']]
r$plotGeneHeatmap(genes=rownames(de)[1:15], groups=r$clusters$PCA[[1]])

## -----------------------------------------------------------------------------
gene <-"CD74"
r$plotEmbedding(type='PCA', embeddingType='tSNE', colors=r$counts[,gene], shuffle.colors=FALSE, mark.cluster.cex=1, alpha=0.1, main=gene)

