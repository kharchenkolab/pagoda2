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

## -----------------------------------------------------------------------------
suppressMessages(library(org.Hs.eg.db))
# translate gene names to ids
ids <- unlist(lapply(mget(colnames(r$counts), org.Hs.egALIAS2EG, ifnotfound=NA), function(x) x[1]))
# reverse map
rids <- names(ids)
names(rids) <- ids
# list all the ids per GO category
go.env <- list2env(eapply(org.Hs.egGO2ALLEGS,function(x) as.character(na.omit(rids[x]))))

## -----------------------------------------------------------------------------
## DON'T RUN
## test over dispersion
# r$testPathwayOverdispersion(go.env, verbose=TRUE, correlation.distance.threshold=0.95, recalculate.pca=FALSE, top.aspects=15)

## -----------------------------------------------------------------------------
hdea <- r$getHierarchicalDiffExpressionAspects(type='PCA', clusterName='community', z.threshold=3)

## -----------------------------------------------------------------------------
genesets <- hierDiffToGenesets(hdea)
str(genesets[1:2])

## -----------------------------------------------------------------------------
library(GO.db)
termDescriptions <- Term(GOTERM[names(go.env)]) # saves a good minute or so compared to individual lookups
sn <- function(x) { names(x) <- x; x}
genesets.go <- lapply(sn(names(go.env)),function(x) {
  list(properties=list(locked=TRUE, genesetname=x, shortdescription=as.character(termDescriptions[x])), genes=c(go.env[[x]]))
})
## concatenate
genesets <- c(genesets, genesets.go)

## -----------------------------------------------------------------------------
deSets <- get.de.geneset(r, groups = r$clusters$PCA[['community']], prefix = 'de_')
## concatenate
genesets <- c(genesets, deSets)

## -----------------------------------------------------------------------------
appmetadata <- list(apptitle = 'October_Demo_App')

## -----------------------------------------------------------------------------
r$makeGeneKnnGraph(n.cores = 2)

## -----------------------------------------------------------------------------
# Make a list for our metadata
additionalMetadata <- list()
## for Infomap use hue values from 0.1 to 0.5
additionalMetadata$community <- p2.metadata.from.factor(r$clusters$PCA[['community']], displayname = 'Infomap', s = 0.7, v = 0.8,start = 0.1, end = 0.5)
# use different colors for multilevel
additionalMetadata$multilevel <- p2.metadata.from.factor(r$clusters$PCA[['multilevel']], displayname = 'Multilevel', s = 0.9, v = 0.8,start = 0.5, end = 1)
## Manual palette generation for walktrap
a <- r$clusters$PCA[['walktrap']]
library(colorRamps)
p1 <- colorRamps::primary.colors(n = nlevels(a))
names(p1) <- levels(a)
additionalMetadata$walktrap <- p2.metadata.from.factor(r$clusters$PCA[['walktrap']], displayname = 'Walktrap', pal = p1)

## -----------------------------------------------------------------------------
p2web <-
  make.p2.app(
    r,
    dendrogramCellGroups = r$clusters$PCA$community,
    additionalMetadata = additionalMetadata,
    geneSets = genesets,
    appmetadata = appmetadata,
    show.clusters = FALSE # Hide the clusters that were used for the dendrogram from the metadata
  );

## -----------------------------------------------------------------------------
show.app(app=p2web, name='app')

## -----------------------------------------------------------------------------
p2web$serializeToStaticFast('october2018_pbmc.bin', verbose=TRUE)

## -----------------------------------------------------------------------------
saveRDS(r, 'pagoda2object.rds')

## ----eval=FALSE---------------------------------------------------------------
#  
#  # Get your count matrix in sparse or full format
#  # For example you can use the
#  # readMM() from the Matrix package or the
#  # read10xMatrix() function from the github barkasn/nbHelpers package
#  # This matrix can be sparse matrix object from the Matrix package
#  
#  # countMatrix <- load_my_count_matrix()
#  # Rows correspond to genes and Columns to cells
#  # If you experience problems with duplicate cells you can use
#  # rownames(countMatrix) <- make.unique(rownames(countMatrix))
#  
#  p2 <- basicP2proc(countMatrix, n.cores = 4) ## 'multilevel' run here
#  
#  p2$getKnnClusters(method = igraph::infomap.community, type = 'PCA' ,name = 'infomap')
#  p2$getKnnClusters(method = igraph::walktrap.community, type = 'PCA', name = 'walktrap')
#  
#  ext.res <- extendedP2proc(p2, n.cores = 4, organism = 'hs')
#  
#  
#  p2 <- ext.res$p2
#  go.env <- ext.res$go.env
#  rm(ext.res)
#  
#  # Make cell metadata from the default clusters generated during basicP2proc()
#  # This is optional, metadata.forweb can also be NULL
#  metadata.listfactors <- list(
#      infomap = p2$clusters$PCA$infomap,
#      multilevel = p2$clusters$PCA$multilevel,
#      walktrap = p2$clusters$PCA$walktrap
#  );
#  metadata.forweb <- factorListToMetadata(metadata.listfactors)
#  
#  # Make the web object
#  p2.webobject <- webP2proc(p2, additionalMetadata = metadata.forweb, title = 'Quick pagoda2 application', go.env = go.env)
#  
#  # Serialize to file
#  # The serialisedApp.bin file will now contain all the information
#  # required to view the files via the web browser
#  p2.webobject$serializeToStaticFast('serialisedApp.bin');
#  
#  # Alternatively you can view your dataset from the R session
#  # show.app(p2.webobject, browse = TRUE)
#  

## -----------------------------------------------------------------------------
## 
## 'con' is the Conos object created in the Conos walkthrough
##
## p2app = p2app4conos(conos=con, file="conosApp1.bin", save=TRUE)
## show.app(app=p2app, name='conos_app')
## 

