## ----message=FALSE------------------------------------------------------------
library(Matrix)
library(igraph)
library(pagoda2)
library(dplyr)
library(ggplot2)

## ----message=FALSE------------------------------------------------------------
install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')

## ----message=FALSE------------------------------------------------------------
## load the dataset
countMatrix <- p2data::sample_BM1
## all basic pagoda2 processing with basicP2proc()
p2.processed <- basicP2proc(countMatrix, n.cores=2, min.cells.per.gene=10, 
                    n.odgenes=2e3, get.largevis=FALSE, make.geneknn=FALSE)

## -----------------------------------------------------------------------------
## calculate pathway overdispersion for human
## ext.res <- extendedP2proc(p2.processed, organism = 'hs')

## create app object
## p2app <- webP2proc(ext.res$p2, title = 'Quick pagoda2 app', go.env = ext.res$go.env)

## open app in the web browser via R session
## show.app(app=p2app, name='pagoda2 app')

## ----message=FALSE------------------------------------------------------------
library(Matrix)
library(igraph)
library(pagoda2)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
cm <- p2data::sample_BM1
dim(cm)

## -----------------------------------------------------------------------------
cm[1:3, 1:3]

## -----------------------------------------------------------------------------
str(cm)

## ---- fig.height=8, fig.width=10----------------------------------------------
old_par <- par(mfrow=c(1,2), mar = c(3.5,3.5,2.0,0.5), mgp = c(2,0.65,0), cex = 1.0)
on.exit(par(old_par))
hist(log10(colSums(cm)+1), main='molecules per cell', col='cornsilk', xlab='molecules per cell (log10)')
hist(log10(rowSums(cm)+1), main='molecules per gene', col='cornsilk', xlab='molecules per gene (log10)')

## ---- fig.height=8, fig.width=10----------------------------------------------
counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)

## -----------------------------------------------------------------------------
hist(log10(rowSums(counts)+1), main='Molecules per gene', xlab='molecules (log10)', col='cornsilk')
abline(v=1, lty=2, col=2)

## -----------------------------------------------------------------------------
counts <- counts[rowSums(counts)>=10, ]
dim(counts)

## -----------------------------------------------------------------------------
rownames(counts) <- make.unique(rownames(counts))
r <- Pagoda2$new(counts, log.scale=TRUE, n.cores=1)

## ---- fig.height=8, fig.width=10----------------------------------------------
r$adjustVariance(plot=TRUE, gam.k=10)

## -----------------------------------------------------------------------------
r$calculatePcaReduction(nPcs=50, n.odgenes=3e3)

## -----------------------------------------------------------------------------
r$makeKnnGraph(k=40, type='PCA', center=TRUE, distance='cosine')

## -----------------------------------------------------------------------------
r$getKnnClusters(method=infomap.community, type='PCA')

## -----------------------------------------------------------------------------
M <- 30
r$getEmbedding(type='PCA', embeddingType = 'largeVis', M=M, perplexity=30, gamma=1/M)

## -----------------------------------------------------------------------------
r$plotEmbedding(type='PCA', show.legend=FALSE, mark.groups=TRUE, min.cluster.size=50, shuffle.colors=FALSE, font.size=3, alpha=0.3, title='clusters (largeVis)', plot.theme=theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

## -----------------------------------------------------------------------------
r$getEmbedding(type='PCA', embeddingType='tSNE', perplexity=50,verbose=FALSE)
r$plotEmbedding(type='PCA', embeddingType='tSNE', show.legend=FALSE, mark.groups=TRUE, min.cluster.size=1, shuffle.colors=FALSE, font.size=3, alpha=0.3, title='clusters (tSNE)', plot.theme=theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

## -----------------------------------------------------------------------------
gene <-"HBB"
r$plotEmbedding(type='PCA', embeddingType='tSNE', colors=r$counts[,gene], shuffle.colors=FALSE, font.size=3, alpha=0.3, title=gene, plot.theme=theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

## -----------------------------------------------------------------------------
gene <-"LYZ"
r$plotEmbedding(type='PCA', embeddingType='tSNE', colors=r$counts[,gene], shuffle.colors=FALSE, font.size=3, alpha=0.3, title=gene, plot.theme=theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

## -----------------------------------------------------------------------------
r$getKnnClusters(method=multilevel.community, type='PCA', name='multilevel')
r$getKnnClusters(method=walktrap.community, type='PCA', name='walktrap')

## -----------------------------------------------------------------------------
str(r$clusters)

## ---- fig.height=8, fig.width=12----------------------------------------------
plt1 = r$plotEmbedding(type='PCA', embeddingType='tSNE', groups=r$clusters$PCA$community, show.legend=FALSE, mark.groups=TRUE, min.cluster.size=1, shuffle.colors=FALSE, font.size=3, alpha=0.3, title='infomap clusters (tSNE)', plot.theme=theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
plt2 = r$plotEmbedding(type='PCA',embeddingType='tSNE', clusterType='multilevel', show.legend=FALSE, mark.groups=TRUE, min.cluster.size=1, shuffle.colors=FALSE, font.size=3, alpha=0.3, title='multlevel clusters (tSNE)', plot.theme=theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
plt3 = r$plotEmbedding(type='PCA',embeddingType='tSNE', clusterType='walktrap', show.legend=FALSE, mark.groups=TRUE, min.cluster.size=1, shuffle.colors=FALSE, font.size=3, alpha=0.3, title='walktrap clusters (tSNE)', plot.theme=theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
gridExtra::grid.arrange(plt1, plt2, plt3, ncol=3)

## -----------------------------------------------------------------------------
r$getDifferentialGenes(type='PCA', verbose=TRUE, clusterType='community')

## -----------------------------------------------------------------------------
de <- r$diffgenes$PCA[[1]][['2']]
r$plotGeneHeatmap(genes=rownames(de)[1:15], groups=r$clusters$PCA[[1]])

## -----------------------------------------------------------------------------
gene <-"CD74"
r$plotEmbedding(type='PCA', embeddingType='tSNE', colors=r$counts[,gene], shuffle.colors=FALSE, font.size=3, alpha=0.3, title=gene, legend.title=gene)

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
## test overdispersion
# r$testPathwayOverdispersion(go.env, verbose=TRUE, correlation.distance.threshold=0.95, recalculate.pca=FALSE, top.aspects=15)

## -----------------------------------------------------------------------------
hdea <- r$getHierarchicalDiffExpressionAspects(type='PCA', clusterName='community', z.threshold=3)

## -----------------------------------------------------------------------------
##  saveRDS(r, 'pagoda2object.rds')

## -----------------------------------------------------------------------------
genesets <- hierDiffToGenesets(hdea)
str(genesets[1:2])

## -----------------------------------------------------------------------------
library(GO.db)
termDescriptions <- Term(GOTERM[names(go.env)]) # saves a good minute or so compared to individual lookups

sn <- function(x) { names(x) <- x; x}  ## utility function

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
appmetadata <- list(apptitle = 'Demo_App')

## -----------------------------------------------------------------------------
r$makeGeneKnnGraph(n.cores = 1)

## -----------------------------------------------------------------------------
## # Make a list for our metadata
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
p2web <- make.p2.app(r, 
    dendrogramCellGroups = r$clusters$PCA$community,
    additionalMetadata = additionalMetadata,
    geneSets = genesets,
    appmetadata = appmetadata,
    show.clusters = FALSE # Hide the clusters that were used for the dendrogram from the metadata
  )

## -----------------------------------------------------------------------------
##  show.app(app=p2web, name='app')

## -----------------------------------------------------------------------------
##  p2web$serializeToStaticFast('demo_pbmc.bin', verbose=TRUE)

