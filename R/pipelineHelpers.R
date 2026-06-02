### A Collection of functions for quick analysis of single cell data with 'pagoda2'

#' Perform basic 'pagoda2' processing, i.e. adjust variance, calculate pca reduction,
#' make knn graph, identify clusters with multilevel, and generate
#' largeVis and tSNE embeddings.
#' 
#' @param cd count matrix whereby rows are genes, columns are cells.
#' @param n.cores numeric Number of cores to use (default=1)
#' @param n.odgenes numeric Number of top overdispersed genes to use (dfault=3e3)
#' @param nPcs numeric Number of PCs to use (default=100)
#' @param k numeric Default number of neighbors to use in kNN graph (default=30)
#' @param perplexity numeric Perplexity to use in generating tSNE and largeVis embeddings (default=50)
#' @param log.scale boolean Whether to use log scale normalization (default=TRUE)
#' @param trim numeric Number of cells to trim in winsorization (default=10)
#' @param keep.genes optional set of genes to keep from being filtered out (even at low counts) (default=NULL)
#' @param min.cells.per.gene numeric Minimal number of cells required for gene to be kept (unless listed in keep.genes) (default=0)
#' @param min.transcripts.per.cell numeric Minimumal number of molecules/reads for a cell to be admitted (default=100)
#' @param get.largevis boolean Whether to caluclate largeVis embedding (default=TRUE)
#' @param get.tsne boolean Whether to calculate tSNE embedding (default=TRUE) 
#' @param make.geneknn boolean Whether pre-calculate gene kNN (for gene search) (default=TRUE) 
#' @return a new 'Pagoda2' object
#' 
#' @export 
basicP2proc <- function(cd, n.cores=1, n.odgenes=3e3, nPcs=100, k=30, perplexity=50, 
  log.scale=TRUE, trim=10, keep.genes=NULL, min.cells.per.gene=0, min.transcripts.per.cell=100, 
  get.largevis=TRUE, get.tsne=TRUE, make.geneknn=TRUE) {

  rownames(cd) <- make.unique(rownames(cd))
  ## Basic Processing
  p2 <- Pagoda2$new(cd, n.cores = n.cores, keep.genes = keep.genes, trim=trim, log.scale=log.scale, min.cells.per.gene=min.cells.per.gene, min.transcripts.per.cell=min.transcripts.per.cell)
  p2$adjustVariance(plot=FALSE, gam.k=10)
  p2$calculatePcaReduction(nPcs = nPcs, n.odgenes = n.odgenes, maxit = 1000)
  ## Make KNN graph and generate clustering
  p2$makeKnnGraph(k = k, type='PCA', center=TRUE, weight.type = 'none', n.cores = n.cores, distance = 'cosine')
  ##p2$getKnnClusters(method = igraph::infomap.community, type = 'PCA' ,name = 'infomap')
  p2$getKnnClusters(method = igraph::multilevel.community, type = 'PCA', name = 'multilevel')
  ##p2$getKnnClusters(method = igraph::walktrap.community, type = 'PCA', name = 'walktrap');

  ## Generate embeddings
  if (get.largevis) {
      M <- 30
      p2$getEmbedding(type = 'PCA', embeddingType = 'largeVis', M = M, perplexity = perplexity, gamma = 1/ M, alpha =1)
  }
  if (get.tsne) {
    if(perplexity > nrow(p2$counts)/5) {
      perplexity <- floor((nrow(p2$counts)-1)/3)
      message("perplexity is too large, reducing to ",perplexity,"\n")
    }

      p2$getEmbedding(type = 'PCA', embeddingType = 'tSNE', perplexity = perplexity, distance='L2');
  }
  ## Pre-calculate gene kNN graph when requested.
  if (make.geneknn) {
      p2$makeGeneKnnGraph()
  }
  ## return
  invisible(p2)
}

#' Perform extended 'Pagoda2' processing. 
#' Generate organism specific GO environment and calculate pathway overdispersion.
#' 
#' @param p2 the 'Pagoda2' object 
#' @param organism character Organisms hs (Homo Sapiens), mm (M. Musculus, mouse) or dr (D. Rerio, zebrafish) (default='hs')
#' @return list of a 'Pagoda2' object and go.env
#' 
#' @export 
extendedP2proc <- function(p2, organism = 'hs') {

  if (organism == 'hs') {
    go.env <- p2.generate.human.go(p2)
  } else if (organism == 'mm') {
    go.env <- p2.generate.mouse.go(p2);
  } else if (organism == 'dr') {
    go.env <- p2.generate.dr.go(p2);
  } else {
    stop('unknown organism');
  }

  p2$testPathwayOverdispersion(
    setenv = go.env,
    verbose =TRUE,
    correlation.distance.threshold = 0.8,
    recalculate.pca = FALSE,
    min.pathway.size = 50,
    max.pathway.size = 1000)

  invisible(list(p2 = p2, go.env = go.env))
}

#' Generate a GO environment for the organism specified
#' 
#' @param r a 'Pagoda2' object
#' @param organism the organism (default=NULL). Currently 'hs' (human), 'mm' (mouse) and 'dr' (zebrafish) are supported.
#' @param go2all.egs mappings between a given GO identifier and all of the Entrez Gene identifiers 
#'     annotated at that GO term or to one of its child nodes in the GO ontology (default=NULL)
#' @param eg.alias2eg mappings between common gene symbol identifiers and entrez gene identifiers (default=NULL)
#' @param min.env.length numeric Minimum environment length (default=5)
#' 
#' @export 
p2.generate.go <- function(r, organism=NULL, go2all.egs=NULL, eg.alias2eg=NULL, min.env.length=5) {
  

  if (is.null(organism) && (is.null(go2all.egs) || is.null(eg.alias2eg))) {
    stop('Either organism or go2all.egs and eg.alias2eg must be specified');
  }
  
  if (is.null(go2all.egs) || is.null(eg.alias2eg)) {
    if (organism == 'hs') {
      if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
        stop("Package \"org.Hs.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
      }
      
      eg.alias2eg <- org.Hs.eg.db::org.Hs.egALIAS2EG
      go2all.egs <- org.Hs.eg.db::org.Hs.egGO2ALLEGS
    } else if (organism == 'mm') {
      if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
        stop("Package \"org.Mm.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
      }
      
      eg.alias2eg <- org.Mm.eg.db::org.Mm.egALIAS2EG
      go2all.egs <- org.Mm.eg.db::org.Mm.egGO2ALLEGS
    } else if (organism == 'dr') {
      if (!requireNamespace("org.Dr.eg.db", quietly = TRUE)) {
        stop("Package \"org.Dr.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
      }
      
      eg.alias2eg <- org.Dr.eg.db::org.Dr.egALIAS2EG
      go2all.egs <- org.Dr.eg.db::org.Dr.egGO2ALLEGS
    } else {
      stop('Unknown organism specified')
    }
  }

  # translate gene names to ids
  ids <- unlist(lapply(BiocGenerics::mget(colnames(r$counts), eg.alias2eg,ifnotfound=NA),function(x) x[1]))
  
  # reverse map
  rids <- names(ids); names(rids) <- ids;
  
  # list all the ids per GO category
  if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
    stop("Package \"AnnotationDbi\" needed for this function to work. Please install it with `BiocManager::install('AnnotationDbi')`.", call. = FALSE)
  }
  go.env <- AnnotationDbi::eapply(go2all.egs,function(x) as.character(na.omit(rids[x])))
  
  return(list2env(go.env[sapply(go.env, length) > min.env.length]))
}

#' Generate a GO environment for human for overdispersion analysis for the the back end
#' 
#' @param r a 'Pagoda2' object
#' @return a GO environment object
#' 
#' @export
p2.generate.dr.go <- function(r) {
  p2.generate.go(r, "dr")
}


#' Generate a GO environment for human for overdispersion analysis for the the back end
#' 
#' @param r a 'Pagoda2' object
#' @return a GO environment object
#' 
#' @export
p2.generate.human.go <- function(r) {
  p2.generate.go(r, "hs")
}

#' Generate a GO environment for mouse for overdispersion analysis for the the back end
#' 
#' @param r a 'Pagoda2' object
#' @return a GO environment object
#' 
#' @export 
p2.generate.mouse.go <- function(r) {
  p2.generate.go(r, "mm")
}
