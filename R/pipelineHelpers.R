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
#' @examples
#' \donttest{
#' ## load count matrix
#' cm <- p2data::sample_BM1
#' ## perform basic p2 processing
#' p2 <- basicP2proc(cm)
#' }
#' 
#' @export 
basicP2proc <- function(cd, n.cores=1, n.odgenes=3e3, nPcs=100, k=30, perplexity=50, 
  log.scale=TRUE, trim=10, keep.genes=NULL, min.cells.per.gene=0, min.transcripts.per.cell=100, 
  get.largevis=TRUE, get.tsne=TRUE, make.geneknn=TRUE) {

  if (!requireNamespace("p2data", quietly = TRUE)) {
    stop("Package \"p2data\" needed for the Pagoda2 class to work. This can be installed via a drat repository, using \"install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')\". Please read the details provided within the README at https://github.com/kharchenkolab/pagoda2.", call. = FALSE)
  }
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
  ## Required for web app generation
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

  if (!requireNamespace("p2data", quietly = TRUE)) {
    stop("Package \"p2data\" needed for the Pagoda2 class to work. This can be installed via a drat repository, using \"install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')\". Please read the details provided within the README at https://github.com/kharchenkolab/pagoda2.", call. = FALSE)
  }

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

#' Converts a list of factors into 'pagoda2' metadata optionally
#' filtering down to the cells present in the provided 'pagoda2' app.
#' 
#' @param factor.list list of factors named by the cell identifier
#' @param p2 'pagoda2' app to filter the factors by, optional (default=NULL)
#' @return 'pagoda2' web metadata object
#' @export 
factorListToMetadata <- function(factor.list, p2=NULL) {
  if(! class(p2) %in% c('Pagoda2', 'NULL')) {
    stop('p2 must be NULL or a pagoda2 app')
  }
  ## A pagoda 2 object has been provided, filter the factors by the
  ## cells in the app
  if (!is.null(p2)) {
    lapply(factor.list , function(x) {
      if (is.null(names(x))) {
        stop('A factor does not have cell names');
      }
      if (any(!rownames(p2$counts) %in% names(x))) {
        stop('A factor is missing some cells in the app');
      }
    })
  }
  ## Subset all factors by the cells in the app
  if (!is.null(p2)) {
    factor.list <- lapply(factor.list, function(x) {
      x[rownames(p2$counts)]
    });
  }
  metadata <- mapply(function(x,n) {
    p2.metadata.from.factor(x, displayname = n);
  }, factor.list, names(factor.list), SIMPLIFY = FALSE)
  invisible(metadata)
}



#' Generate a 'pagoda2' web object
#' 
#' @param p2 a 'Pagoda2' object
#' @param additionalMetadata 'pagoda2' web metadata object (default=NULL)
#' @param title character string Title for the web app (default='Pagoda2')
#' @param make.go.sets boolean Whether GO sets should be made (default=TRUE)
#' @param make.de.sets boolean Whether differential expression sets should be made (default=TRUE)
#' @param go.env the GO environment used for the overdispersion analysis (default=NULL)
#' @param make.gene.graph logical specifying if the gene graph should be make, if FALSE the find similar genes functionality will be disabled on the web app
#' @param appmetadata 'pagoda2' web application metadata (default=NULL)
#' @return a 'pagoda2' web application
#' @export 
webP2proc <- function(p2, additionalMetadata=NULL, title='Pagoda2',
                      make.go.sets=TRUE, make.de.sets=TRUE, go.env=NULL,
                      make.gene.graph=TRUE, appmetadata=NULL) {
  # Get the gene names
  gene.names <- colnames(p2$counts);
  # Build go terms for the web apps
  if (make.go.sets) {
    if (is.null(go.env)) {
      warning("make.go.sets is TRUE and go.env is null")
    } else {
      goSets <- p2.generate.go.web.fromGOEnv(go.env)
    }
  } else {
    goSets <- NULL
  }
  
  # Get differentially expressed genes for each group
  if (make.de.sets) {
    deSets <- get.de.geneset(p2, groups = p2$clusters$PCA[[1]], prefix = 'de_')
  } else {
    deSets <- NULL
  }
  # Make genesets with go terms and de genes
  genesets <- c(goSets, deSets)
  # Make gene graph for nearest genes
  if(make.gene.graph)  {
    p2$makeGeneKnnGraph()
  }
  # Put the application metadat in a list
  appmetadata <- list(apptitle = title)
  # make the pagoda2 app
  p2web <- make.p2.app(p2,
                       dendrogramCellGroups = p2$clusters$PCA[[1]],
                       additionalMetadata = additionalMetadata,
                       geneSets = genesets,
                       appname=title,
                       show.clusters = FALSE,
                       appmetadata = appmetadata)
  invisible(p2web)
}


#' Generate a GO environment for the organism specified
#' 
#' @param r a 'Pagoda2' object
#' @param organism the organism (default=NULL). Currently 'hs' (human), 'mm' (mouse) and 'dr' (zebrafish) are supported.
#' @param go2all.egs mappings between a given GO identifier and all of the Entrez Gene identifiers 
#'     annotated at that GO term or to one of its child nodes in the GO ontology (default=NULL)
#' @param eg.alias2eg mappings between common gene symbol identifiers and entrez gene identifiers (default=NULL)
#' @param min.env.length numeric Minimum environment length (default=5)
#' @examples
#' \donttest{
#' cm <- p2data::sample_BM1
#' p2 <- basicP2proc(cm)
#' p2.generate.go(p2, organism='hs')
#' }
#' 
#' @export 
p2.generate.go <- function(r, organism=NULL, go2all.egs=NULL, eg.alias2eg=NULL, min.env.length=5) {
  
  if (!requireNamespace("p2data", quietly = TRUE)) {
    stop("Package \"p2data\" needed for the Pagoda2 class to work. This can be installed via a drat repository, using \"install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')\". Please read the details provided within the README at https://github.com/kharchenkolab/pagoda2.", call. = FALSE)
  }

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
#' @examples
#' \donttest{
#' cm <- p2data::sample_BM1
#' p2 <- basicP2proc(cm)
#' p2.generate.dr.go(p2)
#' }
#' 
#' @export
p2.generate.dr.go <- function(r) {
  if (!requireNamespace("p2data", quietly = TRUE)) {
    stop("Package \"p2data\" needed for the Pagoda2 class to work. This can be installed via a drat repository, using \"install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')\". Please read the details provided within the README at https://github.com/kharchenkolab/pagoda2.", call. = FALSE)
  }
  p2.generate.go(r, "dr")
}


#' Generate a GO environment for human for overdispersion analysis for the the back end
#' 
#' @param r a 'Pagoda2' object
#' @return a GO environment object
#' @examples
#' \donttest{
#' cm <- p2data::sample_BM1
#' p2 <- basicP2proc(cm)
#' p2.generate.human.go(p2)
#' }
#' 
#' @export
p2.generate.human.go <- function(r) {
  if (!requireNamespace("p2data", quietly = TRUE)) {
    stop("Package \"p2data\" needed for the Pagoda2 class to work. This can be installed via a drat repository, using \"install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')\". Please read the details provided within the README at https://github.com/kharchenkolab/pagoda2.", call. = FALSE)
  }
  p2.generate.go(r, "hs")
}

#' Generate a GO environment for mouse for overdispersion analysis for the the back end
#' 
#' @param r a 'Pagoda2' object
#' @return a GO environment object
#' @examples
#' \donttest{
#' cm <- p2data::sample_BM1
#' p2 <- basicP2proc(cm)
#' p2.generate.mouse.go(p2)
#' }
#' 
#' @export 
p2.generate.mouse.go <- function(r) {
  if (!requireNamespace("p2data", quietly = TRUE)) {
    stop("Package \"p2data\" needed for the Pagoda2 class to work. This can be installed via a drat repository, using \"install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')\". Please read the details provided within the README at https://github.com/kharchenkolab/pagoda2.", call. = FALSE)
  }
  p2.generate.go(r, "mm")
}


#' Generate a list metadata structure that can be passed to a
#' 'pagoda2' web object constructor as additional metadata given a named factor
#' 
#' @param metadata named factor with metadata for individual cells, names must correspond to cells
#' @param displayname character Name to display for the metadata (default=NULL)
#' @param s numeric Value for rainbow palette (default=1)
#' @param v numeric Value for rainbow palette (default=1)
#' @param start numeric Starting value (default=0)
#' @param end numeric Ending value (default=NULL)
#' @param pal optional vector of colours to use, if provided overrides s,v,start and end parameters (default=NULL)
#' @return list of data, levels, palette to be passed to 'pagoda2' web object constructor
#' @export 
p2.metadata.from.factor <- function(metadata, displayname=NULL, s=1, v=1, start=0, end=NULL, pal=NULL) {
  # Check input
  if (!is.factor(metadata)) {
    stop('Metadata is not a factor')
  }

  if (is.null(names(metadata))) {
    stop('Metadata needs to be named with cell identifiers');
  }

  if (is.null(end)) {
    n <- nlevels(metadata)
    end <-  max(1, n - 1)/n
  }

  # Convert input factor to named number vector
  data <- as.numeric(metadata) - 1  # because it's 0-indexed on js
  names(data) <- names(metadata);

  # Get the labels
  labs <- levels(metadata)

  # Genereate palette
  if (!is.null(pal)){
    # Do some checks on pal and set that
    if (nlevels(metadata) != length(pal)) {
      stop("The provided palette contains a different number of colours from the levels in the metadata");
    } else {
      if (is.null(names(pal))) {
        # Palette doesn't have names use as is
        pal0 <- pal;
      } else {
        if(!all(names(pal) %in% levels(metadata))) {
          stop('Some palette names do not correspond to metadata levels')
        }
        if (!all(levels(metadata) %in% names(pal))) {
          stop('Some metadata names do not appear in palette levels')
        }
        # Order according to metadata levels
        pal0 <- pal[as.character(levels(metadata))];
      }
    }
  } else {
    # No palette has been specified use the parameters to make a rainbow palette
    pal0 <- rainbow(n = nlevels(metadata), s = s, v = v, start = start, end = end, alpha = 1);
  }

  ret <- list(
    data = data,
    levels = labs,
    palette = unname(as.character(pal0)),
    displayname = ""
  );

  if (!is.null(displayname)) {
    ret$displayname <- displayname
  }

  invisible(ret)
}


#' Generate a Rook Server app from a 'Pagoda2' object. 
#' This generates a 'pagoda2' web object from a 'Pagoda2' object by automating steps that most
#' users will want to run. This function is a wrapper about the 'pagoda2' web constructor. 
#' (Advanced users may wish to use that constructor directly.)
#'
#' @param r a 'Pagoda2' object
#' @param dendrogramCellGroups a named factor of cell groups, used to generate the main dendrogram, limits zoom in
#' @param additionalMetadata a list of metadata other than depth, batch and cluster that are automatically added (default=list())
#' @param geneSets a list of genesets to show
#' @param show.depth boolean Include depth as a metadata row (default=TRUE)
#' @param show.batch boolean Include batch as a metadata row (default=TRUE)
#' @param show.clusters boolean Include clusters as a metadata row (default=TRUE)
#' @param appname character Application name (default="Pagoda2 Application")
#' @param innerOrder Ordering of cells inside the clusters provided in dendrogramCellGroups (default=NULL). This should be one of "odPCA", "reductdist", "graphbased", "knn". Defaults to NULL
#' @param orderDend boolean Whether to order dendrogram (default=FALSE)
#' @param appmetadata a 'pagoda2' web application metadata (default=NULL)
#' @return a 'pagoda2' web object that presents a Rook compatible interface
#' @export 
make.p2.app <- function(r, dendrogramCellGroups, additionalMetadata = list(), geneSets, show.depth = TRUE,
                        show.batch = TRUE, show.clusters = TRUE, appname = "Pagoda2 Application",
                        innerOrder=NULL, orderDend=FALSE, appmetadata = NULL) {
  # Build the metadata
  metadata <- list()

  if (show.depth) {
      if (!is.null(r$depth)) {
        levels  <- 20
        dpt <- log10(r$depth+0.00001)
        max <- max(dpt)
        min <- min(dpt)
        dptnorm <- floor((dpt - min) / (max - min) * levels)
        metadata$depth <- list(
          data = dptnorm,
          palette = colorRampPalette(c('white','black'))(levels+1),
          displayname = 'Depth'
        )
      }
  }

  if (show.batch) {
    if (!nlevels(r$batch)<=1){
      batchData <- as.numeric(r$batch) - 1
      names(batchData) <- names(r$batch)
        if ( !is.null(r$batch)  ) {
          metadata$batch <- list(
            data = batchData,
            palette = rainbow(n = length(levels(r$batch))),
            displayname = 'Batch'
          )
        }
    }
  }

  if (show.clusters) {
    clusterData <- as.numeric(dendrogramCellGroups) - 1;
    names(clusterData) <- names(dendrogramCellGroups);
    metadata$clusters <- list(
      data = clusterData,
      levels = levels(dendrogramCellGroups),
      palette = rainbow(n =  length(levels(dendrogramCellGroups))),
      displayname = 'Clusters'
    )
  }

  # User provided metadata
  for (itemName in names(additionalMetadata)) {
    metadata[[itemName]] <- additionalMetadata[[itemName]]
  }

  #deGenes <- r$getDifferentialGenes(type='counts', groups=dendrogramCellGroups)

  # Make the app object
  p2w <- pagoda2WebApp$new(
    pagoda2obj = r,
    appName = appname,
    dendGroups = dendrogramCellGroups,
    verbose = 0,
    debug = TRUE,
    geneSets = geneSets,
    metadata = metadata,
    innerOrder = innerOrder,
    orderDend = orderDend,
    appmetadata = appmetadata
  )

  invisible(p2w)
}

#' Generate differential expression genesets for the web app given a cell grouping by
#' calculating DE sets between each cell set and everything else 
#' 
#' @param pagObj pagoda object
#' @param groups named factor to do the de by
#' @param prefix chararcter Prefix to assign to genesets generated (default="de_")
#' @return a 'pagoda2' web object
#' @export 
get.de.geneset <- function(pagObj, groups, prefix = 'de_') {

  deResults <- pagObj$getDifferentialGenes(
    type='counts', groups = groups, upregulated.only = TRUE)

  deSets <- lapply(names(deResults), function(x) {
    resT <- deResults[[x]]
    list(
      properties = list(
        locked =T,
        genesetname=paste0(prefix, x),
        shortdescription = paste0('Cluster ', x, ' differentially expressed genes')
      ),
      genes = c(rownames(resT))
    )
  })

  names(deSets) <- unlist(lapply(deSets, function(x){x$properties$genesetname}));

  invisible(deSets)
}

#' Converts the output of hierarchical differential expression aspects
#' into genesets that can be loaded into a 'pagoda2' web app to retrive the genes
#' that make the geneset interactively
#' 
#' @param output output of getHierarchicalDiffExpressionAspects
#' @return a geneset that can be loaded into p2 web genesets
#' @export
hierDiffToGenesets <- function(output) {
  l <- as.list(output$env)
  lapply(namedNames(l), function(n) {
    list(
      properties = list(locked = TRUE, genesetname=n,shortdescription=n),
      genes = l[[n]]
    )
  })
}

#' Generate a 'pagoda2' web object from a 'Pagoda2' object using hierarchical differential expression
#' 
#' @param p2 p2 object
#' @param title character Name of the pagoda object (default="")
#' @return a 'pagoda2' web object
#' @export p2.toweb.hdea
p2.toweb.hdea <- function(p2, title="") {
  hdea <- p2$getHierarchicalDiffExpressionAspects(type='PCA',clusterName='multilevel',z.threshold=3)
  metadata.forweb <- list()
  metadata.forweb$multilevel <- p2.metadata.from.factor(p2$clusters$PCA$multilevel, displayname='joint')
  deSets <- get.de.geneset(p2, groups=p2$clusters$PCA[[1]], prefix='de_')
  genesets <- c(deSets, hierDiffToGenesets(hdea))
  appmetadata <- list(apptitle=title)
  p2$makeGeneKnnGraph()
  wp <- make.p2.app(p2, additionalMetadata = metadata.forweb, geneSets = genesets,
                    dendrogramCellGroups = p2$clusters$PCA[[1]], show.clusters=FALSE,
                    appmetadata = appmetadata)
  invisible(wp)
}


#' Generate a 'pagoda2' web application from a 'Pagoda2' object
#'
#' @param p2 a 'Pagoda2' object
#' @param app.title name of application as displayed in the browser title (default='Pagoda2')
#' @param extraWebMetadata additional metadata generated by p2.metadata.from.fractor (default=NULL)
#' @param n.cores numeric Number of cores to use for differential expression calculation (default=4)
#' @return a 'pagoda2' web object
#' @export 
basicP2web <- function(p2, app.title='Pagoda2', extraWebMetadata=NULL, n.cores=4) {
  message('Calculating hdea...\n')
  hdea <- p2$getHierarchicalDiffExpressionAspects(type='PCA',clusterName='multilevel',z.threshold=3, n.cores = n.cores)
  metadata.forweb <- list()
  metadata.forweb$multilevel <- p2.metadata.from.factor(p2$clusters$PCA$multilevel,displayname='Multilevel')
  metadata.forweb <- c(metadata.forweb, extraWebMetadata)
  genesets <- hierDiffToGenesets(hdea)
  appmetadata = list(apptitle=app.title)
  message('Making KNN graph...\n')
  make.p2.app(p2, additionalMetadata = metadata.forweb, geneSets = genesets, dendrogramCellGroups = p2$clusters$PCA$multilevel, show.clusters=FALSE, appmetadata = appmetadata)
}