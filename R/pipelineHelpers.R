### A Collection of functions for quick analysis of single cell data with pagoda2

#' Perform basic pagoda2 processing
#' @description adjust variance, calculate pca reduction
#' make knn graph, identify clusters with infomap, multilevel and walktrap and make
#' largeVis and tSNE embeddings
#' @param cd count matrix, rows are genes, columns are cells
#' @param n.cores number of cores to use
#' @param batch optional batch factor (default=NULL)
#' @param n.odgenes number of top overdispersed genes to use (dfault=3e3)
#' @param nPcs number of PCs to use
#' @param k default number of neighbors to use in kNN graph
#' @param perplexity perplexity to use in generating tSNE and largeVis embeddings (default=50)
#' @param log.scale whether to use log scale normalization (default=T)
#' @param trim number of cells to trim in winsorization (default=10)
#' @param keep.genes optional set of genes to keep from being filtered out (even at low counts, default=NULL)
#' @param min.cells.per.gene minimal number of cells required for gene to be kept (unless listed in keep.genes)
#' @param min.transcripts.per.cell minimumal number of molecules/reads for a cell to be admitted
#' @param get.largevis whether to caluclate largeVis embedding
#' @param get.tsne whether to calculate tSNE embedding
#' @param make.geneknn whether pre-calculate gene kNN (for gene search)
#' @return a new pagoda2 object
#' @export basicP2proc
basicP2proc <- function(cd, n.cores = 1, batch = NULL,  n.odgenes=3e3, nPcs=100, k=30, perplexity=50, log.scale=TRUE, trim=10, keep.genes = NULL, min.cells.per.gene=0, min.transcripts.per.cell=100, get.largevis=TRUE, get.tsne=TRUE, make.geneknn=TRUE) {
  rownames(cd) <- make.unique(rownames(cd))
  ## Basic Processing
  p2 <- Pagoda2$new(cd, n.cores = n.cores, batch = batch, keep.genes = keep.genes, trim=trim, log.scale=log.scale, min.cells.per.gene=min.cells.per.gene, min.transcripts.per.cell=min.transcripts.per.cell);
  p2$adjustVariance(plot=F, gam.k=10);
  p2$calculatePcaReduction(nPcs = nPcs, n.odgenes = n.odgenes, maxit = 1000)
  ## Make KNN graph and generate clustering
  p2$makeKnnGraph(k = k, type='PCA', center=TRUE, weight.type = 'none', n.cores = n.cores, distance = 'cosine')
  #p2$getKnnClusters(method = igraph::infomap.community, type = 'PCA' ,name = 'infomap')
  p2$getKnnClusters(method = igraph::multilevel.community, type = 'PCA', name = 'multilevel');
  #p2$getKnnClusters(method = igraph::walktrap.community, type = 'PCA', name = 'walktrap');

  ## Generate embeddings
  if (get.largevis) {
      M <- 30
      p2$getEmbedding(type = 'PCA', embeddingType = 'largeVis', M = M, perplexity = perplexity, gamma = 1/ M, alpha =1)
  }
  if (get.tsne) {
    if(perplexity > nrow(p2$counts)/5) {
      perplexity <- floor((nrow(p2$counts)-1)/3)
      cat("perplexity is too large, reducing to",perplexity,"\n");
    }

      p2$getEmbedding(type = 'PCA', embeddingType = 'tSNE', perplexity = perplexity, distance='L2');
  }
  ## Required for web app generation
  if (make.geneknn) {
      p2$makeGeneKnnGraph();
  }
  ## return
  invisible(p2)
}

#' Perform extended pagoda2 processing
#' @description generate organism specific GO environment and
#' calculate pathway overdispersion
#' @param p2 the pagoda 2 object 
#' @param n.cores number of cores to use
#' @param organism organisms hs, mm or dr
#' @return a list of a p2 object and a go.env
#' @export extendedP2proc
extendedP2proc <- function(p2, n.cores = 20, organism = 'hs') {
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
    verbose =T,
    correlation.distance.threshold = 0.8,
    recalculate.pca = F,
    min.pathway.size = 50,
    max.pathway.size = 1000)

  invisible(list(p2 = p2, go.env = go.env))
}

#' Convert a list of factors to pagoda2 web metadata
#' @description  Converts a list of factors into pagoda2 metadata optionally
#' filtering down to the cells present in the provided pagoda2 app
#' @param factor.list a list of factors named by the cell identifier
#' @param p2 optional, a pagoda2 app to filter the factors by
#' @return a pagoda2 web metadata object
#' @export factorListToMetadata
factorListToMetadata <- function(factor.list, p2 = NULL) {
  if(! class(p2) %in% c('Pagoda2', 'NULL')) stop('p2 must be NULL or a pagoda2 app');
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

#' Generate a pagoda2 web object
#' @param p2 a pagoda2 object
#' @param additionalMetadata a pagoda2 web metadata object
#' @param title title for the web app
#' @param n.cores number of cores to use
#' @param make.go.sets logical specifying if go sets hould be made
#' @param make.de.sets logical specifying if differential expression sets should be made
#' @param go.env the go environment used for the overdispersion analysis
#' @param make.gene.graph logical specifying if the gene graph should be make, if FALSE the find similar genes functionality will be disabled on the web app
#' @return a pagoda2 web application
#' @export webP2proc
webP2proc <- function(p2, additionalMetadata =  NULL, title = 'Pagoda 2', n.cores =20,
                      make.go.sets = TRUE, make.de.sets = TRUE, go.env = NULL,
                      make.gene.graph = TRUE, appmetadata = NULL) {
  # Get the gene names
  gene.names <- colnames(p2$counts);
  # Build go terms for the web apps
  if (make.go.sets) {
    if (is.null(go.env)) {
      warning("make.go.sets is TRUE and go.env is null");
    } else {
      goSets <- p2.generate.go.web.fromGOEnv(go.env);
    }
  } else {
    goSets <- NULL
  }
  
  # Get differentially expressed genes for each group
  if (make.de.sets) {
    deSets <- get.de.geneset(p2, groups = p2$clusters$PCA[[1]], prefix = 'de_')
  } else {
    deSets <- NULL;
  }
  # Make genesets with go terms and de genes
  genesets <- c(goSets, deSets);
  # Make gene graph for nearest genes
  if(make.gene.graph)  {
    p2$makeGeneKnnGraph();
  }
  # Put the application metadat in a list
  appmetadata <- list(apptitle = title);
  # make the pagoda2 app
  p2web <- make.p2.app(p2,
                       dendrogramCellGroups = p2$clusters$PCA[[1]],
                       additionalMetadata = additionalMetadata,
                       geneSets = genesets,
                       appname=title,
                       show.clusters = F,
                       appmetadata = appmetadata);
  invisible(p2web);
}

#' Generate a GO environment for the organism specified
#' @description This is a wrapper function for generating a go envirnoment
#' @param r a pagoda2 object
#' @param organism the organism, current hs, mm and dr are supported for the human and mouse
#' @export p2.generate.go
p2.generate.go <- function(r, organism = NULL) {
  if (is.null(organism)) {
    stop('organism must be specified');
  }
  ret <- NULL;
  if (organism == 'hs') {
    ret <- p2.generate.human.go(r);
  } else if (organism == 'mm') {
    ret <- p2.generate.mouse.go(r);
  } else if (organism == 'dr') {
    ret <- p2.generate.dr.go(r);
  } else {
    stop('Unknown organism specified')
  }
  ret;
}

#' Generate a GO environment for human for overdispersion analysis for the the back end
#' @param r a pagoda2 object
#' @return a go environment object
#' @export p2.generate.dr.go
p2.generate.dr.go <- function(r, verbose=T) {
  # Generate GO environment
  if (!requireNamespace("org.Dr.eg.db", quietly = TRUE)) {
    stop("Package \"org.Dr.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
  }
  
  # translate gene names to ids
  if(verbose) message("Preparing GO environment ", appendLF=F)
  ids <- unlist(lapply(BiocGenerics::mget(colnames(r$counts), org.Dr.eg.db::org.Dr.egALIAS2EG,ifnotfound=NA),function(x) x[1]))
  if(verbose) cat(".")
                       
  # reverse map
  rids <- names(ids); names(rids) <- ids;
  
  # list all the ids per GO category
  go.env <- AnnotationDbi::eapply(org.Dr.eg.db::org.Dr.egGO2ALLEGS,function(x) as.character(na.omit(rids[x])))
  if(verbose) cat(".")
  go.env <- go.env[unlist(lapply(go.env,length))>5];
  if(verbose) cat(".")
  go.env <- list2env(go.env);
  if(verbose) cat(" done\n")
  go.env
}


#' Generate a GO environment for human for overdispersion analysis for the the back end
#' @param r a pagoda2 object
#' @return a go environment object
#' @export p2.generate.human.go
p2.generate.human.go <- function(r, verbose=T) {
  # Generate GO environment
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    stop("Package \"org.Hs.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
  }

  # translate gene names to ids
  if(verbose) message("Preparing GO environment ", appendLF=F)
  ids <- unlist(lapply(BiocGenerics::mget(colnames(r$counts),org.Hs.eg.db::org.Hs.egALIAS2EG,ifnotfound=NA),function(x) x[1]))
  if(verbose) cat(".")
  # reverse map
  rids <- names(ids); names(rids) <- ids;

  # list all the ids per GO category 

  go.env <- AnnotationDbi::eapply(org.Hs.eg.db::org.Hs.egGO2ALLEGS,function(x) as.character(na.omit(rids[x])))
  if(verbose) cat(".")
  go.env <- go.env[unlist(lapply(go.env,length))>5];
  if(verbose) cat(".")
  go.env <- list2env(go.env);
  if(verbose) cat(" done\n")
  go.env
}

#' Generate a GO environment for mouse for overdispersion analysis for the the back end
#' @param r a pagoda2 object
#' @return a go environment object
#' @export p2.generate.mouse.go
p2.generate.mouse.go <- function(r, verbose=T) {
  # Generate GO environment
  if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
    stop("Package \"org.Mm.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
  }

  # translate gene names to ids
  if(verbose) message("Preparing GO environment ", appendLF=F)
  ids <- unlist(lapply(BiocGenerics::mget(colnames(r$counts), org.Mm.eg.db::org.Mm.egALIAS2EG,ifnotfound=NA),function(x) x[1]))
  if(verbose) cat(".")
  # reverse map
  rids <- names(ids); names(rids) <- ids;

  # list all the ids per GO category
  go.env <- AnnotationDbi::eapply(org.Mm.eg.db::org.Mm.egGO2ALLEGS,function(x) as.character(na.omit(rids[x])))
  if(verbose) cat(".")
  go.env <- go.env[unlist(lapply(go.env,length))>5];
  if(verbose) cat(".")
  go.env <- list2env(go.env);
  if(verbose) cat(" done\n")
  go.env
}

#' @title Generate a metadata structure for a p2 web object from a named factor
#' @description This function will generate a metadata structure that can be passed to
#' p2 web object constructor as additional metadata given a named factor
#' @param metadata named factor with metadata for individual cells, names must correspond to cells
#' @param displayname name to display for the metadata
#' @param s s value for rainbow palette
#' @param v v value for rainbow palette
#' @param start starting value
#' @param end ending value
#' @param pal optional vector of colours to use, if provided overrides s,v,start and end parameters
#' @export p2.metadata.from.factor
#' @examples
#' additionalMetadata <- list()
#'
#' # Generate metadata the easy way
#' additionalMetadata$altCluster <- p2.metadata.from.factor(myPagoda2Object$clusters$PCA[[1]], displayname = 'Infomap')
#'
#' # Generate metadata by specifying parameters to be passes to the rainbow function
#' additionalMetadata$altCluster <- p2.metadata.from.factor(myPagoda2Object$clusters$PCA[[2]], displayname = 'Multilevel', s = 0.7, v = 0.8,start = 0, end = 0.5)
#'
#' # Generate metadata by specifying a palette
#' a <- myPagoda2Object$clusters$PCA[[3]]
#' library(colorRamps)
#' p1 <- colorRamps::primary.colors(n = nlevels(a))
#' names(p1) <- levels(a) # This is optional
#' additionalMetadata$altCluster2 <- p2.metadata.from.factor(myPagoda2Object$clusters$PCA[[3]], displayname = 'Walktrap', pal = p1)
p2.metadata.from.factor <- function(metadata, displayname = NULL, s = 1, v = 1, start = 0, end = NULL, pal = NULL) {
  # Check input
  if ( !is.factor(metadata) ) {
    stop('metadata is not a factor');
  }

  if (  is.null(names(metadata))) {
    stop('metadata needs to be named with cell identifiers');
  }

  if ( is.null(end) ) {
    n <- nlevels(metadata)
    end <-  max(1, n - 1)/n
  }

  # Convert input factor to named number vector
  data <- as.numeric(metadata) - 1; # because it's 0-indexed on js
  names(data) <- names(metadata);

  # Get the labels
  labs <- levels(metadata);

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
    ret$displayname <- displayname;
  }

  invisible(ret);
}

#' @title Generate a Rook Server app from a pagoda2 object
#' @description Generates a pagoda 2 web object from pagoda2 object by automating steps that most
#' users will want to run. This function is a wrapper about the pagoda2 web constructor. Advanced users
#' may wish to use use the constructor directly
#' @param r pagoda2 object
#' @param dendrogramCelllGoups a named factor of cell groups, used to generate the main dendrogram, limits zoom in
#' @param additionalMetadata a list of metadata other than depth, batch and cluster that are automatically added
#' @param geneSets a list of genesets to show
#' @param show.depth logical, include depth as a metadata row
#' @param show.batch logical, include batch as a metadata row
#' @param show.clusters logical, include clusters as a metadata row
#' @param appname application name
#' @param innerOrder Ordering of cells inside the clusters provided in dendrogramCellGroups. This should be one of "odPCA", "reductdist", "graphbased", "knn". Defaults to NULL
#' @return a pagoda2 web object that presents a Rook compatible interface
#' @export make.p2.app
make.p2.app <- function(r, dendrogramCellGroups, additionalMetadata = list(), geneSets, show.depth = T,
                        show.batch = T, show.clusters = T, appname = "Pagoda2 Application",
                        innerOrder=NULL, orderDend=FALSE, appmetadata = NULL) {
  # Build the metadata
  metadata <- list();

  if (show.depth) {
    if ( "depth" %in% names(r@.xData) ) {
      if ( !is.null(r@.xData$depth ) ) {
        levels  <- 20

        dpt <- log10(r@.xData$depth+0.00001)
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
  }

  if (show.batch) {
    if(!nlevels(r$batch)<=1){
      batchData <- as.numeric(r$batch) - 1;
      names(batchData) <- names(r$batch);
      if ( "batch" %in% names(r@.xData) ) {
        if ( !is.null(r@.xData$batch)  ) {
          metadata$batch <- list(
            data = batchData,
            palette = rainbow(n = length(levels(r$batch))),
            displayname = 'Batch'
          )
        }
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
  for ( itemName in names(additionalMetadata)) {
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
  );

  invisible(p2w);
}

#' @title Generate differential expression genesets for the web app given a cell grouping
#' @description Generate differential expression genesets for the web app given a cell grouping by
#' Calculating de sets between every cell set and everything else individually
#' @param pagObj pagoda object
#' @param groups a named factor to do the de by
#' @param prefix a character  prefix to assign to genesets generated
#' @export get.de.geneset
get.de.geneset <- function(pagObj, groups, prefix = 'de_') {

  deResults <- pagObj$getDifferentialGenes(
    type='counts', groups = groups, upregulated.only = T)

  deSets <- lapply(names(deResults), function(x) {
    resT <- deResults[[x]];
    list(
      properties = list(
        locked =T,
        genesetname=paste0(prefix, x),
        shortdescription = paste0('Cluster ', x, ' differentially expressed genes')
      ),
      genes = c(rownames(resT))
    );
  });

  names(deSets) <- unlist(lapply(deSets, function(x){x$properties$genesetname}));

  deSets
}

#' Converts the output of hierarchical differential expression aspects
#' into genesets that can be loaded into a p2 web app to retrive the genes
#' that make the geneset interactively
#' @param o output of getHierarchicalDiffExpressionAspects
#' @return a geneset that can be loaded into p2 web genesets
#' @export hierDiffToGenesets
hierDiffToGenesets <- function(o) {
  l <- as.list(o$env)
  lapply(namedNames(l), function(n) {
    list(
      properties = list(locked = T, genesetname=n,shortdescription=n),
      genes = l[[n]]
    )
  })
}

#' Generate a pagoda2 web object from a pagoda2 object using hierarchical differential expression
#' @param p2 p2 object
#' @param title name of the pagoda object
#' @export p2.toweb.hdea
p2.toweb.hdea <- function(p2, title="") {
  hdea <- p2$getHierarchicalDiffExpressionAspects(type='PCA',clusterName='multilevel',z.threshold=3)
  metadata.forweb <- list();
  metadata.forweb$multilevel <- p2.metadata.from.factor(p2$clusters$PCA$multilevel, displayname='joint')
  deSets <- get.de.geneset(p2, groups=p2$clusters$PCA[[1]], prefix='de_')
  genesets <- c(deSets, hierDiffToGenesets(hdea))
  appmetadata <- list(apptitle=title)
  p2$makeGeneKnnGraph();
  wp <- make.p2.app(p2, additionalMetadata = metadata.forweb, geneSets = genesets,
                    dendrogramCellGroups = p2$clusters$PCA[[1]], show.clusters=F,
                    appmetadata = appmetadata)
  wp
}






#' Generate a p2 web application from a pagoda2 object
#' @param p2 pagoda2 application object
#' @param app.title name of application as displayed in the browser title
#' @param extraWebMetadata additional metadata generated by p2.metadata.from.fractor
#' @param n.cores number of cores to use for differential expression calculation
#' @return a pagoda2 web object
#' @export basicP2web
basicP2web <- function(p2,app.title = 'Pagoda2', extraWebMetadata = NULL, n.cores = 4) {
    cat('Calculating hdea...\n')
    hdea <- p2$getHierarchicalDiffExpressionAspects(type='PCA',clusterName='multilevel',z.threshold=3, n.cores = n.cores)
    metadata.forweb <- list();
    metadata.forweb$multilevel <- p2.metadata.from.factor(p2$clusters$PCA$multilevel,displayname='Multilevel')
    metadata.forweb <- c(metadata.forweb, extraWebMetadata)
    genesets <- hierDiffToGenesets(hdea)
    appmetadata = list(apptitle=app.title)
    cat('Making KNN graph...\n')
    #p2$makeGeneKnnGraph(n.cores=n.cores)
    make.p2.app(p2, additionalMetadata = metadata.forweb, geneSets = genesets, dendrogramCellGroups = p2$clusters$PCA$multilevel, show.clusters=F, appmetadata = appmetadata)
}
