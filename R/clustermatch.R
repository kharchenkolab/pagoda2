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
                                 extra.info = F, var.scale.joint = F) {
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
        if (var.scale.joint) {
            joint.var <- sqrt((referenceP2$misc$varinfo[odgenes,]$gsf ^ 2 + r2$misc$varinfo[odgenes,]$gsf ^ 2) / 2 )
            x1 <- sweep(x1, 2, joint.var, FUN='*')
            x2 <- sweep(x2, 2, joint.var, FUN='*')
        } else {
            x1 <- sweep(x1, 2, referenceP2$misc$varinfo[odgenes,]$gsf, FUN='*')
            x2 <- sweep(x2, 2, r2$misc$varinfo[odgenes,]$gsf, FUN='*')
        }
        
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
#' @param include.sample.internal.edges augment MNN network with intra app network
#' @param mnn.edge.weigth weight of edges from inter-sample matches
#' @param internal.edge.weight weigth of edges from intra-sample matches (sample KNN)
#' @param extra.info return extra debugging information (modifies return value structure)
#' @param neighbourhood.average logical, average cell neighbourhoods before doing MNN lookup
#' @param neighbourhood.k number of nearest neighbours to average over
#' @param mutualOnly logical if true (defalt) only interapp links that are mutual are used, otherwise the links don'e need to be mutual (experimental) 
#' @return a named (by cell name) factor of groups if extra.info is false, otherwise a list the factor and the extra information
#' @export getJointClustering
getJointClustering <- function(r.n,
                               k=30,
                               community.detection.method = walktrap.community,
                               community.detection.params = list(),
                               min.group.size = 10,
                               ncomps=100,
                               include.sample.internal.edges=TRUE,
                               mnn.edge.weight = 1,
                               internal.edge.weight =1,
                               extra.info = F,
                               neighbourhood.average = TRUE,
                               neighbourhood.k = 20,
                               mutualOnly = TRUE,
                               neighbourhood.cleanup = FALSE,
                               custom.neighbourhood.cleanup = NULL,
                               neighbourhood.cleanup.params = list(),
                               stop.return.graph = FALSE,
                               networkPairFunction = NULL) {
  
  require('gtools')
  require('pbapply')
  require('igraph')

  if (is.null(networkPairFunction)) {
      networkPairFunction <- getNNforP2pair;
  }
  
  ## Get all non-redundant pair of apps
  nms <- names(r.n)
  combs <- combinations(n = length(nms), r = 2, v = nms, repeats.allowed =F)
  
  ## Convert to list for lapply
  combsl <- split(t(combs), rep(1:nrow(combs), each=ncol(combs)))
  
  ## get MNN pairs from all possible app pairs
  cat('Calculating MNN for application pairs ...\n')
  mnnres <- pblapply(combsl, function(x) {
    networkPairFunction(r.n[[x[1]]], r.n[[x[2]]], k = k, verbose =F,
                    ncomps = ncomps, neighbourhood.average = neighbourhood.average,
                    neighbourhood.k, neighbourhood.k, mutualOnly = mutualOnly);
  });
  
  ## Merge the results into a edge table
  mnnres.all <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab')]
  summary(mnnres.all)
  mnnres.all$weight <- c(mnn.edge.weight)
  mnnres.all$type <- c('inter')
  
  ## Optionally use the sample internal edges as an extra source of information
  if (include.sample.internal.edges) {
    withinappedges <- lapply(r.n, function(x) {
      as_edgelist(x$graphs$PCA)
    })
    withinappedges <- as.data.frame(do.call(rbind, withinappedges),stringsAsFactors=F)
    colnames(withinappedges) <- c('mA.lab','mB.lab')
    withinappedges$weight <- c(internal.edge.weight)
    withinappedges$type <- c('intra')
    # Append internal edges to the mnn edges
    mnnres.all <- rbind(withinappedges, mnnres.all)
  }
  
  ## Make a graph with the MNNs
  el <- matrix(c(mnnres.all$mA.lab, mnnres.all$mB.lab), ncol=2)
  g  <- graph_from_edgelist(el, directed =FALSE)
    
  
  # Add weights
  E(g)$weight <- as.numeric(mnnres.all$weight)

  # Add type
  E(g)$type <- mnnres.all$type

  if (stop.return.graph) {
      return(g);
  }
    
  if (neighbourhood.cleanup) {
      if (is.null(custom.neighbourhood.cleanup)) {
          g <- cleanupGraph(g)
      } else {
          # optionally use custom provided cleanup function
          neighbourhood.cleanup.params$graph <- g;
          g <- do.call(custom.neighbourhood.cleanup, neighbourhood.cleanup.params);
          # g <- custom.neighbourhood.cleanup(g)
      }
  }
    
  ## Do community detection on this graph
  cat('Detecting clusters ...');
  community.detection.params$graph <- g  
  cls <- do.call(community.detection.method, community.detection.params);
  # cls <- community.detection.method(g)
  cat('done\n')
  ## Extract groups from this graph
  cls.mem <- membership(cls)
  cls.groups <- as.character(cls.mem)
  names(cls.groups) <- names(cls.mem)
  
  ## Filter groups
  lvls.keep <- names(which(table(cls.groups)  > min.group.size))
  cls.groups[! as.character(cls.groups) %in% as.character(lvls.keep)] <- NA
  cls.groups <- as.factor(cls.groups)
  
  ret <- cls.groups;
  if (extra.info) {
    ret <- list(cls.groups = cls.groups, el = el);
  }
  ret;
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

    if (is.null(main)) main <- names(r.n);
    
    pal <- rainbow(nlevels(cl), v = 0.8)
    pal <- alpha(pal, alpha)
    names(pal) <- levels(cl)
    
    mapply(function(r,n) {
        r$plotEmbedding(type='PCA',
                        embeddingType ='tSNE',
                        groups=cl[rownames(r$embeddings$PCA$tSNE)],
                        mark.clusters =T,
                        main = n
                       );
                        
        ##plot(r$embeddings$PCA$tSNE, col = pal[cl[rownames(r$embeddings$PCA$tSNE)]], pch=20, cex=2,main=n)
    },r.n, main);

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
#' @param log.scale work on logarithmic scale
#' @param center center the data
#' @param verbose verbosity level
#' @param ncomps number of components form the gsvd to use
#' @return a data frame with all the pairs of the mutual neighbours
#' @export getNNforP2pair
getNNforP2pair <- function(r1, r2, var.scale =T , k = 30, log.scale=T,
                            center=T, verbose =T, ncomps = 100, plot.projection = F,
                            neighbourhood.average = TRUE, mutualOnly = TRUE, var.scale.joint = FALSE) {
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
        if (var.scale.joint) {
            joint.var <- sqrt((r1$misc$varinfo[odgenes,]$gsf ^ 2 + r2$misc$varinfo[odgenes,]$gsf ^ 2) / 2 )
            x1 <- sweep(x1, 2, joint.var, FUN='*')
            x2 <- sweep(x2, 2, joint.var, FUN='*')
        } else {
            x1 <- sweep(x1, 2, r1$misc$varinfo[odgenes,]$gsf, FUN='*')
            x2 <- sweep(x2, 2, r2$misc$varinfo[odgenes,]$gsf, FUN='*')
        }
    }

    ## Optionally log scale
    pseudocount <- 1e-6;
    if (log.scale) {
        x1 <- log10(x1 + pseudocount);
        x2 <- log10(x2 + pseudocount);
    }

    ## Remove mean, jointly
    if (center) {
        x1.colMean <- Matrix::colMeans(x1)
        x2.colMean <- Matrix::colMeans(x2)
        means <- (x1.colMean + x2.colMean) /2
        x1 <- sweep(x1, 2, means, FUN='-')
        x2 <- sweep(x2, 2, means, FUN='-')
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
    x1.rot <- o$U %*% gsvd.D1(o) %*% gsvd.oR(o)
    x2.rot <- o$V %*% gsvd.D2(o) %*% gsvd.oR(o)

    # This is the same thing
    # toqi <- solve(t(o$Q))
    #x1.rot <- x1 %*% toqi
    #x2.rot <- x2 %*% toqi
    if (verbose) cat('done\n');

    # If ncomps is specified take only those from each dataset
    if (!is.null(ncomps)) {
        D1 <- gsvd.D1(o)
        D2 <- gsvd.D2(o)

        D1d <- diag(D1);
        D2d <- diag(D2);

        if (ncomps > length(D1d)) {
            warning('More components than available were specified');
            ncomps <- length(D1d);
        }

        D1.cutoff <- sort(D1d,decr=T)[ncomps]
        D2.cutoff <- sort(D2d,decr=T)[ncomps]

        comps.keep <- c(which(D1d > D1.cutoff), which(D2d > D2.cutoff))

        if (plot.projection) {
            if (comps.keep <= 5) {
            plot(as.data.frame(rbind(x1.rot,x2.rot)[,comps.keep]),
                 col=c(rep('red',dim(x1.rot)[1]),rep('blue',dim(x2.rot)[1])),
                 pch=16,cex=0.2)
            } else {
                warning('plot.projection set to TRUE and comps.keep > 5: Can\'t plot that many components')
            }
        }

        x1.rot <- x1.rot[,comps.keep]
        x2.rot <- x2.rot[,comps.keep]
    }

    ## In this space assign cells by mutual NNs
    if (verbose) {cat('Finding MNNs... ');}
    mnnres <- pagoda2:::interNN(x1.rot,x2.rot,k1,k2,2,verbose= verbose, 
                                 neighbourhoodAverage = neighbourhood.average,
                                 mutualOnly = mutualOnly)
    if (verbose) cat('done\n');

    mnnres$mA.lab <- rownames(r1$counts)[mnnres$mA.id]
    mnnres$mB.lab <- rownames(r2$counts)[mnnres$mB.id]

    mnnres
}

#' Classify cells in a pagoda2 application given multiple pagoda2 objects
#' @description This function is similar to identifyCellsGSVDRF() but it can
#' take as input a list of annotated pagoda2 objects. It then classifies cells
#' against all applications and summarises the results.
#' @param referencesets input datasets (p2 objects in a names list)
#' @param annotset p2 dataset to be annotated
#' @param clustersOrigin a names factor with cluster assignments for referenceset cells
#' @param n.trees number of trees to use per app in total
#' @param n.cores number of cores to use for the random forests
#' @return a named factor with labells for annotset
#' @export identifyCellsGSVDMNNmulti
identifyGSVDRFMulti <- function(referencesets, annotset, clustersOrig,
                                n.trees = 100, n.cores = 1) {
  require(dplyr)
  d1 <- lapply(seq_along(referencesets), function(i) {
    cat(paste0('Processing reference set ', names(referencesets)[i]), '\n')
    curset <- referencesets[[i]]
    cl1 <- clustersOrig[rownames(curset$counts)]
    ident <- identifyCellsGSVDRF(curset, annotset, cl1, n.cores=n.cores, n.trees=n.trees)
  })

  cat('Summarizing ... ');
  d2 <- unlist(lapply(d1, function(x) {n <- names(x); t <- as.character(x); names(t) <- n; t}))
  d2 <- data.frame(cell.id = names(d2), match=d2)
  d3 <- ddply(d2, .(cell.id), function(x) {data.frame(cell.id=x$cell.id[1], match=Mode(x$match))})
  d4 <- as.factor(d3$match)
  names(d4) <- as.character(d3$cell.id)
  cat('done\n');

  d4
}


#' Classify cells in a pagoda2 application given an annotated pagoda2 object
#' @description Given a reference pagoda2 application an a set of labels for each
#' cell, classify cells in a new application. This function works by placing both
#' datasets onto a common GSVD and using random forests in that sapce
#' @param referenceP2 pagoda2 object to use as reference for the annotation
#' @param r2 pagoda2 object to the annotation of
#' @param referenceP2lables a factor providing labels for every cell in reference P2
#' @param var.scale perform variance scaling (according to p2 variance estimate)
#' @param center perform centering
#' @param verbose print progress messages
#' @param extra.info return extra information from the run
#' @param n.cores number of cores to use
#' @param n.trees number of trees to use in total
#' @export identifyCellsGSVDMNN
identifyCellsGSVDRF <- function (referenceP2, r2, referenceP2labels,
                                 var.scale =T, log.scale =T , center =T,
                                 verbose = T, extra.info = F, n.cores = 1, n.trees = 1000) {

  require('plyr')
  require('geigen')
  require('randomForest')
  require('doMC')

  ## Check referenceP2 and r2 objects
  if( !(class(referenceP2) == 'Pagoda2' & class(r2) == 'Pagoda2')) {
    stop('Both referenceP2 and r2 have to be objects of type Pagoda2');
  }

  ## Add check for referenceP2labels
  if (verbose) cat('Preparing data... ');

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
  x1.rot <- x1 %*% o$Q
  x2.rot <- x2 %*% o$Q

  if (verbose) cat('done\n');

  # Use RF in the rotated space to classify the cells

  if (verbose) cat('Building random forest...')

  ## Make the target labels
  resp <- referenceP2labels[rownames(x1.rot)]
  resp <- droplevels(resp[!is.na(resp)])

  x <- x1.rot[names(resp),]
  y <- resp

  registerDoMC(n.cores)

  # Allocate jobs per core
  n.per.core <- n.trees %/% n.cores
  s <- rep(n.per.core, n.cores)
  modulo <- n.trees %% n.cores
  if (modulo) { s[1:modulo] <- s[1:modulo] + 1 }

  # Run the random forests
  rf <- foreach(ntree=s, .combine = randomForest::combine, .multicombine =T, .packages='randomForest' ) %dopar%
    randomForest(x=x, y=y , ntree= ntree)
  cat('done\n');

  cat('Classifying target dataset... ')
  p <- predict(rf,newdata = x2.rot)
  cat('done.\n')

  p
}

#' Clean up an inter-sample graph
#' @param g the graph to cleanup
#' @param min.neigh.con the number of neighbour matches to require
#' @return a new cleaned up graph
#' @export cleanupGraph
cleanupGraph <- function(g, min.neigh.con = 1, show.progress = TRUE) {

#    g <- g3; min.neigh.con <- 2; show.progress <- T

    ## Get a copy of the graph with out intersample edges
    g.intra <- g
    g.intra <- delete_edges(g.intra,E(g.intra)[type == 'inter'])

    ## Keep info about edges
    nes <- c()
    netype <- c()
    neweight <- c()

    edgeList <- E(g)
    vertexList <- V(g)
    edgeCount <- length(edgeList)

    if(show.progress) pb <- txtProgressBar(min=0,max=edgeCount, style =3)

    # igraph doesn't work well with apply
    # so using an env here for speed
    res.counter <- 0
    res <- new.env()


    
    for (ei in 1:edgeCount) {
        if (show.progress) setTxtProgressBar(pb, ei);

        e <- edgeList[ei]

        ## Get the ends of the edge
        cur.ends <- ends(g,e)
        v1 <- cur.ends[1]
        v2 <- cur.ends[2]

        if (e$type == 'inter') {        
            ## Intra-sample neighbours
            nv1 <- names(neighbors(g.intra, v1))
            nv2 <- names(neighbors(g.intra, v2))

            c <- 0;
            for (i in 1:length(nv1)) {
                if (are.connected(g, vertexList[nv1[i]], v2))  {
                    c <- c + 1;
                    if (c >= min.neigh.con) {
                        break
                    }
                }
            }

            
            if (c < min.neigh.con) {
                for (i in 1:length(nv2)) {
                    if (are.connected(g, vertexList[nv2[i]], v1)) {
                        c <- c + 1;
                        if (c >= min.neigh.con) {
                            break
                        }
                    }
                }
            }

            ## Keep edge if sufficiently connected
            if (c >= min.neigh.con) {
                res[[as.character(res.counter)]] <- c(v1=v1,v2=v2,type='inter',weight=e$weight)
                res.counter <- res.counter + 1
            }
        } else {
            res[[as.character(res.counter)]] <- c(v1=v1,v2=v2,type='intra',weight=e$weight)
            res.counter <- res.counter + 1
        } ## Edge type
    } ## For

    ## Recover results
    res <- as.list(res)
    nFrom <- unname(unlist(lapply(res,'[',1)))
    nTo <- unname(unlist(lapply(res,'[',2)))
    nType <-unname(unlist(lapply(res,'[',3)))
    nWeight <-unname(unlist(lapply(res,'[',4)))

    nes <- c(rbind(nFrom,nTo))
    
    if (show.progress) close(pb)
                                        ## Make and return a graph just with the desired edges
    gn <- delete_edges(g, E(g))
    gn <- add_edges(gn, nes, attr=list(type=nType,weight = nWeight))

    gn
}


#' A replacement function of getMNNforP2pairCustom
#' @export getMNNforP2pairCustom
getMNNforP2pairCustom <- function(r1, r2, var.scale =T , k = 30, log.scale=T,
                            center=T, verbose =T, ncomps = 100, plot.projection = F) {

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

    ## Remove mean, jointly
    if (center) {
        x1.colMean <- Matrix::colMeans(x1)
        x2.colMean <- Matrix::colMeans(x2)
        means <- (x1.colMean + x2.colMean) /2
        x1 <- sweep(x1, 2, means, FUN='-')
        x2 <- sweep(x2, 2, means, FUN='-')
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
    #x1.rot <- o$U %*% gsvd.D1(o) %*% gsvd.oR(o)
    #x2.rot <- o$V %*% gsvd.D2(o) %*% gsvd.oR(o)

    ## This is the same thing
    stoq <- solve(t(o$Q)) 
    x1.rot <- x1 %*% stoq
    x2.rot <- x2 %*% stoq
    
    if (verbose) cat('done\n');

                                        # If ncomps is specified take only those from each dataset
    if (!is.null(ncomps)) {
        D1 <- gsvd.D1(o)
        D2 <- gsvd.D2(o)

        D1d <- diag(D1);
        D2d <- diag(D2);

        if (ncomps > length(D1d)) {
            warning('More components than available were specified');
            ncomps <- length(D1d);
        }

        D1.cutoff <- sort(D1d,decr=T)[ncomps]
        D2.cutoff <- sort(D2d,decr=T)[ncomps]

        comps.keep <- c(which(D1d > D1.cutoff), which(D2d > D2.cutoff))

        if (plot.projection) {
            if (comps.keep <= 5) {
                plot(as.data.frame(rbind(x1.rot,x2.rot)[,comps.keep]),
                     col=c(rep('red',dim(x1.rot)[1]),rep('blue',dim(x2.rot)[1])),
                     pch=16,cex=0.2)
            } else {
                warning('plot.projection set to TRUE and comps.keep > 5: Can\'t plot that many components')
            }
        }

        x1.rot <- x1.rot[,comps.keep]
        x2.rot <- x2.rot[,comps.keep]
    }

    if (verbose) {cat('Finding inter-app network...')}
    all.data <- rbind(x1.rot, x2.rot);
    xn <- pagoda2:::hnswKnn2(all.data,k=30,nThreads=n.cores, verbose=T)

    ## Now from this network only keep intersample connections
    xn$s.cell <- rownames(all.data)[xn$s + 1]
    xn$e.cell <- rownames(all.data)[xn$e + 1]

    ## Identify sample of origin
    xn$s.cell.sample <- c(1)
    xn[as.character(xn$s.cell) %in% rownames(x2.rot),]$s.cell.sample <- c(2)
    xn$e.cell.sample <- c(1);
    xn[as.character(xn$e.cell) %in% rownames(x2.rot),]$e.cell.sample <- c(2)

    ## Keep only intersample nodes
    xn <- xn[xn$s.cell.sample != xn$e.cell.sample,]

    if (verbose) {cat('done\n')} ## finding interapp net

    ## Make a suitable data.frame
    mnnres <- data.frame();
    mnnres$mA.lab <- rownames(r1$counts)[mnnres$mA.id]
    mnnres$mB.lab <- rownames(r2$counts)[mnnres$mB.id]

    mnnres
}



## #' Devel version of getJointClustering
## #' @export getJointClustering2
## getJointClustering2<- function(r.n, k=30,
##                                      community.detection.method = walktrap.community,
##                                      min.group.size = 10,ncomps=100,
##                                      include.sample.internal.edges=TRUE,
##                                      mnn.edge.weight = 1, internal.edge.weight =1,
##                                      extra.info = F, mnnPairFunction =  NULL) {
##       if (is.null(mnnPairFunction)) { error('No mnnPairFunction provided') }
    
##       require('gtools')
##       require('pbapply')
##       require('igraph')

##       ## Get all non-redundant pair of apps
##       nms <- names(r.n)
##       combs <- combinations(n = length(nms), r = 2, v = nms, repeats.allowed =F)

##       ## Convert to list for lapply
##       combsl <- split(t(combs), rep(1:nrow(combs), each=ncol(combs)))

    
##       ## get MNN pairs from all possible app pairs
##       cat('Calculating MNN for application pairs ...\n')
##       mnnres <- pblapply(combsl, function(x) {
##           mnnPairFunction(r.n[[x[1]]], r.n[[x[2]]], k = k, verbose =F, ncomps = ncomps);
##       });

##       ## Merge the results into a edge table
##       mnnres.all <- do.call(rbind, mnnres)[,c('mA.lab','mB.lab')]
##       summary(mnnres.all)
##       mnnres.all$weight <- c(mnn.edge.weight)

##       ## Optionally use the sample internal edges as an extra source of information
##       if (include.sample.internal.edges) {
##               withinappedges <- lapply(r.n, function(x) {
##                         as_edgelist(x$graphs$PCA)
##                             })
##                   withinappedges <- as.data.frame(do.call(rbind, withinappedges),stringsAsFactors=F)
##                   colnames(withinappedges) <- c('mA.lab','mB.lab')
##                   withinappedges$weight <- c(internal.edge.weight)
##                   # Append internal edges to the mnn edges
##                   mnnres.all <- rbind(withinappedges, mnnres.all)
##                 }

##       ## Make a graph with the MNNs
##       el <- matrix(c(mnnres.all$mA.lab, mnnres.all$mB.lab), ncol=2)
##       g  <- graph_from_edgelist(el, directed =FALSE)

##       # Add weights
##       E(g)$weight <- as.numeric(mnnres.all$weight)

##       ## Do community detection on this graph
##       cat('Detecting clusters ...');
##       cls <- community.detection.method(g)
##       cat('done\n')
##       ## Extract groups from this graph
##       cls.mem <- membership(cls)
##       cls.groups <- as.character(cls.mem)
##       names(cls.groups) <- names(cls.mem)

##       ## Filter groups
##       lvls.keep <- names(which(table(cls.groups)  > min.group.size))
##       cls.groups[! as.character(cls.groups) %in% as.character(lvls.keep)] <- NA
##       cls.groups <- as.factor(cls.groups)

##       ret <- cls.groups;
##       if (extra.info) {
##           ret <- list(cls.groups = cls.groups, el = el);
##       }
##       ret;
## }
