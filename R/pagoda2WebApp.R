## Filename: pagoda2WebApp.R
## Author: Nikolas Barkas
## Date: Jan - Mar 2017
## Description: The rook server for pagoda 2


#' @import Rook
#' @importFrom urltools url_decode
#' @importFrom rjson fromJSON toJSON
#' @importFrom dendsort dendsort
#' @import base64enc

#' @export pagoda2WebApp
#' @exportClass pagoda2WebApp
pagoda2WebApp <- setRefClass(
  'pagoda2WebApp',
  'contains' = 'Middleware', # Inherit from Middleware to handle static and dynamic files seperately
  fields = c(
    "name", # The name of this application for display purposes
    "verbose", # Server verbosity level
    "mat",
    "cellmetadata",
    "mainDendrogram",
    "geneSets",
    "originalP2object",
    "rookRoot",
    "appmetadata"
  ),
  
  methods = list(
    ## pagoda2obj: a pagoda2 object
    ## appName: the display name for this app
    ## verbose: verbosity level, def: 0, higher values will printmore
    ## debug: T|F load debug version?, def: F
    ## dendGroups: a factor defining the groups of cells to use for the dendrogram
    ## keepOriginal: maintain a copy to the original RF object -- this allows for some extra capabilities
    initialize = function(pagoda2obj, appName = "DefaultPagoda2Name", dendGroups,
                          verbose = 0, debug, geneSets, metadata=metadata,
                          innerOrder=NULL,orderDend=FALSE,appmetadata = NULL) {

      ## Check that the object we are getting is what it should be
      if (class(pagoda2obj) != "Pagoda2") {
        cat("We have an error");
        stop("ERROR: The provided object is not a pagoda 2 object")
      }
      
      ## Keep the original pagoda 2 object
      originalP2object <<- pagoda2obj
      
      ## Check that the dendGroups we are getting is what it should be
      if (length(dendGroups) != nrow(pagoda2obj$counts)) {
        stop("ERROR: The provided dendGroups has a different number of cells than the pagoda 2 object")
      }
      
      ## Keep the name for later (consistent) use
      name <<- appName;
      
      ## Using the cell grouping provided in dendGroups
      # Generate an hclust object of these cell groups
      # a cell ordering compatible with these groups
      # an the number of cells in each group (for plotting purposes)
      mainDendrogram <<- .self$generateDendrogramOfGroups(pagoda2obj,dendGroups,innerOrder,orderDend);
      
      # Verbosity level
      verbose <<- verbose;
      
      # Genesets
      geneSets <<- geneSets;
      # The cell metadata
      cellmetadata <<- metadata;
      
      # The application metadata for things like the title
      appmetadata <<- appmetadata;
      
      # Rook sever root directory to be changed to package subdirectory
      # this holds all the static files required by the app
      rookRoot <<- file.path(system.file(package='pagoda2'),'rookServerDocs');
      
      # This Uses Middleware to process all the requests that
      # our class doesn't process
      callSuper(app = Builder$new(
        # JS and CSS that are NOT part of ExtJS
        Static$new(
          urls = c('/js','/css','/img'),
          root = c(rookRoot)
        ),
        
        # Unhandled requests go here
        App$new(function(env){
          # everything else end here
          res <- Response$new();
          res$header('Content-Type', 'text/html');
          cat("Unhandled request for: ");
          cat(env[['PATH_INFO']]);
          cat("\n");
          res$finish();
        })
      ));
    },
    
    generateDendrogramOfGroups = function(r, dendrogramCellGroups,innerOrder = NULL,orderDend=FALSE){
      cl0 <- dendrogramCellGroups
      # Generate an hclust objct of the above groups
      dendrogramCellGroups <- dendrogramCellGroups[match(rownames(r$counts),names(dendrogramCellGroups))]
      lvec <- colSumByFac(r$misc[['rawCounts']],as.integer(cl0))[-1,,drop=F] + 1
      lvec <- t(lvec/pmax(1,rowSums(lvec)))
      colnames(lvec) <- which(table(cl0)>0)
      rownames(lvec) <- colnames(r$misc[['rawCounts']])
      ld <- jsDist(lvec);
      colnames(ld) <- rownames(ld) <- colnames(lvec)
      
      #hcGroup is a hclust object of whatever cell groupings we provided above
      hcGroups <- hclust(as.dist(ld), method = 'ward.D');
      
      if(orderDend){
        hcGroups <- dendsort::dendsort(hcGroups)
      }
      # We now need to derive a cell order compatible with the order
      # of the above dendrogram
      if(!is.null(innerOrder) && innerOrder == "knn") {
        message("Creating reductions and knn-embedding for all groups")
        message("This might take a bit")
      }
      
      cellorder <- unlist(lapply(hcGroups$labels[hcGroups$order], function(x) {
        if(is.null(innerOrder)){
          base::sample(names(cl0)[cl0 == x]) # Sample for random order
          
        } else {
          if(length(names(cl0)[cl0 == x]) < 5){
            base::sample(names(cl0)[cl0 == x]);
            message(paste("Cluster", x ,"contains less than 5 cells - no Ordering applied"))
          } else {
            if(innerOrder == "odPCA") {
              
              if(!"odgenes" %in% names(r$misc)){
                stop("Missing odgenes for odPCA");
              } else {
                celsel <- names(cl0)[cl0 == x]
                vpca <- r$counts[celsel, r$misc$odgenes] %*% irlba::irlba(r$counts[celsel,r$misc$odgenes],nv = 1,nu=0)$v # run a PCA on overdispersed genes just in this selection.
                celsel[order(vpca,decreasing = T)] # order by this PCA
              }
              
            } else if (innerOrder == "reductdist") {
              if(!"PCA" %in% names(originalP2object$reductions)){
                stop("Missing PCA reduction, , run calculatePcaReduction first");
              } else {
                
                celsel <- names(cl0)[cl0 == x]
                celsel[hclust(as.dist(1-WGCNA::cor(t(originalP2object$reductions$PCA[celsel,]))))$order] # Hierarchical clustering of cell-cell correlation of the PCA reduced gene-expressions
              }
              
            } else if(innerOrder == "graphbased") {
              
              if(!"PCA" %in% names(r$graphs)){
                stop("Missing graph, , run makeKnnGraph first");
              } else {
                celsel <- names(cl0)[cl0==x]
                sgraph <- igraph::induced_subgraph(r$graphs$PCA,(celsel))
                celsel[hclust(as.dist(1-WGCNA::cor(t(igraph::layout.auto(sgraph,dim=3)))))$order]
              }
              
            } else if(innerOrder == "knn") {
              celsel <- names(cl0)[cl0 == x]
              k <- round(pmax(length(celsel)*0.20,5)) # Coarse estimate for k
              nv <- ceiling(pmax(length(celsel)*0.10,5)) # Coarse estimate for appropriate number of PCs
              
              xx <- r$counts[celsel,] %*% irlba::irlba(r$counts[celsel,],nv = nv,nu=0)$v
              colnames(xx) <- paste('PC',seq(ncol(xx)),sep='')
              
              xn <- n2Knn(as.matrix(xx),k,nThreads=r$n.cores,verbose=0,indexType='L2')
              
              xn <- xn[!xn$s==xn$e,]
              xn$r <-  unlist(lapply(diff(c(0,which(diff(xn$s)>0),nrow(xn))),function(x) seq(x,1)))
              
              df <- data.frame(from=rownames(xx)[xn$s+1],to=rownames(xx)[xn$e+1],weight=xn$d,stringsAsFactors=F)
              
              df$weight <- pmax(0,df$weight);
              xn <- cbind(xn,rd=df$weight)
              edgeMat <- sparseMatrix(i=xn$s+1,j=xn$e+1,x=xn$rd,dims=c(nrow(xx),nrow(xx)))
              edgeMat <- edgeMat + t(edgeMat);
              wij <- buildWijMatrix(edgeMat,perplexity=100,threads=r$n.cores)
              coords <- projectKNNs(wij = wij, dim=1, M = 5, verbose = FALSE,sgd_batches = 2e6,gamma=1, seed=1)
              
              rownames(xx)[order(coords)]
              
            } else {
              stop(paste0(innerOrder," is not a possible option for the inner Clustering."))
            }
          }
        }
      }))
      
      # We need the cell order cluster sizes
      cellorder.cluster.sizes <- table(cl0)
      
      list(
        hc = hcGroups,
        cellorder = cellorder,
        cluster.sizes = cellorder.cluster.sizes
      );
    },
    
    packCompressInt32Array = function(v) {
      rawConn <-  rawConnection(raw(0), "wb");
      writeBin(v, rawConn);
      xCompress <- memCompress(rawConnectionValue(rawConn), 'gzip')
      xSend <- base64encode(xCompress);
      close(rawConn);
      
      xSend
    },
    
    packCompressFloat64Array = function(v){
      rawConn <- rawConnection(raw(0), "wb");
      writeBin(v, rawConn);
      xCompress <- memCompress(rawConnectionValue(rawConn),'gzip')
      xSend <- base64encode(xCompress)
      close(rawConn)
      
      xSend
    },
    
    # Handle httpd server calls
    call = function(env) {
      path <- env[['PATH_INFO']];
      request <- Request$new(env);
      response <- Response$new()
      switch( path,
              ## Static files that are not handled by the Static class
              # Get the main script
              '/index.html' = {
                response$header('Content-Type', 'text/html');
                response$write(readStaticFile('/index.html'));
                return(response$finish());
              },
              
              ## Dynamic Request Handling
              '/getData.php' = {
                
                requestArguments <- request$GET();
                dataIdentifier <- requestArguments[['dataidentifier']];
                
                if (!is.null(dataIdentifier)) {
                  # Handle a getData request
                  switch(dataIdentifier,
                         'appmetadata' = {
                           response$write(toJSON(appmetadata));
                           return(response$finish());
                         },
                         
                         'relatedgenes' = {
                           # This requires the original object -- must add check
                           postArgs <- request$POST();
                           
                           geneListName <- postArgs[['querygenes']];
                           relatedGenes <- originalP2object$genegraphs$graph$to[originalP2object$genegraphs$graph$from %in% geneListName]
                           relatedGenes <- relatedGenes[!duplicated(relatedGenes)];
                           relatedGenes <- relatedGenes[!relatedGenes %in% geneListName];
                           
                           response$header("Content-type", "application/javascript");
                           response$write(toJSON(relatedGenes));
                           return(response$finish());
                         },
                         
                         'embeddingstructure' = {
                           response$write(toJSON(generateEmbeddingStructure()));
                           return(response$finish());
                         },
                         
                         'expressionmatrixsparsebycellname' = {
                           cat('Got Request 1');
                           postArgs <- request$POST();
                           cellNames <- url_decode(postArgs[['cellnames']]);
                           cellNames <- unlist(strsplit(cellNames, split = "|", fixed =T));
                           
                           getCellNames <- TRUE;
                           
                           counts.transposed <- t(originalP2object$counts)
                           
                           ## Matrix to send
                           matrixToSend <- counts.transposed[,cellNames,drop = F]
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x);
                           iSend <- .self$packCompressInt32Array(matrixToSend@i);
                           pSend <- .self$packCompressInt32Array(matrixToSend@p);
                           
                           Dimnames1Send <- "";
                           if (getCellNames) {
                             Dimnames1Send <- matrixToSend@Dimnames[[1]];
                           };
                           
                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" );
                           response$write(toJSON(objToSend));
                           
                           return(response$finish());
                         },
                         
                         ## Return a gene information table for all the genes
                         'geneinformation' = {
                           retd <- geneInformationJSON();
                           response$header("Content-type", "application/javascript");
                           response$write(retd);
                           return(response$finish());
                         },
                         
                         # Very similar to 'geneinformation'
                         # but only returns information for the genes that belong
                         # to the specified geneset
                         'genesetgeneinformation' = {
                           geneListName <- requestArguments[['genesetname']];
                           
                           # Get the genes in this geneset
                           geneList <- geneSets[[geneListName]]$genes
                           
                           # Subset to genes that exist
                           geneList <- geneList[geneList %in% rownames(originalP2object$misc$varinfo)];
                           
                           # Generate dataset
                           dataset <-  originalP2object$misc$varinfo[geneList, c("m","qv")];
                           dataset$name <-  rownames(dataset);
                           
                           # Convert to row format
                           retd <-  apply(dataset,
                                          1, function(x) {
                                            list(genename = x[["name"]],
                                                 dispersion =x[["qv"]],
                                                 meanExpr = x[["m"]])
                                          });
                           retd <- unname(retd);
                           
                           response$header("Content-type", "application/javascript");
                           response$write(toJSON(retd));
                           return(response$finish());
                         },
                         
                         # Get the names of the available gene sets
                         'availablegenesets' = {
                           ret <- lapply(geneSets, function(x) {x$properties})
                           # Neet to unname because otherwise its not a JSON array
                           # names are in properties anyway
                           ret <- unname(ret);
                           response$write(toJSON(ret));
                           return(response$finish());
                         },
                         
                         'availableaspects' = {
                           # /getData.php?dataidentifier=availableaspects
                           response$header("Content-type", "application/javascript");
                           response$write(toJSON(rownames(originalP2object$misc$pathwayOD$xv)));
                           return(response$finish());
                         },
                         
                         'genesetsinaspect' = {
                           requestArguments <- request$GET();
                           aspectId <- url_decode(requestArguments[['aspectId']]);
                           # Genesets in this aspect
                           genesets <- unname(originalP2object$misc$pathwayOD$cnam[[aspectId]]);
                           # Get the metadata for these gene sets
                           colOfInterest <- c("name","n","cz");
                           retTable <- originalP2object$misc$pathwayODInfo[genesets, colOfInterest];
                           # Convert to JSON friendly format
                           retObj <- unname(apply(retTable, 1, function(x) {
                             # Must be in genesets for short description
                             desc <- geneSets[[x[[1]]]]$properties$shortdescription;
                             list(name = x[[1]], n = x[[2]], cz = x[[3]], shortdescription = desc);
                           }))
                           
                           response$header("Content-type", "application/javascript");
                           response$write(toJSON(retObj));
                           return(response$finish());
                         },
                         
                         # Request for reduced dendrogram, down to some
                         # cell partitioning, this returns an hcluse object
                         # as well as the number of cells in each cluster
                         'reduceddendrogram' = {
                           response$header("Content-type", "application/javascript");
                           response$write(reducedDendrogramJSON());
                           return(response$finish());
                         },
                         
                         'cellorder' = {
                           response$header("Content-type", "application/javascript");
                           response$write(cellOrderJSON());
                           return(response$finish());
                         },
                         
                         # Get cell metadata information
                         'cellmetadata' = {
                           response$header("Content-type", "application/javascript");
                           response$write(toJSON(cellmetadata));
                           return(response$finish());
                         },
                         
                         'aspectmatrixbyaspect' = {
                           postArgs <- request$POST();
                           aspectIds <- url_decode(postArgs[['aspectids']]);
                           cellIndexStart <- url_decode(postArgs[['cellindexstart']]);
                           cellIndexEnd <- url_decode(postArgs[['cellindexend']]);
                           aspectIds <- unlist(strsplit(aspectIds, split = "|", fixed =T));
                           
                           cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)];
                           matrixToSend <- originalP2object$misc$pathwayOD$xv[aspectIds,cellIndices,drop=F];

                           # Transpose and make sparse
                           matrixToSend <- Matrix(t(matrixToSend), sparse = T);
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x);
                           iSend <- .self$packCompressInt32Array(matrixToSend@i);
                           pSend <- .self$packCompressInt32Array(matrixToSend@p);

                           Dimnames1Send <- matrixToSend@Dimnames[[1]];

                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" );
                           response$write(toJSON(objToSend));
                           return(response$finish());
                         },
                         
                         'aspectmatrixsparsebyindexbinary' = {
                           
                           postArgs <- request$GET();
                           
                           cellIndexStart <- url_decode(postArgs[['cellindexstart']]);
                           cellIndexEnd <- url_decode(postArgs[['cellindexend']]);
                           getCellNames <- url_decode(postArgs[['getcellnames']]);
                           
                           cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)];
                           matrixToSend <- originalP2object$misc$pathwayOD$xv[,cellIndices,drop=F];

                           # Transpose and make sparse
                           matrixToSend <- Matrix(t(matrixToSend), sparse = T);
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x);
                           iSend <- .self$packCompressInt32Array(matrixToSend@i);
                           pSend <- .self$packCompressInt32Array(matrixToSend@p);
                           
                           Dimnames1Send <- "";
                           if (getCellNames) {
                             Dimnames1Send <- matrixToSend@Dimnames[[1]];
                           };
                           
                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" );
                           response$write(toJSON(objToSend));
                           return(response$finish());
                         },
                         
                         'expressionmatrixsparsebyindexbinary' = {
                           postArgs <- request$POST();

                           if (is.null(postArgs[['geneids']])) {
                             serverLog("Error postArgs[['geneids']] is NULL");
                           }
                           if (is.null(postArgs[['cellindexstart']])) {
                             serverLog("Error postArgs[['cellindexstart']] is NULL");
                           }
                           if (is.null(postArgs[['cellindexend']])) {
                             serverLog("Error postArgs[['cellindexend']] is NULL");
                           }
                           if (is.null(postArgs[['getCellNames']])) {
                             serverLog("Error postArgs[['getCellNames']] is NULL");
                           }
                           
                           geneIdentifiers <- url_decode(postArgs[['geneids']]);
                           geneIdentifiers <- unlist(strsplit(geneIdentifiers, split = "|", fixed =T));
                           
                           cellIndexStart <- url_decode(postArgs[['cellindexstart']]);
                           cellIndexEnd <- url_decode(postArgs[['cellindexend']]);
                           getCellNames <- url_decode(postArgs[['getCellNames']]);
                           
                           
                           if (!all(c(geneIdentifiers %in% colnames(originalP2object$counts)))) {
                             serverLog("Error: The request contains gene names that are not in originalP2object$counts!");
                             geneIdentifiers <- geneIdentifiers[geneIdentifiers %in% colnames(originalP2object$counts)];
                           }
                           
                           # Ordering of the matrix according to the hclust
                           cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)]
                           matrixToSend <- originalP2object$counts[cellIndices,geneIdentifiers,drop=F];
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x);
                           iSend <- .self$packCompressInt32Array(matrixToSend@i);
                           pSend <- .self$packCompressInt32Array(matrixToSend@p);
                           
                           Dimnames1Send <- "";
                           if (getCellNames) {
                             Dimnames1Send <- matrixToSend@Dimnames[[1]];
                           };
                           
                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" );
                           response$write(toJSON(objToSend));
                           return(response$finish());
                         },
                         
                         # Get a reduction
                         #
                         # GET Arguments accepted
                         # type -- the reduction type (e.g. mat, odgenes, PCA)
                         #         available types can be obtained from the
                         #         'availablereductiontypes' command
                         # colnames --  the names of the columns (cells) to return
                         #              if empty return all
                         # rownames --  the names of the rows (genes) to return
                         #              if empty return all
                         # ignoremissing -- if set return reduction subset even
                         #                  if rows or columns that do not exist have
                         #                  been specified
                         'reduction' = {
                           reductionname <- url_decode(requestArguments[['type']]);
                           
                           # Is the requested reduction available?
                           if (!is.null(reductionname) && reductionname  %in% c("mat", names(originalP2object$reductions)) ) {
                             
                             workingReduction <- NULL;
                             if (reductionname == "mat") {
                               workingReduction <- mat;
                             } else {
                               workingReduction <- originalP2object$reductions[[reductionname]];
                             }
                             
                             selColNames <- requestArguments[['colnames']];
                             selRowNames <- requestArguments[['rownames']];
                             ignoreMissing <- requestArguments[['ignoremissing']];
                             
                             if (is.null(ignoreMissing)) {
                               ignoreMissing <- F;
                             } else {
                               ignoreMissing <- F;
                             }
                             
                             # Default to all
                             if (is.null(selColNames) ) { selColNames = colnames(workingReduction) }
                             if (is.null(selRowNames) ) { selRowNames = rownames(workingReduction) }
                             
                             # If non-existent rows or cols are specified
                             if ( (!all(selColNames %in% colnames(workingReduction))) | (!all(selRowNames %in% rownames(workingReduction))) ) {
                               if (ignoreMissing) {
                                 selColNames <- selColNames[selColNames %in% colnames(workingReduction)];
                                 selRowNames <- selRowNames[selRowNames %in% rownames(workingReduction)];
                               } else {
                                 response$write("Error: Non existent rows or columns specified and ignoreMissing is not set");
                                 return(response$finish());
                               }
                             }
                             
                             response$header('Content-type', 'application/javascript');
                             response$write(arrayToJSON(workingReduction[selRowNames, selColNames]));
                             return(response$finish());
                           } else {
                             response$write("Error: Unknown reduction type requested");
                             return(response$finish());
                           } # if (!is.null(reductionname
                         }, # 'reduction'
                         
                         ## Get data for an embedding
                         ##
                         ## GET Arguments accepted
                         ## type -- specifies the dataset transformation from which this
                         ##         the requested embedding was derived from
                         ##         ( e.g. mat, PCA, etc...)
                         ## embeddingtype -- specifies the embedding type for this particular
                         ##                  transformation, e.g tSNE, largeVis
                         ##                  values for this parameter are returned
                         ##                  by doing an 'availableembeddings' request
                         'embedding' = {
                           type <- url_decode(requestArguments[['type']]);
                           
                           if (!is.null(type)) {
                             if ( type %in% c( names(originalP2object$embeddings), "mat") ) {
                               response$header('Content-type', "application/javascript");
                               
                               # Which embedding?
                               embeddingType <- url_decode(requestArguments[['embeddingtype']]);
                               
                               if ( (!is.null(embeddingType)) &&  embeddingType %in% names(originalP2object$embeddings[[type]]) ) {
                                 compEmb <- getCompressedEmbedding(type,embeddingType)
                                 response$write(compEmb);
                                 return(response$finish());
                               } else {
                                 response$write(paste0("Error: Unknown embedding specified: ",embeddingType));
                                 return(response$finish());
                               }
                               
                             } else {
                               # TODO: Set the headers and possibly return a JSON encoded error
                               response$write("Error: Unknown type specified");
                               return(response$finish());
                             }
                           }
                           
                         }, # 'embedding'
                         
                         {
                           response$write("Error: Unknown request");
                           return(response$finish());
                         }
                  ) # switch(dataidentifier
                } #if(!is.null(dataIdentifier
              },
              
              # Perform a computation and return the results
              # e.g. Differential expression
              '/doComputation.php' = {
                requestArguments <- request$GET();
                compIdentifier <- url_decode(requestArguments[['compidentifier']]);
                
                if (!is.null(compIdentifier)) {
                  switch(compIdentifier,
                         'doDifferentialExpression1selection' = {
                           cat('doDifferentialExpression1selection\n');
                           
                           postArguments <- request$POST();
                           selectionA <- fromJSON(url_decode(postArguments[['selectionA']]));

                           allcells <- rownames(originalP2object$counts)
                           othercells <- allcells[!allcells %in% selectionA]
                           
                           # Generate factor for de
                           v1 <- c(rep('selectionA',length(selectionA)),rep('othercells',length(othercells)))
                           names(v1) <- c(selectionA, othercells)
                           v1 <- factor(v1)
                           
                           # run de with factor
                           de <- originalP2object$getDifferentialGenes(groups=v1)
                           
                           
                           de$selectionA$name <- rownames(de$selectionA)
                           results <- de$selectionA
                           
                           
                           # Convert to JSON-suitable format
                           # For performance this best done client side, or the resulting string is compressed
                           p <- lapply(c(1:dim(results)[1]), function(x) {
                             list(Z = results[x,][[1]],
                                  absZ = abs(results[x,][[1]]),
                                  M = results[x,][[2]],
                                  highest=results[x,][[3]],
                                  fe=results[x,][[4]],
                                  name=results[x,][[5]])
                           });
                           
                           response$write(toJSON(p));
                           
                           #originalP2object
                           return(response$finish());
                           
                         }, # doDifferentialExpression1selectoin
                         
                         'doDifferentialExpression2selections' = {
                           postArguments <- request$POST();
                           
                           selAarg <- postArguments[['selectionA']];
                           selBarg <- postArguments[['selectionB']];
                           
                           selectionA <- fromJSON(url_decode(selAarg));
                           selectionB <- fromJSON(url_decode(selBarg));
                           
                           # TODO: check that the originalP2object field is populated, as this is optional
                           
                           # Generate factor for de
                           v1 <- c(rep('selectionA',length(selectionA)),rep('selectionB',length(selectionB)))
                           names(v1) <- c(selectionA, selectionB)
                           v1 <- factor(v1)
                           
                           # run de with factor
                           de <- originalP2object$getDifferentialGenes(groups=v1)
                           
                           
                           de$selectionA$name <- rownames(de$selectionA)
                           # de$selectionB$name <- rownames(de$selectionB)
                           # results <- rbind(de$selectionA, de$selectionB)
                           results <- de$selectionA;
                           
                           # Convert to JSON-suitable format
                           # For performance this best done client side, or the resulting string is compressed
                           p <- lapply(c(1:dim(results)[1]), function(x) {
                             list(Z = results[x,][[1]],
                                  absZ = abs(results[x,][[1]]),
                                  M = results[x,][[2]],
                                  highest=results[x,][[3]],
                                  fe=results[x,][[4]],
                                  name=results[x,][[5]])
                           });
                           
                           response$write(toJSON(p));
                           
                           #originalP2object
                           return(response$finish());
                         }
                  );
                }
              }, # doComputation
              
              # Default
              {
                # TODO: Fix the path here, redirects to root
                #response$redirect('index.html');
                app$call(env);
              }
      ) # switch
    }, # call = function(env)
    
    ## Various Helper functions
    
    # Read a static file from the filesystem
    #
    # Read a file from the filesystem and put it in the response
    # @param contentType ContentType header to send out
    # @return content to display or error page
    
    # TODO: Switch to content type autodetect
    readStaticFile =  function(filename) {
      filename <- file.path(rookRoot,filename);
      content <- NULL;
      tryCatch({
        content <- readChar(filename, file.info(filename)$size);
      }, warning = function(w) {
        content <- paste0("File not found: ",filename);
      }, error = function(e) {
        content <- paste0("File not found: ", filename);
      })
    },
    
    updateRookRoot = function(newRoot) {
      # Update the object variable
      rookRoot <<- file.path(system.file(package='pagoda2'),'rookServerDocs');
      
      # Update the middleware static server
      .self$app$app$file_server$root <- rookRoot;
    },
    
    serializeToStaticFast = function(binary.filename=NULL, verbose = FALSE){
      if (is.null(binary.filename)) { stop('Please specify a directory'); }
      
      exportList <- new("list");
      
      # Preparation of objects to pass to Rcpp
      # Create embedding strucutre for export
      embStructure <- generateEmbeddingStructure();
      
      # Export to list For all contained embeddings
      for (reduc in names(embStructure)) {
        for (embed in names(embStructure[[reduc]])) {
          id <- embStructure[[reduc]][[embed]][[1]];
          filename <- paste0(id, '.json');
          e <- getCompressedEmbedding(reduc,embed)
          exportList[filename] <- e;
        }
      }
      
      # Export list with all included embeddings for easier iteration in Rcpp-function.
      exportList[["embedList"]] <- grep("emb_",names(exportList),value=T);
      
      # Main Sparse count matrix to save
      matsparseToSave <- originalP2object$counts[mainDendrogram$cellorder,]
      
      # Main Sparse count matrix TRANSPOSED for de
      matsparseTransposedToSave <- Matrix::t(originalP2object$counts)
      
      # Serialise aspect matrix
      cellIndices <- mainDendrogram$cellorder;
      aspectMatrixToSave <- originalP2object$misc$pathwayOD$xv[,cellIndices,drop=F];
      
      aspectMatrixToSave <- Matrix::t(aspectMatrixToSave);
      aspectMatrixToSave <- Matrix(aspectMatrixToSave, sparse=T);
      
      # Serialise the aspect information
      
      aspectInformation <- list();
      for (curAspect in rownames(originalP2object$misc$pathwayOD$xv)) {
        # Genesets in this aspect
        curgenesets <- unname(originalP2object$misc$pathwayOD$cnam[[curAspect]]);
        
        # Get the metadata for these gene sets
        colOfInterest <- c("name","n","cz");
        retTable <- originalP2object$misc$pathwayODInfo[curgenesets, colOfInterest];
        
        # Convert to JSON friendly format
        aspectInformation[[curAspect]]  <- unname(apply(retTable, 1, function(x) {
          # Must be in genesets for short description
          desc <- geneSets[[x[[1]]]]$properties$shortdescription;
          
          list(name = x[[1]], n = x[[2]], cz = x[[3]], shortdescription = desc);
        }));
      }
      
      ## Serialising geneset Information
      genesetInformation <- unname(lapply(geneSets, function(x) {x$properties}))
      
      
      ## Serialise geneset Genes:
      geneListName <- names(geneSets);
      
      ## Export gene names in the gos
      geneListGenes <- lapply( geneSets, function(gos) make.unique(gos$genes))
      
      ## Creation of the export List for Rcpp
      
      ## JSON & Annotation part
      exportList[["reduceddendrogram"]] <- reducedDendrogramJSON();
      exportList[["cellorder"]] <- cellOrderJSON();
      exportList[["cellmetadata"]] <- cellmetadataJSON();
      exportList[["geneinformation"]] <- geneInformationJSON();
      
      exportList[["embeddingstructure"]] <- toJSON(embStructure);
      exportList[["aspectInformation"]] <- toJSON(aspectInformation);
      exportList[["genesets"]] <- toJSON(genesetInformation);
      exportList[["genesetGenes"]] <- toJSON(geneListGenes);
      
      exportList[["appmetadata"]] <- toJSON(appmetadata);
      
      ## The gene Knn is optional
      if(!is.null(originalP2object$genegraphs$graph)){
        exportList[["geneknn"]] <- generateGeneKnnJSON();
      } else if(verbose) {
        warning("No genegraph provided. It allows you to search for similar genes in the webinterface. \n This is optional, but you can create it with the function makeGeneKnnGraph() \n")
      }
      
      ## Exports sparse Matrix as List with Dimnames converted to JSON
      ## Sparse Count Matrix & Sparse Aspect Matrix
      exportList[["matsparse"]] <- sparseMatList(matsparseToSave); ## This count values
      exportList[["mataspect"]] <- sparseMatList(aspectMatrixToSave); ## This is the aspect values
      exportList[["sparseMatrixTransp"]] <- sparseMatList(matsparseTransposedToSave); ## To save transposed expr for de
      
      ## Tell Cpp what is a sparse matrix
      exportList[["sparsematnames"]] <- c("matsparse", "mataspect","sparseMatrixTransp");
      
      ## Call Rcpp function to write to static file
      WriteListToBinary(expL=exportList,outfile = binary.filename,verbose=verbose);
      ##return(invisible(exportList));
      
      ## Return NULL
      NULL;
    },
    
    # Create simple List from sparse Matrix with Dimnames as JSON
    #
    # Arguments: sparse matrix
    # Returns a list with slots i,p,x
    sparseMatList = function(matsparse){
      mslist <- new("list")
      mslist[["matsparse_i"]] <- matsparse@i
      mslist[["matsparse_p"]] <- matsparse@p
      mslist[["matsparse_x"]] <- matsparse@x
      mslist[["matsparse_dim"]] <- matsparse@Dim
      mslist[["matsparse_dimnames1"]] <- toJSON(matsparse@Dimnames[[1]])
      mslist[["matsparse_dimnames2"]] <- toJSON(matsparse@Dimnames[[2]])
      return(mslist)
    },
    
    ## Serialise an R array to a JSON object
    ##
    ## Arguments: accepts an R array
    ## Returns a serialised version of the array in JSON
    ## and includes dimention information as separate fields
    arrayToJSON = function(a = NULL) {
      if (is.null(a) | !is.array(a) ) {
        NULL
      } else {
        # Serialised Array
        toJSON(list(values = a, dim =dim(a), rownames = rownames(a), colnames= colnames(a)));
      }
    },
    
    getCompressedEmbedding = function(reduc, embed) {
      emb <- originalP2object$embeddings[[reduc]][[embed]];
      ## Flip Y coordinate
      emb[,2] <- (-1) * emb[,2]
      ret <- list(
        values = .self$packCompressFloat64Array(as.vector(emb)),
        dim =  dim(emb),
        rownames = rownames(emb),
        colnames = colnames(emb)
      );
      toJSON(ret)
    },
    
    
    # Logging function for console
    serverLog = function(message) {
      print(message);
    },
    
    reducedDendrogramJSON = function() {
      h <- mainDendrogram$hc
      l <- unclass(h)[c("merge", "height", "order","labels")];
      l$clusterMemberCount <-  mainDendrogram$cluster.sizes;
      return(toJSON(l));
    },
    
    cellOrderJSON = function() {
      toJSON(mainDendrogram$cellorder);
    },
    
    availableAspectsJSON = function() {
      toJSON(rownames(originalP2object$misc$pathwayOD$xv));
    },
    
    cellmetadataJSON = function() {
      toJSON(cellmetadata);
    },
    
    geneInformationJSON = function() {
      dataset <- originalP2object$misc$varinfo[,c("m","qv")];
      dataset$name <- rownames(dataset);
      # Don't allow NaNs in dispesion, replace with  negative value
      dataset$qv[is.nan(dataset$qv)] <- 0
      dataset$qv[!is.finite(dataset$qv)] <- 0
      
      dataset$m[is.nan(dataset$m)] <- 0
      dataset$m[!is.finite(dataset$m)] <- 0
      
      
      ## Convert to row format
      retd <-  apply(dataset,
                     1, function(x) {
                       list(genename = x[["name"]],
                            dispersion =x[["qv"]],
                            meanExpr = x[["m"]])
                     });
      retd <- unname(retd);
      
      toJSON(retd);
    },
    
    ## Generate a JSON list representation of the gene KNN network
    generateGeneKnnJSON = function() {
        froms <- unique(originalP2object$genegraphs$graph$from)
        names(froms) <- froms
        y <- lapply(froms, function(n) {
            subset(originalP2object$genegraphs$graph, from == n)$to
        })
        toJSON(y)
    },
    
    ## Generate information about the embeddings we are exporting
    generateEmbeddingStructure = function() {
      resp <- list();
      i <- 0;
      for( r in names(originalP2object$reductions)) {
        resp[[r]] <- list();
        for ( n in names(originalP2object$embeddings[[r]])) {
          id <- paste('emb',r,n,sep='_')
          resp[[r]][[n]] <- id;
          i <- i+1
        }
      }
      resp
    } ## generateEmbeddingStructure
    
  ) # methods list
) # setRefClass
