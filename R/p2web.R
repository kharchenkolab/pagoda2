# Filename: pagoda2WebApp.R
# Author: Nikolas Barkas
# Date: Jan - Mar 2017
# Description: The rook server for pagoda 2


#' @import Rook
#' @importFrom utils URLdecode
#' @importFrom rjson fromJSON toJSON
#' @import base64enc

##' @export pagoda2WebApp
##' @exportClass pagoda2WebApp
pagoda2WebApp <- setRefClass(
    'pagoda2WebApp',

    # Use Middleware to handle static and dynamic files seperately
    'contains' = 'Middleware',

    # The fields of the pagoda app should be minimal
    # and only information that is essential should be kept
    #
    # We need:
    # * the embeddings (and possibly not all)
    # * The clusters (again possibly not all)
    # * The original matrix
    # * Possibly some reduced matrices
    fields = c(
        "name", # The name of this application for display purposes
        "verbose", # Server verbosity level
        "odgenes", # List of overdispersed genes
        "embeddings",
        "clusters",
        "mat",
        "matsparse",
        "cellmetadata",
        "reductions",
        "mainDendrogram",
        "geneSets",
        "varinfo",
        "pathways",
        "pathwayODInfo", # Information about the pathways
        "originalP2object",
        "rookRoot"
    ),


    methods = list(

        # pagoda2obj: a pagoda2 object
        # appName: the display name for this app
        # verbose: verbosity level, def: 0, higher values will printmore
        # debug: T|F load debug version?, def: F
        # dendGroups: a factor defining the groups of cells to use for the dendrogram
        # keepOriginal: maintain a copy to the original RF object -- this allows for some extra capabilities

        initialize = function(pagoda2obj, appName = "DefaultPagoda2Name", dendGroups,
                              verbose = 0, debug, geneSets, metadata=metadata, keepOriginal=TRUE) {

            # Keep the original pagoda 2 object
            # This is required for things like differential expression and
            # gene KNN lookups
            if (keepOriginal) {
              originalP2object <<- pagoda2obj
            }

		        # Check that the object we are getting is what it should be
            if (class(pagoda2obj) != "Pagoda2") {
                cat("We have an error");
                stop("ERROR: The provided object is not a pagoda 2 object")
            }

            # Check that the dendGroups we are getting is what it should be
            if (length(dendGroups) != nrow(pagoda2obj$counts)) {
                cat("We have an error");
                stop("ERROR: The provided dendGroups has a different number of cells than the pagoda 2 object")
            }


            # Keep the name for later (consistent) use
            name <<- appName;

            # Copy data we need
            embeddings <<- pagoda2obj$embeddings;
            clusters <<- pagoda2obj$clusters;

            # Data matrix kept as sparse matrix
            matsparse <<- pagoda2obj$counts;

            # Get the pathways
            pathways <<- pagoda2obj$misc$pathwayOD;

            # Using the cell grouping provided in dendGroups
            # Generate an hclust object of these cell groups
            # a cell ordering compatible with these groups
            # an the number of cells in each group (for plotting purposes)
            mainDendrogram <<- .self$generateDendrogramOfGroups(pagoda2obj,dendGroups);

            # Available reductions
            reductions <<- pagoda2obj$reductions;

            # Gene variance information
            varinfo <<- pagoda2obj$misc$varinfo;

            # List of overdispersed genes
            odgenes <<- pagoda2obj$misc$odgenes

            # Verbosity level
            verbose <<- verbose;

            # Genesets
            geneSets <<- geneSets;
            # The cell metadata
            cellmetadata <<- metadata;

             # Rook sever root directory to be changed to package subdirectory
            # this holds all the static files required by the app
            rookRoot <<- file.path(system.file(package='pagoda2'),'rookServerDocs');

            pathwayODInfo <<- pagoda2obj$misc$pathwayODInfo;

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


        generateDendrogramOfGroups = function(r, dendrogramCellGroups){
            cl0 <- dendrogramCellGroups
            # Generate an hclust objct of the above groups
            dendrogramCellGroups <- dendrogramCellGroups[match(rownames(r$counts),names(dendrogramCellGroups))]
            lvec <- colSumByFac(r$misc[['rawCounts']],as.integer(cl0))[-1,] + 1
            lvec <- t(lvec/pmax(1,rowSums(lvec)))
            colnames(lvec) <- which(table(cl0)>0)
            rownames(lvec) <- colnames(r$misc[['rawCounts']])
            ld <- jsDist(lvec);
            colnames(ld) <- rownames(ld) <- colnames(lvec)

            #hcGroup is a hclust object of whatever cell groupings we provided above
            hcGroups <- hclust(as.dist(ld), method = 'ward.D');

            # We now need to derive a cell order compatible with the order
            # of the above dendrogram
            cellorder <- unlist(lapply(hcGroups$labels[hcGroups$order], function(x) {
                base::sample(names(cl0)[cl0 == x]) # Sample for random order
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

            # DEBUG
            #if (is.null(env)) {
            #    response$write("An error occured: env is null");
            #    response$finish();
            #}

            # TODO: Extract this from request
            path <- env[['PATH_INFO']];


            request <- Request$new(env);
            response <- Response$new()

            switch( path,

                   ### Static files that are not handled by the Static class

                   # Get the main script
                   '/index.html' = {
                       response$header('Content-Type', 'text/html');
                       response$write(readStaticFile('/index.html'));
                       return(response$finish());
                   },


                   ### Dynamic Request Handling

                   # Retrieve data or subset of data
                   # Only retrieve one item of data per request
                   # use the 'dataidentifier' GET argument to specify dataset
                   '/getData.php' = {

                           requestArguments <- request$GET();
                           dataIdentifier <- requestArguments[['dataidentifier']];

                           if (!is.null(dataIdentifier)) {
                               # Handle a getData request
                               switch(dataIdentifier,
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

                                      # Return a gene information table
                                      # for all the genes
                                      'geneinformation' = {
                                          dataset <- varinfo[,c("m","v")];
                                          dataset$name <- rownames(dataset);

                                          # Convert to row format
                                          retd <-  apply(dataset,
                                                         1, function(x) {
                                                             list(genename = x[["name"]],
                                                                  dispersion =x[["v"]],
                                                                  meanExpr = x[["m"]])
                                                         });
                                          retd <- unname(retd);


                                          response$header("Content-type", "application/javascript");
                                          response$write(toJSON(retd));
                                          return(response$finish());
                                      },

                                      'odgeneinformation' = {
                                          # Only genes in the odgene list
                                          dataset <- varinfo[odgenes,c("m","v")];
                                          dataset$name <- rownames(dataset);

                                          # Convert to row format
                                          retd <-  apply(dataset,
                                                         1, function(x) {
                                                             list(genename = x[["name"]],
                                                                  dispersion =x[["v"]],
                                                                  meanExpr = x[["m"]])
                                                         });
                                          retd <- unname(retd);

                                          response$header("Content-type", "application/javascript");
                                          response$write(toJSON(retd));
                                          return(response$finish());
                                       },

                                      # Very similar to 'geneinformation'
                                      # but only returns information for the genes that belong
                                      # to the specified geneset
                                      'genesetgeneinformation' = {


                                        # FIXME: to work with genelistnames not
                                        # gene names

                                        geneListName <- requestArguments[['genesetname']];

                                        # TODO: Check that the specified gene set actually
                                        # exists

                                        # Get the genes in this geneset
                                        geneList <- geneSets[[geneListName]]$genes

                                        # Subset to genes that exist
                                        geneList <- geneList[geneList %in% rownames(varinfo)];

                                        # Generate dataset
                                        dataset <-  varinfo[geneList, c("m","v")];
                                        dataset$name <-  rownames(dataset);

                                        # Convert to row format
                                        retd <-  apply(dataset,
                                                       1, function(x) {
                                                         list(genename = x[["name"]],
                                                              dispersion =x[["v"]],
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
                                        response$write(toJSON(rownames(pathways$xv)));
                                        return(response$finish());
                                      },

                                      'genesetsinaspect' = {
                                          requestArguments <- request$GET();
                                          aspectId <- URLdecode(requestArguments[['aspectId']]);

                                          # Genesets in this aspect
                                          genesets <- unname(pathways$cnam[[aspectId]]);

                                          # Get the metadata for these gene sets
                                          colOfInterest <- c("name","n","cz");
                                          retTable <- pathwayODInfo[genesets, colOfInterest];

                                          # Convert to JSON friendly format
                                          retObj <- unname(apply(retTable, 1, function(x) {
                                            # Must be in genesets for short description
                                            desc <- geneSets[[x[[1]]]]$properties$shortdescription;

                                            list(name = x[[1]], n = x[[2]], cz = x[[3]], shortdescription = desc);
                                          }))

                                          response$header("Content-type", "application/javascript");
                                          #response$write(toJSON(genesets));


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
                                      # This returns the full hierarchy -- deprecated
                                      ## 'hierarchy' = {
                                      ##     hierarchyName <- requestArguments[['type']];

                                      ##     # TODO: Implement this for other hierarchies
                                      ##     # as it is unlikely that we will want to implements
                                      ##     # mutliple cell hierarchies for the moment this will suffice
                                      ##     if (!is.null(hierarchyName) && hierarchyName == 'dummy') {
                                      ##         h <- hierarchies$dummy
                                      ##         l <- unclass(h)[c("merge","height","order")];

                                      ##         # Append cell names
                                      ##         #l[["names"]] = (rownames(r$counts));
                                      ##         l[["names"]] = hclusts$dummy$labels;

                                      ##         response$header("Content-type","application/javascript")
                                      ##         response$write(toJSON(l));
                                      ##         return(response$finish());
                                      ##     } else {
                                      ##         response$write('Not implemented: only the dummy hierarchy is implemented');
                                      ##         return(response$finish());
                                      ##     }
                                      ## },

                                      # Get cell metadata information
                                      'cellmetadata' = {
                                          response$header("Content-type", "application/javascript");
                                          response$write(toJSON(cellmetadata));
                                          return(response$finish());
                                      },

                                      'aspectmatrixbyaspect' = {

                                        postArgs <- request$POST();

                                        aspectIds <- URLdecode(postArgs[['aspectids']]);
                                        cellIndexStart <- URLdecode(postArgs[['cellindexstart']]);
                                        cellIndexEnd <- URLdecode(postArgs[['cellindexend']]);

                                        cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)];
                                        matrixToSend <- pathways$xv[aspectIds,cellIndices,drop=F];

                                        # Discard values < 1/50 of the max
                                        #trimPoint <-  max(abs(matrixToSend)) / 50;
                                        #matrixToSend[abs(matrixToSend) < trimPoint] <- 0;

                                        # Transpose and make sparse
                                        matrixToSend <- Matrix(t(matrixToSend), sparse = T);

                                        # Bit pack and compress arrays
                                        xSend <- .self$packCompressFloat64Array(matrixToSend@x);
                                        iSend <- .self$packCompressInt32Array(matrixToSend@i);
                                        pSend <- .self$packCompressInt32Array(matrixToSend@p);

                                        #Dimnames1Send <- "";
                                        #if (getCellNames) {
                                          Dimnames1Send <- matrixToSend@Dimnames[[1]];
                                        #};

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

                                          cellIndexStart <- URLdecode(postArgs[['cellindexstart']]);
                                          cellIndexEnd <- URLdecode(postArgs[['cellindexend']]);
                                          getCellNames <- URLdecode(postArgs[['getcellnames']]);

                                          cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)];
                                          matrixToSend <- pathways$xv[,cellIndices,drop=F];

                                          # Discard values < 1/50 of the max
                                          trimPoint <-  max(abs(matrixToSend)) / 50;
                                          matrixToSend[abs(matrixToSend) < trimPoint] <- 0;

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

                                          geneIdentifiers <- (postArgs[['geneids']]); # DONT decode it's an array
                                          cellIndexStart <- URLdecode(postArgs[['cellindexstart']]);
                                          cellIndexEnd <- URLdecode(postArgs[['cellindexend']]);
                                          getCellNames <- URLdecode(postArgs[['getCellNames']]);



                                          if (!all(c(geneIdentifiers %in% colnames(matsparse)))) {
                                              serverLog("Error: The request contains gene names that are not in matsparse!");
                                              geneIdentifiers <- geneIdentifiers[geneIdentifiers %in% colnames(matsparse)];
                                          }

                                          # Ordering of the matrix according to the hclust
                                          cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)]
                                          matrixToSend <- matsparse[cellIndices,geneIdentifiers,drop=F];


                                          # FOR DEBUGGING HEATMAP
                                          # Plot the heatmap we are expected to see
                                          ## heatmap(t(as.matrix(matrixToSend)), scale='row', Rowv=NA, Colv=NA,
                                          ##         col = c("#FF0000","#FF3838","#FF7171","#FFAAAA","#FFE2E2","#E2E2FF","#AAAAFF","#7171FF","#3838FF","#0000FF")

                                          ##         )

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

                                      # Get available clusters for a specific reduction type
                                      #
                                      # GET Arguments accepted
                                      # type -- the reduction type (e.g. mat, odgenes, PCA)
                                      'availableclusterings' = {
                                          reductionname <- URLdecode(requestArguments[['type']]);

                                          if (!is.null(reductionname) &&
                                              reductionname %in% c("mat", names(reductions))) {
                                              # TODO: Get clusterings for this reduction ....
                                              # CONTINUE HERE
                                          } else {
                                              response$write("Unknown reduction type requested");
                                              return(response$finish());
                                          }
                                      },



                                      # Return a list of available reduction types
                                      # Does not take any GET arguments
                                      'availablereductiontypes' = {
                                          availReductions <- c("mat", names(reductions));
                                          response$header("Content-type","application/javascript");
                                          response$write(toJSON(availReductions));
                                          return(response$finish());
                                      }, # availablereductiontypes

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
                                           reductionname <- URLdecode(requestArguments[['type']]);

                                           # Is the requested reduction available?
                                           if (!is.null(reductionname) &&
                                               reductionname  %in% c("mat", names(reductions)) ) {

                                              workingReduction <- NULL;
                                              if (reductionname == "mat") {
                                                  workingReduction <- mat;
                                              } else {
                                                  workingReduction <- reductions[[reductionname]];
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
                                                      # TODO: Expand above with the identifiers that are missing
                                                      # TODO: Exit case
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

                                      # Get available embeddings for a specific type
                                      #
                                      # GET arguments accepted
                                      # type -- specifies the dataset transformation/reduction for
                                      #         which to return available embeddings
                                      #         e.g. mat, PCA, etc..., values for this can be obtained
                                      #         from the 'availablereductiontypes' call
                                      'availableembeddings' = {

                                          type <- URLdecode(requestArguments[['type']]);

                                          if (!is.null(type)) {
                                              if( type %in% c( names(embeddings), "mat") ) {
                                                  response$header('Content-type', "application/javascript");
                                                  response$write(toJSON( names(embeddings[[type]]) ));
                                                  return(response$finish());
                                              } else {
                                                  # TODO: Set the headers and possibly return a JSON encoded error
                                                  response$write("Error: Unknown type specified");
                                                  return(response$finish());
                                              }
                                          } else {
                                              # TODO: Set the headers and possibly return a JSON encoded error
                                              response$write("Error: No type specified");
                                              return(response$finish());
                                          }
                                      },

                                      # Get data for an embedding
                                      #
                                      # GET Arguments accepted
                                      # type -- specifies the dataset transformation from which this
                                      #         the requested embedding was derived from
                                      #         ( e.g. mat, PCA, etc...)
                                      # embeddingtype -- specifies the embedding type for this particular
                                      #                  transformation, e.g tSNE, largeVis
                                      #                  values for this parameter are returned
                                      #                  by doing an 'availableembeddings' request
                                      'embedding' = {
                                          type <- URLdecode(requestArguments[['type']]);

                                          if (!is.null(type)) {
                                              if ( type %in% c( names(embeddings), "mat") ) {
                                                  response$header('Content-type', "application/javascript");

                                                  # Which embedding?
                                                  embeddingType <- URLdecode(requestArguments[['embeddingtype']]);

                                                  if ( (!is.null(embeddingType)) &&
                                                       embeddingType %in% names(embeddings[[type]]) ) {

                                                      a <- embeddings[[type]][[embeddingType]];
                                                      ret <- list(
                                                          values = .self$packCompressFloat64Array(as.vector(a)),
                                                          dim =  dim(a),
                                                          rownames = rownames(a),
                                                          colnames = colnames(a)
                                                      );
                                                      response$write(toJSON(ret));
                                                      return(response$finish());
                                                  } else {
                                                      response$write(paste0("Error: Unknown embedding specified: ",embeddingType));
                                                      return(response$finish());
                                                  }


                                                  #response$write(toJSON( embeddings[[type]] ));
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


                           # DEBUG Code
                           if (F) {
                               response$write("<pre>");
                               dataIdentifier <- request$GET();
                               response$write(capture.output(str(dataIdentifier)));
                               response$write("</pre>");
                               return(response$finish());
                           }


                   },

                   # Perform a computation and return the results
                   # e.g. Differential expression
                   '/doComputation.php' = {


                      requestArguments <- request$GET();
                      compIdentifier <- URLdecode(requestArguments[['compidentifier']]);

                      if (!is.null(compIdentifier)) {
                        switch(compIdentifier,
                               'doDifferentialExpression1selection' = {
                                  cat('doDifferentialExpression1selection\n');

                                 postArguments <- request$POST();

                                 selectionA <- fromJSON(URLdecode(postArguments[['selectionA']]));

                                 # TODO: check that the originalP2object field is populated, as this is optional

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

                                    selectionA <- fromJSON(URLdecode(selAarg));
                                    selectionB <- fromJSON(URLdecode(selBarg));

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
            ) # switch(path

            # DONT DO THIS INTERFERES WITH DEFERRED REQUESTS
            #response$finish();

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

        # Update the rook server root directory
        # This is useful if the app is generated on one machine and
        # then moved to another with a different installation configuration
        updateRookRoot = function(newRoot) {
          # Update the object variable
          rookRoot <<- file.path(system.file(package='pagoda2'),'rookServerDocs');

          # Update the middleware static server
          .self$app$app$file_server$root <- rookRoot;
        },

        # Create a List consisting of JSON-strings and two sparse Matrices
        # Write to static binary file by Calling Rcpp function
        # Author: Simon Steiger
        #
        # Arguments: Specifiy the name of the output file.
        # Takes the other objects to export from RefClass
        # Returns the exportList which could be passed to the WriteListToBinary

        serializeToStaticFast = function(binary.filename=NULL){
            if (is.null(binary.filename)) {
              stop('Please specify a directory');
            }

            # TODO: Add the gene Knn information if the p2 object is available. They need to reformated in to JSON
            #   for fast js lookups



            exportList <- new("list");
            # TODO: optimize the R-part, move stuff over to Rcpp
            # Preparation of objects to pass to Rcpp
            # Create embedding strucutre for export
            embStructure <- generateEmbeddingStructure();

            # Export to list For all contained embeddings
            for (reduc in names(embStructure)) {
                for (embed in names(embStructure[[reduc]])) {
                id <- embStructure[[reduc]][[embed]][[1]];
                filename <- paste0(id, '.json');

                a <- embeddings[[reduc]][[embed]];
                ret <- list(
                    values = .self$packCompressFloat64Array(as.vector(a)),
                    dim =  dim(a),
                    rownames = rownames(a),
                    colnames = colnames(a)
                );
                e <- toJSON(ret);
                exportList[filename] <- e;
                }
            }

            # Export list with all included embeddings for easier iteration in Rcpp-function.
            exportList[["embedList"]] <- grep("emb_",names(exportList),value=T);

            # Main Sparse count matrix to save
            matsparseToSave <- matsparse[mainDendrogram$cellorder,]

            # Serialise aspect matrix
            cellIndices <- mainDendrogram$cellorder;
            aspectMatrixToSave <- pathways$xv[,cellIndices,drop=F];
            trimPoint <- max(abs(aspectMatrixToSave)) / 50;
            aspectMatrixToSave[abs(aspectMatrixToSave) < trimPoint] <- 0;
            aspectMatrixToSave <- t(aspectMatrixToSave);
            aspectMatrixToSave <- Matrix(aspectMatrixToSave, sparse=T);

            # Serialise the aspect information

            aspectInformation <- list();
            for (curAspect in rownames(pathways$xv)) {
                # Genesets in this aspect
                curgenesets <- unname(pathways$cnam[[curAspect]]);

                # Get the metadata for these gene sets
                colOfInterest <- c("name","n","cz");
                retTable <- pathwayODInfo[curgenesets, colOfInterest];

                # Convert to JSON friendly format
                aspectInformation[[curAspect]]  <- unname(apply(retTable, 1, function(x) {
                # Must be in genesets for short description
                desc <- geneSets[[x[[1]]]]$properties$shortdescription;

                list(name = x[[1]], n = x[[2]], cz = x[[3]], shortdescription = desc);
                }));
            }

            # Serialising geneset Information
            genesetInformation <- unname(lapply(geneSets, function(x) {x$properties}))


            # Serialise geneset Genes:
            geneListName <- names(geneSets);

            # This is super inefficient. Adapted to to the same with a sapply
            # geneListGenes <- list();
            # for(geneListName in names(geneSets)) {
            #     # Get the genes in this geneset
            #     geneList <- geneSets[[geneListName]]$genes
            #     # Subset to genes that exist
            #     geneList <- geneList[geneList %in% rownames(varinfo)];

            #     # Generate dataset
            #     dataset <-  varinfo[geneList, c("m","v")];
            #     dataset$name <-  rownames(dataset);

            #     # Convert to row format
            #     retd <-  apply(dataset,
            #                 1, function(x) {
            #                     x[["name"]];
            #                 });
            #     geneListGenes[[geneListName]] <- unname(retd);
            # }
            geneListGenes <- lapply(myPagoda2WebObject$geneSets, function(gos) make.unique(gos$genes))

            # Creation of the export List for Rcpp

            ## JSON & Annotation part
            exportList[["reduceddendrogram"]] <- reducedDendrogramJSON();
            exportList[["cellorder"]] <- cellOrderJSON();
            exportList[["cellmetadata"]] <- cellmetadataJSON();
            exportList[["geneinformation"]] <- geneInformationJSON();

            exportList[["embeddingstructure"]] <- toJSON(embStructure);
            exportList[["aspectInformation"]] <- toJSON(aspectInformation);
            exportList[["genesets"]] <- toJSON(genesetInformation);
            exportList[["genesetGenes"]] <- toJSON(geneListGenes);

            # The gene Knn
            exportList[["geneknn"]] <- generateGeneKnnJSON();

            ## Sparse Count Matrix & dimnames as JSON.
            # TODO: export matsparse as S4 object and move "splitting" over to Rcpp.
            #exportList[["matsparse"]] <- matsparseToSave;
            exportList[["matsparse_i"]] <- matsparseToSave@i;
            exportList[["matsparse_p"]] <- matsparseToSave@p;
            exportList[["matsparse_x"]] <- matsparseToSave@x;
            exportList[["matsparse_dim"]] <- matsparseToSave@Dim;
            exportList[["matsparse_dimnames1"]] <- toJSON(matsparseToSave@Dimnames[[1]]);
            exportList[["matsparse_dimnames2"]] <- toJSON(matsparseToSave@Dimnames[[2]]);

            ## Sparse Aspect Matrix & dimnames as JSON.
            # TODO: export mataspect as S4 object and move "splitting" over to Rcpp.
            #exportList[["mataspect"]] <- aspectMatrixToSave;
            exportList[["mataspect_i"]] <- aspectMatrixToSave@i;
            exportList[["mataspect_p"]] <- aspectMatrixToSave@p;
            exportList[["mataspect_x"]] <- aspectMatrixToSave@x;
            exportList[["mataspect_dim"]] <- aspectMatrixToSave@Dim;
            exportList[["mataspect_dimnames1"]] <- toJSON(aspectMatrixToSave@Dimnames[[1]]);
            exportList[["mataspect_dimnames2"]] <- toJSON(aspectMatrixToSave@Dimnames[[2]]);


            #binary.filename <- file.path(getwd(),binary.filename);
            WriteListToBinary(expL=exportList,outfile = binary.filename);
            return(invisible(exportList));
        },

        # Serialise an R array to a JSON object
        #
        # Arguments: accepts an R array
        # Returns a serialised version of the array in JSON
        # and includes dimention information as separate fields
        arrayToJSON = function(a = NULL) {
            if (is.null(a) | !is.array(a) ) {
                NULL
            } else {
                # Serialised Array
                toJSON(list(values = a, dim =dim(a), rownames = rownames(a), colnames= colnames(a)));
            }
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
		      toJSON(rownames(pathways$xv));
		    },

		    cellmetadataJSON = function() {
		      toJSON(cellmetadata);
		    },

		    geneInformationJSON = function() {
		      dataset <- varinfo[,c("m","v")];
		      dataset$name <- rownames(dataset);

		      # Convert to row format
		      retd <-  apply(dataset,
		                     1, function(x) {
		                       list(genename = x[["name"]],
		                            dispersion =x[["v"]],
		                            meanExpr = x[["m"]])
		                     });
		      retd <- unname(retd);

		      toJSON(retd);
		    },

		    # Generate a JSON list representation of the gene KNN network
		    generateGeneKnnJSON = function() {
		      require(rjson)
		      geneList <- unique(originalP2object$genegraphs$graph$from)
		      names(geneList) <- geneList
		      y <- lapply(geneList, function(x) {
		        originalP2object$genegraphs$graph$to[originalP2object$genegraphs$graph$from == x]
		      })
		      toJSON(y)
		    },

		    # Generate information about the embeddings we are exporting
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
		    }
		    ,

		    # deprecated, use serialiseToStaticFast
		    serialiseToStatic = function(text.file.directory = null, binary.filename = null) {
		      dir <- text.file.directory;

		      if (is.null(dir)) {
		        stop('Please specify a directory');
		      }

		      # Simple save function for text data
          writeDataToFile <- function(dir, filename, data) {
             filename <- file.path(dir, filename);
             conn <- file(filename);
             writeChar(data, conn);
             close(conn);
          }

          # Content that is delivered as JSON on Server backed up -- unchanged
          writeDataToFile(dir, 'reduceddendrogram.json', reducedDendrogramJSON());
          writeDataToFile(dir, 'cellorder.json', cellOrderJSON());
          writeDataToFile(dir, 'cellmetadata.json', cellmetadataJSON());
          writeDataToFile(dir, 'geneinformation.json', geneInformationJSON());

          # Reduction and embedding hierarchy two different calls into one data structure
          embStructure <- generateEmbeddingStructure()
          writeDataToFile(dir, 'embeddingstructure.json', toJSON(embStructure));

          # Now serialise the embeddings
          for (reduc in names(embStructure)) {
            for (embed in names(embStructure[[reduc]])) {
              id <- embStructure[[reduc]][[embed]][[1]];
              filename <- paste0(id, '.json');

              a <- embeddings[[reduc]][[embed]];
              ret <- list(
                values = .self$packCompressFloat64Array(as.vector(a)),
                dim =  dim(a),
                rownames = rownames(a),
                colnames = colnames(a)
              );
              e <- toJSON(ret);

              writeDataToFile(dir, filename, e);
            }
          }

          # Serialise the matsparse array

          # Functions for serialising sparse arrays
          simpleSerializeArrayToFile <- function(array, dir, filename) {
            conn <- file(file.path(dir, filename), open='w');
            x_length <- length(array);
            for (i in 1:x_length) {
              cat(array[i], file=conn);
              cat(' ', file=conn);
            }
            close(conn);
          }

          serialiseSparseArray <- function(array, dir, filename) {
            # Serialise the i
            cat('Serialising i...\n')
            simpleSerializeArrayToFile(array@i, dir, paste0(filename,'i.txt'))
            cat('Serialising p...\n')
            simpleSerializeArrayToFile(array@p, dir,  paste0(filename,'p.txt'))
            cat('Serialising x...\n')
            simpleSerializeArrayToFile(array@x, dir,  paste0(filename,'x.txt'))
            cat('Serialising Dim...\n')
            simpleSerializeArrayToFile(array@Dim, dir,  paste0(filename,'Dim.txt'))
            cat('Serialising Dimnames1...\n')
            writeDataToFile(dir, paste0(filename,'Dimnames1.json') , toJSON(array@Dimnames[[1]]));
            cat('Serialising Dimnames2...\n')
            writeDataToFile(dir, paste0(filename,'Dimnames2.json'), toJSON(array@Dimnames[[2]]));
          }

          # Serialise the main sparse matrix
          matsparseToSave <- matsparse[mainDendrogram$cellorder,]
          serialiseSparseArray(matsparseToSave, dir, 'matsparse_');

          # Serialise aspect matrix
          cellIndices <- mainDendrogram$cellorder;
          aspectMatrixToSave <- pathways$xv[,cellIndices,drop=F];
          trimPoint <- max(abs(aspectMatrixToSave)) / 50;
          aspectMatrixToSave[abs(aspectMatrixToSave) < trimPoint] <- 0;
          aspectMatrixToSave <- t(aspectMatrixToSave);
          aspectMatrixToSave <- Matrix(aspectMatrixToSave, sparse=T);
          serialiseSparseArray(aspectMatrixToSave, dir, 'mataspect_');

          # Serialise the aspect information


          cat('Serialising aspects..\n');
          aspectInformation <- list();
          for (curAspect in rownames(pathways$xv)) {
            # Genesets in this aspect
            curgenesets <- unname(pathways$cnam[[curAspect]]);

            # Get the metadata for these gene sets
            colOfInterest <- c("name","n","cz");
            retTable <- pathwayODInfo[curgenesets, colOfInterest];

            # Convert to JSON friendly format
            aspectInformation[[curAspect]]  <- unname(apply(retTable, 1, function(x) {
              # Must be in genesets for short description
              desc <- geneSets[[x[[1]]]]$properties$shortdescription;

              list(name = x[[1]], n = x[[2]], cz = x[[3]], shortdescription = desc);
            }));
          }
          writeDataToFile(dir, 'aspectInformation.json', toJSON(aspectInformation));

          cat('Serialising gene sets...\n');
          writeDataToFile(dir, 'genesets.json',toJSON(unname(lapply(geneSets, function(x) {x$properties}))));

          # TODO: Continue here
          cat('Serialising geneset genes...\n');

          geneListName <- names(geneSets);

          geneListGenes <- list();
          for(geneListName in names(geneSets)) {
            # Get the genes in this geneset
            geneList <- geneSets[[geneListName]]$genes
            # Subset to genes that exist
            geneList <- geneList[geneList %in% rownames(varinfo)];

            # Generate dataset
            dataset <-  varinfo[geneList, c("m","v")];
            dataset$name <-  rownames(dataset);

            # Convert to row format
            retd <-  apply(dataset,
                           1, function(x) {
                             x[["name"]];
                           #  list(genename = x[["name"]],
                           #        dispersion =x[["v"]],
                           #        meanExpr = x[["m"]])
                           });
            geneListGenes[[geneListName]] <- unname(retd);
          }
          writeDataToFile(dir, 'genesetsgenes.json',toJSON(geneListGenes));


          #We want to make a binary file as well
          if (!is.null(binary.filename)) {
            p2packExec <- file.path(system.file(package='pagoda2'),'utilities','p2packSource','p2pack');

            # Append trailing slash if missing
            if (grepl('/$', text.file.directory)) {
              dirVar = text.file.directory;
            } else {
              dirVar = paste0(text.file.directory, '/');
            }

            args <- paste0(' --input-directory ', dirVar, ' --output-file ', binary.filename)
            cmdRetVal <- system2(p2packExec, args)
            if(cmdRetVal != 0) {
              cat('Error conversion to binary failed! Have you built the conversion utility? Go to ',
                  file.path(system.file(package='pagoda2'),'utilities','p2packSource'), 'and run "make".');
            }
          }

		    }

    ) # methods list
) # setRefClass
