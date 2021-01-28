
#' @import Rook
#' @importFrom urltools url_decode
#' @importFrom rjson fromJSON toJSON
#' @importFrom dendsort dendsort
NULL


#' pagoda2WebApp class to create 'pagoda2' web applications via a Rook server
#'
#' @rdname pagoda2WebApp
#' @exportClass pagoda2WebApp
#' @field originalP2object Input 'Pagoda2' object 
#' @field name string Display name for the application 
#' @field mat Embedding
#' @field cellmetadata Metadata associated with 'Pagoda2' object 
#' @field mainDendrogram Dendrogram from hclust() of all cells in the 'Pagoda2' object 
#' @field geneSets Gene sets in the 'Pagoda2' object 
#' @field rookRoot Rook server root directory 
#' @field appmetadata pagoda2 web application metadata 
#' @export 
pagoda2WebApp <- setRefClass(
  'pagoda2WebApp',
  'contains' = 'Middleware', # Inherit from Middleware to handle static and dynamic files seperately
  fields = c(
    "originalP2object", 
    "name", # The name of this application for display purposes (default="DefaultPagoda2Name")
    "verbose", # Server verbosity level
    "mat",
    "cellmetadata",
    "mainDendrogram",
    "geneSets",
    "rookRoot",
    "appmetadata"
  ),
  
  methods = list(

    ## @titlepagoda2WebApp initalize
    ## @description Initalize the pagoda2WebApp
    ##
    ## @param pagoda2obj 'Pagoda2' object
    ## @param appName string Display name for the app (default="DefaultPagoda2Name")
    ## @param dendGroups factor defining the groups of cells to use for the dendrogram
    ## @param geneSets Gene sets in the 'Pagoda2' object 
    ## @param metadata 'Pagoda2' cell metadata 
    ## @param appmetadata 'Pagoda2' web application metadata (default=NULL)
    ##
    ## @return new pagoda2WebApp object
    initialize = function(pagoda2obj, appName = "DefaultPagoda2Name", dendGroups,
                          verbose = 0, debug, geneSets, metadata=metadata,
                          innerOrder=NULL,orderDend=FALSE,appmetadata = NULL) {

      if (!requireNamespace("base64enc", quietly = TRUE)) {
        stop("Package \"base64enc\" needed for the pagoda2WebApp class to work. Please install in with install.packages('base64enc').", call. = FALSE)
      }

      ## Check that the object we are getting is what it should be
      ## should be both `"Pagoda2" "R6"`
      '%ni%' <- Negate('%in%')
      if (all(class(pagoda2obj) %ni% "Pagoda2")) {   
        stop("The provided object 'pagoda2obj' is not a Pagoda2 object")
      }
      
      ## Keep the original 'Pagoda2' object
      originalP2object <<- pagoda2obj
      
      ## Check that the dendGroups we are getting is what it should be
      if (length(dendGroups) != nrow(pagoda2obj$counts)) {
        stop("The provided dendGroups has a different number of cells than the Pagoda2 object")
      }
      
      ## Keep the name for later (consistent) use
      name <<- appName
      
      ## Using the cell grouping provided in dendGroups
      # Generate an hclust object of these cell groups
      # a cell ordering compatible with these groups
      # and the number of cells in each group (for plotting purposes)
      mainDendrogram <<- .self$generateDendrogramOfGroups(pagoda2obj,dendGroups,innerOrder,orderDend)
      
      # Verbosity level
      verbose <<- verbose
      
      # Genesets
      geneSets <<- geneSets
      # The cell metadata
      cellmetadata <<- metadata
      
      # The application metadata for things like the title
      appmetadata <<- appmetadata
      
      # Rook sever root directory to be changed to package subdirectory
      # this holds all the static files required by the app
      rookRoot <<- file.path(system.file(package='pagoda2'),'rookServerDocs')
      
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
          res <- Response$new()
          res$header('Content-Type', 'text/html')
          message("Unhandled request for: ")
          cat(env[['PATH_INFO']])
          message("\n")
          res$finish()
        })
      ))
    }
  ) # methods list
) # setRefClass



#' Generate a dendrogram of groups
#'
#' @name pagoda2WebApp_generateDendrogramOfGroups
#' @param dendrogramCellGroups Cell groups to input into hclust()
#' 
#' @return List of hcGroups, cellorder, and cluster.sizes
NULL 
pagoda2WebApp$methods(
    generateDendrogramOfGroups = function(r, dendrogramCellGroups,innerOrder = NULL,orderDend=FALSE){
      cl0 <- dendrogramCellGroups
      # Generate an hclust objct of the above groups
      dendrogramCellGroups <- dendrogramCellGroups[match(rownames(r$counts),names(dendrogramCellGroups))]
      lvec <- colSumByFac(r$misc[['rawCounts']],as.integer(cl0))[-1,,drop=FALSE] + 1
      lvec <- t(lvec/pmax(1,rowSums(lvec)))
      colnames(lvec) <- which(table(cl0)>0)
      rownames(lvec) <- colnames(r$misc[['rawCounts']])
      ld <- jsDist(lvec)
      colnames(ld) <- rownames(ld) <- colnames(lvec)
      
      #hcGroup is a hclust object of whatever cell groupings we provided above
      hcGroups <- stats::hclust(as.dist(ld), method = 'ward.D')
      
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
            base::sample(names(cl0)[cl0 == x])
            message(paste("Cluster", x ,"contains less than 5 cells - no Ordering applied"))
          } else {
            if(innerOrder == "odPCA") {
              
              if(!"odgenes" %in% names(r$misc)){
                stop("Missing odgenes for odPCA")
              } else {
                celsel <- names(cl0)[cl0 == x]
                vpca <- r$counts[celsel, r$misc$odgenes] %*% irlba::irlba(r$counts[celsel,r$misc$odgenes],nv = 1,nu=0)$v # run a PCA on overdispersed genes just in this selection.
                celsel[order(vpca,decreasing = TRUE)] # order by this PCA
              }
              
            } else if (innerOrder == "reductdist") {
              if(!"PCA" %in% names(originalP2object$reductions)){
                stop("Missing PCA reduction, , run calculatePcaReduction first")
              } else {
                
                celsel <- names(cl0)[cl0 == x]
                celsel[stats::hclust(as.dist(1-WGCNA::cor(t(originalP2object$reductions$PCA[celsel,]))))$order] # Hierarchical clustering of cell-cell correlation of the PCA reduced gene-expressions
              }
              
            } else if(innerOrder == "graphbased") {
              
              if(!"PCA" %in% names(r$graphs)){
                stop("Missing graph, , run makeKnnGraph first")
              } else {
                celsel <- names(cl0)[cl0==x]
                sgraph <- igraph::induced_subgraph(r$graphs$PCA,(celsel))
                celsel[stats::hclust(as.dist(1-WGCNA::cor(t(igraph::layout.auto(sgraph,dim=3)))))$order]
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
              
              df <- data.frame(from=rownames(xx)[xn$s+1],to=rownames(xx)[xn$e+1],weight=xn$d,stringsAsFactors=FALSE)
              
              df$weight <- pmax(0,df$weight)
              xn <- cbind(xn,rd=df$weight)
              edgeMat <- sparseMatrix(i=xn$s+1,j=xn$e+1,x=xn$rd,dims=c(nrow(xx),nrow(xx)))
              edgeMat <- edgeMat + t(edgeMat)
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
      )
    }
)

  
#' @name pagoda2WebApp_packCompressInt32Array
#' @title pagoda2WebApp_packCompressInt32Array
#' @description Compress int32 array
#'
#' @param v int32 array
#' @return compressed array
NULL 
pagoda2WebApp$methods(
    packCompressInt32Array = function(v) {
      rawConn <- rawConnection(raw(0), "wb")
      writeBin(v, rawConn)
      xCompress <- memCompress(rawConnectionValue(rawConn), 'gzip')
      xSend <- base64enc::base64encode(xCompress)
      close(rawConn)
      
      xSend
    }
)

#' @name pagoda2WebApp_packCompressFloat64Array
#' @title pagoda2WebApp_packCompressFloat64Array
#' @description Compress float64 array
#' 
#' @param v float64 array
#' @return compressed array 
NULL 
pagoda2WebApp$methods(
    packCompressFloat64Array = function(v){
      rawConn <- rawConnection(raw(0), "wb")
      writeBin(v, rawConn)
      xCompress <- memCompress(rawConnectionValue(rawConn),'gzip')
      xSend <- base64enc::base64encode(xCompress)
      close(rawConn)
      
      xSend
    }
)

#' @name pagoda2WebApp_call
#' @title pagoda2WebApp_call   
#' @description Handle httpd server calls
#'
#' @param env The environment argument is a true R environment object which the application is free to modify. Please see the Rook documentation for more details.
#' 
#' @return NULL
NULL 
pagoda2WebApp$methods(
    call = function(env) {
      path <- env[['PATH_INFO']]
      request <- Request$new(env)
      response <- Response$new()
      switch( path,
              ## Static files that are not handled by the Static class
              # Get the main script
              '/index.html' = {
                response$header('Content-Type', 'text/html')
                response$write(readStaticFile('/index.html'))
                return(response$finish())
              },
              
              ## Dynamic Request Handling
              '/getData.php' = {
                
                requestArguments <- request$GET()
                dataIdentifier <- requestArguments[['dataidentifier']]
                
                if (!is.null(dataIdentifier)) {
                  # Handle a getData request
                  switch(dataIdentifier,
                         'appmetadata' = {
                           response$write(toJSON(appmetadata))
                           return(response$finish())
                         },
                         
                         'relatedgenes' = {
                           # This requires the original object -- must add check
                           postArgs <- request$POST()
                           
                           geneListName <- postArgs[['querygenes']]
                           relatedGenes <- originalP2object$genegraphs$graph$to[originalP2object$genegraphs$graph$from %in% geneListName]
                           relatedGenes <- relatedGenes[!duplicated(relatedGenes)]
                           relatedGenes <- relatedGenes[!relatedGenes %in% geneListName]
                           
                           response$header("Content-type", "application/javascript")
                           response$write(toJSON(relatedGenes))
                           return(response$finish())
                         },
                         
                         'embeddingstructure' = {
                           response$write(toJSON(generateEmbeddingStructure()))
                           return(response$finish())
                         },
                         
                         'expressionmatrixsparsebycellname' = {
                           message('Got Request 1')
                           postArgs <- request$POST()
                           cellNames <- url_decode(postArgs[['cellnames']])
                           cellNames <- unlist(strsplit(cellNames, split = "|", fixed =TRUE))
                           
                           getCellNames <- TRUE
                           
                           counts.transposed <- t(originalP2object$counts)
                           
                           ## Matrix to send
                           matrixToSend <- counts.transposed[,cellNames,drop = FALSE]
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x)
                           iSend <- .self$packCompressInt32Array(matrixToSend@i)
                           pSend <- .self$packCompressInt32Array(matrixToSend@p)
                           
                           Dimnames1Send <- ""
                           if (getCellNames) {
                             Dimnames1Send <- matrixToSend@Dimnames[[1]]
                           }
                           
                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" )
                           response$write(toJSON(objToSend))
                           
                           return(response$finish())
                         },
                         
                         ## Return a gene information table for all the genes
                         'geneinformation' = {
                           retd <- geneInformationJSON()
                           response$header("Content-type", "application/javascript")
                           response$write(retd)
                           return(response$finish())
                         },
                         
                         # Very similar to 'geneinformation'
                         # but only returns information for the genes that belong
                         # to the specified geneset
                         'genesetgeneinformation' = {
                           geneListName <- requestArguments[['genesetname']]
                           
                           # Get the genes in this geneset
                           geneList <- geneSets[[geneListName]]$genes
                           
                           # Subset to genes that exist
                           geneList <- geneList[geneList %in% rownames(originalP2object$misc$varinfo)]
                           
                           # Generate dataset
                           dataset <-  originalP2object$misc$varinfo[geneList, c("m","qv")]
                           dataset$name <-  rownames(dataset)
                           
                           # Convert to row format
                           retd <-  apply(dataset,
                                          1, function(x) {
                                            list(genename = x[["name"]],
                                                 dispersion =x[["qv"]],
                                                 meanExpr = x[["m"]])
                                          })
                           retd <- unname(retd)
                           
                           response$header("Content-type", "application/javascript")
                           response$write(toJSON(retd))
                           return(response$finish())
                         },
                         
                         # Get the names of the available gene sets
                         'availablegenesets' = {
                           ret <- lapply(geneSets, function(x) {x$properties})
                           # Neet to unname because otherwise its not a JSON array
                           # names are in properties anyway
                           ret <- unname(ret)
                           response$write(toJSON(ret))
                           return(response$finish())
                         },
                         
                         'availableaspects' = {
                           # /getData.php?dataidentifier=availableaspects
                           response$header("Content-type", "application/javascript")
                           response$write(toJSON(rownames(originalP2object$misc$pathwayOD$xv)))
                           return(response$finish())
                         },
                         
                         'genesetsinaspect' = {
                           requestArguments <- request$GET()
                           aspectId <- url_decode(requestArguments[['aspectId']])
                           # Genesets in this aspect
                           genesets <- unname(originalP2object$misc$pathwayOD$cnam[[aspectId]])
                           # Get the metadata for these gene sets
                           colOfInterest <- c("name","n","cz")
                           retTable <- originalP2object$misc$pathwayODInfo[genesets, colOfInterest]
                           # Convert to JSON friendly format
                           retObj <- unname(apply(retTable, 1, function(x) {
                             # Must be in genesets for short description
                             desc <- geneSets[[x[[1]]]]$properties$shortdescription
                             list(name = x[[1]], n = x[[2]], cz = x[[3]], shortdescription = desc)
                           }))
                           
                           response$header("Content-type", "application/javascript")
                           response$write(toJSON(retObj))
                           return(response$finish())
                         },
                         
                         # Request for reduced dendrogram, down to some
                         # cell partitioning, this returns an hcluse object
                         # as well as the number of cells in each cluster
                         'reduceddendrogram' = {
                           response$header("Content-type", "application/javascript")
                           response$write(reducedDendrogramJSON())
                           return(response$finish())
                         },
                         
                         'cellorder' = {
                           response$header("Content-type", "application/javascript")
                           response$write(cellOrderJSON())
                           return(response$finish())
                         },
                         
                         # Get cell metadata information
                         'cellmetadata' = {
                           response$header("Content-type", "application/javascript")
                           response$write(toJSON(cellmetadata))
                           return(response$finish())
                         },
                         
                         'aspectmatrixbyaspect' = {
                           postArgs <- request$POST()
                           aspectIds <- url_decode(postArgs[['aspectids']])
                           cellIndexStart <- url_decode(postArgs[['cellindexstart']])
                           cellIndexEnd <- url_decode(postArgs[['cellindexend']])
                           aspectIds <- unlist(strsplit(aspectIds, split = "|", fixed =TRUE))
                           
                           cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)]
                           matrixToSend <- originalP2object$misc$pathwayOD$xv[aspectIds,cellIndices,drop=FALSE]

                           # Transpose and make sparse
                           matrixToSend <- Matrix(t(matrixToSend), sparse = TRUE)
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x)
                           iSend <- .self$packCompressInt32Array(matrixToSend@i)
                           pSend <- .self$packCompressInt32Array(matrixToSend@p)

                           Dimnames1Send <- matrixToSend@Dimnames[[1]]

                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" )
                           response$write(toJSON(objToSend))
                           return(response$finish())
                         },
                         
                         'aspectmatrixsparsebyindexbinary' = {
                           
                           postArgs <- request$GET()
                           
                           cellIndexStart <- url_decode(postArgs[['cellindexstart']])
                           cellIndexEnd <- url_decode(postArgs[['cellindexend']])
                           getCellNames <- url_decode(postArgs[['getcellnames']])
                           
                           cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)]
                           matrixToSend <- originalP2object$misc$pathwayOD$xv[,cellIndices,drop=FALSE]

                           # Transpose and make sparse
                           matrixToSend <- Matrix(t(matrixToSend), sparse = TRUE)
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x)
                           iSend <- .self$packCompressInt32Array(matrixToSend@i)
                           pSend <- .self$packCompressInt32Array(matrixToSend@p)
                           
                           Dimnames1Send <- ""
                           if (getCellNames) {
                             Dimnames1Send <- matrixToSend@Dimnames[[1]]
                           }
                           
                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" )
                           response$write(toJSON(objToSend))
                           return(response$finish())
                         },
                         
                         'expressionmatrixsparsebyindexbinary' = {
                           postArgs <- request$POST()

                           if (is.null(postArgs[['geneids']])) {
                             serverLog("Error postArgs[['geneids']] is NULL")
                           }
                           if (is.null(postArgs[['cellindexstart']])) {
                             serverLog("Error postArgs[['cellindexstart']] is NULL")
                           }
                           if (is.null(postArgs[['cellindexend']])) {
                             serverLog("Error postArgs[['cellindexend']] is NULL")
                           }
                           if (is.null(postArgs[['getCellNames']])) {
                             serverLog("Error postArgs[['getCellNames']] is NULL")
                           }
                           
                           geneIdentifiers <- url_decode(postArgs[['geneids']])
                           geneIdentifiers <- unlist(strsplit(geneIdentifiers, split = "|", fixed =TRUE))
                           
                           cellIndexStart <- url_decode(postArgs[['cellindexstart']])
                           cellIndexEnd <- url_decode(postArgs[['cellindexend']])
                           getCellNames <- url_decode(postArgs[['getCellNames']])
                           
                           
                           if (!all(c(geneIdentifiers %in% colnames(originalP2object$counts)))) {
                             serverLog("Error: The request contains gene names that are not in originalP2object$counts!")
                             geneIdentifiers <- geneIdentifiers[geneIdentifiers %in% colnames(originalP2object$counts)]
                           }
                           
                           # Ordering of the matrix according to the hclust
                           cellIndices <- mainDendrogram$cellorder[c(cellIndexStart:cellIndexEnd)]
                           matrixToSend <- originalP2object$counts[cellIndices,geneIdentifiers,drop=FALSE]
                           
                           # Bit pack and compress arrays
                           xSend <- .self$packCompressFloat64Array(matrixToSend@x)
                           iSend <- .self$packCompressInt32Array(matrixToSend@i)
                           pSend <- .self$packCompressInt32Array(matrixToSend@p)
                           
                           Dimnames1Send <- ""
                           if (getCellNames) {
                             Dimnames1Send <- matrixToSend@Dimnames[[1]]
                           }
                           
                           # Convert the attributes to list for JSON packing
                           objToSend <- list(
                             i = iSend,
                             p = pSend,
                             Dim = matrixToSend@Dim,
                             Dimnames1 = Dimnames1Send,
                             Dimnames2 = matrixToSend@Dimnames[[2]],
                             x = xSend
                           )
                           
                           response$header("Content-type", "application/javascript" )
                           response$write(toJSON(objToSend))
                           return(response$finish())
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
                           reductionname <- url_decode(requestArguments[['type']])
                           
                           # Is the requested reduction available?
                           if (!is.null(reductionname) && reductionname  %in% c("mat", names(originalP2object$reductions)) ) {
                             
                             workingReduction <- NULL
                             if (reductionname == "mat") {
                               workingReduction <- mat
                             } else {
                               workingReduction <- originalP2object$reductions[[reductionname]]
                             }
                             
                             selColNames <- requestArguments[['colnames']]
                             selRowNames <- requestArguments[['rownames']]
                             ignoreMissing <- requestArguments[['ignoremissing']]
                             
                             if (is.null(ignoreMissing)) {
                               ignoreMissing <- FALSE
                             } else {
                               ignoreMissing <- FALSE
                             }
                             
                             # Default to all
                             if (is.null(selColNames) ) { selColNames = colnames(workingReduction) }
                             if (is.null(selRowNames) ) { selRowNames = rownames(workingReduction) }
                             
                             # If non-existent rows or cols are specified
                             if ( (!all(selColNames %in% colnames(workingReduction))) | (!all(selRowNames %in% rownames(workingReduction))) ) {
                               if (ignoreMissing) {
                                 selColNames <- selColNames[selColNames %in% colnames(workingReduction)]
                                 selRowNames <- selRowNames[selRowNames %in% rownames(workingReduction)]
                               } else {
                                 response$write("Error: Non existent rows or columns specified and ignoreMissing is not set")
                                 return(response$finish())
                               }
                             }
                             
                             response$header('Content-type', 'application/javascript')
                             response$write(arrayToJSON(workingReduction[selRowNames, selColNames]))
                             return(response$finish())
                           } else {
                             response$write("Error: Unknown reduction type requested")
                             return(response$finish())
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
                           type <- url_decode(requestArguments[['type']])
                           
                           if (!is.null(type)) {
                             if ( type %in% c( names(originalP2object$embeddings), "mat") ) {
                               response$header('Content-type', "application/javascript")
                               
                               # Which embedding?
                               embeddingType <- url_decode(requestArguments[['embeddingtype']])
                               
                               if ( (!is.null(embeddingType)) &&  embeddingType %in% names(originalP2object$embeddings[[type]]) ) {
                                 compEmb <- getCompressedEmbedding(type,embeddingType)
                                 response$write(compEmb)
                                 return(response$finish())
                               } else {
                                 response$write(paste0("Error: Unknown embedding specified: ",embeddingType))
                                 return(response$finish())
                               }
                               
                             } else {
                               # TODO: Set the headers and possibly return a JSON encoded error
                               response$write("Error: Unknown type specified")
                               return(response$finish())
                             }
                           }
                           
                         }, # 'embedding'
                         
                         {
                           response$write("Error: Unknown request")
                           return(response$finish())
                         }
                  ) # switch(dataidentifier
                } #if(!is.null(dataIdentifier
              },
              
              # Perform a computation and return the results
              # e.g. Differential expression
              '/doComputation.php' = {
                requestArguments <- request$GET()
                compIdentifier <- url_decode(requestArguments[['compidentifier']])
                
                if (!is.null(compIdentifier)) {
                  switch(compIdentifier,
                         'doDifferentialExpression1selection' = {
                           message('doDifferentialExpression1selection\n')
                           
                           postArguments <- request$POST()
                           selectionA <- fromJSON(url_decode(postArguments[['selectionA']]))

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
                           })
                           
                           response$write(toJSON(p))
                           
                           #originalP2object
                           return(response$finish())
                           
                         }, # doDifferentialExpression1selectoin
                         
                         'doDifferentialExpression2selections' = {
                           postArguments <- request$POST()
                           
                           selAarg <- postArguments[['selectionA']]
                           selBarg <- postArguments[['selectionB']]
                           
                           selectionA <- fromJSON(url_decode(selAarg))
                           selectionB <- fromJSON(url_decode(selBarg))
                           
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
                           })
                           
                           response$write(toJSON(p))
                           
                           #originalP2object
                           return(response$finish())
                         }
                  )
                }
              }, # doComputation
              
              # Default
              {
                # TODO: Fix the path here, redirects to root
                #response$redirect('index.html')
                app$call(env)
              }
      ) # switch
    } # call = function(env)
)

#' @name pagoda2WebApp_readStaticFile
#' @title pagoda2WebApp_readStaticFile   
#' @description Read a static file from the filesystem, and put in the response
#'
#' @param filename path to filename
#' @return Content to display or error page
NULL 
pagoda2WebApp$methods(
    readStaticFile =  function(filename) {
      filename <- file.path(rookRoot,filename)
      content <- NULL
      tryCatch({
        content <- readChar(filename, file.info(filename)$size)
      }, warning = function(w) {
        content <- paste0("File not found: ",filename)
      }, error = function(e) {
        content <- paste0("File not found: ", filename)
      })
    }
)
    
##updateRookRoot = function(newRoot) {
##  # Update the object variable
##  rookRoot <<- file.path(system.file(package='pagoda2'),'rookServerDocs')
##  
##  # Update the middleware static server
##  .self$app$app$file_server$root <- rookRoot
##},

#' @name pagoda2WebApp_serializeToStaticFast
#' @title pagoda2WebApp_serializeToStaticFast 
#' @description Convert serialized file to static file
#'
#' @param binary.filename path to binary file (default=NULL)
#' @param verbose boolean Whether to give verbose output (default=FALSE)
#' @return static file written by WriteListToBinary(expL=exportList, outfile=binary.filename, verbose=verbose) 
NULL 
pagoda2WebApp$methods(
    serializeToStaticFast = function(binary.filename=NULL, verbose = FALSE, verbose.timings = FALSE){
      if (is.null(binary.filename)) { 
        stop('Please specify a directory')
      }

      
      exportList <- new("list")
      
      # Preparation of objects to pass to Rcpp
      # Create embedding strucutre for export
      embStructure <- generateEmbeddingStructure()
      
      ## Export to list For all contained embeddings
      t0 <- Sys.time()
      for (reduc in names(embStructure)) {
        for (embed in names(embStructure[[reduc]])) {
          id <- embStructure[[reduc]][[embed]][[1]]
          filename <- paste0(id, '.json')
          e <- getCompressedEmbedding(reduc,embed)
          exportList[filename] <- e
        }
      }
      t1 <- Sys.time()
      if(verbose.timings) 
          message(paste0('Export list of embeddings: ', as.double(t1-t0,units="secs")," seconds \n"))
      
      # Export list with all included embeddings for easier iteration in Rcpp-function.
      exportList[["embedList"]] <- grep("emb_",names(exportList),value=TRUE)
      
      ## Main Sparse count matrix to save
      t0 <- Sys.time()
      matsparseToSave <- originalP2object$counts[mainDendrogram$cellorder,]
      t1 <- Sys.time()
      if(verbose.timings) 
          message(paste0('Reordered Sparse matrix in: ', as.double(t1-t0,units="secs")," seconds \n"))
            
      ## Main Sparse count matrix TRANSPOSED for de
      t0 <- Sys.time()
      matsparseTransposedToSave <- Matrix::t(originalP2object$counts)
      t1 <- Sys.time()
      if(verbose.timings) 
          message(paste0('Generated transposed Sparse matrix in: ', as.double(t1-t0,units="secs")," seconds \n"))
            
      ## Serialise aspect matrix
      t0  <- Sys.time()
      cellIndices <- mainDendrogram$cellorder
      aspectMatrixToSave <- originalP2object$misc$pathwayOD$xv[,cellIndices,drop=FALSE]
      aspectMatrixToSave <- Matrix::t(aspectMatrixToSave)
      aspectMatrixToSave <- Matrix(aspectMatrixToSave, sparse=TRUE)
      t1 <- Sys.time()
      if(verbose.timings) 
          message(paste0('Serializing aspect matrix in: ', as.double(t1-t0,units="secs")," seconds \n"))
      
      # Serialise the aspect information
      t0 <- Sys.time()
      aspectInformation <- list()
      for (curAspect in rownames(originalP2object$misc$pathwayOD$xv)) {
        # Genesets in this aspect
        curgenesets <- unname(originalP2object$misc$pathwayOD$cnam[[curAspect]])
        
        # Get the metadata for these gene sets
        colOfInterest <- c("name","n","cz")
        retTable <- originalP2object$misc$pathwayODInfo[curgenesets, colOfInterest]
        
        # Convert to JSON friendly format
        aspectInformation[[curAspect]]  <- unname(apply(retTable, 1, function(x) {
          # Must be in genesets for short description
          desc <- geneSets[[x[[1]]]]$properties$shortdescription
          
          list(name = x[[1]], n = x[[2]], cz = x[[3]], shortdescription = desc)
        }));
      }
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('Serializing aspect information in: ', as.double(t1-t0,units="secs")," seconds \n"))

      t0 <- Sys.time()
      ## Serialising geneset Information
      genesetInformation <- unname(lapply(geneSets, function(x) {x$properties}))
            
      ## Serialise geneset Genes:
      geneListName <- names(geneSets);
      
      ## Export gene names in the gos
      geneListGenes <- lapply( geneSets, function(gos) make.unique(gos$genes))
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('Serializing aspect information in: ', as.double(t1-t0,units="secs")," seconds \n"))
            
      ## Creation of the export List for Rcpp
      
      ## JSON & Annotation part
      t0 <- Sys.time()
      exportList[["reduceddendrogram"]] <- reducedDendrogramJSON()
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('ReduceDendrogramJSON took: ', as.double(t1-t0,units="secs")," seconds \n"))

      t0 <- Sys.time()
      exportList[["cellorder"]] <- cellOrderJSON()
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('cellOrderJSON took: ', as.double(t1-t0,units="secs")," seconds \n"))

      t0 <- Sys.time()
      exportList[["cellmetadata"]] <- cellmetadataJSON();
      t1 <- Sys.time()
      if (verbose.timings) 
          message(paste0('CellmetadataJSON took: ', as.double(t1-t0,units="secs")," seconds \n"))

      t0 <- Sys.time()
      exportList[["geneinformation"]] <- geneInformationJSON();
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('geneInformationJSON took: ', as.double(t1-t0,units="secs")," seconds \n"))
      
      t0 <- Sys.time()
      exportList[["embeddingstructure"]] <- toJSON(embStructure);
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('embStructure to JSON took: ', as.double(t1-t0,units="secs")," seconds \n"))

      t0 <- Sys.time()
      exportList[["aspectInformation"]] <- toJSON(aspectInformation);
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('ascpectInformation to JSON took: ', as.double(t1-t0,units="secs")," seconds \n"))

      t0 <- Sys.time()
      exportList[["genesets"]] <- toJSON(genesetInformation);
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('genesetInformation to JSON took: ', as.double(t1-t0,units="secs")," seconds \n"))
      
      t0 <- Sys.time()
      exportList[["genesetGenes"]] <- toJSON(geneListGenes);
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('geneListGenes to JSON took: ', as.double(t1-t0,units="secs")," seconds \n"))

      t0 <- Sys.time()
      exportList[["appmetadata"]] <- toJSON(appmetadata);
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('appmetadata to JSON took: ', as.double(t1-t0,units="secs")," seconds \n"))
      
      ## The gene Knn is optional
      t0 <- Sys.time()
      if(!is.null(originalP2object$genegraphs$graph)){
        exportList[["geneknn"]] <- generateGeneKnnJSON(originalP2object$genegraphs$graph);
      } else if(verbose) {
        warning("No genegraph provided. It allows you to search for similar genes in the webinterface. \n This is optional, but you can create it with the function makeGeneKnnGraph() \n")
      }
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('geneKnn to JSON took: ', as.double(t1-t0,units="secs")," seconds \n"))

      
      ## Exports sparse Matrix as List with Dimnames converted to JSON
      ## Sparse Count Matrix & Sparse Aspect Matrix
      t0 <- Sys.time()
      exportList[["matsparse"]] <- sparseMatList(matsparseToSave); ## This count values
      exportList[["mataspect"]] <- sparseMatList(aspectMatrixToSave); ## This is the aspect values
      exportList[["sparseMatrixTransp"]] <- sparseMatList(matsparseTransposedToSave); ## To save transposed expr for de
      t1 <- Sys.time()
      if (verbose.timings)
          message(paste0('Matrix exporting took: ', as.double(t1-t0,units="secs")," seconds \n"))
      
      ## Tell Cpp what is a sparse matrix
      exportList[["sparsematnames"]] <- c("matsparse", "mataspect","sparseMatrixTransp");

      ## check if elements missing in list
      "%notin%" = Negate("%in%")
      requiredListElements = c("embedList", "reduceddendrogram", "cellorder","cellmetadata","geneinformation", "embeddingstructure", "aspectInformation", "genesets", "genesetGenes", "appmetadata", "geneknn", "matsparse", "mataspect", "sparseMatrixTransp")
      if (any(requiredListElements %notin% names(exportList))) {
        missingElements = requiredListElements %notin% names(exportList) ## boolean list
        ## requiredListElements[missingElements] will list the missing list names
        stop("Missing list elements for exportList: ", requiredListElements[missingElements], ". This will cause errors in the function WriteListToBinary(expL=exportList).")
      }
      
      ## Call Rcpp function to write to static file
      WriteListToBinary(expL=exportList, outfile=binary.filename, verbose=verbose);
      ##return(invisible(exportList));
      
      ## Return NULL
      NULL
    }
)
  
#' @name pagoda2WebApp_sparseMatList
#' @title pagoda2WebApp_sparseMatList
#' @description Create simple List from sparse Matrix with Dimnames as JSON
#'
#' @param matsparse Sparse matrix
#' @return List with slots i, p, x
NULL
pagoda2WebApp$methods(
    sparseMatList = function(matsparse){
      mslist <- new("list")
      mslist[["matsparse_i"]] <- matsparse@i
      mslist[["matsparse_p"]] <- matsparse@p
      mslist[["matsparse_x"]] <- matsparse@x
      mslist[["matsparse_dim"]] <- matsparse@Dim
      mslist[["matsparse_dimnames1"]] <- toJSON(matsparse@Dimnames[[1]])
      mslist[["matsparse_dimnames2"]] <- toJSON(matsparse@Dimnames[[2]])
      return(mslist)
    }
)
    
#' @name pagoda2WebApp_arrayToJSON
#' @title pagoda2WebApp_arrayToJSON
#' @description Serialise an R array to a JSON object
#'
#' @param arr An array (default=NULL)
#' @return Serialised version of the array in JSON, which includes dimension information as separate fields
NULL
pagoda2WebApp$methods(
    arrayToJSON = function(a = NULL) {
      if (is.null(a) | !is.array(a) ) {
        NULL
      } else {
        # Serialised Array
        toJSON(list(values = a, dim =dim(a), rownames = rownames(a), colnames= colnames(a)));
      }
    }
)

#' @name pagoda2WebApp_getCompressedEmbedding
#' @title pagoda2WebApp_getCompressedEmbedding
#' @description Compress the embedding
#'
#' @param reduc reduction
#' @param embed embedding
#' @return compressed embedding as JSON
NULL
pagoda2WebApp$methods(
    getCompressedEmbedding = function(reduc, embed) {
      emb <- originalP2object$embeddings[[reduc]][[embed]]
      ## Flip Y coordinate
      emb[,2] <- (-1) * emb[,2]
      ret <- list(
        values = .self$packCompressFloat64Array(as.vector(emb)),
        dim =  dim(emb),
        rownames = rownames(emb),
        colnames = colnames(emb)
      )
      toJSON(ret)
    }
)
    
#' @name pagoda2WebApp_serverLog
#' @title pagoda2WebApp_serverLog
#' @description Logging function for console
#'
#' @param message Message to output for the console
#' @return printed message
NULL
pagoda2WebApp$methods(
    serverLog = function(message) {
      print(message)
    }
)
 
#' @name pagoda2WebApp_reducedDendrogramJSON
#' @title pagoda2WebApp_reducedDendrogramJSON
#' @description Parse dendrogram into JSON
#'
#' @return JSON with parsed dendrogram 
NULL
pagoda2WebApp$methods(
    reducedDendrogramJSON = function() {
      h <- mainDendrogram$hc
      l <- unclass(h)[c("merge", "height", "order","labels")]
      l$clusterMemberCount <-  mainDendrogram$cluster.sizes
      return(toJSON(l))
    }
)
 
#' @name pagoda2WebApp_cellOrderJSON
#' @title pagoda2WebApp_cellOrderJSON
#' @description Parse mainDendrogram$cellorder into JSON
#'
#' @return JSON with parsed cell order from mainDendrogram$cellorder
NULL
pagoda2WebApp$methods(
    cellOrderJSON = function() {
      toJSON(mainDendrogram$cellorder)
    }
)

#' @name pagoda2WebApp_availableAspectsJSON
#' @title pagoda2WebApp_availableAspectsJSON
#' @description Parse pathways from originalP2object$misc$pathwayOD$xv into JSON
#'
#' @return JSON with parsed cell order from mainDendrogram$cellorder  
NULL 
pagoda2WebApp$methods(
    availableAspectsJSON = function() {
      toJSON(rownames(originalP2object$misc$pathwayOD$xv))
    }
)
 
#' @name pagoda2WebApp_cellmetadataJSON
#' @title pagoda2WebApp_cellmetadataJSON
#' @description Parse cellmetadata into JSON
#'
#' @return JSON with parsed cellmetadata  
NULL
pagoda2WebApp$methods(
    cellmetadataJSON = function() {
      toJSON(cellmetadata)
    }
)
 
#' @name pagoda2WebApp_geneInformationJSON
#' @title pagoda2WebApp_geneInformationJSON
#' @description Parse originalP2object$misc$varinfo[,c("m","qv")] into JSON
#'
#' @return JSON with parsed information from genename, dispersion, mean gene expression   
NULL
pagoda2WebApp$methods(
    geneInformationJSON = function() {
      dataset <- originalP2object$misc$varinfo[,c("m","qv")]
      dataset$name <- rownames(dataset)
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
                     })
      retd <- unname(retd)
      
      toJSON(retd)
    }
)
 

#' @name pagoda2WebApp_generateGeneKnnJSON
#' @title pagoda2WebApp_generateGeneKnnJSON
#' @description Generate a JSON list representation of the gene kNN network
#'
#' @param graph Input graph
#' @return JSON with gene kNN network
NULL
pagoda2WebApp$methods(
    generateGeneKnnJSON = function(graph) {
        toJSON(split(originalP2object$genegraphs$graph$to, originalP2object$genegraphs$graph$from))
    }
)


#' @name pagoda2WebApp_generateEmbeddingStructure 
#' @title pagoda2WebApp_generateEmbeddingStructure 
#' @description Generate information about the embeddings we are exporting
#'
#' @return List with embeddings
NULL
pagoda2WebApp$methods(
    generateEmbeddingStructure = function() {
      resp <- list()
      i <- 0
      for( r in names(originalP2object$embeddings)) {
        resp[[r]] <- list()
        for ( n in names(originalP2object$embeddings[[r]])) {
          id <- paste('emb',r,n,sep='_')
          resp[[r]][[n]] <- id
          i <- i+1
        }
      }
      resp
    } ## generateEmbeddingStructure
)