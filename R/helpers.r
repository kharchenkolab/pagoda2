#' @import org.Hs.eg.db
#' @import GO.db
#' @import Rook
#' @importFrom parallel mclapply
#'
NULL


#' @export p2.generate.human.go
p2.generate.human.go <- function(r) {
  # Generate GO environment
  require(org.Hs.eg.db)
  require(GO.db)
  require(BiocGenerics)
  require(AnnotationDbi)

  # translate gene names to ids
  ids <- unlist(lapply(BiocGenerics::mget(colnames(r$counts),org.Hs.egALIAS2EG,ifnotfound=NA),function(x) x[1]))

  # reverse map
  rids <- names(ids); names(rids) <- ids;

  # list all the ids per GO category
  go.env <- AnnotationDbi::eapply(org.Hs.egGO2ALLEGS,function(x) as.character(na.omit(rids[x])))
  go.env <- go.env[unlist(lapply(go.env,length))>5];
  go.env <- list2env(go.env);

  go.env
}


#' @title Generate a metadata structure for a p2 web object from a named factor
#' @description This function will genereate a metadata structure that can be passed to
#' p2 web object constructor as additional metadata from a named factor of arbitrary values
#' @param metadata named factor with metadata for individual cells, names must correspond to cells
#' @param displayname name to display for the metadata
#' @param s s value for rainbow palette
#' @param v v value for rainbow palette
#' @param start starting value
#' @param end ending value
#' @param pal optional vector of colours to use, if provided overrides s,v,start and end parameters
#' @export p2.metadata.from.factor
p2.metadata.from.factor <- function(metadata, displayname = NULL, s = 1, v = 1, start = 0, end = 1, pal = NULL) {
  # Check input
  if ( !is.factor(metadata) ) {
    stop('metadata is not a factor');
  }

  if (  is.null(names(metadata))) {
    stop('metadata needs to be named with cell identifiers');
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
                        show.batch = T, show.clusters = T, appname = "Pagoda2 Application",innerOrder=NULL,orderDend=FALSE) {
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
        orderDend = orderDend
    );

    invisible(p2w);
}

#' @export show.app
show.app <- function(app, name, port, ip, browse = TRUE,  server = NULL) {
                                        # replace special characters
    name <- gsub("[^[:alnum:.]]", "_", name)

    if(is.null(server)) {
        server <- get.scde.server(port=port,ip=ip)
    }
    server$add(app = app, name = name)
    if(is.function(server$listenPort)) {
        url <- paste("http://", server$listenAddr, ":", server$listenPort(), server$appList[[name]]$path,"/index.html",sep='')
    } else {
        url <- paste("http://", server$listenAddr, ":", server$listenPort, server$appList[[name]]$path,"/index.html",sep='')
    }
    print(paste("app loaded at: ",url,sep=""))
    if(browse) {
        browseURL(url);
    }

    return(invisible(server))
}

                                        # get SCDE server from saved session
get.scde.server <- function(port,ip) {
    if(exists("___scde.server", envir = globalenv())) {
        server <- get("___scde.server", envir = globalenv())
    } else {
        require(Rook)
        server <- Rhttpd$new()
        assign("___scde.server", server, envir = globalenv())
        if(!missing(ip)) {
            if(missing(port)) {
                server$start(listen = ip)
            } else {
                server$start(listen = ip, port = port)
            }
        } else {
            if(missing(port)) {
                server$start()
            } else {
                server$start(port=port)
            }
        }
    }
    return(server)
}

# BH P-value adjustment with a log option
bh.adjust <- function(x, log = FALSE) {
    nai <- which(!is.na(x))
    ox <- x
    x<-x[nai]
    id <- order(x, decreasing = FALSE)
    if(log) {
        q <- x[id] + log(length(x)/seq_along(x))
    } else {
        q <- x[id]*length(x)/seq_along(x)
    }
    a <- rev(cummin(rev(q)))[order(id)]
    ox[nai]<-a
    ox
}

# returns enriched categories for a given gene list as compared with a given universe
# returns a list with over and under fields containing list of over and underrepresented terms
calculate.go.enrichment <- function(genelist, universe, pvalue.cutoff = 1e-3, mingenes = 3, env = go.env, subset = NULL, list.genes = FALSE, over.only = FALSE) {
    genelist <- unique(genelist)
    all.genes <- unique(ls(env))
    # determine sizes
    universe <- unique(c(universe, genelist))
    universe <- universe[universe != ""]
    genelist <- genelist[genelist != ""]
    ns <- length(intersect(genelist, all.genes))
    us <- length(intersect(universe, all.genes))
    #pv <- lapply(go.map, function(gl) { nwb <- length(intersect(universe, gl[[1]])) if(nwb<mingenes) { return(0.5)} else { p <- phyper(length(intersect(genelist, gl[[1]])), nwb, us-nwb, ns) return(ifelse(p > 0.5, 1.0-p, p)) }})

    # compile count vectors
    stab <- table(unlist(mget(as.character(genelist), env, ifnotfound = NA), recursive = TRUE))
    utab <- table(unlist(mget(as.character(universe), env, ifnotfound = NA), recursive = TRUE))
    if(!is.null(subset)) {
        stab <- stab[names(stab) %in% subset]
        utab <- utab[names(utab) %in% subset]
    }

    tabmap <- match(rownames(stab), rownames(utab))

    cv <- data.frame(cbind(utab, rep(0, length(utab))))
    names(cv) <- c("u", "s")
    cv$s[match(rownames(stab), rownames(utab))] <- as.vector(stab)
    cv <- na.omit(cv)
    cv <- cv[cv$u > mingenes, ]

    if(over.only) {
        lpr <- phyper(cv$s-1, cv$u, us-cv$u, ns, lower.tail = FALSE, log.p = TRUE)
    } else {
        pv <- phyper(cv$s, cv$u, us-cv$u, ns, lower.tail = FALSE)
        lpr <- ifelse(pv<0.5, phyper(cv$s-1, cv$u, us-cv$u, ns, lower.tail = FALSE, log.p = TRUE), phyper(cv$s+1, cv$u, us-cv$u, ns, lower.tail = TRUE, log.p = TRUE))
    }
    lpr <- phyper(cv$s-1, cv$u, us-cv$u, ns, lower.tail = FALSE, log.p = TRUE)
    lpra <- bh.adjust(lpr, log = TRUE)
    z <- qnorm(lpr, lower.tail = FALSE, log.p = TRUE)
    za <- qnorm(lpra, lower.tail = FALSE, log.p = TRUE)
    # correct for multiple hypothesis
    mg <- length(which(cv$u > mingenes))
    if(over.only) {
        if(pvalue.cutoff<1) {
            ovi <- which(lpra<= log(pvalue.cutoff))
            uvi <- c()
        } else {
            ovi <- which((lpr+mg)<= log(pvalue.cutoff))
            uvi <- c()
        }
    } else {
        if(pvalue.cutoff<1) {
            ovi <- which(pv<0.5 & lpra<= log(pvalue.cutoff))
            uvi <- which(pv > 0.5 & lpra<= log(pvalue.cutoff))
        } else {
            ovi <- which(pv<0.5 & (lpr+mg)<= log(pvalue.cutoff))
            uvi <- which(pv > 0.5 & (lpr+mg)<= log(pvalue.cutoff))
        }
    }
    ovi <- ovi[order(lpr[ovi])]
    uvi <- uvi[order(lpr[uvi])]

    #return(list(over = data.frame(t = rownames(cv)[ovi], o = cv$s[ovi], u = cv$u[ovi], p = pr[ovi]*mg), under = data.frame(t = rownames(cv)[uvi], o = cv$s[uvi], u = cv$u[uvi], p = pr[uvi]*mg)))
    if(list.genes) {
        x <- mget(as.character(genelist), env, ifnotfound = NA)
        df <- data.frame(id = rep(names(x), unlist(lapply(x, function(d) length(na.omit(d))))), go = na.omit(unlist(x)), stringsAsFactors = FALSE)
        ggl <- tapply(df$id, as.factor(df$go), I)
        ovg <- as.character(unlist(lapply(ggl[rownames(cv)[ovi]], paste, collapse = " ")))
        uvg <- as.character(unlist(lapply(ggl[rownames(cv)[uvi]], paste, collapse = " ")))
        return(list(over = data.frame(t = rownames(cv)[ovi], o = cv$s[ovi], u = cv$u[ovi], Za = za, fe = cv$s[ovi]/(ns*cv$u[ovi]/us), genes = ovg), under = data.frame(t = rownames(cv)[uvi], o = cv$s[uvi], u = cv$u[uvi], Za = za, fe = cv$s[uvi]/(ns*cv$u[uvi]/us), genes = uvg)))
    } else {
        return(list(over = data.frame(t = rownames(cv)[ovi], o = cv$s[ovi], u = cv$u[ovi], p.raw = exp(lpr[ovi]), fdr = exp(lpra)[ovi], Z = z[ovi], Za = za[ovi], fe = cv$s[ovi]/(ns*cv$u[ovi]/us), fer = cv$s[ovi]/(length(genelist)*cv$u[ovi]/length(universe))), under = data.frame(t = rownames(cv)[uvi], o = cv$s[uvi], u = cv$u[uvi], p.raw = exp(lpr[uvi]), fdr = exp(lpra)[uvi], Z = z[uvi], Za = za[uvi], fe = cv$s[uvi]/(ns*cv$u[uvi]/us))))
    }
}


# fater matrix correlations wtih armadillo
#' @title armaCor - matrix column correlations
#' @description similar to cor() call, will calculate correlation between matrix columns
#' @param mat matrix, whose columns will be
#' @export armaCor
armaCor <- function(mat) {
  cd <- arma_mat_cor(mat);
  rownames(cd) <- colnames(cd) <- colnames(mat);
  return(cd)
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
