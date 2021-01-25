#' @import Rook
#' @import rjson
NULL

#' Internal function to visualize aspects of transcriptional heterogeneity as a heatmap.
#'
#' @param mat Numeric matrix
#' @param row.clustering Row dendrogram (default=NA)
#' @param cell.clustering Column dendrogram (default=NA)
#' @param zlim numeric Range of the normalized gene expression levels, inputted as a list: c(lower_bound, upper_bound) (default=c(-1, 1)*quantile(mat, p = 0.95)). Values outside this range will be Winsorized. Useful for increasing the contrast of the heatmap visualizations. Default, set to the 5th and 95th percentiles.
#' @param row.cols Matrix of row colors (default=NULL)
#' @param col.cols Matrix of column colors (default=NULL). Useful for visualizing cell annotations such as batch labels.
#' @param cols Heatmap colors (default=colorRampPalette(c("darkgreen", "white", "darkorange"), space = "Lab")(1024))
#' @param show.row.var.colors boolean Whether to show row variance as a color track (default=TRUE)
#' @param top integer Restrict output to the top n aspects of heterogeneity (default=Inf)
#' @param ... additional arguments for heatmap plotting
#' @return A heatmap
#'
#' @keywords internal
view.aspects <- function(mat, row.clustering = NA, cell.clustering = NA, zlim = c(-1, 1)*quantile(mat, p = 0.95), 
    row.cols=NULL, col.cols=NULL, cols = colorRampPalette(c("darkgreen", "white", "darkorange"), space = "Lab")(1024), show.row.var.colors=TRUE, top=Inf, ...) {
    #row.cols, col.cols are matrices for now
    rcmvar <- apply(mat, 1, var)
    mat[mat<zlim[1]] <- zlim[1]
    mat[mat > zlim[2]] <- zlim[2]
    if(class(row.clustering) == "hclust") { row.clustering <- as.dendrogram(row.clustering) }
    if(class(cell.clustering) == "hclust") { cell.clustering <- as.dendrogram(cell.clustering) }
    if(show.row.var.colors) {
        if(is.null(row.cols)) {
            icols <- colorRampPalette(c("white", "black"), space = "Lab")(1024)[1023*(rcmvar/max(rcmvar))+1]
            row.cols <- cbind(var = icols)
        }
    }
    my.heatmap2(mat, Rowv = row.clustering, Colv = cell.clustering, zlim = zlim, RowSideColors = row.cols, ColSideColors = col.cols, col = cols, ...)
}


#' Create 'PAGODA1' web application from a 'Pagoda2' object
#' 'PAGODA1' found here, with 'SCDE': <https://www.bioconductor.org/packages/release/bioc/html/scde.html>
#'
#' @param p2 'Pagoda2' object
#' @param col.cols Matrix of column colors (default=NULL). Useful for visualizing cell annotations such as batch labels. 
#' @param row.clustering Row dendrogram (default=NULL)
#' @param title character Title to use (default="pathway clustering")
#' @param zlim Range of the normalized gene expression levels (default=NULL). Input as a list: c(lower_bound, upper_bound). Values outside this range will be Winsorized. Useful for increasing the contrast of the heatmap visualizations. If NULL, set to the 5th and 95th percentiles. 
#' @param embedding A 2-D embedding of the cells (PCA, tSNE, etc.), passed as a data frame with two columns (two dimensions) and rows corresponding to cells (row names have to match cell names) (default=NULL).
#' @param inner.clustering boolean Whether to get overall cell clustering (default=TRUE).
#' @param groups factor describing grouping of different cells. If provided, the cross-fits and the expected expression magnitudes will be determined separately within each group. The factor should have the same length as ncol(counts) (default=NULL).
#' @param clusterType cluster type (default=NULL). If NULL, takes the latest cluster in the 'Pagoda2' object using 'p2$clusters[[type]][[1]]'
#' @param embeddingType embedding type (default=NULL). If NULL, takes the latest embedding in the 'Pagoda2' object using p2$embeddings[[type]][[1]] 
#' @param veloinfo cell velocity information, cell velocities (grid and cell) (default=NULL)
#' @param type character Either 'counts' or a name of a 'reduction' in the 'Pagoda2' object (default='PCA')
#' @param min.group.size integer Minimum group size (default=1)
#' @param batch.colors colors of the batches, i.e. the factor (corresponding to rows of the model matrix) specifying batch assignment of each cell(default=NULL)
#' @param n.cores numeric Number of cores (default=10)
#' @return 'PAGODA1' web application
#' @export 
p2.make.pagoda1.app <- function(p2, col.cols=NULL, row.clustering=NULL, title = "pathway clustering", 
  zlim = NULL, embedding=NULL, inner.clustering=TRUE, groups=NULL, clusterType=NULL,
  embeddingType=NULL, veloinfo=NULL, type='PCA', min.group.size=1, batch.colors=NULL, n.cores=10) {
  if (!requireNamespace("GO.db", quietly = TRUE)) {
    stop("Package \"GO.db\" needed for this function to work. Please install it with `BiocManager::install('GO.db')`.", call. = FALSE)
  }
  if (!requireNamespace("BiocGenerics", quietly = TRUE)) {
    stop("Package \"BiocGenerics\" needed for this function to work. Please install it with `BiocManager::install('BiocGenerics')`.", call. = FALSE)
  }
  # rcm - xv
  if (type=='counts') {
    x <- p2$counts;
    x <- t(t(x)*p2$misc[['varinfo']][colnames(x),'gsf'])
  } else {
    if(!type %in% names(p2$reductions)) { stop("reduction ",type,' not found')}
    x <- p2$reductions[[type]]
  }

  if(is.null(groups)) {
    # look up the clustering based on a specified type
    if(is.null(clusterType)) {
      # take the first one
      groups <- p2$clusters[[type]][[1]]
    } else {
      groups <- p2$clusters[[type]][[clusterType]]
      if(is.null(groups)) { stop("Clustering ",clusterType," for type ", type," doesn't exist")}
    }
  }
  groups <- as.factor(groups[rownames(x)]);
  groups <- droplevels(groups);

  if(is.null(embedding)) {
    if(is.null(p2$embeddings[[type]])) { stop("First, generate embeddings for type ",type)}
    if(is.null(embeddingType)) {
      # take the first one
      embedding <- p2$embeddings[[type]][[1]]
    } else {
      embedding <- p2$embeddings[[type]][[embeddingType]]
    }
  }

  # cluster groups
  if(type=='counts') { # sparse matrix
    rowFac <- rep(-1,nrow(x)); names(rowFac) <- rownames(x);
    rowFac[names(groups)] <- as.integer(groups);
    tc <- colSumByFac(x,as.integer(rowFac))[-1,]
    rownames(tc) <- levels(groups)
  } else { # assume dense matrix
    tc <- do.call(rbind,tapply(1:nrow(x),groups,function(ii) colMeans(x[ii,,drop=F],na.rm=TRUE)))
  }
  d <- 1-cor(t(tc))
  hc <- stats::hclust(as.dist(d),method='ward.D')

  # get overall cell clustering
  if(inner.clustering) {
    # determine refined cell order
    if(type=='counts') {
      stop("inner.clustering with type='counts' is not yet supported")
    } 

    clo <- papply(levels(groups),function(lev) {
      ii <- which(groups==lev);
      if(length(ii)>3) {
        dd <- as.dist(1-abs(cor(t(as.matrix(x[ii,,drop=F])))))
        dd[is.na(dd)] <- 1
        hcc <- fastcluster::hclust(dd, method = "ward.D")
        return(ii[hcc$order])
      }

      return(ii)
    },n.cores=n.cores)
  } else {
    # use random cell order within the clusters
    clo <- tapply(1:length(groups),groups,I)
  }

  # make fake hc
  # come up with a joint order and a position of each leaf (to fake out the tree plotting method)
  hca <- hc
  hca$original.order <- hca$order
  #mo <- match(1:length(hca$original.order),rev(hca$original.order))
  mo <- hca$original.order
  hca$order <- unlist(clo[mo])
  cluster.sizes <- unlist(lapply(clo[mo],length))
  #hca$plotting.order <- c(cumsum(cluster.sizes)-round(cluster.sizes/2),rep(0,length(hca$order)-length(hca$original.order)))
  hca$plotting.order <- c((cumsum(cluster.sizes)-round(cluster.sizes/2))[match(1:length(mo),mo)],rep(0,length(hca$order)-length(hca$original.order)))

  if(is.null(p2$misc[['pathwayOD']])){
    stop("pathwayOD missing, please run testPathwayOverdispersion()")
  }
  tamr <- p2$misc[['pathwayOD']]
  env <- tamr$env; 
  if(is.null(zlim)) { zlim <- c(-1, 1)*quantile(tamr$xv, p = 0.95) }

  if(is.null(row.clustering) || is.null(row.clustering$order)) {
    row.clustering <- stats::hclust(dist(tamr$xv))
  } else if(class(row.clustering)!="hclust") {
    # make a fake clustering to match the provided order
    or <- row.clustering$order;
    row.clustering <- stats::hclust(dist(tamr$xv),method='single')
    names(or) <- as.character(-1*row.clustering$order)
    nmm <- -1*or[as.character(row.clustering$merge)]
    nmm[is.na(nmm)] <- as.character(row.clustering$merge)[is.na(nmm)]
    row.clustering$merge <- matrix(as.integer(nmm),ncol=ncol(row.clustering$merge))
    row.clustering$order <- as.integer(or);
  }


  if(!is.null(embedding)) {
    if(is.null(rownames(embedding))) { stop("provided 2D embedding lacks cell names") }
    vi <- rownames(embedding) %in% colnames(tamr$xv)
    if(!all(vi)) {
      warning("provided 2D embedding contains cells that are not in the tamr")
      embedding <- embedding[vi,];
      if(nrow(embedding)<2) {
        stop("provided 2D embedding contains too few cells after intersecting with the cell names in tamr")
      }
    }
    # flip embedding y axis since this is what p1 does
    embedding[,2] <- -1*embedding[,2]

    if(!is.null(veloinfo)) {
      # flip vertical coordinates on the arrows
      veloinfo$proj$garrows[,c(2,4)] <- -1*veloinfo$proj$garrows[,c(2,4)];
      # put in order of the embedding cells
      x <- veloinfo$proj$arrows; x <- x[rownames(x) %in% rownames(embedding),]; 
      x[,c(2,4)] <- -1*x[,c(2,4)];
      y <- cbind(embedding,embedding);
      mi <- match(rownames(y),rownames(x));
      y[!is.na(mi),] <- x[mi[!is.na(mi)],]
      veloinfo$proj$arrows <- y;
    }
  }

  # cleanup colcols
  if(!is.null(col.cols)) {
    if(is.list(col.cols)) {
      # look up the cells
      col.cols <- lapply(col.cols,function(x) {
        if(is.null(names(x$data))) {
          if(ncol(x$data)!=ncol(tamr$xv)) { stop("col.cols data vectors have to be either named with cell names or be of length equal to tamr$xv column number") }
        } else {
          vi <- match(colnames(tamr$xv),names(x$data));
          x$data <- x$data[vi];
          if(!is.null(x$text)) { x$text <- x$text[vi] }
        }
        x
      })
    } else {
      # assume it's the old kind
      rn <- rownames(col.cols)
      col.cols <- lapply(1:nrow(col.cols),function(i) {
        list("data"=col.cols[i,],"legacy"=TRUE)
      })
      names(col.cols) <- rn
      col.cols <- rev(col.cols)
    }
  }

  # prepend groups, depth and batch
  clean.groups <- groups;
  if(min.group.size>1) { clean.groups[clean.groups %in% levels(clean.groups)[unlist(tapply(clean.groups,clean.groups,length))<min.group.size]] <- NA; clean.groups <- as.factor(clean.groups); }
  factor.colors <- fac2col(clean.groups,s=0.8,v=0.8,shuffle=FALSE,min.group.size=min.group.size,return.details=TRUE)
  acol <- list('clusters'=list(data=droplevels(clean.groups),
                               colors=as.character(factor.colors$palette),
                               text=paste('cl',as.character(groups))),
               "depth"=list(data=p2$depth,
                            colors=colorRampPalette(c("white","black"),space="Lab")(1024),
                            quantile.range=0.95))
  if(!is.null(p2$batch)) {
    batch.colors <- fac2col(p2$batch,s=1.0,v=0.5,shuffle=FALSE,level.colors=batch.colors,return.details=TRUE)
    acol <- c(acol,list('batch'=list(data=p2$batch,colors=as.character(batch.colors$palette),text=as.character(p2$batch))))
  }
  col.cols <- rev(c(acol,col.cols))


  #fct - which tam row in which tamr$xv cluster.. remap tamr$cnams
  cn <- tamr$cnam
  fct <- rep(1:length(cn), lapply(cn, length))
  names(fct) <- unlist(cn)
  #fct <- fct[rownames(tamr$xv)]

  vdf <- p2$misc[['pathwayODInfo']]
  matvar <- vdf$sd[vdf$valid]
  fres <- list(hvc = hca, tvc = row.clustering, rcm = tamr$xv, zlim2 = zlim, matvar = matvar, ct = fct, matrcmcor = rep(1, length(matvar)), cols = colorRampPalette(c("darkgreen", "white", "darkorange"), space = "Lab")(1024), colcol = col.cols, p2=p2,hc=hc)

  # gene df
  gdf <- p2$misc[['varinfo']]
  gene.df <- data.frame(var = gdf$qv,gene=rownames(gdf))
  gene.df <- gene.df[order(gene.df$var, decreasing = TRUE), ]

  # prepare pathway df
  df <- data.frame(name = vdf$name, npc = vdf$npc, n = vdf$n, score = vdf$oe, z = vdf$z, adj.z = vdf$cz, stringsAsFactors = FALSE)
  
  df$desc <- unlist(lapply(BiocGenerics::mget(df$name,GO.db::GOTERM,ifnotfound=NA),function(x) if(typeof(x)=="S4") { return(x@Term) }else { return("") } ))

  min.z <- -9
  df$z[df$z<min.z] <- min.z
  df$adj.z[df$adj.z<min.z] <- min.z
  df <- data.frame(id = paste("#PC", df$npc, "# ", df$name, sep = ""), npc = df$npc, n = df$n, score = df$score, Z = df$z, aZ = df$adj.z, sh.Z = 0, sh.aZ = 0, name = paste(df$name, df$desc))

  df <- df[order(df$score, decreasing = TRUE), ]

  # merge go.env
  sa <- p2ViewPagodaApp$new(results=fres, pathways=df, genes=gene.df, goenv=env, batch=NULL, name = title, trim = 0, embedding=embedding,type=type,veloinfo=veloinfo)

}


#' @title p2ViewPagodaApp R6 class
#' @description Modified 'PAGODA1' app (from 'SCDE') for browsing 'pagoda2' results. 
#' Refer to 'ViewPagodaAppOld' and 'make.pagoda.app()' in 'SCDE'
#'
#' @export p2ViewPagodaApp 
p2ViewPagodaApp <- R6::R6Class("p2ViewPagodaApp ", lock_objects=FALSE,
    ## fields = c('results', 'tam', 'genes', 'pathways', 'goenv', 'renv', 
    ##  'name', 'trim', 'batch','embedding','type','veloinfo'),

    ## tam? 
    ## @field tam Combined pathways that are driven by the same gene sets
    public = list(

      #' @field results Result object returned by \code{scde.expression.difference()} (default=NULL). Note to browse group posterior levels, use \code{return.posteriors = TRUE} in the \code{scde.expression.difference()} call.
      results = NULL,

      #' @field type Either 'counts' or a name of a 'reduction' in the 'Pagoda2' object 
      type = NULL,

      #' @field genes List of genes to display in the Detailed clustering panel (default=list())
      genes = list(),

      #' @field batch Any batch or other known confounders to be included in the visualization as a column color track (default=NULL)
      batch = NULL,

      #' @field pathways character vector Pathway or gene names (default=NULL)
      pathways = NULL,

      #' @field name App name (needs to be altered only if adding more than one app to the server using the 'server' parameter) (default=NULL)
      name = NULL,

      #' @field trim Trim quantity used for Winsorization for visualization
      trim = NULL,

      #' @field embedding Embedding information (default=NULL)
      embedding = NULL,

      #' @field veloinfo Velocity information (default=NULL)
      veloinfo = NULL,

      #' @field goenv environment mapping pathways to genes (default=NULL)
      goenv = NULL,

      #' @field renv Global environment (default=NULL)
      renv = NULL,



      #' @description Initialize p2ViewPagodaApp class
      #'
      #' @param results Result object returned by \code{scde.expression.difference()}. Note to browse group posterior levels, use \code{return.posteriors = TRUE} in the \code{scde.expression.difference()} call.
      #' @param pathways character vector Pathway or gene names (default=NULL)
      #' @param genes list Genes to display in the Detailed clustering panel (default=list())
      #' @param goenv Environment mapping pathways to genes (default=NULL)
      #' @param batch Any batch or other known confounders to be included in the visualization as a column color track (default=NULL)
      #' @param name string App name (needs to be altered only if adding more than one app to the server using the 'server' parameter) (default="pathway overdispersion")
      #' @param trim numeric Trim quantity used for Winsorization for visualization (default=1.1/nrow(p2$counts) whereby the 'counts' from the 'Pagoda2' object is the gene count matrix, normalized on total counts (default=NULL)
      #' @param embedding Embedding information (default=NULL)
      #' @param type Either 'counts' or a name of a 'reduction' in the 'pagoda2' object 
      #' @param veloinfo Velocity information (default=NULL)
      #' 
      #' @return new 'p2ViewPagodaApp' object 
      initialize = function(results, pathways, genes, goenv, batch = NULL, name = "pathway overdispersion", 
        trim = 1.1/nrow(p2$counts), embedding=NULL, type, veloinfo=NULL) {
        if (!requireNamespace("BiocGenerics", quietly = TRUE)) {
          stop("Package \"BiocGenerics\" needed for the p2ViewPagodaApp class to work. Please install it with `BiocManager::install('BiocGenerics')`.", call. = FALSE)
        }
        if (!requireNamespace("GO.db", quietly = TRUE)) {
          stop("Package \"GO.db\" needed for the p2ViewPagodaApp class to work. Please install it with `BiocManager::install('GO.db')`.", call. = FALSE)
        }
        ##if (!missing(results) && class(results)=='p2ViewPagodaApp') { # copy constructor
          ##callSuper(results);
        ##} else {
          ##callSuper();
          self$results <- results
          self$type <- type
          #results$tvc$order <- rev(results$tvc$order);
          self$results$tvc$labels <- as.character(1:nrow(self$results$rcm))
          rownames(self$results$rcm) <- as.character(1:nrow(self$results$rcm))
          self$genes <- genes
          self$genes$svar <- self$genes$var/max(self$genes$var)
          ## genes <- genes  ??
          self$batch <- self$results$p2$batch
          self$pathways <- pathways
          self$name <- name
          self$trim <- trim
          self$embedding <- embedding
          self$veloinfo <- veloinfo
          # reverse lookup environment
          xl <- as.list(self$goenv)
          gel <- tapply(rep(names(xl), unlist(lapply(xl, length))), unlist(xl), I)
          gel <- gel[nchar(names(gel)) > 0]
          self$renv <- list2env(gel, parent=emptyenv())
          self$goenv <- list2env(xl, parent=emptyenv())
          rm(xl,gel)
          gc()
      },

      #' @description Helper function to get the heatmap data for a given set of genes
      #'
      #' @param genes character vector Gene names (default=NULL)
      #' @param gcl pathway or gene-weighted PCA (default=NULL). If NULL, uses tp2c.view.pathways(self$genes, self$results$p2, goenv=goenv, vhc=self$results$hvc, plot=FALSE, trim=ltrim, n.genes=Inf).
      #' @param ltrim numeric Winsorization trim that should be applied (default=0)
      #' 
      #' @return heatmap data for a given set of genes
      getgenecldata = function(genes = NULL, gcl = NULL, ltrim = 0) { # helper function to get the heatmap data for a given set of genes
        if(is.null(gcl)) {
          gcl <- tp2c.view.pathways(self$genes, self$results$p2, goenv=goenv, vhc=self$results$hvc, plot=FALSE, trim=ltrim, n.genes=Inf)
          #gcl <- t.view.pathways(genes, mat = mat, matw = matw, env = goenv, vhc = results$hvc, plot = FALSE, trim = ltrim)
        }

        matrix <- gcl$vmap[rev(gcl$row.order), self$results$hvc$order, drop = FALSE]
        matrix <- list(data = as.numeric(t(matrix)),
                       dim = dim(matrix),
                       rows = rownames(matrix),
                       cols = colnames(matrix),
                       colors = gcl$col,
                       zlim = as.numeric(gcl$zlim)
                       )

        ol <- list(matrix = matrix)
        if(!is.null(gcl$rotation)) {
          rcmvar <- matrix(gcl$rotation[rownames(gcl$vmap)[rev(gcl$row.order)], , drop = FALSE], ncol = 1)
          rowcols <- list(data = as.numeric(t(rcmvar)),
                          dim = dim(rcmvar),
                          colors = gcl$oc.col,
                          zlim = c(-1,1)*max(abs(rcmvar))
                          )

          colcols <- matrix(gcl$oc[self$results$hvc$order], nrow = 1)
          colcols <- list(data = as.numeric(t(colcols)),
                          dim = dim(colcols),
                          colors = gcl$oc.col,
                          zlim = c(-1,1)*quantile(abs(colcols),p=c(0.9))
                          )
          ol <- c(ol, list(rowcols = rowcols, colcols = colcols))
        }
        ol
      },

      #' @description Call Rook application. Using client-side ExtJS framework and Inchlib HTML5 canvas libraries to create the graphical user interface for PAGODA
      #'
      #' @param env The environment argument is a true R environment object which the application is free to modify. Please see the Rook documentation for more details.
      #' 
      #' @return modified 'PAGODA1' app
      call = function(env){
            path <- env[['PATH_INFO']]
            req <- Request$new(env)
            res <- Response$new()
            switch(path,
                   # INDEX
                   '/index.html' = {
                       body <- paste('<!DOCTYPE html >
                                     <meta charset = "utf-8" >
                                     <html >
                                     <head >
                                     <title > ', self$name, '</title >
                                     <meta http-equiv = "Content-Type" content = "text/html charset = iso-8859-1" >
                                     <link rel = "stylesheet" type = "text/css" href = "http://pklab.med.harvard.edu/sde/extjs/resources/ext-theme-neptune/ext-theme-neptune-all.css" / >
                                     <link rel = "stylesheet" type = "text/css" href = "http://pklab.med.harvard.edu/sde/extjs/examples/shared/example.css" / >
                                     <link rel = "stylesheet" type = "text/css" href = "http://pklab.med.harvard.edu/sde/pathcl_velo.css" / >
                                     <head profile = "http://www.w3.org/2005/10/profile" >
                                     <link rel = "icon" type = "image/png" href = "http://pklab.med.harvard.edu/sde/pagoda.png" >
                                     <script type = "text/javascript" src = "http://pklab.med.harvard.edu/sde/extjs/ext-all.js" > </script >
                                     <script type = "text/javascript" src = "http://pklab.med.harvard.edu/sde/jquery-1.11.1.min.js" > </script >
                                     <script src = "http://d3js.org/d3.v3.min.js" charset = "utf-8" > </script >
                                     <script type = "text/javascript" src = "http://pklab.med.harvard.edu/sde/pathcl_velo_1.4.js" > </script >
                                     </head >
                                     <body > </body >
                                     </html >
                                     ', sep = "")
                       res$header('"Content-Type": "text/html"')
                       res$write(body)
                   },
                   '/pathcl.json' = { # report pathway clustering heatmap data
                     # column dendrogram
                     treeg <- list(merge=as.vector(t(self$results$hvc$merge)),height=self$results$hvc$height,order=self$results$hvc$plotting.order)

                     matrix <- self$results$rcm[rev(self$results$tvc$order), self$results$hvc$order]
                     matrix <- list(data = as.numeric(t(matrix)),
                                    dim = dim(matrix),
                                    rows = rownames(matrix),
                                    cols = colnames(matrix),
                                    colors = self$results$cols,
                                    zlim = as.numeric(self$results$zlim2)
                                    )

                     icols <- colorRampPalette(c("white", "black"), space = "Lab")(256)
                     rcmvar <- matrix(apply(self$results$rcm[rev(self$results$tvc$order), , drop = FALSE], 1, var), ncol = 1)
                     rowcols <- list(data = as.numeric(t(rcmvar)),
                                     dim = dim(rcmvar),
                                     colors = icols,
                                     zlim = c(0, max(rcmvar))
                                     )
                     # translate colcol structure into a uniform data/dim/colors/zlim (for gradients)/text
                     colcols <- lapply(self$results$colcol,function(x) {
                       # is it a numeric gradient?
                       if(is.numeric(x$data)) {
                         colors <- x$colors;
                         data <- as.numeric(x$data[self$results$hvc$order]);
                         dim <- c(1,length(data))
                         # establish limits
                         if(is.null(x$zlim)) {
                           quantile.range <- ifelse(is.null(x$quantile.range),1,x$quantile.range)
                           if(all(sign(data)>=0)) {
                             if(is.null(colors)) {
                               colors <- colorRampPalette(c('gray90','red'), space = "Lab")(1024)
                             }
                             zlim <- as.numeric(quantile(data,p=c(1-quantile.range,quantile.range)))
                             if(diff(zlim)==0) {
                               zlim <- as.numeric(range(data))
                             }
                           } else {
                             if(is.null(colors)) {
                               colors <- colorRampPalette(c("blue", "grey90", "red"), space = "Lab")(1024)
                             }
                             zlim <- c(-1,1)*as.numeric(quantile(abs(data),p=gradient.range.quantile))
                             if(diff(zlim)==0) {
                               zlim <- c(-1,1)*as.numeric(max(abs(data)))
                             }
                           }
                         } else {
                           zlim <- x$zlim;
                         }
                         text <- x$text; if(!is.null(text)) { text <- text[self$results$hvc$order]; }
                         return(list(data=data,dim=dim,colors=col2hex(colors),zlim=zlim))
                       } else if(!is.null(x$legacy)) {
                         data <- col2hex(as.character(x$data[self$results$hvc$order]));
                         dim <- c(1,length(data));
                         return(list(data=data,dim=dim,legacy=TRUE))
                       } else { # treat as a factor
                         data <- as.integer(x$data)[self$results$hvc$order];
                         if(is.null(x$text)) { 
                          text <- as.character(x$data) 
                         } else { 
                          text <- x$text 
                         }
                         text <- text[self$results$hvc$order];
                         dim <- c(1,length(data));
                         colors <- x$colors;
                         if(is.null(colors)) { colors <- rainbow(length(levels(x$data))) }
                         return(list(data=data,dim=dim,colors=col2hex(colors),text=text,factor=is.factor(x$data)))
                       }
                     })

                     #colcols <- list(data = unlist(lapply(as.character(t(results$colcol[nrow(results$colcol):1, results$hvc$order, drop = FALSE])), col2hex)),
                     #                dim = dim(results$colcol),
                     #                rows=rev(rownames(results$colcol))
                     #)
                     ol <- list(matrix = matrix, rowcols = rowcols, colcols = colcols, coldend = treeg, trim = self$trim)
                     if(!is.null(self$embedding)) {
                       # report embedding, along with the position of each cell in the pathway matrix
                       edf <- data.frame(t(cbind(self$embedding,match(rownames(self$embedding),matrix$cols))))
                       rownames(df) <- NULL
                       ol$embedding <- list(data=edf,xrange=range(self$embedding[,1]),yrange=range(self$embedding[,2]),hasvelo=(!is.null(self$veloinfo) && !(class(self$veloinfo) == 'uninitializedField') && is.list(self$veloinfo)))
                     }

                     s <- toJSON(ol)

                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/getvel.json' = { # report cell velocities (grid and cell)
                     if(is.null(self$veloinfo)) return(NULL);
                     x <- self$veloinfo$proj$garrows;
                     y <- self$veloinfo$proj$arrows;
                     cellorder <- colnames(self$results$rcm)[self$results$hvc$order]
                     y <- y[match(cellorder,rownames(y)),]
                     
                     ol <- list(gvel=unname(split(x, 1:nrow(x))),cvel=unname(split(y, 1:nrow(y)))); # some gymnatics to work around rjson limitations
                     #ol <- list(gvel=data.frame(t(veloinfo$proj$garrow)))
                     s <- toJSON(ol)
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/velinfo.json' = {
                     celli <- gridi <- -1;
                     if(!is.null(req$params()$celli)) { celli <- as.integer(req$params()$celli)+1 }
                     if(!is.null(req$params()$gridi)) { gridi <- as.integer(req$params()$gridi)+1 }

                     #cat("celli=",celli," gridi=",gridi)
                     if(gridi>0) {
                       vel <- self$veloinfo$proj$gvel[,gridi]
                       esh <- self$veloinfo$proj$geshifts[,gridi]
                     } else if(celli>0) {
                       cell <- colnames(self$results$rcm)[self$results$hvc$order[celli]]
                       vel <- self$veloinfo$proj$vel[,cell]
                       esh <- self$veloinfo$proj$eshifts[,cell]
                     }
                     df <- data.frame(gene=rownames(self$veloinfo$proj$gvel),vel=vel,proj=esh)
                     vel <- vel/sqrt(sum(vel*vel))
                     esh <- esh/sqrt(sum(esh*esh))
                     vcos <- esh*vel*length(vel);
                     vcos <- sign(vcos)*sqrt(abs(vcos))

                     df$cos <- vcos
                     df <- df[is.finite(df$cos),]
                     df <- df[order(abs(df$cos),decreasing=TRUE),]
                     lgt <- df
                     
                     if(!is.null(req$params()$filter)) {
                       fl <- fromJSON(url_decode(req$params()$filter))
                       for( fil in fl) {
                         lgt <- lgt[grep(fil$value, lgt[, fil$property], perl = TRUE, ignore.case = TRUE), ]
                       }
                     }
                     start <- ifelse(is.null(req$params()$start), 1, as.integer(req$params()$start)+1)
                     limit <- ifelse(is.null(req$params()$limit), 1000, as.integer(req$params()$limit))
                     dir <- ifelse(is.null(req$params()$dir), "DESC", req$params()$dir)
                     trows <- nrow(lgt)
                     if(trows > 0) {
                       if(!is.null(req$params()$sort)) {
                         if(req$params()$sort %in% colnames(lgt)) {
                           lgt <- lgt[order(lgt[, req$params()$sort], decreasing = (dir == "DESC")), ]
                         }
                       } else { # default sort
                         # already done
                       }
                     }
                     lgt <- format(lgt[min(start, nrow(lgt)):min((start+limit), nrow(lgt)), ], nsmall = 2, digits = 2)
                     ol <- apply(lgt, 1, function(x) as.list(x))
                     names(ol) <- NULL
                     s <- toJSON(list(totalCount = trows, genes = ol))
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }

                   },
                   '/gvelfit.json' = {
                     gene <- fromJSON(url_decode(req$params()$gene))
                     if(is.null(gene)) { return(NULL) }
                     # report value vectors using the same order of cells as in the matrix
                     cellorder <- colnames(self$results$rcm)[self$results$hvc$order]
                     om <- match(cellorder,colnames(self$veloinfo$fit$conv.emat.norm));
                     #df <- data.frame(e=veloinfo$fit$conv.emat.norm[gene,om],n=veloinfo$fit$conv.nmat.norm[gene,om],r=0)
                     # working around lack of NA index support in Matrix
                     ev <- rep(NA,length(cellorder));
                     df <- data.frame(e=ev,n=ev,r=ev);
                     vi <- !is.na(om);
                     df[vi,] <- data.frame(e=self$veloinfo$fit$conv.emat.norm[gene,om[vi]],n=self$veloinfo$fit$conv.nmat.norm[gene,om[vi]],r=0)
                     
                     rownames(df) <- cellorder
                     
                     # quick quantile range
                     qrng <- function(x, gradient.range.quantile=0.95) {
                       if(all(sign(na.omit(x))>=0)) {
                         zlim <- as.numeric(quantile(na.omit(x),p=c(1-gradient.range.quantile,gradient.range.quantile),na.rm=TRUE))
                         if(diff(zlim)==0) {
                           zlim <- as.numeric(range(na.omit(x)))
                         }
                       } else {
                         zlim <- c(-1,1)*as.numeric(quantile(na.omit(abs(x)),p=gradient.range.quantile,na.rm=TRUE))
                         if(diff(zlim)==0) {
                           zlim <- c(-1,1)*as.numeric(na.omit(max(abs(x))))
                         }
                       }
                       zlim
                     }
                     #df <- data.frame(e=veloinfo$fit$conv.emat.norm[gene,],n=veloinfo$fit$conv.nmat.norm[gene,],r=0,embedding[match(colnames(veloinfo$fit$conv.emat.norm),rownames(embedding)),])
                     
                     # estimate residual
                     df$r=df$n- (df$e*self$veloinfo$fit$ko[gene,'g'] + self$veloinfo$fit$ko[gene,'o'])

                     full.rng <- apply(df,2,range,na.rm=TRUE);
                     full.rng <- full.rng+c(-1,1) %o% apply(full.rng,2,diff)*0.1/2

                     df[is.na(df)] <- 0;
                     
                     ol <- list(fit=df,rng=data.frame(apply(df,2,qrng)),fullrng=data.frame(full.rng),gamma=self$veloinfo$fit$ko[gene,'g'],offset=self$veloinfo$fit$ko[gene,'o'],gene=gene)

                     #ol <- list(fit=data.frame(t(df)),rng=data.frame(t(apply(df,2,range))),gamma=veloinfo$fit$ko[gene,'g'],offset=veloinfo$fit$ko[gene,'o'],gene=gene)
                     
                     s <- toJSON(ol)
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   
                   '/genecl.json' = { # report heatmap data for a selected set of genes
                     # Under Rstudio server, the URL decoding is not done automatically ..
                     #  .. here we're calling url_decode again for everything (it shouldn't have an effect on a properly formed list)
                     selgenes <- fromJSON(url_decode(req$POST()$genes))
                     ltrim <- ifelse(is.null(req$params()$trim), 0, as.numeric(req$params()$trim))
                     ol <- getgenecldata(selgenes, ltrim = ltrim)
                     s <- toJSON(ol)
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/pathwaygenes.json' = { # report heatmap data for a selected set of pathways
                     ngenes <- ifelse(is.null(req$params()$ngenes), 20, as.integer(req$params()$ngenes))
                     twosided <- ifelse(is.null(req$params()$twosided), FALSE, as.logical(req$params()$twosided))
                     ltrim <- ifelse(is.null(req$params()$trim), 0, as.numeric(req$params()$trim))
                     pws <- fromJSON(url_decode(req$POST()$genes))

                     n.pcs <- as.integer(gsub("^#PC(\\d+)# .*", "\\1", pws))
                     n.pcs[is.na(n.pcs)]<-1
                     x <- tp2c.view.pathways(gsub("^#PC\\d+# ", "", pws), self$results$p2, goenv = goenv, n.pc = n.pcs, n.genes = ngenes, two.sided = twosided, vhc = self$results$hvc, plot = FALSE, trim = ltrim, batch = self$batch)
                     ol <- getgenecldata(genes = NULL, gcl = x, ltrim = ltrim)
                     s <- toJSON(ol)

                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/patterngenes.json' = { # report heatmap of genes most closely matching a given pattern
                     # manual parse
                     x <- rawToChar(env[['rook.input']]$read())
                     x <- do.call(rbind,lapply(strsplit(url_decode(x),'&'),strsplit,'=')[[1]])
                     par <- as.list(x[,2]); names(par) <- x[,1];
                     ngenes <- ifelse(is.null(par$ngenes), 20, as.integer(par$ngenes))
                     twosided <- ifelse(is.null(par$twosided), FALSE, as.logical(par$twosided))
                     ltrim <- ifelse(is.null(par$trim), 0/nrow(self$results$p2$counts), as.numeric(par$trim))
                     pat <- fromJSON(par$pattern)
                     # reorder the pattern back according to column clustering
                     pat[self$results$hvc$order] <- pat
                     #patc <- matCorr(as.matrix(t(mat)), as.matrix(pat, ncol = 1))
                     patc <- smatColVecCorr(self$results$p2$counts,pat,FALSE)
                     if(twosided) { patc <- abs(patc) }
                     mgenes <- colnames(self$results$p2$counts)[order(as.numeric(patc), decreasing = TRUE)[1:ngenes]]
                     ol <- getgenecldata(mgenes, ltrim = ltrim)
                     ol$pattern <- pat
                     s <- toJSON(ol)
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(par$callback)) {
                       res$write(paste(par$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/diffexpressedgenes.json' = { # report heatmap of genes most closely matching a given pattern
                     #par <- req$POST();
                     # manual parse
                     x <- rawToChar(env[['rook.input']]$read())
                     x <- do.call(rbind,lapply(strsplit(url_decode(x),'&'),strsplit,'=')[[1]])
                     par <- as.list(x[,2]); names(par) <- x[,1];
                     
                     ngenes <- ifelse(is.null(par$ngenes), 20, as.integer(par$ngenes))
                     twosided <- ifelse(is.null(par$twosided), FALSE, as.logical(par$twosided))
                     ltrim <- ifelse(is.null(par$trim), 0, as.numeric(par$trim))
                     cells <- fromJSON(url_decode(par$cells))
                     ci <- rownames(self$results$p2$counts) %in% cells;
                     groups <- rep('other',length(ci)); groups[ci] <- 'group'; names(groups) <- rownames(self$results$p2$counts);
                     ds <- self$results$p2$getDifferentialGenes(type='PCA',groups=groups,upregulated.only=TRUE,verbose=FALSE)
                     mgenes <- rownames(ds[['group']])[1:ngenes]
                     ol <- getgenecldata(mgenes, ltrim = ltrim); #ol$sigdiff <- names(sigdiff);
                     s <- toJSON(ol)
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(par$callback)) {
                       res$write(paste(par$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/clinfo.json' = {
                     pathcl <- ifelse(is.null(req$params()$pathcl), 1, as.integer(req$params()$pathcl))
                     ii <- which(self$results$ct == pathcl)
                     tpi <- order(self$results$matvar[ii], decreasing = TRUE)
                     #tpi <- tpi[seq(1, min(length(tpi), 15))]
                     npc <- gsub("^#PC(\\d+)#.*", "\\1", names(ii[tpi]))
                     nams <- gsub("^#PC\\d+# ", "", names(ii[tpi]))
                     tpn <- paste(nams, unlist(lapply(BiocGenerics::mget(nams,GO.db::GOTERM,ifnotfound=NA),function(x) if(typeof(x)=="S4") { return(x@Term) }else { return("") } )),sep=" ")

                     lgt <- data.frame(do.call(rbind, lapply(seq_along(tpn), function(i) c(id = names(ii[tpi[i]]), name = tpn[i], npc = npc[i], od = as.numeric(self$results$matvar[ii[tpi[i]]])/max(self$results$matvar), sign = as.numeric(self$results$matrcmcor[ii[tpi[i]]]), initsel = as.integer(self$results$matvar[ii[tpi[i]]] >= self$results$matvar[ii[tpi[1]]]*0.8)))))

                     # process additional filters
                     if(!is.null(req$params()$filter)) {
                       fl <- fromJSON(url_decode(req$params()$filter))
                       for( fil in fl) {
                         lgt <- lgt[grep(fil$value, lgt[, fil$property], perl = TRUE, ignore.case = TRUE), ]
                       }
                     }
                     start <- ifelse(is.null(req$params()$start), 1, as.integer(req$params()$start)+1)
                     limit <- ifelse(is.null(req$params()$limit), 100, as.integer(req$params()$limit))
                     dir <- ifelse(is.null(req$params()$dir), "DESC", req$params()$dir)
                     trows <- nrow(lgt)
                     if(trows > 0) {
                       if(!is.null(req$params()$sort)) {
                         if(req$params()$sort %in% colnames(lgt)) {
                           lgt <- lgt[order(lgt[, req$params()$sort], decreasing = (dir == "DESC")), ]
                         }
                       }
                     }
                     lgt <- lgt[min(start, nrow(lgt)):min((start+limit), nrow(lgt)), ]
                     lgt$od <- format(lgt$od, nsmall = 2, digits = 2)
                     ol <- apply(lgt, 1, function(x) as.list(x))
                     names(ol) <- NULL
                     s <- toJSON(list(totalCount = trows, genes = ol))

                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/genes.json' = {
                     lgt <- self$genes
                     if(!is.null(req$params()$filter)) {
                       fl <- fromJSON(url_decode(req$params()$filter))
                       for( fil in fl) {
                         lgt <- lgt[grep(fil$value, lgt[, fil$property], perl = TRUE, ignore.case = TRUE), ]
                       }
                     }
                     start <- ifelse(is.null(req$params()$start), 1, as.integer(req$params()$start)+1)
                     limit <- ifelse(is.null(req$params()$limit), 1000, as.integer(req$params()$limit))
                     dir <- ifelse(is.null(req$params()$dir), "DESC", req$params()$dir)
                     trows <- nrow(lgt)
                     if(trows > 0) {
                       if(!is.null(req$params()$sort)) {
                         if(req$params()$sort %in% colnames(lgt)) {
                           lgt <- lgt[order(lgt[, req$params()$sort], decreasing = (dir == "DESC")), ]
                         }
                       } else { # default sort
                         # already done
                       }
                     }
                     lgt <- format(lgt[min(start, nrow(lgt)):min((start+limit), nrow(lgt)), ], nsmall = 2, digits = 2)
                     ol <- apply(lgt, 1, function(x) as.list(x))
                     names(ol) <- NULL
                     s <- toJSON(list(totalCount = trows, genes = ol))
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/pathways.json' = {
                     lgt <- self$pathways
                     if(!is.null(req$params()$filter)) {
                       fl <- fromJSON(url_decode(req$params()$filter))
                       for( fil in fl) {
                         lgt <- lgt[grep(fil$value, lgt[, fil$property], perl = TRUE, ignore.case = TRUE), ]
                       }
                     }
                     start <- ifelse(is.null(req$params()$start), 1, as.integer(req$params()$start)+1)
                     limit <- ifelse(is.null(req$params()$limit), 1000, as.integer(req$params()$limit))
                     dir <- ifelse(is.null(req$params()$dir), "DESC", req$params()$dir)
                     trows <- nrow(lgt)
                     if(trows > 0) {
                       if(!is.null(req$params()$sort)) {
                         if(req$params()$sort %in% colnames(lgt)) {
                           lgt <- lgt[order(lgt[, req$params()$sort], decreasing = (dir == "DESC")), ]
                         }
                       } else { # default sort
                         # already done
                       }
                     }
                     lgt <- format(lgt[min(start, nrow(lgt)):min((start+limit), nrow(lgt)), ], nsmall = 2, digits = 2)
                     ol <- apply(lgt, 1, function(x) as.list(x))
                     names(ol) <- NULL
                     s <- toJSON(list(totalCount = trows, genes = ol))
                     res$header('Content-Type', 'application/javascript')
                     if(!is.null(req$params()$callback)) {
                       res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                     } else {
                       res$write(s)
                     }
                   },
                   '/testenr.json' = { # run an enrichment test
                       selgenes <- fromJSON(url_decode(req$POST()$genes))
                       lgt <- calculate.go.enrichment(selgenes, colnames(self$results$p2$counts), pvalue.cutoff = 0.99, env = renv, over.only = TRUE)$over
                       lgt <- lgt[is.finite(lgt$Z),]
                       lgt$nam <- paste(lgt$t, unlist(lapply(BiocGenerics::mget(as.character(lgt$t),GO.db::GOTERM,ifnotfound=NA),function(x) if(typeof(x)=="S4") { return(x@Term) }else { return("") } )),sep=" ")
                       lgt <- data.frame(id = paste("#PC1#", lgt$t), name = lgt$nam, o = lgt$o, u = lgt$u, Z = lgt$Z, Za = lgt$Za, fe = lgt$fe, stringsAsFactors = FALSE)

                       if(!is.null(req$params()$filter)) {
                         fl <- fromJSON(url_decode(req$params()$filter))
                         for( fil in fl) {
                           lgt <- lgt[grep(fil$value, lgt[, fil$property], perl = TRUE, ignore.case = TRUE), ]
                         }
                       }
                       start <- ifelse(is.null(req$params()$start), 1, as.integer(req$params()$start)+1)
                       limit <- ifelse(is.null(req$params()$limit), 1000, as.integer(req$params()$limit))
                       dir <- ifelse(is.null(req$params()$dir), "DESC", req$params()$dir)
                       trows <- nrow(lgt)
                       if(trows > 0) {
                         if(!is.null(req$params()$sort)) {
                           if(req$params()$sort %in% colnames(lgt)) {
                             lgt <- lgt[order(lgt[, req$params()$sort], decreasing = (dir == "DESC")), ]
                           }
                         }
                       }
                       lgt <- format(lgt[min(start, nrow(lgt)):min((start+limit), nrow(lgt)), ], nsmall = 2, digits = 2)
                       ol <- apply(lgt, 1, function(x) as.list(x))
                       names(ol) <- NULL
                       s <- toJSON(list(totalCount = trows, genes = ol))
                       res$header('Content-Type', 'application/javascript')
                       if(!is.null(req$params()$callback)) {
                         res$write(paste(req$params()$callback, "(", s, ")", sep = ""))
                       } else {
                         res$write(s)
                       }

                   },
                   {
                     res$header('Location', 'index.html')
                     res$write('Redirecting to <a href = "index.html" > index.html</a >  for interactive browsing.')
                   }
                   )
            res$finish()
        }
    )
)

#' View pathway or gene-weighted PCA
#' 'Pagoda2' version of the function pagoda.show.pathways()
#' Takes in a list of pathways (or a list of genes), runs weighted PCA, optionally showing the result.
#'
#' @param pathways character vector of pathway or gene names
#' @param p2 'Pagoda2' object
#' @param goenv environment mapping pathways to genes (default=NULL)
#' @param batch factor (corresponding to rows of the model matrix) specifying batch assignment of each cell, to perform batch correction (default=NULL).
#' @param n.genes integer Number of genes to show (default=20)
#' @param two.sided boolean If TRUE, the set of shown genes should be split among highest and lowest loading (default=TRUE). If FALSE, genes with highest absolute loading should be shown.
#' @param n.pc integer vector Number of principal component to show for each listed pathway(default=rep(1, length(pathways)))
#' @param colcols column color matrix (default=NULL)
#' @param zlim numeric z color limit (default=NULL)
#' @param labRow row labels (default=NA)
#' @param vhc cell clustering (default=NULL)
#' @param cexCol positive numbers, used as cex.axis in for the row or column axis labeling(default=1)
#' @param cexRow positive numbers, used as cex.axis in for the row or column axis labeling(default=1)
#' @param nstarts integer Number of random starts to use (default=50)
#' @param row.order row order (default=NULL). If NULL, uses order from hclust.
#' @param show.Colv boolean Whether to show cell dendrogram (default=TRUE)
#' @param plot boolean Whether to plot (default=TRUE)
#' @param trim numeric Winsorization trim that should be applied (default=1.1/nrow(p2$counts)). Note that p2 is a 'Pagoda2' object.
#' @param showPC boolean (default=TRUE)
#' @param ... parameters to pass to my.heatmap2. Only if plot is TRUE.
#' @return cell scores along the first principal component of shown genes (returned as invisible)
#' @export tp2c.view.pathways
tp2c.view.pathways <- function(pathways, p2, goenv = NULL, batch = NULL, n.genes = 20, two.sided = TRUE, n.pc = rep(1, length(pathways)), 
  colcols = NULL, zlim = NULL, labRow = NA, vhc = NULL, cexCol = 1, cexRow = 1, nstarts = 50, row.order = NULL, show.Colv = TRUE, 
  plot = TRUE, trim = 1.1/nrow(p2$counts), showPC = TRUE,  ...) {

  # are these genes or pathways being passed?
  if(!is.null(goenv)) {
    x <- pathways %in% ls(goenv)
  } else {
    x <- rep(FALSE, length(pathways))
  }
  if(sum(x) > 0) { # some pathways matched
    if(!all(x)) {
      message("WARNING: partial match to pathway names. The following entries did not match: ", paste(pathways[!x], collapse = " "))
    }
    # look up genes for each pathway
    pathways <- pathways[x]
    p.genes <- mget(pathways, goenv, ifnotfound = NA)
  } else { # try as genes
    x <- pathways %in% colnames(p2$counts)
    if(sum(x) > 0) {
      if(!all(x)) {
        message("WARNING: partial match to gene names. The following entries did not match: ", paste(pathways[!x], collapse = " "))
      }
      p.genes <- list("genes" = pathways[x])
      pathways <- c("genes");
    } else { # neither genes nor pathways are passed
      stop("ERROR: provided names do not match either gene nor pathway names (if the pathway environment was provided)")
    }
  }
  gvi <- colnames(p2$counts) %in% unlist(p.genes)
  lab <- colnames(p2$counts)[gvi]


  if(length(lab) == 0){  return(NULL) }
  #if(length(lab)<3) { return(NULL) }

  d <- p2$counts[,gvi,drop=F]
  d <- t(t(d)*p2$misc[['varinfo']][colnames(d),'gsf'])
  if(trim > 0) {
    inplaceWinsorizeSparseCols(d,trim);
  }

  # simply calculate one PCA here
  if(length(lab) > 2) {
    cm <- Matrix::colMeans(d)
    xp <- irlba(d,nv=1,nu=0,center=cm)
    rownames(xp$v) <- colnames(d);
    xp$rotation <- xp$v;
    xp$scores <- as.matrix(t(t(d %*% xp$v) - as.numeric(t(cm %*% xp$v))))

    cs <- unlist(lapply(seq_len(ncol(xp$scores)), function(i) sign(cor(xp$scores[,i], Matrix::colMeans(t(d)*abs(xp$rotation[, i]))))))
    xp$scores <- t(t(xp$scores)*cs)
    xp$rotation <- t(t(xp$rotation)*cs)


    if(two.sided) {
      # positive
      vi <- which(xp$v[,1]>=0)
      if(length(vi)>0) {
        selected.genes.pos <- lab[vi[order(xp$v[vi],decreasing=TRUE)[1:min(round(n.genes/2),length(vi))]]]
      } else {
        selected.genes.pos <- c();
      }
      vi <- which(xp$v[,1]<0)
      if(length(vi)>0) {
        selected.genes.neg <- lab[vi[order(xp$v[vi],decreasing=FALSE)[1:min(round(n.genes/2),length(vi))]]]
      } else {
        selected.genes.neg <- c();
      }
      selected.genes <- unique(c(selected.genes.pos, selected.genes.neg))
    } else {
      selected.genes <- lab[order(abs(xp$v[,1]),decreasing=TRUE)[1:min(round(n.genes),nrow(xp$v))]]
    }
    d <- as.matrix(p2$counts[,selected.genes,drop=FALSE])
    d <- t(t(d)*p2$misc[['varinfo']][colnames(d),'gsf'])
  } else {
    xp <- list()
    d <- as.matrix(d);
  }
  d <- t(d);
  lab <- rownames(d)
  names(lab) <- lab;

  d <- d-rowMeans(d)
  dd <- as.dist(1-abs(cor(t(as.matrix(d)))))
  dd[is.na(dd)] <- 1
  if(is.null(row.order)) {
    if(length(lab) > 2) {
      hc <- fastcluster::hclust(dd, method = "ward.D")
      row.order <- hc$order
    } else {
      row.order <- c(seq_along(lab))
      if(length(lab)>1) {
        hc<-list()
        attributes(hc)<-list(members=length(lab),height=1);
        class(hc)<-"dendrogram";
        hc[[1]] <- list();
        attributes(hc[[1]]) <- list(members=1,height=0,label=lab[1],leaf=TRUE)
        hc[[2]] <- list();
        attributes(hc[[2]]) <- list(members=1,height=0,label=lab[2],leaf=TRUE)
      } else {
        hc <- list(); attributes(hc) <- list(members=1,height=0,label=lab[1],leaf=TRUE); class(hc) <- "dendrogram";
      }
    }
  } else {
    hc <- NULL;
  }

  if(is.null(vhc)) {
    vd <- as.dist(1-cor(as.matrix(d)))
    vd[is.na(vd)] <- 1
    vhc <- fastcluster::hclust(vd, method = "ward.D")
  }

  #if(is.null(zlim)) { zlim <- quantile(d, p = c(0.01, 0.99)) }
  if(is.null(zlim)) { zlim <- c(-1,1)*min(max(abs(d))/5,quantile(abs(d), p = c(0.99))) }
  vmap <- d
  vmap[vmap<zlim[1]] <- zlim[1]
  vmap[vmap > zlim[2]] <- zlim[2]
  rownames(vmap) <- rownames(d)

  if(!is.null(xp$scores)) {
    oc <- xp$scores[, 1]
    z <- rbind(colorRampPalette(c("darkgreen", "white", "darkorange"), space = "Lab")(100)[round(oc/max(abs(oc))*49)+50])
    ld <- xp$v[lab[row.order], 1]
    ld <- colorRampPalette(c("darkgreen", "white", "darkorange"), space = "Lab")(100)[round(ld/max(abs(ld))*49)+50]
  } else {
    oc <- ld <- z <- NULL;

  }

  if((!showPC) || length(lab)<= 1 || is.null(xp$scores)) {
    z <- NULL
  }


  col <- colorRampPalette(c("blue", "white", "red"), space = "Lab")(256)

  if(!is.null(colcols)) {
    if(is.null(z)) {
      z <- colcols;
    } else {
      z <- rbind(colcols, z)
    }
  }

  if (plot){
    if(show.Colv) {
      my.heatmap2(vmap[row.order, , drop = FALSE], Rowv = NA, Colv = as.dendrogram(vhc), zlim = zlim, col = col, scale = "none", RowSideColors = ld, ColSideColors = z, labRow = labRow, cexCol = cexCol, cexRow = cexRow, ...)
    } else {
      my.heatmap2(vmap[row.order, vhc$order, drop = FALSE], Rowv = NA, Colv = NA, zlim = zlim, col = col, scale = "none", RowSideColors = ld, ColSideColors = z[,vhc$order], labRow = labRow, cexCol = cexCol, cexRow = cexRow, ...)
    }
  }
  xp$vhc <- vhc
  xp$lab <- lab
  if(!is.null(hc)) {
    xp$hc <- as.dendrogram(hc)
  }
  xp$row.order <- row.order
  xp$oc <- oc
  xp$col <- col
  xp$oc.col <- colorRampPalette(c("darkgreen", "white", "darkorange"), space = "Lab")(256)
  xp$vmap <- vmap
  xp$zlim <- zlim

  #xp$consensus.pc <- consensus.npc
  invisible(xp)
}


# convert R color to a web hex representation
#' @keywords internal
col2hex <- function(col) {
    unlist(lapply(col, function(c) {
        c <- col2rgb(c)
        sprintf("#%02X%02X%02X", c[1], c[2], c[3])
    }))
}

#' @keywords internal
my.heatmap2 <- function(x, Rowv=NULL, Colv=if(symm)"Rowv" else NULL,
          distfun = dist, hclustfun = stats::hclust,
          reorderfun = function(d,w) reorder(d,w),
          add.expr, symm = FALSE, revC = identical(Colv, "Rowv"),
          scale = c("none","row", "column"), na.rm=TRUE,
          margins = c(5, 5),internal.margin=0.5, ColSideColors, RowSideColors,
          cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc),
          labRow = NULL, labCol = NULL, main = NULL, xlab = NULL, ylab = NULL,
          keep.dendro = FALSE,
          grid = FALSE, grid.col=1,grid.lwd=1,
          verbose = getOption("verbose"), Colv.vsize=0.15, Rowv.hsize=0.15, 
          ColSideColors.unit.vsize=0.02, RowSideColors.hsize=0.02, lasCol=2, 
          lasRow=2, respect=FALSE, box=FALSE, zlim=NULL, ...)
{
    scale <- if(symm && missing(scale)) "none" else match.arg(scale)
    if(length(di <- dim(x)) != 2 || !is.numeric(x)){
        stop("'x' must be a numeric matrix")
    }
    nr <- di[1]
    nc <- di[2]
    if(nr < 1 || nc <= 1){
        stop("'x' must have at least one row and 2 columns")
    }
    if(!is.numeric(margins) || length(margins) != 2){
        stop("'margins' must be a numeric vector of length 2")
    }

    if(is.null(zlim)) {
      zlim <- range(x[is.finite(x)])
    } else {
      x[x<zlim[1]] <- zlim[1]; x[x>zlim[2]] <- zlim[2];
    }

    doRdend <- !identical(Rowv,NA)
    doCdend <- !identical(Colv,NA)
    ## by default order by row/col means
    if(is.null(Rowv)) Rowv <- rowMeans(x, na.rm = na.rm)
    if(is.null(Colv)) Colv <- colMeans(x, na.rm = na.rm)

    ## get the dendrograms and reordering indices

    if(doRdend) {
        if(inherits(Rowv, "dendrogram"))
            ddr <- Rowv
        else {
            hcr <- hclustfun(distfun(x))
            ddr <- as.dendrogram(hcr)
            if(!is.logical(Rowv) || Rowv)
                ddr <- reorderfun(ddr, Rowv)
        }
        if(nr != length(rowInd <- order.dendrogram(ddr)))
            stop("row dendrogram ordering gave index of wrong length")
    }
    else rowInd <- 1:nr

    if(doCdend) {
        if(inherits(Colv, "dendrogram"))
            ddc <- Colv
        else if(identical(Colv, "Rowv")) {
            if(nr != nc)
                stop('Colv = "Rowv" but nrow(x) != ncol(x)')
            ddc <- ddr
        }
        else {
            hcc <- hclustfun(distfun(if(symm)x else t(x)))
            ddc <- as.dendrogram(hcc)
            if(!is.logical(Colv) || Colv)
                ddc <- reorderfun(ddc, Colv)
        }
        if(nc != length(colInd <- order.dendrogram(ddc)))
            stop("column dendrogram ordering gave index of wrong length")
    }
    else colInd <- 1:nc

    ## reorder x
    x <- x[rowInd, colInd, drop=FALSE]

    labRow <-
        if(is.null(labRow))
            if(is.null(rownames(x))) (1:nr)[rowInd] else rownames(x)
        else labRow[rowInd]
    labCol <-
        if(is.null(labCol))
            if(is.null(colnames(x))) (1:nc)[colInd] else colnames(x)
        else labCol[colInd]

    if(scale == "row") {
        x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
        sx <- apply(x, 1, sd, na.rm = na.rm)
        x <- sweep(x, 1, sx, "/")
    }
    else if(scale == "column") {
        x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
        sx <- apply(x, 2, sd, na.rm = na.rm)
        x <- sweep(x, 2, sx, "/")
    }

    ## Calculate the plot layout
    ds <- dev.size(units="cm");


    lmat <- rbind(c(NA, 3), 2:1)
    if(doRdend) {
      lwid <- c(if(is.character(Rowv.hsize)) Rowv.hsize else lcm(Rowv.hsize*ds[1]), 1)
    } else {
      lmat[2,1] <- NA; lmat[1,2] <- 2; lwid <- c(0, 1)
    }
    if(doCdend) {
      lhei <- c(if(is.character(Colv.vsize)) Colv.vsize else lcm(Colv.vsize*ds[2]), 1)
    } else {
      lmat[1,2] <- NA; lhei <- c(0,1);
    }
    #lwid <- c(if(doRdend) lcm(Rowv.hsize*ds[1]) else "0.5 cm", 1)
    #lhei <- c((if(doCdend) lcm(Colv.vsize*ds[2]) else "0.5 cm"), 1)
    if(!missing(ColSideColors) && !is.null(ColSideColors)) { ## add middle row to layout

      if(is.matrix(ColSideColors)) {
        if(ncol(ColSideColors)!=nc)
          stop("'ColSideColors' matrix must have the same number of columns as length ncol(x)")
        if(is.character(ColSideColors.unit.vsize)) {
          ww <- paste(as.numeric(gsub("(\\d+\\.?\\d*)(.*)","\\1",ColSideColors.unit.vsize,perl=TRUE))*nrow(ColSideColors),gsub("(\\d+\\.?\\d*)(.*)","\\2",ColSideColors.unit.vsize,perl=TRUE),sep="")
        } else {
          ww <- lcm(ColSideColors.unit.vsize*ds[2]*nrow(ColSideColors))
        }
        lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
        lhei <- c(lhei[1], ww, lhei[2])
      } else {
        if(!is.character(ColSideColors) || length(ColSideColors) != nc)
          stop("'ColSideColors' must be a character vector of length ncol(x)")
        if(is.character(ColSideColors.unit.vsize)) {
          ww <- paste(as.numeric(gsub("(\\d+\\.?\\d*)(.*)","\\1",ColSideColors.unit.vsize,perl=TRUE)),gsub("(\\d+\\.?\\d*)(.*)","\\2",ColSideColors.unit.vsize,perl=TRUE),sep="")
        } else {
          ww <- lcm(ColSideColors.unit.vsize*ds[2])
        }
        lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
        lhei <- c(lhei[1], ww, lhei[2])
      }
    }
    if(!missing(RowSideColors) && !is.null(RowSideColors)) { ## add middle column to layout
        if(!is.character(RowSideColors) || length(RowSideColors) != nr)
            stop("'RowSideColors' must be a character vector of length nrow(x)")
        lmat <- cbind(lmat[,1]+1, c(rep(NA, nrow(lmat)-1), 1), lmat[,2]+1)
        lwid <- c(lwid[1], if(is.character(RowSideColors.hsize)) RowSideColors.hsize else lcm(RowSideColors.hsize*ds[1]), lwid[2])
      }
    lmat[is.na(lmat)] <- 0
    if(verbose) {
        message("layout: widths = ", lwid, ", heights = ", lhei,"; lmat=\n")
        print(lmat)
    }

    ## Graphics `output' -----------------------

    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = respect)
    ## draw the side bars
    if(!missing(RowSideColors) && !is.null(RowSideColors)) {
        side_bars_par <- par(mar = c(margins[1],0, 0,internal.margin))
        on.exit(par(side_bars_par))
        image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
        if (box) { box() }
    }
    if(!missing(ColSideColors) && !is.null(ColSideColors)) {
        colsidepar <- par(mar = c(internal.margin,0, 0,margins[2]))
        on.exit(par(colsidepar))
        if(is.matrix(ColSideColors)) {
          image(t(matrix(1:length(ColSideColors),byrow=TRUE,nrow=nrow(ColSideColors),ncol=ncol(ColSideColors))), col = as.vector(t(ColSideColors[,colInd,drop=FALSE])), axes = FALSE)
          if(box) { box(); }
        } else {
          image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
          if (box) { box() }
        }
    }
    ## draw the main carpet
    main_carpet_par <- par(mar = c(margins[1], 0, 0, margins[2]))
    on.exit(par(main_carpet_par))
    if(!symm || scale != "none")
        x <- t(x)
    if(revC) { # x columns reversed
        iy <- nr:1
        ddr <- rev(ddr)
        x <- x[,iy,drop=FALSE]
    } else iy <- 1:nr

    image(1:nc, 1:nr, x, xlim = 0.5+ c(0, nc), ylim = 0.5+ c(0, nr),
          axes = FALSE, xlab = "", ylab = "", zlim=zlim, ...)
    if(box) { box(); }
    axis(1, 1:nc, labels= labCol, las= lasCol, line= -0.5, tick= 0, cex.axis= cexCol)
    if(!is.null(xlab)) mtext(xlab, side = 1, line = margins[1] - 1.25)
    axis(4, iy, labels= labRow, las= lasRow, line= -0.5, tick= 0, cex.axis= cexRow)
    if(!is.null(ylab)) mtext(ylab, side = 4, line = margins[2] - 1.25,las=lasRow)
    if (!missing(add.expr))
        eval(substitute(add.expr))


    if(grid) {
      abline(v=c(1:nc)-0.5,col=grid.col,lwd=grid.lwd)
      abline(h=c(1:nr)-0.5,col=grid.col,lwd=grid.lwd)
      box(col=grid.col,lwd=grid.lwd)
    }

    ## the two dendrograms :
    if(doRdend) {
      rdendpar <- par(mar = c(margins[1], 0, 0, 0))
      on.exit(par(rdendpar))
      plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none",xaxs="i")
    }

    if(doCdend) {
      cdendpar <- par(mar = c(internal.margin, 0, if(!is.null(main)) 1 else 0, margins[2]))
      on.exit(par(cdendpar))
      plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none",yaxs="i")
    }
    invisible(list(rowInd = rowInd, colInd = colInd,
                   Rowv = if(keep.dendro && doRdend) ddr,
                   Colv = if(keep.dendro && doCdend) ddc ))
}