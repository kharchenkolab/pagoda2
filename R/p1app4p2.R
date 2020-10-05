#' @import Rook
#' @import rjson

#' @export p2.make.pagoda1.app
p2.make.pagoda1.app <- function(p2, col.cols = NULL, row.clustering = NULL, title = "pathway clustering", zlim = NULL,embedding=NULL,inner.clustering=TRUE,groups=NULL,clusterType=NULL,embeddingType=NULL,veloinfo=NULL,type='PCA', min.group.size=1, batch.colors=NULL,n.cores=10) {
  if (!requireNamespace("GO.db", quietly = TRUE)) {
    stop("Package \"GO.db\" needed for this function to work. Please install it with `BiocManager::install('GO.db')`.", call. = FALSE)
  }

  # rcm - xv
  if(type=='counts') {
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
      if(is.null(groups)) { stop("clustering ",clusterType," for type ", type," doesn't exist")}
    }
  }
  groups <- as.factor(groups[rownames(x)]);
  groups <- droplevels(groups);

  if(is.null(embedding)) {
    if(is.null(p2$embeddings[[type]])) { stop("first, generate embeddings for type ",type)}
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
  hc <- hclust(as.dist(d),method='ward.D')

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

  if(is.null(p2$misc[['pathwayOD']])) stop("pathwayOD missing, please run testPathwayOverdispersion()")
  tamr <- p2$misc[['pathwayOD']];
  env <- tamr$env; 
  if(is.null(zlim)) { zlim <- c(-1, 1)*quantile(tamr$xv, p = 0.95) }

  if(is.null(row.clustering) || is.null(row.clustering$order)) {
    row.clustering <- hclust(dist(tamr$xv))
  } else if(class(row.clustering)!="hclust") {
    # make a fake clustering to match the provided order
    or <- row.clustering$order;
    row.clustering <- hclust(dist(tamr$xv),method='single')
    names(or) <- as.character(-1*row.clustering$order)
    nmm <- -1*or[as.character(row.clustering$merge)]
    nmm[is.na(nmm)] <- as.character(row.clustering$merge)[is.na(nmm)]
    row.clustering$merge <- matrix(as.integer(nmm),ncol=ncol(row.clustering$merge))
    row.clustering$order <- as.integer(or);
  }


  if(!is.null(embedding)) {
    if(is.null(rownames(embedding))) { stop("provided 2D embedding lacks cell names") }
    vi <- rownames(embedding) %in% colnames(tamr$xv);
    if(!all(vi)) {
      warning("provided 2D embedding contains cells that are not in the tamr");
      embedding <- embedding[vi,];
      if(nrow(embedding)<2) {
        stop("provided 2D embedding contains too few cells after intersecting with the cell names in tamr");
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
      rn <- rownames(col.cols);
      col.cols <- lapply(1:nrow(col.cols),function(i) {
        list("data"=col.cols[i,],"legacy"=TRUE)
      })
      names(col.cols) <- rn;
      col.cols <- rev(col.cols);
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
    batch.colors <- fac2col(p2$batch,s=1.0,v=0.5,shuffle=F,level.colors=batch.colors,return.details=TRUE)
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
  if(is.element("GO.db",installed.packages()[,1])) {
    df$desc <- unlist(lapply(BiocGenerics::mget(df$name,GO.db::GOTERM,ifnotfound=NA),function(x) if(typeof(x)=="S4") { return(x@Term) }else { return("") } ))
  } else {
    df$desc <- ""
  }
  min.z <- -9
  df$z[df$z<min.z] <- min.z
  df$adj.z[df$adj.z<min.z] <- min.z
  df <- data.frame(id = paste("#PC", df$npc, "# ", df$name, sep = ""), npc = df$npc, n = df$n, score = df$score, Z = df$z, aZ = df$adj.z, sh.Z = 0, sh.aZ = 0, name = paste(df$name, df$desc))

  df <- df[order(df$score, decreasing = TRUE), ]

  # merge go.env
  sa <- p2ViewPagodaApp$new(results=fres, pathways=df, genes=gene.df, goenv=env, batch=NULL, name = title, trim = 0, embedding=embedding,type=type,veloinfo=veloinfo)

}

# modified PAGODA1 app for browsing p2 results
#' @export p2ViewPagodaApp
#' @exportClass p2ViewPagodaApp
p2ViewPagodaApp <- setRefClass(
    'p2ViewPagodaApp',
    fields = c('results', 'tam', 'genes', 'pathways', 'goenv', 'renv', 'name', 'trim', 'batch','embedding','type','veloinfo'),
    methods = list(
      initialize = function(results, ..., pathways, genes, goenv, batch = NULL, name = "pathway overdispersion", trim = 1.1/nrow(p2$counts), embedding=NULL,type,veloinfo=NULL) {
        if(!missing(results) && class(results)=='p2ViewPagodaApp') { # copy constructor
          callSuper(results);
        } else {
          callSuper();
          results <<- results
          type <<- type;
          #results$tvc$order <<- rev(results$tvc$order);
          results$tvc$labels <<- as.character(1:nrow(results$rcm));
          rownames(results$rcm) <<- as.character(1:nrow(results$rcm));
          genes <<- genes
          genes$svar <<- genes$var/max(genes$var)
          genes <<- genes
          batch <<- results$p2$batch
          pathways <<- pathways
          name <<- name
          trim <<- trim
          embedding <<- embedding
          veloinfo <<- veloinfo;
          # reverse lookup environment
          xl <- as.list(goenv);
          gel <- tapply(rep(names(xl), unlist(lapply(xl, length))), unlist(xl), I)
          gel <- gel[nchar(names(gel)) > 0]
          renv <<- list2env(gel,parent=emptyenv());
          goenv <<- list2env(xl,parent=emptyenv());
          rm(xl,gel)
          gc()
        }
      },

      getgenecldata = function(genes = NULL, gcl = NULL, ltrim = 0) { # helper function to get the heatmap data for a given set of genes
        if(is.null(gcl)) {
          gcl <- t.p2c.view.pathways(genes,results$p2,goenv=goenv,vhc=results$hvc,plot=FALSE,trim=ltrim,n.genes=Inf)
          #gcl <- t.view.pathways(genes, mat = mat, matw = matw, env = goenv, vhc = results$hvc, plot = FALSE, trim = ltrim)
        }

        matrix <- gcl$vmap[rev(gcl$row.order), results$hvc$order, drop = FALSE]
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

          colcols <- matrix(gcl$oc[results$hvc$order], nrow = 1)
          colcols <- list(data = as.numeric(t(colcols)),
                          dim = dim(colcols),
                          colors = gcl$oc.col,
                          zlim = c(-1,1)*quantile(abs(colcols),p=c(0.9))
                          )
          ol <- c(ol, list(rowcols = rowcols, colcols = colcols))
        }
        ol
      },

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
                                     <title > ', name, '</title >
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
                     treeg <- list(merge=as.vector(t(results$hvc$merge)),height=results$hvc$height,order=results$hvc$plotting.order)

                     matrix <- results$rcm[rev(results$tvc$order), results$hvc$order]
                     matrix <- list(data = as.numeric(t(matrix)),
                                    dim = dim(matrix),
                                    rows = rownames(matrix),
                                    cols = colnames(matrix),
                                    colors = results$cols,
                                    zlim = as.numeric(results$zlim2)
                                    )

                     icols <- colorRampPalette(c("white", "black"), space = "Lab")(256)
                     rcmvar <- matrix(apply(results$rcm[rev(results$tvc$order), , drop = FALSE], 1, var), ncol = 1)
                     rowcols <- list(data = as.numeric(t(rcmvar)),
                                     dim = dim(rcmvar),
                                     colors = icols,
                                     zlim = c(0, max(rcmvar))
                                     )
                     # translate colcol structure into a uniform data/dim/colors/zlim (for gradients)/text
                     colcols <- lapply(results$colcol,function(x) {
                       # is it a numeric gradient?
                       if(is.numeric(x$data)) {
                         colors <- x$colors;
                         data <- as.numeric(x$data[results$hvc$order]);
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
                         text <- x$text; if(!is.null(text)) { text <- text[results$hvc$order]; }
                         return(list(data=data,dim=dim,colors=col2hex(colors),zlim=zlim))
                       } else if(!is.null(x$legacy)) {
                         data <- col2hex(as.character(x$data[results$hvc$order]));
                         dim <- c(1,length(data));
                         return(list(data=data,dim=dim,legacy=TRUE))
                       } else { # treat as a factor
                         data <- as.integer(x$data)[results$hvc$order];
                         if(is.null(x$text)) { text <- as.character(x$data) } else { text <- x$text }
                         text <- text[results$hvc$order];
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
                     ol <- list(matrix = matrix, rowcols = rowcols, colcols = colcols, coldend = treeg, trim = trim)
                     if(!is.null(embedding)) {
                       # report embedding, along with the position of each cell in the pathway matrix
                       edf <- data.frame(t(cbind(embedding,match(rownames(embedding),matrix$cols)))); rownames(df) <- NULL;
                       ol$embedding <- list(data=edf,xrange=range(embedding[,1]),yrange=range(embedding[,2]),hasvelo=(!is.null(veloinfo) && !(class(veloinfo) == 'uninitializedField') && is.list(veloinfo)));
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
                     if(is.null(veloinfo)) return(NULL);
                     x <- veloinfo$proj$garrows;
                     y <- veloinfo$proj$arrows;
                     cellorder <- colnames(results$rcm)[results$hvc$order]
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
                       vel <- veloinfo$proj$gvel[,gridi]
                       esh <- veloinfo$proj$geshifts[,gridi]
                     } else if(celli>0) {
                       cell <- colnames(results$rcm)[results$hvc$order[celli]]
                       vel <- veloinfo$proj$vel[,cell]
                       esh <- veloinfo$proj$eshifts[,cell]
                     }
                     df <- data.frame(gene=rownames(veloinfo$proj$gvel),vel=vel,proj=esh)
                     vel <- vel/sqrt(sum(vel*vel))
                     esh <- esh/sqrt(sum(esh*esh))
                     vcos <- esh*vel*length(vel);
                     vcos <- sign(vcos)*sqrt(abs(vcos))

                     df$cos <- vcos;
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
                     cellorder <- colnames(results$rcm)[results$hvc$order]
                     om <- match(cellorder,colnames(veloinfo$fit$conv.emat.norm));
                     #df <- data.frame(e=veloinfo$fit$conv.emat.norm[gene,om],n=veloinfo$fit$conv.nmat.norm[gene,om],r=0)
                     # working around lack of NA index support in Matrix
                     ev <- rep(NA,length(cellorder));
                     df <- data.frame(e=ev,n=ev,r=ev);
                     vi <- !is.na(om);
                     df[vi,] <- data.frame(e=veloinfo$fit$conv.emat.norm[gene,om[vi]],n=veloinfo$fit$conv.nmat.norm[gene,om[vi]],r=0)
                     
                     rownames(df) <- cellorder
                     
                     # quick quantile range
                     qrng <- function(x,gradient.range.quantile=0.95) {
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
                     df$r=df$n- (df$e*veloinfo$fit$ko[gene,'g'] + veloinfo$fit$ko[gene,'o'])

                     full.rng <- apply(df,2,range,na.rm=TRUE);
                     full.rng <- full.rng+c(-1,1) %o% apply(full.rng,2,diff)*0.1/2

                     df[is.na(df)] <- 0;
                     
                     ol <- list(fit=df,rng=data.frame(apply(df,2,qrng)),fullrng=data.frame(full.rng),gamma=veloinfo$fit$ko[gene,'g'],offset=veloinfo$fit$ko[gene,'o'],gene=gene)

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
                     x <- t.p2c.view.pathways(gsub("^#PC\\d+# ", "", pws), results$p2, goenv = goenv, n.pc = n.pcs, n.genes = ngenes, two.sided = twosided, vhc = results$hvc, plot = FALSE, trim = ltrim, batch = batch)
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
                     ltrim <- ifelse(is.null(par$trim), 0/nrow(results$p2$counts), as.numeric(par$trim))
                     pat <- fromJSON(par$pattern)
                     # reorder the pattern back according to column clustering
                     pat[results$hvc$order] <- pat
                     #patc <- matCorr(as.matrix(t(mat)), as.matrix(pat, ncol = 1))
                     patc <- smatColVecCorr(results$p2$counts,pat,FALSE)
                     if(twosided) { patc <- abs(patc) }
                     mgenes <- colnames(results$p2$counts)[order(as.numeric(patc), decreasing = TRUE)[1:ngenes]]
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
                     ci <- rownames(results$p2$counts) %in% cells;
                     groups <- rep('other',length(ci)); groups[ci] <- 'group'; names(groups) <- rownames(results$p2$counts);
                     ds <- results$p2$getDifferentialGenes(type='PCA',groups=groups,upregulated.only=TRUE,verbose=FALSE)
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
                     ii <- which(results$ct == pathcl)
                     tpi <- order(results$matvar[ii], decreasing = TRUE)
                     #tpi <- tpi[seq(1, min(length(tpi), 15))]
                     npc <- gsub("^#PC(\\d+)#.*", "\\1", names(ii[tpi]))
                     nams <- gsub("^#PC\\d+# ", "", names(ii[tpi]))
                     if(is.element("GO.db",installed.packages()[,1])) {
                       tpn <- paste(nams, unlist(lapply(BiocGenerics::mget(nams,GO.db::GOTERM,ifnotfound=NA),function(x) if(typeof(x)=="S4") { return(x@Term) }else { return("") } )),sep=" ")
                     } else {
                       tpn <- nams;
                     }

                     lgt <- data.frame(do.call(rbind, lapply(seq_along(tpn), function(i) c(id = names(ii[tpi[i]]), name = tpn[i], npc = npc[i], od = as.numeric(results$matvar[ii[tpi[i]]])/max(results$matvar), sign = as.numeric(results$matrcmcor[ii[tpi[i]]]), initsel = as.integer(results$matvar[ii[tpi[i]]] >= results$matvar[ii[tpi[1]]]*0.8)))))

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
                     lgt <- genes
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
                     lgt <- pathways
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
                       lgt <- calculate.go.enrichment(selgenes, colnames(results$p2$counts), pvalue.cutoff = 0.99, env = renv, over.only = TRUE)$over
                       lgt <- lgt[is.finite(lgt$Z),];
                       if(is.element("GO.db",installed.packages()[,1])) {
                         lgt$nam <- paste(lgt$t, unlist(lapply(BiocGenerics::mget(as.character(lgt$t),GO.db::GOTERM,ifnotfound=NA),function(x) if(typeof(x)=="S4") { return(x@Term) }else { return("") } )),sep=" ")
                       } else {
                         lgt$name <- lgt$t
                       }
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

#' @export t.p2c.view.pathways
t.p2c.view.pathways <- function(pathways, p2, goenv = NULL, batch = NULL, n.genes = 20, two.sided = TRUE, n.pc = rep(1, length(pathways)), colcols = NULL, zlim = NULL, labRow = NA, vhc = NULL, cexCol = 1, cexRow = 1, nstarts = 50, row.order = NULL, show.Colv = TRUE, plot = TRUE, trim = 1.1/nrow(p2$counts), showPC = TRUE,  ...) {
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
      if(is.element("fastcluster", installed.packages()[, 1])) {
        hc <- fastcluster::hclust(dd, method = "ward.D")
      } else {
        hc <- stats::hclust(dd, method = "ward.D")
      }
      row.order <- hc$order
    } else {
      row.order <- c(seq_along(lab))
      if(length(lab)>1) {
        hc<-list();
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
    if(is.element("fastcluster", installed.packages()[, 1])) {
      vhc <- fastcluster::hclust(vd, method = "ward.D")
    } else {
      vhc <- stats::hclust(vd, method = "ward.D")
    }
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

  if(plot) {
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
  return(invisible(xp))
}

# convert R color to a web hex representation
col2hex <- function(col) {
    unlist(lapply(col, function(c) {
        c <- col2rgb(c)
        sprintf("#%02X%02X%02X", c[1], c[2], c[3])
    }))
}



#' Utility function to generate a pagoda2 app from a conos object
#' 
#' @param conos Conos object
#' @param cdl list Optional list of raw matrices (so that gene merging doesn't have to be redone) (default=NULL)
#' @param metadata list Optional list of (named) metadata factors (default=NULL)
#' @param filename string Name of the *.bin file to seralize for the pagoda2 application if save=TRUE (default='conos_app.bin')
#' @param go.env GO environment for the organism of interest (default=NULL)
#' @param organism string Organism of interest, either 'hs' (Homo sapiens) or 'mm' (Mus musculus, i.e. mouse) (default='hs')
#' @param save boolean Save serialized *bin file specified in filename (default=TRUE)
#' @param n.cores integer Number of cores (default=2)
#' @param cell.subset string Cells to subset with the conos embedding conos$embedding. If NULL, uses all cells via rownames(conos$embedding) (default-NULL)
#' @param max.cells numeric Limit to the cells that are included in the conos. If Inf, there is no limit (default=Inf)
#' @param additional.embeddings list Additional embeddings to add to conos for the pagoda2 app (default=NULL)
#' @param test.pathway.overdispersion boolean Find all IDs using GO category against either org.Hs.eg.db or org.Mm.eg.db (default=TRUE)
#' @param return.details boolean If TRUE, return list of p2 application, pagoda2 object, list of raw matrices, and cell names. If FALSE, simply return pagoda2 app object. (default=FALSE)
#' @return pagoda2 app object
#' @export 
p2app4conos <- function(conos, cdl=NULL, metadata=NULL, filename='conos_app.bin', go.env=NULL, organism='hs', save=TRUE, n.cores=2, cell.subset=NULL, max.cells=Inf, additional.embeddings=NULL, test.pathway.overdispersion=TRUE, return.details=FALSE) {
  
  if(is.null(cdl)) {
    #cdl <- lapply(conos$samples,function(p) t(p$misc$rawCounts))
    cdl <- lapply(conos:::rawMatricesWithCommonGenes(conos),t)
  }
  samf <- lapply(conos$samples,function(x) rownames(x$counts))
  samf <- as.factor(setNames(rep(names(samf),unlist(lapply(samf,length))),unlist(samf)))

  if(!is.null(cell.subset)) {
    cell.subset <- intersect(cell.subset,rownames(conos$embedding))
  } else {
    cell.subset <- rownames(conos$embedding)  
  }
  
  samf <- droplevels(samf[names(samf) %in% cell.subset])
  cdl <- lapply(cdl,function(x) x[,colnames(x) %in% cell.subset,drop=F])
  
  # limit to the cells that are included in the conos
  vc <- unlist(lapply(cdl,colnames));
  if(length(vc)>max.cells) { # subsample
    cat("subsampling",length(vc),"cells down to",max.cells,'...');
    vc <- sample(vc,max.cells)
    cat('done\n');
  } 
  cdl <- lapply(cdl,function(d) d[,colnames(d) %in% intersect(vc,names(samf))])
  cm <- do.call(cbind,cdl);
  
   
  cp2 <- basicP2proc(cm,min.cells.per.gene=1,  nPcs=50, get.tsne=T, get.largevis=F, make.geneknn=T, n.cores=n.cores)
  
  if(test.pathway.overdispersion) {
    if(organism=='mm') { 
      suppressMessages(library(org.Mm.eg.db))
      # translate gene names to ids
      ids <- unlist(lapply(mget(colnames(cp2$counts),org.Mm.egALIAS2EG,ifnotfound=NA),function(x) x[1]))
      # reverse map
      rids <- names(ids); names(rids) <- ids;
      # list all the ids per GO category
      go.env <- list2env(eapply(org.Mm.egGO2ALLEGS,function(x) as.character(na.omit(rids[x]))))
    } else if(organism=='hs') {
      suppressMessages(library(org.Hs.eg.db))
      ids <- unlist(lapply(mget(colnames(cp2$counts),org.Hs.egALIAS2EG,ifnotfound=NA),function(x) x[1]))
      rids <- names(ids); names(rids) <- ids;
      # list all the ids per GO category
      go.env <- list2env(eapply(org.Hs.egGO2ALLEGS,function(x) as.character(na.omit(rids[x]))))
    } else { 
      stop("unknown organism")
    }
  }
    
  if(!is.null(go.env)) {
    #cp2$getHierarchicalDiffExpressionAspects(type='PCA',clusterName='community',z.threshold=3)
    # here we test for pathway overdispersion, but recursive diff expression, as shown above will work just as well
    cp2$testPathwayOverdispersion(go.env,verbose=T,correlation.distance.threshold=0.95,recalculate.pca=F,top.aspects=15)
    
    library(GO.db)
    termDescriptions <- Term(GOTERM[names(go.env)]); # saves a good minute or so compared to individual lookups
    sn <- function(x) { names(x) <- x; x}
    geneSets <- lapply(sn(names(go.env)),function(x) {
      list(properties=list(locked=T,genesetname=x,shortdescription=as.character(termDescriptions[x])),genes=c(go.env[[x]]))
    })
  } else {
    hdea <- cp2$getHierarchicalDiffExpressionAspects(type='PCA',z.threshold=3)
    geneSets <- hierDiffToGenesets(hdea);
  }
  

  # add all kinds of embeddings
  # various joint embeddding versions
  #cp2$embeddings$Conos <- list("All"=t(conO$embedding),"cochlea"=t(conC$embedding),"DRG"=t(conD$embedding),"Merged"=t(conO$embedding),"Refined"=t(con2$embedding))
  cn <- rownames(cp2$counts); # cell names
  cp2$embeddings$Conos <- list("All"=conos$embedding[cn,])
  # hack: add placeholder spaces, so that old p2 version picks up the new embeddings
  cp2$reductions$Conos <- list(); 

  cp2$embeddings$sample <- lapply(conos$samples,function(x) { em <- x$embeddings$PCA[[1]]; em[rownames(em) %in% cn,] }) # embeddings of the individual samples
  cp2$embeddings$sample <- cp2$embeddings$sample[!unlist(lapply(cp2$embeddings$sample,is.null))]
  if(length(cp2$embeddings$sample)>1) {
    cp2$reductions$sample <- list();
  } else {
    cp2$embeddings$sample <- NULL;
    
  }
  
  if(!is.null(additional.embeddings)) {
    cp2$embeddings$Other <- lapply(additional.embeddings, function(em) { em[rownames(em) %in% cn,] })
    cp2$reductions$Other <- list();
  }
  
  
  # additional metadata with different factors .. you probably want to include something like sample or tissue (or patient type)
  metadata <- c(metadata,list(sample=samf));
  metadata <- lapply(metadata, function(d) d[cn])
  meta <- lapply(conos:::sn(names(metadata)),function(n) p2.metadata.from.factor(droplevels(as.factor(metadata[[n]])),displayname=n))
  
  p2app <- make.p2.app(cp2, dendrogramCellGroups = as.factor(conos$clusters[[1]]$groups[cn]), additionalMetadata = meta, geneSets = geneSets,innerOrder='odPCA');
  
  # Optional showing of app
  #show.app(p2app, name='newPagoda',browse=F)
  # Save serialised web object, RDS app and session image
  if(save){
    p2app$serializeToStaticFast(binary.filename = filename,verbose=TRUE)
  } 
  if(return.details) {
    return(list(app=p2app,p2=cp2,cdl=cdl,cn=cn))
  } else {
    invisible(p2app)
  }
}

