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
    if(inherits(row.clustering, "hclust")) { row.clustering <- as.dendrogram(row.clustering) }
    if(inherits(cell.clustering, "hclust")) { cell.clustering <- as.dendrogram(cell.clustering) }
    if(show.row.var.colors) {
        if(is.null(row.cols)) {
            icols <- colorRampPalette(c("white", "black"), space = "Lab")(1024)[1023*(rcmvar/max(rcmvar))+1]
            row.cols <- cbind(var = icols)
        }
    }
    my.heatmap2(mat, Rowv = row.clustering, Colv = cell.clustering, zlim = zlim, RowSideColors = row.cols, ColSideColors = col.cols, col = cols, ...)
}


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