my.heatmap2 <- function (x, Rowv=NULL, Colv=if(symm)"Rowv" else NULL,
          distfun = dist, hclustfun = hclust,
          reorderfun = function(d,w) reorder(d,w),
          add.expr, symm = FALSE, revC = identical(Colv, "Rowv"),
          scale = c("none","row", "column"), na.rm=TRUE,
          margins = c(5, 5),internal.margin=0.5, ColSideColors, RowSideColors,
          cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc),
          labRow = NULL, labCol = NULL, main = NULL, xlab = NULL, ylab = NULL,
          keep.dendro = FALSE,
          grid = FALSE, grid.col=1,grid.lwd=1,
          verbose = getOption("verbose"), Colv.vsize=0.15, Rowv.hsize=0.15, ColSideColors.unit.vsize=0.02, RowSideColors.hsize=0.02,lasCol=2, lasRow=2, respect=FALSE, box=FALSE, zlim=NULL, ...)
{
    scale <- if(symm && missing(scale)) "none" else match.arg(scale)
    if(length(di <- dim(x)) != 2 || !is.numeric(x))
        stop("'x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if(nr < 1 || nc <= 1)
        stop("'x' must have at least one row and 2 columns")
    if(!is.numeric(margins) || length(margins) != 2)
        stop("'margins' must be a numeric vector of length 2")

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
        cat("layout: widths = ", lwid, ", heights = ", lhei,"; lmat=\n")
        print(lmat)
    }

    ## Graphics `output' -----------------------

    op <- par(no.readonly = TRUE)
    #on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = respect)
    ## draw the side bars
    if(!missing(RowSideColors) && !is.null(RowSideColors)) {
        par(mar = c(margins[1],0, 0,internal.margin))
        image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
        if(box) { box(); }
    }
    if(!missing(ColSideColors) && !is.null(ColSideColors)) {
        par(mar = c(internal.margin,0, 0,margins[2]))
        if(is.matrix(ColSideColors)) {
          image(t(matrix(1:length(ColSideColors),byrow=TRUE,nrow=nrow(ColSideColors),ncol=ncol(ColSideColors))), col = as.vector(t(ColSideColors[,colInd,drop=FALSE])), axes = FALSE)
          if(box) { box(); }
        } else {
          image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
          if(box) { box(); }
        }
    }
    ## draw the main carpet
    par(mar = c(margins[1], 0, 0, margins[2]))
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
      par(mar = c(margins[1], 0, 0, 0))
      plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none",xaxs="i")
    }

    if(doCdend) {
      par(mar = c(internal.margin, 0, if(!is.null(main)) 1 else 0, margins[2]))
      plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none",yaxs="i")
    }
    invisible(list(rowInd = rowInd, colInd = colInd,
                   Rowv = if(keep.dendro && doRdend) ddr,
                   Colv = if(keep.dendro && doCdend) ddc ))
}
