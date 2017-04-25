
## FIXME: need larger par("mar")[1] or [4] for longish labels !
## {probably don't change, just print a warning ..}
my.plot.dendrogram <-
    function (x, type = c("rectangle", "triangle"), center = FALSE,
	      edge.root = is.leaf(x) || !is.null(attr(x, "edgetext")),
	      nodePar = NULL, edgePar = list(),
	      leaflab = c("perpendicular", "textlike", "none"), dLeaf = NULL,
	      xlab = "", ylab = "", xaxt="n", yaxt="s",
	      horiz = FALSE, frame.plot = FALSE, ...)
{
    type <- match.arg(type)
    leaflab <- match.arg(leaflab)
    hgt <- attr(x, "height")
    if (edge.root && is.logical(edge.root))
	edge.root <- 0.0625 * if(is.leaf(x)) 1 else hgt
    mem.x <- .my.memberDend(x)
    yTop <- hgt + edge.root
    if(center) { x1 <- 0.5 ; x2 <- mem.x + 0.5 }
    else       { x1 <- 1   ; x2 <- mem.x }
    xlim <- c(x1 - 1/2, x2 + 1/2)
    ylim <- c(0, yTop)
    if (horiz) {## swap and reverse direction on `x':
	xl <- xlim; xlim <- rev(ylim); ylim <- xl
	tmp <- xaxt; xaxt <- yaxt; yaxt <- tmp
    }
    plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab, ylab = ylab,
	 xaxt = xaxt, yaxt = yaxt, frame.plot = frame.plot, ...)
    if(is.null(dLeaf))
        dLeaf <- .75*(if(horiz) strwidth("w") else strheight("x"))

    if (edge.root) {
### FIXME: the first edge + edgetext is drawn here, all others in plotNode()
### -----  maybe use trick with adding a single parent node to the top ?
	x0 <- my.plotNodeLimit(x1, x2, x, center)$x
	if (horiz)
	    segments(hgt, x0, yTop, x0)
	else segments(x0, hgt, x0, yTop)
	if (!is.null(et <- attr(x, "edgetext"))) {
	    my <- mean(hgt, yTop)
	    if (horiz)
		text(my, x0, et)
	    else text(x0, my, et)
	}
    }
    my.plotNode(x1, x2, x, type = type, center = center, leaflab = leaflab,
             dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar, horiz = horiz)
}

### the work horse: plot node (if pch) and lines to all children
my.plotNode <-
    function(x1, x2, subtree, type, center, leaflab, dLeaf,
	     nodePar, edgePar, horiz = FALSE)
{
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- my.plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x
    usrpar <- par("usr");

    ## handle node specific parameters in "nodePar":
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if(!hasP) nPar <- nodePar

    if(getOption("verbose")) {
	cat(if(inner)"inner node" else "leaf", ":")
	if(!is.null(nPar)) { cat(" with node pars\n"); str(nPar) }
	cat(if(inner)paste(" height", formatC(yTop),"; "),
	    "(x1,x2)= (",formatC(x1,wid=4),",",formatC(x2,wid=4),")",
	    "--> xTop=", formatC(xTop, wid=8),"\n", sep="")
    }

    Xtract <- function(nam, L, default, indx)
	rep(if(nam %in% names(L)) L[[nam]] else default,
	    length.out = indx)[indx]
    asTxt <- function(x) # to allow 'plotmath' labels:
	if(is.character(x) || is.expression(x) || is.null(x)) x else as.character(x)

    i <- if(inner || hasP) 1 else 2 # only 1 node specific par

    if(!is.null(nPar)) { ## draw this node
	pch <- Xtract("pch", nPar, default = 1:2,	 i)
	cex <- Xtract("cex", nPar, default = c(1,1),	 i)
	col <- Xtract("col", nPar, default = par("col"), i)
	bg <- Xtract("bg", nPar, default = par("bg"), i)
	points(if (horiz) cbind(yTop, xTop) else cbind(xTop, yTop),
	       pch = pch, bg = bg, col = col, cex = cex)
    }

    if(leaflab == "textlike")
        p.col <- Xtract("p.col", nPar, default = "white", i)
    lab.col <- Xtract("lab.col", nPar, default = par("col"), i)
    lab.cex <- Xtract("lab.cex", nPar, default = c(1,1), i)
    lab.font <- Xtract("lab.font", nPar, default = par("font"), i)
    if (is.leaf(subtree)) {
	## label leaf
	if (leaflab == "perpendicular") { # somewhat like plot.hclust
	    if(horiz) {
                X <- yTop + dLeaf * lab.cex
                Y <- xTop; srt <- 0; adj <- c(0, 0.5)
	    }
	    else {
                Y <- yTop - dLeaf * lab.cex
                X <- xTop; srt <- 90; adj <- 1
	    }
            nodeText <- asTxt(attr(subtree,"label"))
	    text(X, Y, nodeText, xpd = TRUE, srt = srt, adj = adj,
                 cex = lab.cex, col = lab.col, font = lab.font)
	}
    }
    else if (inner) {
	segmentsHV <- function(x0, y0, x1, y1) {
	    if (horiz)
		segments(y0, x0, y1, x1, col = col, lty = lty, lwd = lwd)
	    else segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
	}
	for (k in 1:length(subtree)) {
	    child <- subtree[[k]]
	    ## draw lines to the children and draw them recursively
	    yBot <- attr(child, "height")
	    if (getOption("verbose")) cat("ch.", k, "@ h=", yBot, "; ")
	    if (is.null(yBot))
		yBot <- 0
	    xBot <-
		if (center) mean(bx$limit[k:(k + 1)])
		else bx$limit[k] + .my.midDend(child)

	    hasE <- !is.null(ePar <- attr(child, "edgePar"))
	    if (!hasE)
		ePar <- edgePar
	    i <- if (!is.leaf(child) || hasE) 1 else 2
	    ## define line attributes for segmentsHV():
	    col <- Xtract("col", ePar, default = par("col"), i)
	    lty <- Xtract("lty", ePar, default = par("lty"), i)
	    lwd <- Xtract("lwd", ePar, default = par("lwd"), i)
	    if (type == "triangle") {
		segmentsHV(xTop, yTop, xBot, yBot)
	    }
	    else { # rectangle
		segmentsHV(xTop,yTop, xBot,yTop)# h
		segmentsHV(xBot,yTop, xBot,yBot)# v
	    }
	    vln <- NULL
	    if (is.leaf(child) && leaflab == "textlike") {
		nodeText <- asTxt(attr(child,"label"))
		if(getOption("verbose"))
		    cat('-- with "label"',format(nodeText))
		hln <- 0.6 * strwidth(nodeText, cex = lab.cex)/2
		vln <- 1.5 * strheight(nodeText, cex = lab.cex)/2
		rect(xBot - hln, yBot,
		     xBot + hln, yBot + 2 * vln, col = p.col)
		text(xBot, yBot + vln, nodeText, xpd = TRUE,
                     cex = lab.cex, col = lab.col, font = lab.font)
	    }
	    if (!is.null(attr(child, "edgetext"))) {
		edgeText <- asTxt(attr(child, "edgetext"))
		if(getOption("verbose"))
		    cat('-- with "edgetext"',format(edgeText))
		if (!is.null(vln)) {
		    mx <-
			if(type == "triangle")
			    (xTop+ xBot+ ((xTop - xBot)/(yTop - yBot)) * vln)/2
			else xBot
		    my <- (yTop + yBot + 2 * vln)/2
		}
		else {
		    mx <- if(type == "triangle") (xTop + xBot)/2 else xBot
		    my <- (yTop + yBot)/2
		}
		## Both for "triangle" and "rectangle" : Diamond + Text

                p.col <- Xtract("p.col", ePar, default = "white", i)
                p.border <- Xtract("p.border", ePar, default = par("fg"), i)
                ## edge label pars: defaults from the segments pars
                p.lwd <- Xtract("p.lwd", ePar, default = lwd, i)
                p.lty <- Xtract("p.lty", ePar, default = lty, i)
                t.col <- Xtract("t.col", ePar, default = col, i)
                t.cex <- Xtract("t.cex", ePar, default =  1,  i)
                t.font<- Xtract("t.font",ePar, default= par("font"), i)
                t.shift <- Xtract("t.shift", ePar, default =  0.01,  i)

		vlm <- strheight(c(edgeText,"h"), cex = t.cex)/2
		hlm <- strwidth (c(edgeText,"m"), cex = t.cex)/2
		hl3 <- c(hlm[1], hlm[1] + hlm[2], hlm[1])
                #polygon(mx+ c(-hl3, hl3), my + sum(vlm)*c(-1:1,1:-1),
                #        col = p.col, border= p.border, lty = p.lty, lwd = p.lwd)
		#text(mx, my, edgeText, cex = t.cex, col = t.col, font = t.font)
                if(horiz) {
                  text(my, mx+t.shift*abs(usrpar[3]-usrpar[4]), edgeText, cex = t.cex, col = t.col, font = t.font)
                } else {
                  text(mx+t.shift*abs(usrpar[2]-usrpar[1]), my, edgeText, cex = t.cex, col = t.col, font = t.font)
                }
	    }
	    my.plotNode(bx$limit[k], bx$limit[k + 1], subtree = child,
		     type, center, leaflab, dLeaf, nodePar, edgePar, horiz)
	}
    }
}

my.plotNodeLimit <- function(x1, x2, subtree, center)
{
    ## get the left borders limit[k] of all children k=1..K, and
    ## the handle point `x' for the edge connecting to the parent.
    inner <- !is.leaf(subtree) && x1 != x2
    if(inner) {
	K <- length(subtree)
	mTop <- .my.memberDend(subtree)
	limit <- integer(K)
	xx1 <- x1
	for(k in 1:K) {
	    m <- .my.memberDend(subtree[[k]])
	    ##if(is.null(m)) m <- 1
	    xx1 <- xx1 + (if(center) (x2-x1) * m/mTop else m)
	    limit[k] <- xx1
	}
	limit <- c(x1, limit)
    } else { ## leaf
	limit <- c(x1, x2)
    }
    mid <- attr(subtree, "midpoint")
    center <- center || (inner && !is.numeric(mid))
    x <- if(center) mean(c(x1,x2)) else x1 + (if(inner) mid else 0)
    list(x = x, limit = limit)
}

.my.memberDend <- function(x) {
    r <- attr(x,"x.member")
    if(is.null(r)) {
        r <- attr(x,"members")
        if(is.null(r)) r <- 1:1
    }
    r
}

.my.midDend <- function(x)
    if(is.null(mp <- attr(x, "midpoint"))) 0 else mp


## original Andy Liaw; modified RG, MM :
my.heatmap <- function (x, Rowv=NULL, Colv=if(symm)"Rowv" else NULL,
          distfun = dist, hclustfun = hclust,
          reorderfun = function(d,w) reorder(d,w),
          add.expr, symm = FALSE, revC = identical(Colv, "Rowv"),
          scale = c("none","row", "column"), na.rm=TRUE,
          margins = c(5, 5), ColSideColors, RowSideColors,
          cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc),
          labRow = NULL, labCol = NULL, main = NULL, xlab = NULL, ylab = NULL,
          keep.dendro = FALSE,
          grid = FALSE, grid.col=1,grid.lwd=1,
          verbose = getOption("verbose"), imageSize=4, imageVSize=imageSize,imageHSize=imageSize,lasCol=2, lasRow=2, respect=F, box=F, ...)
{
    scale <- if(symm && missing(scale)) "none" else match.arg(scale)
    if(length(di <- dim(x)) != 2 || !is.numeric(x))
        stop("'x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if(nr <= 1 || nc <= 1)
        stop("'x' must have at least 2 rows and 2 columns")
    if(!is.numeric(margins) || length(margins) != 2)
        stop("'margins' must be a numeric vector of length 2")

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
    x <- x[rowInd, colInd]

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
    lmat <- rbind(c(NA, 3), 2:1)
    lwid <- c(if(doRdend) 1 else 0.05, imageHSize)
    lhei <- c((if(doCdend) 1 else 0.05) + if(!is.null(main)) 0.2 else 0, imageVSize)
    if(!missing(ColSideColors)) { ## add middle row to layout
      if(is.matrix(ColSideColors)) {
        if(ncol(ColSideColors)!=nc) 
          stop("'ColSideColors' matrix must have the same number of columns as length ncol(x)")
        lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
        lhei <- c(lhei[1], 0.2*nrow(ColSideColors), lhei[2])
      } else {
        if(!is.character(ColSideColors) || length(ColSideColors) != nc)
          stop("'ColSideColors' must be a character vector of length ncol(x)")
        lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
        lhei <- c(lhei[1], 0.2, lhei[2])
      }
    }
    if(!missing(RowSideColors)) { ## add middle column to layout
        if(!is.character(RowSideColors) || length(RowSideColors) != nr)
            stop("'RowSideColors' must be a character vector of length nrow(x)")
        lmat <- cbind(lmat[,1]+1, c(rep(NA, nrow(lmat)-1), 1), lmat[,2]+1)
        lwid <- c(lwid[1], 0.2, lwid[2])
    }
    lmat[is.na(lmat)] <- 0
    if(verbose) {
        cat("layout: widths = ", lwid, ", heights = ", lhei,"; lmat=\n")
        print(lmat)
    }

    ## Graphics `output' -----------------------

    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = respect)
    ## draw the side bars
    if(!missing(RowSideColors)) {
        par(mar = c(margins[1],0, 0,0.5))
        image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
        if(box) { box(); }
    }
    if(!missing(ColSideColors)) {
        par(mar = c(0.5,0, 0,margins[2]))
        if(is.matrix(ColSideColors)) {
          image(t(matrix(1:length(ColSideColors),byrow=T,nrow=nrow(ColSideColors),ncol=ncol(ColSideColors))), col = as.vector(t(ColSideColors[,colInd])), axes = FALSE)
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
        x <- x[,iy]
    } else iy <- 1:nr

    image(1:nc, 1:nr, x, xlim = 0.5+ c(0, nc), ylim = 0.5+ c(0, nr),
          axes = FALSE, xlab = "", ylab = "", ...)
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
    par(mar = c(margins[1], 0, 0, 0))
    if(doRdend)
      my.plot.dendrogram(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    else frame()

    par(mar = c(0, 0, if(!is.null(main)) 1 else 0, margins[2]))
    if(doCdend)
      my.plot.dendrogram(ddc,               axes = FALSE, xaxs = "i", leaflab = "none")
    else if(!is.null(main)) frame()

    ## title
    if(!is.null(main)) title(main, cex.main = 1.5*op[["cex.main"]])

    invisible(list(rowInd = rowInd, colInd = colInd,
                   Rowv = if(keep.dendro && doRdend) ddr,
                   Colv = if(keep.dendro && doCdend) ddc ))
}



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
          verbose = getOption("verbose"), Colv.vsize=0.15, Rowv.hsize=0.15, ColSideColors.unit.vsize=0.02, RowSideColors.hsize=0.02,lasCol=2, lasRow=2, respect=F, box=F, zlim=NULL, ...)
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
    x <- x[rowInd, colInd,drop=F]

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
          ww <- paste(as.numeric(gsub("(\\d+\\.?\\d*)(.*)","\\1",ColSideColors.unit.vsize,perl=T))*nrow(ColSideColors),gsub("(\\d+\\.?\\d*)(.*)","\\2",ColSideColors.unit.vsize,perl=T),sep="")
        } else {
          ww <- lcm(ColSideColors.unit.vsize*ds[2]*nrow(ColSideColors))
        }
        lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
        lhei <- c(lhei[1], ww, lhei[2])
      } else {
        if(!is.character(ColSideColors) || length(ColSideColors) != nc)
          stop("'ColSideColors' must be a character vector of length ncol(x)")
        if(is.character(ColSideColors.unit.vsize)) {
          ww <- paste(as.numeric(gsub("(\\d+\\.?\\d*)(.*)","\\1",ColSideColors.unit.vsize,perl=T)),gsub("(\\d+\\.?\\d*)(.*)","\\2",ColSideColors.unit.vsize,perl=T),sep="")
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
          image(t(matrix(1:length(ColSideColors),byrow=T,nrow=nrow(ColSideColors),ncol=ncol(ColSideColors))), col = as.vector(t(ColSideColors[,colInd,drop=F])), axes = FALSE)
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
        x <- x[,iy,drop=F]
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
