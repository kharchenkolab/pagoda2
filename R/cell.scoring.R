
#' Get a control geneset for cell scoring using the method described in
#' Puram, Bernstein (Cell, 2018)
#' @param data matrix of expression, rows are cell, columns are genes
#' @param signature a character vector of genes to use in the signature
#' @param n.bins number of bins to put the genes in
#' @param n.genes.per.bin number of genes to get from each bin
#' @return a character vector that can be used as a background signature
#' @export get.control.geneset
get.control.geneset <- function(data, signature, n.bins = 25, n.genes.per.bin = 100) {
    ## DEVEL
    ## data <- vals
    ## signature <- zheng.treg.common.sign
    ## n.bins <- 25
    ## n.genes.per.bin <- 20
    ## DEVEL END
    ## Filter the signature
    signature <- subset.signature.to.data(data, signature)
    ## Get gene bins by expression
    gene.mean.expr <- apply(vals, 2, mean)
    gene.bins <- factor(ceiling(seq_along(gene.mean.expr) / n.bins))
    names(gene.bins) <- names(sort(gene.mean.expr))
    ## Generate a background signature
    background.signature <- unlist(lapply(signature, function(g) {
        ## Bin of this gene
        gb <- gene.bins[g]
        genes.in.bin <- names(gene.bins[gene.bins == gb])
        ## Sample gene number
        sgn <- min(n.genes.per.bin, length(genes.in.bin))
        sample(genes.in.bin, sgn)
    }))
    ## return the background signature
    background.signature <- unique(background.signature)
    background.signature
}

#' Puram, Bernstein (Cell, 2018)
#' Score cells as described in Puram, Bernstein (Cell, 2018)
#' @param data matrix of expression, rows are cell, columns are genes
#' @param signature the signature to evaluate
#' @param correct logical, perform background correction by getting a semi-random geneset
#' @param show.plot logical, if corrected values are calculated show plot of corrected vs original scores
#' @param ... options for get.control.geneset()
#' @return a score for each cell
#' @export score.cells.puram
score.cells.puram <- function(data, signature, correct = TRUE, show.plot=FALSE, ...) {
    ## DEVEL
    ## data <- vals
    ## signature <- zheng.treg.common.sign
    ##
    ## Filter the signature
    signature <- subset.signature.to.data(data, signature)
    ## Calculate cell scores
    cell.score <- apply(data[,signature], 1, mean)
    ret.vals <- cell.score
    ## Perform correction against pseudo randomized
    ## background
    if (correct) {
        control.gene.set <- get.control.geneset(data, signature);
        bg.score <- apply(data[,control.gene.set], 1, mean)
        cell.score.corrected <- cell.score - bg.score;
        ## Plot corrected vs original cell scores
        if (show.plot) {
            plot(cell.score,cell.score.corrected, main='Original Vs Corrected Cell Score')
        }
        ret.vals <- cell.score.corrected
    }
    ret.vals
}

#' Plot the embedding of a pagoda2 object with the given values
#' @param p2obj the pagoda2 object
#' @param values the values to plot
#' @param title title for the plot
#' @param type the type reduction on which the embedding is based on
#' @param embeddingType the type of embedding to plot
#' @return NULL
#' @export plotOneWithValues
plotOneWithValues <- function (p2obj, values, title = "", type = 'PCA', embeddingType = 'tSNE') 
{
    p2obj$plotEmbedding(type = type, embeddingType = embeddingType, colors = values, alpha = 0.2, do.par = F)
    legend(x = "topleft", bty = "n", legend = title)
    invisible(NULL)
}

#' Plot multiple pagoda2 objects with values, optionally saving to file
#' @param p2.objs list of pagoda2 objects to plot
#' @param values the values to plot
#' @param filename name of file to save the plot in, if NULL plot to current device
#' @param panel.size the size of the panel for saving to file
#' @param mark.cluster.cex size of cluster labels
#' @param verbose logical verbosity
#' @return NULL
#' @export plotAllWithValues
plotAllWithValues <- function (p2.objs, values, filename = NULL, panel.size = 600, 
    mark.cluster.cex = 0.8, verbose = F) 
{
    require(Cairo)
    mfrow = getParMfrow(length(p2.objs))
    if (!is.null(filename)) {
        CairoPNG(file = filename, height = mfrow[[1]] * panel.size, 
            width = mfrow[[2]] * panel.size)
    }
    par(mfrow = mfrow, mar = c(0.5, 0.5, 0.5, 0.5), mgp = c(2, 0.65, 0), cex = 0.85)
    ## Plot all individually
    lapply(names(p2.objs), function(dn) {
        plotOneWithSignature(p2.objs[[dn]], genes = genes, title = dn)
    })
    if (!is.null(filename)) {
        dev.off()
    }
    invisible(NULL)
}

#' Subset a gene singature to the genes in the given matrix
#' optionally warning if genes are missing
#' @param data the matrix
#' @param signature the signature to subset
#' @param raise.warning logical, warn if genes are missing (default: TRUE)
#' @export subset.signature.to.data
subset.signature.to.data <- function(data, signature, raise.warning = TRUE) {
    keep.genes <- signature %in% colnames(data)
    ## Check if all genes found
    if (sum(keep.genes) != length(keep.genes) & raise.warning) {
        ratio.genes.found <- sum(keep.genes) / length(keep.genes)
        warning(paste0('Only ', formatC(ratio.genes.found * 100, format='d'),
                       '% of the genes in the signature were found in the dataset'));
    }
    ## Filter the signature
    signature <- signature[keep.genes]
}

#' Score cells after standardising the expression of each gene removing outliers
#' @param data matrix of expression, rows are cell, columns are genes
#' @param signature a character vector of genes to use in the signature
#' @param quantile.cutoff the quantile extremes to trim before plotting
score.cells.nb1 <- function(data,signature, quantile.cutoff = 0.01) {
    ## DEVEL
    #data <- vals
    #signature <- zheng.treg.common.sign
    ##
    signature <- subset.signature.to.data(data,signature)
    ## Clip extreme values
    data.clip <- apply(data[,signature],2,function(x) {
        qs <- quantile(x, c(quantile.cutoff, 1- quantile.cutoff))
        x[x < qs[1]] <- qs[1]
        x[x > qs[2]] <- qs[2]
        x
    })
    ## Scale
    data.scaled <- scale(data.clip,center=TRUE,scale=TRUE)
    ## Get mean for each cell
    scores <- apply(data.scaled, 1, mean)
    ## return
    scores
}

#' Score cells by getting mean expression of genes in signtures
#' @param data the matrix
#' @param signature the genes in the signature
#' @return cell scores
#' @export score.cells.nb0
score.cells.nb0 <- function(data,signature) {
    signature <- subset.signature.to.data(data,signature)
    scores <- apply(data,1,mean)
    scores
}

#' Scale the designated values between the range of 0 and 1
#' @param x values to scale
#' @return the scaled values
#' @export min.max.scale
min.max.scale <- function(x) { (x - min(x)) / (max(x) - min(x)) }

