#' Get a control geneset for cell scoring using the method described in
#' Puram, Bernstein (Cell, 2018)
#' 
#' @param data matrix of expression, rows are cell, columns are genes
#' @param signature character vector The signature to evaluate, a character vector of genes 
#' @param n.bins numeric Number of bins to put the genes in (default=25)
#' @param n.genes.per.bin numeric Number of genes to get from each bin (default=100)
#' @return a character vector that can be used as a background signature
#' @export get.control.geneset
get.control.geneset <- function(data, signature, n.bins=25, n.genes.per.bin=100) {
    ## DEVEL
    ## data <- vals
    ## signature <- zheng.treg.common.sign
    ## n.bins <- 25
    ## n.genes.per.bin <- 20
    ## DEVEL END
    ## Filter the signature
    signature <- subsetSignatureToData(data, signature)
    ## Get gene bins by expression
    gene.mean.expr <- apply(data, 2, mean)
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
    return(background.signature)
}

#' Puram, Bernstein (Cell, 2018)
#' Score cells as described in Puram, Bernstein (Cell, 2018)
#' 
#' @param data matrix of expression, rows are cell, columns are genes
#' @param signature character vector The signature to evaluate, a character vector of genes 
#' @param correct boolean Perform background correction by getting a semi-random geneset (default=TRUE)
#' @param show.plot boolean If corrected values are calculated show plot of corrected vs original scores (default=FALSE)
#' @param ... options for get.control.geneset()
#' @return a score for each cell
#' @export score.cells.puram
score.cells.puram <- function(data, signature, correct=TRUE, show.plot=FALSE, ...) {
    ## DEVEL
    ## data <- vals
    ## signature <- zheng.treg.common.sign
    ##
    ## Filter the signature
    signature <- subsetSignatureToData(data, signature)
    ## Calculate cell scores
    cell.score <- apply(data[,signature], 1, mean)
    ret.vals <- cell.score
    ## Perform correction against pseudo randomized
    ## background
    if (correct) {
        control.gene.set <- get.control.geneset(data, signature)
        bg.score <- apply(data[,control.gene.set], 1, mean)
        cell.score.corrected <- cell.score - bg.score
        ## Plot corrected vs original cell scores
        if (show.plot) {
            plot(cell.score,cell.score.corrected, main='Original vs. Corrected Cell Score')
        }
        ret.vals <- cell.score.corrected
    }
    return(ret.vals)
}

#' Plot the embedding of a 'Pagoda2' object with the given values
#' 
#' @param p2obj the 'Pagoda2' object
#' @param values the values to plot, fed into p2obj$plotEmbedding(colors=values)
#' @param title character Title for the plot (default="")
#' @param type character Type reduction on which the embedding is based on (default="PCA")
#' @param embeddingType character Type of embedding to plot (default="tSNE")
#' @return NULL, simply updates p2obj$plotEmbedding()
#' @export 
plotOneWithValues <- function (p2obj, values, title = "", type = 'PCA', embeddingType = 'tSNE') {
    p2obj$plotEmbedding(type = type, embeddingType = embeddingType, colors = values, alpha = 0.2, do.par = FALSE)
    legend(x = "topleft", bty = "n", legend = title)
    invisible(NULL)
}

#' Subset a gene signature to the genes in the given matrix
#' with optional warning if genes are missing
#' 
#' @param data matrix
#' @param signature character vector The gene signature from which to subset a character vector of genes 
#' @param raise.warning boolean Warn if genes are missing (default=TRUE)
#' @return The filtered subset of gene signatures
#' @export 
subsetSignatureToData <- function(data, signature, raise.warning=TRUE) {
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
#' 
#' @param data matrix of expression, rows are cell, columns are genes
#' @param signature a character vector of genes to use in the signature
#' @param quantile.cutoff numeric The quantile extremes to trim before plotting (default=0.0.1)
#' @return The filtered subset of gene signatures
#' @keywords internal
score.cells.nb1 <- function(data,signature, quantile.cutoff=0.01) {
    ## DEVEL
    #data <- vals
    #signature <- zheng.treg.common.sign
    ##
    signature <- subsetSignatureToData(data, signature)
    ## Clip extreme values
    data.clip <- apply(data[,signature],2,function(x) {
        qs <- quantile(x, c(quantile.cutoff, 1- quantile.cutoff))
        x[x < qs[1]] <- qs[1]
        x[x > qs[2]] <- qs[2]
        x
    })
    ## Scale
    data.scaled <- scale(data.clip, center=TRUE, scale=TRUE)
    ## Get mean for each cell
    scores <- apply(data.scaled, 1, mean)
    ## return
    return(scores)
}

#' Score cells by getting mean expression of genes in signatures
#' 
#' @param data matrix
#' @param signature the genes in the signature
#' @return cell scores
#' @export score.cells.nb0
score.cells.nb0 <- function(data, signature) {
    signature <- subsetSignatureToData(data,signature)
    scores <- apply(data,1,mean)
    return(scores)
}

#' Scale the designated values between the range of 0 and 1
#' 
#' @param x values to scale
#' @return the scaled values
#' @examples
#' example_matrix =  matrix(rep(c(1:5), 3), 5)
#' minMaxScale(example_matrix)
#'
#' @export
minMaxScale <- function(x) { 
    (x - min(x)) / (max(x) - min(x)) 
}

