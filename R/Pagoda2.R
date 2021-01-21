#' @useDynLib pagoda2
#' @import MASS
#' @import Matrix
#' @importFrom Rcpp evalCpp sourceCpp
#' @import Rook
#' @import igraph
#' @import sccore
#' @import R6
#' @import RMTstat 
#' @importFrom irlba irlba
#' @importFrom parallel mclapply
#' @importFrom magrittr %>%
#' @importFrom mgcv gam
#' @importFrom N2R Knn
#' @importFrom Rtsne Rtsne
#' @import drat
NULL


#' @title Pagoda2 R6 class
#' @description The class encompasses gene count matrices, providing methods for normalization, calculating embeddings, and differential expression.
#' @param type string Data type (default='counts'). Currently only 'counts' supported.
#' @param n.cores numeric Number of cores to use (default=1)
#' @param n.odgenes integer Number of overdispersed genes to retrieve (default=NULL). If NULL, will return all.
#' @param verbose boolean Whether to give verbose output (default=TRUE)
#' @param lib.sizes character vector of library sizes (default=NULL)
#' @param log.scale boolean If TRUE, scale counts by log() (default=TRUE)
#' @param min.cells.per.gene integer Minimum number of cells per gene, used to subset counts for coverage (default=0)
#' @param min.transcripts.per.cell integer Minimum number of transcripts per cells, used to subset counts for coverage (default=10)
#' @param keep.genes list of genes to keep in count matrix after filtering out by coverage but before normalization (default=NULL)
#' @param trim numeric Parameter used for winsorizing count data (default=round(min.cells.per.gene/2)). If value>0, will winsorize counts in normalized space in the hopes of getting a more stable depth estimates. If value<=0, ignored.
#' @param clusterType Optional cluster type to use as a group-defining factor (default=NULL)
#' @param groups factor named with cell names specifying the clusters of cells to be compared (one against all) (default=NULL). To compare two cell clusters against each other, simply pass a factor containing only two levels.
#' @param plot boolean Whether to output the plot (default=FALSE)
#'
#' @export Pagoda2
Pagoda2 <- R6::R6Class("Pagoda2", lock_objects=FALSE,
  public = list(
    #' @field counts Gene count matrix, normalized on total counts (default=NULL)
    counts = NULL,

    #' @field modelType string Model used to normalize count matrices. Only supported values are 'raw', 'plain', and 'linearObs'.
    #'     -- 'plain': Normalize by regressing out on the non-zero observations of each gene (default).
    #'     -- 'raw': Use the raw count matrices, without normalization. The expression matrix taken "as is" without normalization, although log.scale still applies. 
    #'     -- 'linearObs': Fit a linear model of pooled counts across all genes against depth. This approach isn't recommened, as the depth dependency is not completely normalized out.
    modelType = NULL,

    #' @field clusters Results of clustering (default=list())
    clusters = list(),

    #' @field graphs Graph representations of the dataset (default=list())
    graphs = list(),

    #' @field reductions Results of reductions, e.g. PCA (default=list())
    reductions = list(),

    #' @field embeddings Results of visualization algorithms, t-SNE or largeVis (default=list())
    embeddings = list(),

    #' @field diffgenes Lists of differentially expressed genes (default=list())
    diffgenes = list(),

    #' @field n.cores number of cores (default=1)
    n.cores = 1,

    #' @field misc list with additional info (default=list())
    misc = list(),
    
    #' @field batch Batch factor for the dataset (default=NULL)
    batch = NULL,

    #' @field genegraphs Slot to store graphical representations in gene space (i.e. gene kNN graphs) (default=list())
    genegraphs = list(),

    #' @field depth Number of molecules measured per cell (default=NULL)
    depth = NULL,

    #' @description Initialize Pagoda2 class
    #'
    #' @param x input count matrix
    #' @param modelType Model used to normalize count matrices (default='plain'). Only supported values are 'raw', 'plain', and 'linearObs'.
    #' @examples
    #' \donttest{ 
    #' ## Load pre-generated a dataset of 50 bone marrow cells as matrix
    #' cm <- readRDS(system.file("extdata", "sample_BM1_50.rds", package="pagoda2"))
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts, log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' }
    #'
    #' @return new Pagoda2 object 
    initialize=function(x, modelType='plain', ## batchNorm='glm',
                        n.cores=parallel::detectCores(logical=FALSE), verbose=TRUE,
                        min.cells.per.gene=0, trim=round(min.cells.per.gene/2), 
                        min.transcripts.per.cell=10,
                        lib.sizes=NULL, log.scale=TRUE, keep.genes=NULL) {

      if (!requireNamespace("p2data", quietly = TRUE)) {
        stop("Package \"p2data\" needed for the Pagoda2 class to work. This can be installed via a drat repository, using \"install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')\". Please read the details provided within the README at https://github.com/kharchenkolab/pagoda2.", call. = FALSE)
      }
      
      self$n.cores <- n.cores
      self$misc <-list(lib.sizes=lib.sizes, log.scale=log.scale, model.type=modelType, trim=trim)
      self$modelType = modelType

      ##if (!missing(x) && ('Pagoda2' %in% class(x))) { # copy constructor
      ##  super$initialize(x, ..., modelType=modelType, batchNorm=batchNorm, n.cores=n.cores)
      ##} else {
      ##  super$initialize(..., modelType=modelType, batchNorm=batchNorm, n.cores=n.cores,verbose=verbose)
      ##if (!missing(x) && is.null(counts)) { # interpret x as a countMatrix
      if ('matrix' %in% class(x)) {
        x <- as(Matrix(x, sparse=TRUE), "dgCMatrix")
      }
      if (!('dgCMatrix' %in% class(x))) {
        stop("x is not of class dgCMatrix or matrix")
      }
      #if(any(x@x < 0)) {
      #  stop("x contains negative values")
      #}
      self$setCountMatrix(x, min.cells.per.gene=min.cells.per.gene, trim=trim, 
                     min.transcripts.per.cell=min.transcripts.per.cell, lib.sizes=lib.sizes,
                     log.scale=log.scale, keep.genes=keep.genes, verbose=verbose)
      ##}
    },

    #' @description Provide the initial count matrix, and estimate deviance residual matrix (correcting for depth and batch)
    #'
    #' @param countMatrix input count matrix 
    #' @param depthScale numeric Scaling factor for normalizing counts (defaul=1e3). If 'plain', counts are scaled by counts = counts/as.numeric(depth/depthScale).
    #' @return normalized count matrix (or if modelTye='raw', the unnormalized count matrix)
    setCountMatrix=function(countMatrix, depthScale=1e3, min.cells.per.gene=0, 
                            trim=round(min.cells.per.gene/2), min.transcripts.per.cell=10, 
                            lib.sizes=NULL, log.scale=FALSE, keep.genes=NULL, verbose=TRUE) {
      # check names
      if (any(duplicated(rownames(countMatrix)))) {
        stop("Duplicate gene names are not allowed - please reduce")
      }
      if (any(duplicated(colnames(countMatrix)))) {
        stop("Duplicate cell names are not allowed - please reduce")
      }
      
      if (any(is.na(rownames(countMatrix)))) {
        stop("NA gene names are not allowed - please fix")
      }
      if (any(is.na(colnames(countMatrix)))) {
        stop("NA cell names are not allowed - please fix")
      }
      
      if (ncol(countMatrix)<3) { 
        stop("Too few cells remaining after min.count.per.cell filter applied - have you pre-filtered the count matrix to include only cells of a realistic size?") 
      }
      
      counts <- t(countMatrix)
      
      # Keep genes of sufficient coverage or genes that are in the keep.genes list
      counts <- counts[,diff(counts@p) >= min.cells.per.gene | colnames(counts) %in% keep.genes]

      # Save the filtered count matrix in misc$rawCounts
      self$misc[['rawCounts']] <- counts
      self$misc$depthScale <- depthScale

      if (self$modelType == 'raw') {
        return()
      }

      if (!is.null(self$batch)) {
        if (!all(colnames(countMatrix) %in% names(self$batch))) { 
          stop("The supplied batch vector doesn't contain all the cells in its names attribute")
        }
        colBatch <- as.factor(self$batch[colnames(countMatrix)])
        self$batch <- colBatch
      }

      if (!is.null(lib.sizes)) {
        if (!all(colnames(countMatrix) %in% names(lib.sizes))) { 
          stop("The supplied lib.sizes vector doesn't contain all the cells in its names attribute")
        }
        lib.sizes <- lib.sizes[colnames(countMatrix)]
        depth <- lib.sizes/mean(lib.sizes)*mean(Matrix::colSums(countMatrix))
      } else {
        depth <- Matrix::colSums(countMatrix)
      }
      
      cell.filt.mask <- (depth >= min.transcripts.per.cell)
      counts <- counts[cell.filt.mask,]
      depth <- depth[cell.filt.mask]
      
      if (any(depth == 0)) {
        stop("Cells with zero expression over all genes are not allowed")
      }

      if (verbose) message(nrow(counts)," cells, ",ncol(counts)," genes; normalizing ... ")

      # get normalized matrix
      if (self$modelType=='linearObs') { # this shouldn't work well, since the depth dependency is not completely normalized out

        # winsorize in normalized space first in hopes of getting a more stable depth estimate
        if (trim>0) {
          counts <- counts / as.numeric(depth)
          inplaceWinsorizeSparseCols(counts, trim, self$n.cores)
          counts <- counts*as.numeric(depth)
          if (is.null(lib.sizes)) {
            depth <- round(Matrix::rowSums(counts))
          }
        }

        ldepth <- log(depth)

        # rank cells, cut into n pieces
        n.depth.slices <- 20
        #depth.fac <- as.factor(floor(rank(depth)/(length(depth)+1)*n.depth.slices)+1); names(depth.fac) <- rownames(counts);
        depth.fac <- cut(cumsum(sort(depth)),breaks=seq(0,sum(depth),length.out=n.depth.slices))
        names(depth.fac) <- rownames(counts)
        depth.fac <- depth.fac[rank(depth)]
        # dataset-wide gene average
        gene.av <- (Matrix::colSums(counts)+n.depth.slices)/(sum(depth)+n.depth.slices)

        # pooled counts, df for all genes
        tc <- colSumByFac(counts, as.integer(depth.fac))[-1,,drop=FALSE]
        tc <- log(tc+1)- log(as.numeric(tapply(depth,depth.fac,sum))+1)
        md <- log(as.numeric(tapply(depth,depth.fac,mean)))
        # combined lm
        cm <- lm(tc ~ md)
        colnames(cm$coef) <- colnames(counts)
        # adjust counts
        # predict log(p) for each non-0 entry
        count.gene <- rep(1:counts@Dim[2], diff(counts@p))
        exp.x <- exp(log(gene.av)[count.gene] - cm$coef[1,count.gene] - ldepth[counts@i+1]*cm$coef[2,count.gene])
        counts@x <- as.numeric(counts@x*exp.x/(depth[counts@i+1]/depthScale)) # normalize by depth as well
        # perform a another round of trimming
        if (trim>0) {
          inplaceWinsorizeSparseCols(counts, trim, self$n.cores)
        }


        # regress out on non-0 observations of each gene
        #non0LogColLmS(counts,mx,ldepth)
      } else if (self$modelType=='plain') {
        if (verbose) message("Using plain model ")

        if (!is.null(self$batch)) {
          if (verbose) message("Batch ... ")

          # dataset-wide gene average
          gene.av <- (Matrix::colSums(counts)+length(levels(self$batch)))/(sum(depth)+length(levels(self$batch)))

          # pooled counts, df for all genes
          tc <- colSumByFac(counts,as.integer(self$batch))[-1,,drop=FALSE]
          tc <- t(log(tc+1)- log(as.numeric(tapply(depth,self$batch,sum))+1))
          bc <- exp(tc-log(gene.av))

          # adjust every non-0 entry
          count.gene <- rep(1:counts@Dim[2],diff(counts@p))
          
          counts@x <- as.numeric(counts@x/bc[cbind(count.gene,as.integer(self$batch)[counts@i+1])])
        }

        if (trim>0) {
          if (verbose) message("Winsorizing ... ")
          counts <- counts/as.numeric(depth)
          
          inplaceWinsorizeSparseCols(counts, trim, self$n.cores)
          counts <- counts*as.numeric(depth)
          
          if (is.null(lib.sizes)) {
            depth <- round(Matrix::rowSums(counts))
          }
        }

        counts <- counts/as.numeric(depth/depthScale)
      } else {
        stop('modelType ',self$modelType,' is not implemented')
      }
      if (log.scale) {
        if (verbose) message("log scale ... ")
        counts@x <- as.numeric(log(counts@x+1))
      }
      self$misc[['rescaled.mat']] <- NULL
      if (verbose) message("done.\n")

      self$counts <- counts
      self$depth <- depth
    },

    #' @description Adjust variance of the residual matrix, determine overdispersed sites
    #' This is done to normalize the extent to which genes with (very) different expression magnitudes will contribute to the downstream anlaysis.
    #'
    #' @param gam.k integer The k used for the generalized additive model 'v ~ s(m, k =gam.k)' (default=5). If gam.k<2, linear regression is used 'lm(v ~ m)'.
    #' @param alpha numeric The Type I error probability or the significance level (default=5e-2). This is the criterion used to measure statistical significance, i.e. if the p-value < alpha, then it is statistically significant.
    #' @param use.raw.variance (default=FALSE). If modelType='raw', then this conditional will be used as TRUE.
    #' @param use.unadjusted.pvals boolean Whether to use Benjamini-Hochberg adjusted p-values (default=FALSE).
    #' @param do.par boolean Whether to put multiple graphs into a signle plot with par() (default=TRUE)
    #' @param max.adjusted.variance numeric Maximum adjusted variance (default=1e3). The gene scale factor is defined as sqrt(pmax(min.adjusted.variance,pmin(max.adjusted.variance,df$qv))/exp(df$v))
    #' @param min.adjusted.variance numeric Minimum adjusted variance (default=1e-3). The gene scale factor is defined as sqrt(pmax(min.adjusted.variance,pmin(max.adjusted.variance,df$qv))/exp(df$v))
    #' @param cells character vector Subset of cells upon which to perform variance normalization with adjustVariance() (default=NULL)
    #' @param min.gene.cells integer Minimum number of genes per cells (default=0). This parameter is used to filter counts.
    #' @param persist boolean Whether to save results (default=TRUE, i.e. is.null(cells)).
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #'}
    #' 
    #' @return residual matrix with adjusted variance
    adjustVariance=function(gam.k=5, alpha=5e-2, plot=FALSE, use.raw.variance=FALSE, 
      use.unadjusted.pvals=FALSE, do.par=TRUE, max.adjusted.variance=1e3, min.adjusted.variance=1e-3, 
      cells=NULL, verbose=TRUE, min.gene.cells=0, persist=is.null(cells), n.cores = self$n.cores) {
      #persist <- is.null(cells) # persist results only if variance normalization is performed for all cells (not a subset)
      if (!is.null(cells)) { # translate cells into a rowSel boolean vector
        if (!(is.logical(cells) && length(cells)==nrow(self$counts))) {
          if (is.character(cells) || is.integer(cells)) {
            rowSel <- rep(FALSE, nrow(self$counts))
            names(rowSel) <- rownames(self$counts)
            rowSel[cells] <- TRUE
          } else {
            stop("Cells argument must be either a logical vector over rows of the count matrix (cells), a vector of cell names or cell integer ids (row numbers)")
          }
        }
      } else {
        rowSel <- NULL
      }

      if (verbose) message("calculating variance fit ...")
      df <- colMeanVarS(self$counts, rowSel, n.cores)

      if (use.raw.variance) { # use raw variance estimates without relative adjustments
        rownames(df) <- colnames(self$counts)
        vi <- which(is.finite(df$v) & df$nobs>=min.gene.cells)
        df$lp <- df$lpa <- log(df$v)
        df$qv <- df$v
        df$gsf <- 1 # no rescaling of variance
        ods <- order(df$v, decreasing=TRUE)
        if (length(ods)>1e3) { 
          ods <- ods[1:1e3] 
        }
        if (persist) {
          self$misc[['odgenes']] <- rownames(df)[ods]
        }
      } else {
        # gene-relative normalizaton 
        df$m <- log(df$m)
        df$v <- log(df$v)
        rownames(df) <- colnames(self$counts)
        vi <- which(is.finite(df$v) & df$nobs>=min.gene.cells)
        if (length(vi)<gam.k*1.5) { gam.k=1 } # too few genes
        if (gam.k<2) {
          if (verbose) message(" using lm ")
          m <- lm(v ~ m, data = df[vi,])
        } else {
          if (verbose) message(" using gam ")
          m <- mgcv::gam(as.formula(paste0('v ~ s(m, k = ',gam.k,')')), data = df[vi,])
        }
        df$res <- -Inf
        df$res[vi] <- resid(m,type='response')
        n.obs <- df$nobs #diff(counts@p)
        suppressWarnings(df$lp <- as.numeric(pf(exp(df$res),n.obs,n.obs,lower.tail=FALSE,log.p=TRUE)))
        df$lpa <- bh.adjust(df$lp,log=TRUE)
        n.cells <- nrow(self$counts)
        df$qv <- as.numeric(qchisq(df$lp, n.cells-1, lower.tail = FALSE, log.p=TRUE)/n.cells)

        if (use.unadjusted.pvals) {
          ods <- which(df$lp<log(alpha))
        } else {
          ods <- which(df$lpa<log(alpha))
        }
        
        if (persist) {
          self$misc[['odgenes']] <- rownames(df)[ods]
        }
        if (verbose) message(length(ods),' overdispersed genes ... ',length(ods) )

        df$gsf <- geneScaleFactors <- sqrt(pmax(min.adjusted.variance,pmin(max.adjusted.variance,df$qv))/exp(df$v))
        df$gsf[!is.finite(df$gsf)] <- 0
      }

      if (persist) {
        if (verbose) message('persisting ... ')
        self$misc[['varinfo']] <- df
      }

      # rescale mat variance
      ## if(rescale.mat) {
      ##   if(verbose) cat("rescaling signal matrix ... ")
      ##   #df$gsf <- geneScaleFactors <- sqrt(1/exp(df$v));
      ##   inplaceColMult(counts,geneScaleFactors,rowSel);  # normalize variance of each gene
      ##   #inplaceColMult(counts,rep(1/mean(Matrix::colSums(counts)),ncol(counts))); # normalize the column sums to be around 1
      ##   if(persist) misc[['rescaled.mat']] <- geneScaleFactors;
      ## }
      if (plot) {
        if (do.par) {
          adjvar_par <- par(mfrow=c(1,2), mar = c(3.5,3.5,2.0,0.5), mgp = c(2,0.65,0), cex = 1.0)
          on.exit(par(adjvar_par))
        }
        suppressWarnings(smoothScatter(log10(exp(1))*df$m, log10(exp(1))*df$v, main='', xlab='log10[ magnitude ]',ylab='log10[ variance ]'))
        vi <- which(is.finite(log10(exp(1))*df$v) & df$nobs>=min.gene.cells)
        grid <- seq(min(log10(exp(1))*df$m[vi]), max(log10(exp(1))*df$m[vi]), length.out=1000)
        ## re-calculate m
        if (gam.k < 2) {
          if (verbose) message(" using lm ")
          m <- lm(v ~ m, data = log10(exp(1))*df[vi,])
        } else {
          if (verbose) message(" using gam ")
          m <- mgcv::gam(as.formula(paste0('v ~ s(m, k = ',gam.k,')')), data = log10(exp(1))*df[vi,])
        }
        lines(grid,predict(m, newdata=data.frame(m=grid)), col="blue")
        if (length(ods)>0) {
          points(log10(exp(1))*df$m[ods], log10(exp(1))*df$v[ods], pch='.',col=2,cex=1)
        }
        suppressWarnings(smoothScatter(log10(exp(1))*df$m[vi], log10(exp(1))*df$qv[vi], xlab='log10[ magnitude ]',ylab='',main='adjusted'))
        abline(h=1,lty=2,col=8)
        if (is.finite(max.adjusted.variance)) { 
          abline(h=max.adjusted.variance, lty=2, col=1)
        }
        points(log10(exp(1))*df$m[ods], log10(exp(1))*df$qv[ods], col=2, pch='.')
      }
      if (verbose) message("done.")
      invisible(df)
    },

    #' @description Create k-nearest neighbor graph
    #' 
    #' @param k integer Number of k clusters for k-NN (default=30)
    #' @param nrand numeric Number of randomizations i.e. the gene sets (of the same size) to be evaluated in parallel with each gene set (default=1e3)
    #' @param type string Data type of the reduction (default='counts'). If type='counts', this will access the raw counts. Otherwise, 'type' must be name of the reductions.
    #' @param weight.type string 'cauchy', 'normal', 'constant', '1m' (default='1m')
    #' @param odgenes character vector Overdispersed genes to retrieve (default=NULL)
    #' @param distance string Distance metric used: 'cosine', 'L2', 'L1', 'cauchy', 'euclidean' (default='cosine')
    #' @param center boolean Whether to use centering when distance='cosine' (default=TRUE). The parameter is ignored otherwise.
    #' @param x counts or reduction to use (default=NULL). If NULL, uses counts. Otherwise, checks for the reduction in self$reductions[[type]] 
    #' @param p (default=NULL)
    #' @param var.scale boolean Apply scaling if using raw counts (default=TRUE). If type="counts", var.scale is TRUE by default.
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=300)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object   
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=20, center=FALSE, distance='L2')
    #' }
    #' 
    #' @return kNN graph, stored in self$graphs
    makeKnnGraph=function(k=30, nrand=1e3, type='counts', weight.type='1m',
      odgenes=NULL, n.cores=self$n.cores, distance='cosine', center=TRUE, 
      x=NULL, p=NULL, var.scale=(type == "counts"), verbose=TRUE) {
      ## convert "euclidean" to "L2"
      if (tolower(distance)=="euclidean"){
        distance <- "L2"
      }
      if (is.null(x)) {
        x.was.given <- FALSE
        if (type=='counts') {
          x <- self$counts
          # Scale Raw counts
        } else {
          if (type %in% names(self$reductions)) {
            x <- self$reductions[[type]]
          } else {
            stop('Specified reduction does not exist')
          }
        }
        
        if (var.scale) {
          x@x <- x@x*rep(self$misc[['varinfo']][colnames(x),'gsf'],diff(x@p))
        }

        if (!is.null(odgenes)) {
          if (!all(odgenes %in% rownames(x))) { warning("not all of the provided odgenes are present in the selected matrix")}
          if (verbose) message("using provided odgenes ... ")
          x <- x[,odgenes]
        }

      } else { # is.null(x)
        x.was.given <- TRUE
      }

      if (distance %in% c('cosine','angular')) {
        if (center) {
          x<- x - Matrix::rowMeans(x) # centering for cosine distance
        }
        xn <- N2R::Knn(as.matrix(x), k, nThreads=n.cores, verbose=verbose, indexType='angular')
      } else if (distance == "L2") {
        xn <- N2R::Knn(as.matrix(x), k, nThreads=n.cores, verbose=verbose, indexType='L2')
      } else {
        stop("Unknown distance measure specified. Currently supported: angular, L2")
      }
      colnames(xn) <- rownames(xn) <- rownames(x)

      #if(weight.type=='rank') {
      #  xn$r <-  unlist(lapply(diff(c(0,which(diff(xn$s)>0),nrow(xn))),function(x) seq(x,1)))
      #}
      #xn <- xn[!xn$s==xn$e,]
      diag(xn) <- 0
      xn <- Matrix::drop0(xn)

      #if(n.cores==1) { # for reproducibility, sort by node names
      #  if(verbose) cat("ordering neighbors for reproducibility ... ");
      #  xn <- xn[order(xn$s+xn$e),]
      #  if(verbose) cat("done\n");
      #}
      #df <- data.frame(from=rownames(x)[xn$s+1],to=rownames(x)[xn$e+1],weight=xn$d,stringsAsFactors=F)
      #if(weight.type=='rank') { df$rank <- xn$r }

      if (weight.type %in% c("cauchy", "normal") && ncol(x)>sqrt(nrand)) {
        # generate some random pair data for scaling
        if (distance=='cosine') {
          #rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=T),sample(colnames(x),nrand,replace=T)),1,function(z) if(z[1]==z[2]) {return(NA); } else {1-cor(x[,z[1]],x[,z[2]])}))
          rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA) } else {1-sum(x[,z[1]]*x[,z[2]])/sqrt(sum(x[,z[1]]^2)*sum(x[,z[2]]^2))}))
        ## we no longer support 'JS'
        ## } else if(distance=='JS') {
        ##   rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA); } else {jw.disR(x[,z[1]],x[,z[2]])}))
        } else if (distance=='L2') {
          rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA) } else {sqrt(sum((x[,z[1]]-x[,z[2]])^2))}))
        } else if (distance=='L1') {
          rd <- na.omit(apply(cbind(sample(colnames(x),nrand,replace=TRUE),sample(colnames(x),nrand,replace=TRUE)),1,function(z) if(z[1]==z[2]) {return(NA) } else {sum(abs(x[,z[1]]-x[,z[2]]))}))
        }
        suppressWarnings(rd.model <- MASS::fitdistr(rd,weight.type))
        if (weight.type=='cauchy') {
          xn@x <- 1/pcauchy(xn@x,location=rd.model$estimate['location'],scale=rd.model$estimate['scale'])-1
        } else {
          xn@x <- 1/pnorm(xn@x,mean=rd.model$estimate['mean'],sd=rd.model$estimate['sd'])-1
        }
      }
      xn@x <- pmax(0,xn@x)
      if (weight.type=='constant') { 
        xn@x <- 1
      }
      if (weight.type=='1m') { 
        xn@x <- pmax(0,1-xn@x) 
      }
      #if(weight.type=='rank') { xn@x <- sqrt(df$rank) }
      # make a weighted edge matrix for the largeVis as well
      sxn <- (xn+t(xn))/2
      g <- igraph::graph_from_adjacency_matrix(sxn,mode='undirected',weighted=TRUE)
      if (!x.was.given) {
        if (is.null(self$misc[['edgeMat']])) { self$misc[['edgeMat']] <- list() }
        self$misc[['edgeMat']][[type]] <- xn
        self$graphs[[type]] <- g
      }
      invisible(g)
    },

    #' @description Calculate clusters based on the kNN graph
    #' 
    #' @param method Method to use (default=igraph::multilevel.community). Accepted methods are either 'igraph::infomap.community' or 'igraph::multilevel.community'. 
    #'     If NULL, if the number of vertices of the graph is greater than or equal to 2000, 'igraph::multilevel.community' will be used. Otherwise, 'igraph::infomap.community' will be used.
    #' @param name string Name of the community structure calculated from 'method' (default='community')
    #' @param g Input graph (default=NULL). If NULL, access graph from self$graphs[[type]].
    #' @param min.cluster.size Minimum size of clusters (default=1). This parameter is primarily used to remove very small clusters.
    #' @param persist boolean Whether to save the clusters and community structure (default=TRUE)
    #' @param ... Additional parameters to pass to 'method'
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=900)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a KNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=20, center=FALSE, distance='L2')
    #' ## Call clusters based on KNN
    #' p2_object$getKnnClusters(method=infomap.community, type='counts')
    #' }
    #'
    #' @return the community structure calculated from 'method'
    getKnnClusters=function(type='counts',method=igraph::multilevel.community, name='community', 
      n.cores=self$n.cores, g=NULL, min.cluster.size=1, persist=TRUE, ...) {

      if (is.null(g)) {
        if (is.null(self$graphs[[type]])) { 
          stop("Call makeKnnGraph(type='",type,"', ...) first")
        }
        g <- self$graphs[[type]]
      }

      if (is.null(method)) {
        if (length(vcount(g))<2000) {
          method <- igraph::infomap.community
        } else {
          method <- igraph::multilevel.community
        }
      }

      # method <- igraph::multilevel.community; n.cores <- 20
      # n.subsamplings <- 10; cluster.stability.dilution <- 1.5; cluster.stability.fraction <- 0.9; subsampling.rate <- 0.8; metaclustering.method<- 'ward.D'
      # g <- r$graphs$PCA
      # x <- r$counts
      # cls <- method(g)

      #library(parallel)
      #x <- mclapply(1:5,function(z) method(g),mc.cores=20)

      cls <- method(g,...)
      cls.groups <- as.factor(membership(cls))

      # cleanup the clusters to remove very small ones
      if (min.cluster.size>1) {
        cn <- names(cls.groups)
        vg <- which(unlist(tapply(cls.groups,cls.groups,length))>=min.cluster.size)
        cls.groups <- as.integer(cls.groups)
        cls.groups[!cls.groups %in% vg] <- NA
        cls.groups <- as.factor(cls.groups)
        names(cls.groups) <- cn
      }
       
      if (persist) {
        self$clusters[[type]][[name]] <- cls.groups
        self$misc[['community']][[type]][[name]] <- cls
      }
      invisible(cls)
    },

    #' @description Deprecated function. Use makeGeneKnnGraph() instead.
    #' 
    #' @keywords internal
    geneKnnbyPCA = function() {
      ##warning('geneKnnbyPCA is deprecated use makeGeneKnnGraph() instead')
      .Deprecated("makeGeneKnnGraph()")
      self$makeGeneKnnGraph()
    },

    #' @description Take a given clustering and generate a hierarchical clustering
    #' 
    #' @param type string Data type of the reduction (default='counts'). If type='counts', this will access the raw counts. Otherwise, 'type' must be name of the reductions.
    #' @param groups factor named with cell names specifying the clusters of cells to be compared (one against all) (default=NULL). To compare two cell clusters against each other, simply pass a factor containing only two levels.
    #' @param clusterName string Cluster name to access (default=NULL)
    #' @param method string The agglomeration method to be used in stats::hcust(method=method) (default='ward.D'). Accepted values are: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). For more information, see stats::hclust().
    #' @param dist string 'pearson', 'spearman', 'euclidean', 'L2', 'JS' (default='pearson')
    #' @param persist boolean Whether to save the clusters and community structure (default=TRUE)
    #' @param z.threshold numeric Threshold of z-scores to filter, >=z.threshold are kept (default=2)
    #' @param min.set.size integer Minimum threshold of sets to keep (default=5)
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=400)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a KNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=20, type='PCA', center=FALSE, distance='cosine')
    #' ## Call clusters based on KNN
    #' p2_object$getKnnClusters(method=walktrap.community,type='PCA',name='walktrap')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', embeddingType = 'largeVis', M=30, perplexity=30, gamma=1/30)
    #' ## Perform differential expression
    #' p2_object$getDifferentialGenes(type='PCA', verbose=TRUE, clusterType='walktrap')
    #' ## Perform differential expression
    #' hdea <- p2_object$getHierarchicalDiffExpressionAspects(type='PCA', 
    #'             clusterName='walktrap', z.threshold=3)
    #' }
    #' 
    #' @return hierarchical clustering
    getHierarchicalDiffExpressionAspects = function(type='counts', groups=NULL, clusterName=NULL, method='ward.D',
      dist='pearson', persist=TRUE, z.threshold=2, n.cores=self$n.cores, min.set.size=5, verbose=TRUE ){
   
      if (tolower(dist)=="euclidean"){
        dist <- "L2"
      }

      if (type=='counts') {
        x <- self$counts
      } else {
        x <- self$reductions[[type]]
      }
      if (is.null(groups)) {
        # retrieve clustering
        if (is.null(self$clusters)) {
          stop("Please generate clusters first")
        }
        if (is.null(self$clusters[[type]])) {
          stop(paste("Please generate clusters for",type,"first"))
        }
        if (is.null(clusterName)) { 
          if (length(self$clusters[[type]])<1){
            stop(paste("Please generate clusters for",type,"first"))
          }
          ## use last-generated clustering
          cl <- self$clusters[[type]][[length(self$clusters[[type]])]]
        } else {
          cl <- self$clusters[[type]][[clusterName]]
          if (is.null(cl)) {
            stop(paste("Unable to find clustering",clusterName,'for',type))
          }
          if (verbose) message("Using ",clusterName," clustering for ",type," space\n")
        }
      } else {
        if (!all(rownames(x) %in% names(groups))) { warning("Provided cluster vector doesn't list groups for all of the cells")}
        cl <- groups
      }
      cl <- as.factor(cl[match(rownames(x),names(cl))])

      if (dist %in% c('pearson','spearman')) {
        rx <- do.call(cbind,tapply(1:nrow(x),cl,function(ii) {
            if (length(ii) > 1) {
                Matrix::colMeans(x[ii,])
            } else {
                x[ii,]
            }
        }))
        d <- as.dist(1-cor(rx,method=dist))
      } else if (dist=="L2") {
        rx <- do.call(rbind,tapply(1:nrow(x),cl,function(ii) {
            if (legnth(ii) > 1) {
                Matrix::colMeans(x[ii,])
            } else {
                x[ii,]
            }
        }))
        d <- dist(rx)
      } else if (dist=='JS') {
        # this one has to be done on counts, even if we're working with a reduction
        cl <- as.factor(cl[match(rownames(self$misc[['rawCounts']]),names(cl))])
        lvec <- colSumByFac(self$misc[['rawCounts']],as.integer(cl))[-1,] + 1
        d <- jsDist(t(lvec/pmax(1,Matrix::rowSums(lvec))))
        colnames(d) <- rownames(d) <- which(table(cl)>0)
        d <- as.dist(d)
      } else {
        stop("Unknown distance",dist,"requested")
      }

      dd <- as.dendrogram(stats::hclust(d, method=method))

      # walk down the dendrogram to generate diff. expression on every split
      diffcontrasts <- function(l,env) {
        v <- mget("contrasts",envir=env,ifnotfound=0)[[1]]
        if (!is.list(v)) v <- list()
        if (is.leaf(l)) return(NULL)
        lcl <- rep(NA,nrow(x))
        names(lcl) <- rownames(x)
        lcl[names(lcl) %in% names(cl)[cl %in% unlist(l[[1]])]] <- paste(unlist(l[[1]]),collapse='.')
        lcl[names(lcl) %in% names(cl)[cl %in% unlist(l[[2]])]] <- paste(unlist(l[[2]]),collapse='.')
        v <- c(v,list(as.factor(lcl)))
        assign("contrasts",v,envir=env)
        return(1)
      }

      de <- environment()
      assign('contrasts',NULL,envir=de)
      dc <- dendrapply(dd,diffcontrasts,env=de)
      dc <- get("contrasts",env=de)
      names(dc) <- unlist(lapply(dc,function(x) paste(levels(x),collapse=".vs.")))

      #dexp <- papply(dc,function(x) getDifferentialGenes(groups=x,z.threshold=z.threshold),n.cores=n.cores)

      x <- self$counts
      x@x <- x@x*rep(self$misc[['varinfo']][colnames(x),'gsf'],diff(x@p)) # apply variance scaling
      x <- t(x)
      dexp <- papply(dc,function(g) {
        dg <- self$getDifferentialGenes(groups=g, z.threshold=z.threshold)
        dg <- lapply(dg,function(x) x[x$Z>=z.threshold,])
        # calculate average profiles
        x <- x[rownames(x) %in% unlist(lapply(dg,rownames)),]
        if (nrow(x)<1) return(NULL)
        x <- x-rowMeans(x[,!is.na(g)])
        sf <- rep(1,nrow(x))
        names(sf) <- rownames(x)
        if (nrow(dg[[1]])>0) {
          ig <- which(names(sf) %in% rownames(dg[[1]]))
          sf[ig] <- 1/length(ig)
        }
        if (nrow(dg[[2]])>0) {
          ig <- which(names(sf) %in% rownames(dg[[2]]))
          sf[ig] <- -1/length(ig)
        }
        sf <- sf*sqrt(length(sf))
        pt <- colSums(x*sf)
        pt[is.na(g)] <- 0
        return(list(dg=dg,pt=pt))
      }, n.cores=n.cores)

      dexp <- dexp[!unlist(lapply(dexp,is.null))] # remove cases where nothing was reported
      dexp <- dexp[!unlist(lapply(dexp,function(x){class(x) == 'try-error'}))] ## remove cases that failed

      # fake pathwayOD output
      tamr <- list(xv=do.call(rbind,lapply(dexp,function(x) x$pt)),
                   cnam=lapply(sn(names(dexp)),function(n) c(n)))


      dgl <- lapply(dexp,function(d) as.character(unlist(lapply(d$dg,function(x) rownames(x)[x$Z>=z.threshold]))))
      tamr$env <- list2env(dgl[unlist(lapply(dgl,length))>=min.set.size])

      self$misc[['pathwayOD']] <- tamr

      # fake pathwayODInfo
      zs <- unlist(lapply(dexp,function(x) max(unlist(lapply(x$dg,function(y) y$Z)))))
      mval <- unlist(lapply(dexp,function(x) max(unlist(lapply(x$dg,function(y) y$M)))))
      vdf <- data.frame(i=1:nrow(tamr$xv),npc=1,valid=TRUE,sd=apply(tamr$xv,1,sd),cz=zs,z=zs,oe=mval,n=unlist(lapply(dexp,function(x) sum(unlist(lapply(x$dg,nrow))))))
      vdf$name <- rownames(vdf) <- names(dexp)
      self$misc[['pathwayODInfo']] <- vdf

      invisible(tamr)
    },

    #' @description Calculates gene Knn network for gene similarity
    #'
    #' @author Simon Steiger
    #' @param nPcs integer Number of principal components (default=100). This is the parameter 'nv' in irlba::irlba(), the number of right singular vectors to estimate.
    #' @param center boolean Whether to center the PCA (default=TRUE)
    #' @param fastpath boolean Whether to try a (fast) C algorithm implementation if possible (default=TRUE). This parameter is equivalent to 'fastpath' in irlba::irlba().
    #' @param maxit integer Maximum number of iterations (default=1000). This parameter is equivalent to 'maxit' in irlba::irlba().
    #' @param k integer Number of k clusters for calculating k-NN on the resulting principal components (default=30).
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1)
    #' ## Normalize gene expression variance 
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' p2_object$makeGeneKnnGraph(nPcs=50, k=20, center=TRUE)
    #' }
    #' 
    #' @return graph with gene similarity
    makeGeneKnnGraph = function(nPcs=100, center=TRUE, fastpath=TRUE, maxit=1000, k=30, n.cores=self$n.cores, verbose=TRUE) {
       # Transpose first
       x <- t(self$counts)

      # TODO: factor out gene PCA calculation
      # Do the PCA
      nPcs <- min(nrow(x)-1,ncol(x)-1,nPcs)
      if (center) {
          cm <- Matrix::colMeans(x)
          pcs <- irlba(x, nv=nPcs, nu =0, center=cm, right_only = FALSE, fastpath = fastpath, maxit= maxit, reorth = TRUE)
      } else {
         pcs <- irlba(x, nv=nPcs, nu =0, right_only = FALSE, fastpath = fastpath, maxit= maxit, reorth = TRUE)
      }
      rownames(pcs$v) <- colnames(x)

      # Optional centering
      if (center) {
        pcs$center <- cm
        pcas <- as.matrix(t(t(x %*% pcs$v) - t(cm  %*% pcs$v)))
      } else {
        pcas <- as.matrix(x %*% pcs$v)
      }

      # Keep the names
      rownames(pcas) <- rownames(x)
      colnames(pcas) <- paste0('PC',seq(ncol(pcas)))

      # Save into genegraphs slot
      #genegraphs$genePCs <- pcs
      #genegraphs$geneRotated <- pcas

      # Using cosine distance only here
      if (center) {
        pcas <- pcas - Matrix::rowMeans(pcas)
      }
      xn <- N2R::Knn(pcas, k, nThreads= n.cores, verbose=verbose)
      diag(xn) <- 0 # Remove self edges
      xn <- as(xn,'dgTMatrix') # will drop 0s
      # Turn into a dataframe, convert from correlation distance into weight
      df <- data.frame('from'=rownames(pcas)[xn@i+1],'to'=rownames(pcas)[xn@j+1],'w'=pmax(1-xn@x,0),stringsAsFactors=FALSE)

      self$genegraphs$graph <- df
    },

    #' @description Calculate density-based clusters
    #' 
    #' @param embeddingType The type of embedding used when calculating with `getEmbedding()` (default=NULL). Accepted values are: 'largeVis', 'tSNE', 'FR', 'UMAP', 'UMAP_graph' 
    #' @param name string Name fo the clustering (default='density').
    #' @param eps numeric value of the eps parameter, fed into dbscan::dbscan(x=emb, eps=eps, ...)
    #' @param v numeric The “value” to be used to complete the HSV color descriptions (default=0.7). Equivalent to the 'v' parameter in grDevices::rainbow().
    #' @param s numeric The “saturation” to be used to complete the HSV color descriptions (default=1). Equivalent to the 's' parameter in grDevices::rainbow().
    #' @param verbose boolean Whether to give verbose output (default=TRUE)
    #' @param ... Additional parameters passed to dbscan::dbscan(emb, ...)
    #' @examples  
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix 
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance 
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a KNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=50, type='PCA', center=TRUE, distance='cosine')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', embeddingType = 'UMAP', M=20, perplexity=30, gamma=1/20)
    #' p2_object$getDensityClusters(type='PCA')
    #' } 
    #'
    #' @return density-based clusters
    getDensityClusters=function(type='counts', embeddingType=NULL, name='density', eps=0.5, v=0.7, s=1, verbose=TRUE, ...) {
      if (!requireNamespace("dbscan", quietly = TRUE)) {
        stop("Package \"dbscan\" needed for this function to work. Please install it.", call. = FALSE)
      }

      if (is.null(self$embeddings[[type]])) { 
        stop("First, generate embeddings for type ",type)
      }
      if (is.null(embeddingType)) {
        ## take the last embedding generated
        embeddingType <- names(self$embeddings[[type]][length(self$embeddings[[type]])])
        if (verbose) message("using ",embeddingType," embedding\n")
        emb <- self$embeddings[[type]][[embeddingType]]
        if (is.null(emb)) { 
          stop("embedding ",embeddingType," for type ", type," doesn't exist")
        }
      } else {
        emb <- self$embeddings[[type]][[embeddingType]]
        if (is.null(emb)) { 
          stop("embedding ",embeddingType," for type ", type," doesn't exist")
        }
      }

      cl <- dbscan::dbscan(emb, eps=eps, ...)$cluster
      cols <- rainbow(length(unique(cl)),v=v,s=s)[cl+1]
      cols[cl==0] <- "gray70"
      names(cols) <- rownames(emb)
      self$clusters[[type]][[name]] <- cols
      self$misc[['clusters']][[type]][[name]] <- cols
      invisible(cols)
    },
    # determine subpopulation-specific genes

    #' @description Determine differentially expressed genes, comparing each group against all others using Wilcoxon rank sum test
    #' 
    #' @param name string Slot to store the results in (default='customClustering')
    #' @param z.threshold numeric Minimal absolute Z score (adjusted) to report (default=3)
    #' @param upregulated.only boolean Whether to report only genes that are expressed significantly higher in each group (default=FALSE)
    #' @param verbose boolean Whether to give verbose output (default=FALSE)
    #' @param append.specificity.metrics boolean Whether to append specifity metrics (default=TRUE). Uses the function sccore::appendSpecificityMetricsToDE(). 
    #' @param append.auc boolean If TRUE, append AUC values (default=FALSE). Parameter ignored if append.specificity.metrics is FALSE.
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a KNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=50, type='PCA', center=TRUE, distance='cosine')
    #' ## Call clusters based on KNN
    #' p2_object$getKnnClusters(method=multilevel.community,type='PCA',name='multilevel')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', embeddingType = 'UMAP', M=20, perplexity=30, gamma=1/20)
    #' ## Perform differential expression
    #' p2_object$getDifferentialGenes(type='PCA',verbose=TRUE,clusterType='multilevel')
    #' } 
    #'
    #' @return List with each element of the list corresponding to a cell group in the provided/used factor (i.e. factor levels) 
    #'     Each element of a list is a data frame listing the differentially epxressed genes (row names), with the following columns: 
    #'     Z - adjusted Z score, with positive values indicating higher expression in a given group compare to the rest
    #'     M - log2 fold change
    #'     highest- a boolean flag indicating whether the expression of a given gene in a given vcell group was on average higher than in every other cell group
    #'     fe - fraction of cells in a given group having non-zero expression level of a given gene
    getDifferentialGenes=function(type='counts', clusterType=NULL, groups=NULL, name='customClustering', z.threshold=3, upregulated.only=FALSE, verbose=FALSE, append.specificity.metrics=TRUE, append.auc=FALSE) {
      # restrict counts to the cells for which non-NA value has been specified in groups
      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take the last clustering generated
          cols <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        } else {
          cols <- self$clusters[[type]][[clusterType]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        cols <- groups
      }
      cm <- self$counts
      if (!all(rownames(cm) %in% names(cols))) { 
        warning("cluster vector doesn't specify groups for all of the cells, dropping missing cells from comparison")
      }
      # determine a subset of cells that's in the cols and cols[cell]!=NA
      valid.cells <- rownames(cm) %in% names(cols)[!is.na(cols)]
      if (!all(valid.cells)) {
        # take a subset of the count matrix
        cm <- cm[valid.cells, ]
      }
      # reorder cols
      cols <- as.factor(cols[match(rownames(cm),names(cols))])

      cols <- as.factor(cols)
      if (verbose) {
        message("running differential expression with ",length(levels(cols))," clusters ... ")
      }
      # use offsets based on the base model

      # run wilcoxon test comparing each group with the rest
      lower.lpv.limit <- -100
      # calculate rank per-column (per-gene) average rank matrix
      xr <- sparse_matrix_column_ranks(cm)
      # calculate rank sums per group
      grs <- colSumByFac(xr,as.integer(cols))[-1,,drop=FALSE]
      # calculate number of non-zero entries per group
      xr@x <- numeric(length(xr@x))+1
      gnzz <- colSumByFac(xr,as.integer(cols))[-1,,drop=FALSE]
      #group.size <- as.numeric(tapply(cols,cols,length));
      group.size <- as.numeric(tapply(cols,cols,length))[1:nrow(gnzz)]
      group.size[is.na(group.size)]<-0 # trailing empty levels are cut off by colSumByFac
      # add contribution of zero entries to the grs
      gnz <- (group.size-gnzz)
      # rank of a 0 entry for each gene
      zero.ranks <- (nrow(xr)-diff(xr@p)+1)/2 # number of total zero entries per gene
      ustat <- t((t(gnz)*zero.ranks)) + grs - group.size*(group.size+1)/2
      # standardize
      n1n2 <- group.size*(nrow(cm)-group.size)
      # usigma <- sqrt(n1n2*(nrow(cm)+1)/12) # without tie correction
      # correcting for 0 ties, of which there are plenty
      usigma <- sqrt(n1n2*(nrow(cm)+1)/12)
      usigma <- sqrt((nrow(cm) +1 - (gnz^3 - gnz)/(nrow(cm)*(nrow(cm)-1)))*n1n2/12)
      x <- t((ustat - n1n2/2)/usigma) # standardized U value- z score


      # correct for multiple hypothesis
      if (verbose) {
        message("adjusting p-values ... ")
      }
      x <- matrix(qnorm(bh.adjust(pnorm(as.numeric(abs(x)), lower.tail = FALSE, log.p = TRUE), log = TRUE), lower.tail = FALSE, log.p = TRUE),ncol=ncol(x))*sign(x)
      rownames(x) <- colnames(cm)
      colnames(x) <- levels(cols)[1:ncol(x)]
      if (verbose) {
        message("done.\n")
      }

      # add fold change information
      log.gene.av <- log2(Matrix::colMeans(cm))
      group.gene.av <- colSumByFac(cm,as.integer(cols))[-1,,drop=FALSE] / (group.size+1)
      log2.fold.change <- log2(t(group.gene.av)) - log.gene.av
      # fraction of cells expressing
      f.expressing <- t(gnzz / group.size)
      max.group <- max.col(log2.fold.change)

      ds <- lapply(1:ncol(x),function(i) {
        z <- x[,i]
        vi <- which((if (upregulated.only) z else abs(z)) >= z.threshold)
        r <- data.frame(Z=z[vi],M=log2.fold.change[vi,i],highest=max.group[vi]==i,fe=f.expressing[vi,i], Gene=rownames(x)[vi])
        rownames(r) <- r$Gene
        r <- r[order(r$Z,decreasing=TRUE), ]
        r
      })
      names(ds) <- colnames(x)

      if (append.specificity.metrics) {
        ds <- names(ds) %>% setNames(., .) %>%
          papply(function(n) sccore::appendSpecificityMetricsToDE(ds[[n]], cols, n, p2.counts=cm, append.auc=append.auc), n.cores=self$n.cores)
      }

      if (is.null(groups)) {
        if (is.null(clusterType)) {
          # self$diffgenes[[type]][[ names(self$clusters[[type]])[1] ]] <- ds
          ## take last clustering generated
          self$diffgenes[[type]][[names(self$clusters[[type]])[length(self$clusters[[type]])]]] <- ds
        } else {
          self$diffgenes[[type]][[clusterType]] <- ds
        }
      } else {
        self$diffgenes[[type]][[name]] <- ds
      }

      invisible(ds)
    },


    #' @description Plot heatmap of DE results
    #' 
    #' @param n.genes integer Number of genes to plot (default=100)
    #' @param z.score numeric Threshold of z-scores to filter (default=2). Only greater than or equal to this value are kept.
    #' @param gradient.range.quantile numeric Trimming quantile (default=0.95)
    #' @param inner.clustering boolean Whether to cluster cells within each cluster (default=FALSE)
    #' @param gradientPalette palette of colors to use (default=NULL). If NULL, uses 'colorRampPalette(c('gray90','red'), space = "Lab")(1024)'
    #' @param v numeric The “value” to be used to complete the HSV color descriptions (default=0.7). Equivalent to the 'v' parameter in grDevices::rainbow().
    #' @param s numeric The “saturation” to be used to complete the HSV color descriptions (default=1). Equivalent to the 's' parameter in grDevices::rainbow().
    #' @param box boolean Whether to draw a box around the current plot in the given color and linetype (default=TRUE)
    #' @param drawGroupNames boolean Whether to draw group names (default=FALSE)
    #' @param ... Additional parameters passed to internal function used for heatmap plotting, my.heatmap2()
    #' 
    #' @return heatmap of DE results
    plotDiffGeneHeatmap=function(type='counts', clusterType=NULL, groups=NULL, n.genes=100, 
      z.score=2, gradient.range.quantile=0.95, inner.clustering=FALSE, gradientPalette=NULL, 
      v=0.8, s=1, box=TRUE, drawGroupNames=FALSE, ... ) {
      if (!is.null(clusterType)) {
        x <- self$diffgenes[[type]][[clusterType]]
        if (is.null(x)) { 
          stop("Differential genes for the specified cluster type ", clusterType, " haven't been calculated") 
        }
      } else {
        ## x <- self$diffgenes[[type]][[1]]
        ## take last generated item
        x <- self$diffgenes[[type]][length(self$diffgenes[[type]])]
        if (is.null(x)) { 
          stop("No differential genes found for data type ",type) 
        }
      }

      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          cols <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        } else {
          cols <- self$clusters[[type]][[clusterType]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        # use clusters information
        if (!all(rownames(self$counts) %in% names(groups))) { warning("provided cluster vector doesn't list groups for all of the cells")}
        cols <- as.factor(groups[match(rownames(self$counts),names(groups))])
      }
      cols <- as.factor(cols)
      # select genes to show
      if (!is.null(z.score)) {
        x <- lapply(x,function(d) d[d$Z >= z.score & d$highest==TRUE,])
        if (!is.null(n.genes)) {
          x <- lapply(x,function(d) {if(nrow(d)>0) { d[1:min(nrow(d),n.genes),]}})
        }
      } else {
        if (!is.null(n.genes)) {
          x <- lapply(x,function(d) {if(nrow(d)>0) { d[1:min(nrow(d),n.genes),]}})
        }
      }
      x <- lapply(x,rownames)
      # make expression matrix
      #x <- x[!unlist(lapply(x,is.null))]
      #cols <- cols[cols %in% names(x)]
      #cols <- droplevels(cols)
      em <- self$counts[,unlist(x)]
      # renormalize rows
      if (all(sign(em)>=0)) {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c('gray90','red'), space = "Lab")(1024)
        }
        em <- apply(em,1,function(x) {
          zlim <- as.numeric(quantile(x,p=c(1-gradient.range.quantile,gradient.range.quantile)))
          if (diff(zlim)==0) {
            zlim <- as.numeric(range(x))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        })
      } else {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c("blue", "grey90", "red"), space = "Lab")(1024)
        }
        em <- apply(em,1,function(x) {
          zlim <- c(-1,1)*as.numeric(quantile(abs(x),p=gradient.range.quantile))
          if (diff(zlim)==0) {
            zlim <- c(-1,1)*as.numeric(max(abs(x)))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        })
      }

      # cluster cell types by averages
      rowfac <- factor(rep(names(x),unlist(lapply(x,length))),levels=names(x))
      if (inner.clustering) {
        clclo <- stats::hclust(as.dist(1-cor(do.call(cbind,tapply(1:nrow(em),rowfac,function(ii) Matrix::colMeans(em[ii,,drop=FALSE]))))),method='complete')$order
      } else {
        clclo <- 1:length(levels(rowfac))
      }

      if (inner.clustering) {
        # cluster genes within each cluster
        clgo <- tapply(1:nrow(em),rowfac,function(ii) {
          ii[stats::hclust(as.dist(1-cor(t(em[ii,]))),method='complete')$order]
        })
      } else {
        clgo <- tapply(1:nrow(em),rowfac,I)
      }
      if (inner.clustering) {
        # cluster cells within each cluster
        clco <- tapply(1:ncol(em),cols,function(ii) {
          if (length(ii)>3) {
            ii[stats::hclust(as.dist(1-cor(em[,ii,drop=FALSE])),method='complete')$order]
          } else {
            ii
          }
        })
      } else {
        clco <- tapply(1:ncol(em),cols,I)
      }
      #clco <- clco[names(clgo)]
      # filter down to the clusters that are included
      #vic <- cols %in% clclo
      colors <- fac2col(cols,v=v,s=s,return.details=TRUE)
      cellcols <- colors$colors[unlist(clco[clclo])]
      genecols <- rev(rep(colors$palette,unlist(lapply(clgo,length)[clclo])))
      bottomMargin <- ifelse(drawGroupNames,4,0.5)
      my.heatmap2(em[rev(unlist(clgo[clclo])),unlist(clco[clclo])],col=gradientPalette,Colv=NA,Rowv=NA,labRow=NA,labCol=NA,RowSideColors=genecols,ColSideColors=cellcols,margins=c(bottomMargin,0.5),ColSideColors.unit.vsize=0.05,RowSideColors.hsize=0.05,useRaster=TRUE, box=box, ...)
      abline(v=cumsum(unlist(lapply(clco[clclo],length))),col=1,lty=3)
      abline(h=cumsum(rev(unlist(lapply(clgo[clclo],length)))),col=1,lty=3)
    },

    #' @description Recalculate library sizes using robust regression within clusters
    #' 
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=50, type='PCA', center=TRUE, distance='cosine')
    #' ## Call clusters based on kNN
    #' p2_object$getKnnClusters(method=infomap.community, type='PCA')
    #' p2_object$getRefinedLibSizes(type='PCA')
    #' lib.sizes <- p2_object$getRefinedLibSizes(type="PCA")
    #' }
    #'
    #' @return recalculated library sizes
    getRefinedLibSizes=function(clusterType=NULL, groups=NULL, type='counts', n.cores=self$n.cores) {

      if (!requireNamespace("robustbase", quietly = TRUE)) {
        stop("Package \"robustbase\" needed for this function to work. Please install it.", call. = FALSE)
      }
 
      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take the last-generated clustering
          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      }

      # calculated pooled profiles per cluster
      lvec <- colSumByFac(self$misc[['rawCounts']],as.integer(groups))[-1,,drop=FALSE]
      lvec <- t(lvec/pmax(1,Matrix::rowSums(lvec)))*1e4

      # TODO: implement internal robust regression
      ## x <- misc[['rawCounts']]
      ## x <- x/as.numeric(depth);
      ## inplaceWinsorizeSparseCols(x,10);
      ## x <- x*as.numeric(depth);

      x <- mclapply(1:length(levels(groups)),function(j) {
        ii <- names(groups)[which(groups==j)]
        av <- lvec[,j]
        avi <- which(av>0)
        av <- av[avi]
        cvm <- as.matrix(self$misc[['rawCounts']][ii,avi])
        x <- unlist(lapply(ii,function(i) {
          cv <- cvm[i,]
          #as.numeric(coef(glm(cv~av+0,family=poisson(link='identity'),start=sum(cv)/1e4)))
          as.numeric(coef(robustbase::glmrob(cv~av+0,family=poisson(link='identity'),start=sum(cv)/1e4)))
        }))
        names(x) <- ii
        x
      },mc.cores=n.cores)

      lib.sizes <- unlist(x)[rownames(self$misc[['rawCounts']])]
      lib.sizes <- lib.sizes/mean(lib.sizes)*mean(Matrix::rowSums(self$misc[['rawCounts']]))

      self$depth <- lib.sizes
      invisible(lib.sizes)
    },

    #' @description Plot heatmap for a given set of genes
    #' 
    #' @param genes character vector Gene names
    #' @param gradient.range.quantile numeric Trimming quantile (default=0.95)
    #' @param cluster.genes boolean Whether to cluster genes within each cluster using stats::hclust() (default=FALSE)
    #' @param inner.clustering boolean Whether to cluster cells within each cluster (default=FALSE)
    #' @param gradientPalette palette of colors to use (default=NULL). If NULL, uses 'colorRampPalette(c('gray90','red'), space = "Lab")(1024)'
    #' @param v numeric The “value” to be used to complete the HSV color descriptions (default=0.7). Equivalent to the 'v' parameter in grDevices::rainbow().
    #' @param s numeric The “saturation” to be used to complete the HSV color descriptions (default=1). Equivalent to the 's' parameter in grDevices::rainbow().
    #' @param box boolean Whether to draw a box around the current plot in the given color and linetype (default=TRUE)
    #' @param drawGroupNames boolean Whether to draw group names (default=FALSE)
    #' @param useRaster boolean If TRUE a bitmap raster is used to plot the image instead of polygons (default=TRUE). The grid must be regular in that case, otherwise an error is raised. For more information, see graphics::image().
    #' @param smooth.span (default=max(1,round(nrow(self$counts)/1024)))
    #' @param ... Additional parameters passed to internal function used for heatmap plotting, my.heatmap2()
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance 
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=50, type='PCA', center=TRUE, distance='cosine')
    #' ## Call clusters based on kNN
    #' p2_object$getKnnClusters(method=multilevel.community,type='PCA',name='multilevel')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', embeddingType = 'UMAP', M=20, perplexity=30, gamma=1/20)
    #' ## Perform differential expression
    #' p2_object$getDifferentialGenes(type='PCA',verbose=TRUE,clusterType='multilevel')
    #' de <- p2_object$diffgenes$PCA[[1]][['2']]
    #' p2_object$plotGeneHeatmap(genes=rownames(de)[1:15], 
    #'     groups=p2_object$clusters$PCA[[1]], cluster.genes=TRUE)
    #' } 
    #'
    #' @return plot of gene heatmap
    plotGeneHeatmap=function(genes, type='counts', clusterType=NULL, groups=NULL, 
      gradient.range.quantile=0.95, cluster.genes=FALSE, inner.clustering=FALSE, gradientPalette=NULL, 
      v=0.8, s=1, box=TRUE, drawGroupNames=FALSE, useRaster=TRUE, smooth.span=max(1,round(nrow(self$counts)/1024)), ... ) {
      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          cols <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        } else {
          cols <- self$clusters[[type]][[clusterType]]
          if (is.null(cols)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        # use clusters information
        if (!all(rownames(self$counts) %in% names(groups))) { warning("provided cluster vector doesn't list groups for all of the cells")}
        cols <- as.factor(groups[match(rownames(self$counts),names(groups))])
      }
      cols <- as.factor(cols)
      # make expression matrix
      if (!all(genes %in% colnames(self$counts))) { 
        warning(paste("The following specified genes were not found in the data: [",paste(genes[!genes %in% colnames(counts)],collapse=" "),"], omitting",sep="")) 
      }
      x <- intersect(genes,colnames(self$counts))
      if (length(x)<1) { 
        stop("Too few genes") 
      }
      em <- as.matrix(t(self$counts[,x]))

      # renormalize rows
      if (all(sign(em)>=0)) {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c('gray90','red'), space = "Lab")(1024)
        }
        em <- t(apply(em,1,function(x) {
          zlim <- as.numeric(quantile(x, p=c(1-gradient.range.quantile,gradient.range.quantile)))
          if (diff(zlim)==0) {
            zlim <- as.numeric(range(x))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        }))
      } else {
        if (is.null(gradientPalette)) {
          gradientPalette <- colorRampPalette(c("blue", "grey90", "red"), space = "Lab")(1024)
        }
        em <- t(apply(em,1,function(x) {
          zlim <- c(-1,1)*as.numeric(quantile(abs(x),p=gradient.range.quantile))
          if (diff(zlim)==0) {
            zlim <- c(-1,1)*as.numeric(max(abs(x)))
          }
          x[x<zlim[1]] <- zlim[1]
          x[x>zlim[2]] <- zlim[2]
          x <- (x-zlim[1])/(zlim[2]-zlim[1])
        }))
      }

      # cluster cell types by averages
      clclo <- 1:length(levels(cols))

      if (cluster.genes) {
        # cluster genes within each cluster
        clgo <- stats::hclust(as.dist(1-cor(t(em))), method='complete')$order
      } else {
        clgo <- 1:nrow(em)
      }

      if (inner.clustering) {
        # cluster cells within each cluster
        clco <- tapply(1:ncol(em),cols,function(ii) {
          if (length(ii)>3) {
            ii[stats::hclust(as.dist(1-cor(em[,ii,drop=FALSE])), method='single')$order]
          } else {
            ii
          }
          # TODO: implement smoothing span support
        })
      } else {
        clco <- tapply(1:ncol(em),cols,I)
      }

      cellcols <- fac2col(cols,v=v,s=s)[unlist(clco[clclo])]
      #genecols <- rev(rep(fac2col(cols,v=v,s=s,return.level.colors=T),unlist(lapply(clgo,length)[clclo])))
      bottomMargin <- 0.5
      # reorder and potentially smooth em
      em <- em[rev(clgo),unlist(clco[clclo])]

      my.heatmap2(em,col=gradientPalette,Colv=NA,Rowv=NA,labCol=NA,ColSideColors=cellcols,margins=c(bottomMargin,5),ColSideColors.unit.vsize=0.05,RowSideColors.hsize=0.05,useRaster=useRaster, box=box, ...)
      bp <- cumsum(unlist(lapply(clco[clclo],length))) # cluster border positions
      abline(v=bp,col=1,lty=3)
      #abline(h=cumsum(rev(unlist(lapply(clgo[clclo],length)))),col=1,lty=3)
      if (drawGroupNames) {
        clpos <- (c(0,bp[-length(bp)])+bp)/2
        labpos <- rev(seq(0,length(bp)+1)/(length(bp)+1)*nrow(em))
        labpos <- labpos[-1]
        labpos <- labpos[-length(labpos)]
        text(x=clpos,y=labpos,labels = levels(cols),cex=1)
        # par(xpd=TRUE)
        # clpos <- (c(0,bp[-length(bp)])+bp)/2;
        # labpos <- seq(0,length(bp)+1)/(length(bp)+1)*max(bp); labpos <- labpos[-1]; labpos <- labpos[-length(labpos)]
        # text(x=labpos,y=-2,labels = levels(col))
        # segments(labpos,-1,clpos,0.5,lwd=0.5)
        # par(xpd=FALSE)
      }
    },

    #' @description Show embedding
    #' 
    #' @param type string Either 'counts' or the name of a stored embedding, names(self$embeddings) (default=NULL)
    #' @param embeddingType string Embedding type (default=NULL). If NULL, takes the most recently generated embedding.
    #' @param clusterType Name of cluster to access (default=NULL). If NULL, takes the most recently generated clustering. Parameter ignored if groups is not NULL.
    #' @param groups factor named with cell names specifying the clusters of cells (default=NULL)
    #' @param colors character vector List of gene names (default=NULL)
    #' @param gene (default=NULL)
    #' @param plot.theme (default=ggplot2::theme_bw()) 
    #' @param ... Additional parameters passed to sccore::embeddingPlot()
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance 
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=50, type='PCA', center=TRUE, distance='cosine')
    #' ## Call clusters based on kNN
    #' p2_object$getKnnClusters(method=multilevel.community,type='PCA',name='multilevel')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', embeddingType = 'UMAP', M=20, perplexity=30, gamma=1/20)
    #' library(ggplot2)
    #' p2_object$plotEmbedding(type='PCA', show.legend=FALSE, mark.groups=TRUE, min.cluster.size=50,
    #'      shuffle.colors=FALSE, font.size=1, alpha=0.1, title='clusters (UMAP)', 
    #'      plot.theme=theme(plot.title = element_text(hjust = 0.5)))
    #' }
    #' 
    #' @return plot of the embedding
    plotEmbedding=function(type=NULL, embeddingType=NULL, clusterType=NULL,
      groups=NULL, colors=NULL, gene=NULL, plot.theme=ggplot2::theme_bw(), ...) {

      if (is.null(type)) {
        if ('counts' %in% names(self$embeddings)) {
          type <- 'counts'
        } else if (length(self$embeddings) > 0) {
          # type <- names(self$embeddings)[1]
          ## Use the last-generated embedding
          type <- names(self$embeddings[length(self$embeddings)])
        } else {
          stop("First, generate an embedding")
        }
      }

      if (is.null(self$embeddings[[type]])){
        stop("First, generate embeddings for type ",type)
      }

      if (is.null(embeddingType)){
        ## take the most recently generated embedding
        emb <- self$embeddings[[type]][length(self$embeddings[[type]])]
      } else{
        ## check embeddingType exists
        if (is.null(self$embeddings[[type]][[embeddingType]])){
          stop("Embedding does not exist for embeddingType ", embeddingType)
        }
        emb <- self$embeddings[[type]][[embeddingType]]
      }

      if (!is.null(gene)) {
        if (!(gene %in% colnames(self$counts))){
          stop("Gene '", gene, "' isn't presented in the count matrix")
        }
        colors <- self$counts[,gene]
      }

      if (is.null(colors) && is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # groups <- self$clusters[[type]][[1]]
          ## Take last-genereated clustering
          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      }

      sccore::embeddingPlot(emb, groups=groups, colors=colors, plot.theme=plot.theme, ...)
    },

    #' @description Get overdispersed genes
    #' 
    #' @param alpha numeric The Type I error probability or the significance level (default=5e-2). This is the criterion used to measure statistical significance, i.e. if the p-value < alpha, then it is statistically significant.
    #' @param use.unadjusted.pvals boolean Whether to use Benjamini-Hochberg adjusted p-values (default=FALSE).
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance 
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=50, type='PCA', center=TRUE, distance='cosine')
    #' ## Call clusters based on kNN
    #' p2_object$getKnnClusters(method=infomap.community, type='PCA')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', M=20, perplexity=30, gamma=1/20)
    #' ## Perform differential expression
    #' p2_object$getDifferentialGenes(type='PCA',verbose=TRUE)
    #' odGenes <- p2_object$getOdGenes(use.unadjusted.pvals=FALSE)
    #' } 
    #'
    #' @return vector of overdispersed genes
    getOdGenes=function(n.odgenes=NULL, alpha=5e-2, use.unadjusted.pvals=FALSE) {
      if (is.null(self$misc[['varinfo']])) { 
        stop("Please run adjustVariance first")
      }
      if (is.null(n.odgenes)) { #return according to alpha
        if (use.unadjusted.pvals) {
          rownames(self$misc[['varinfo']])[self$misc[['varinfo']]$lp <= log(alpha)]
        } else {
          rownames(self$misc[['varinfo']])[self$misc[['varinfo']]$lpa <= log(alpha)]
        }
      } else { # return top n.odgenes sites
        rownames(self$misc[['varinfo']])[(order(self$misc[['varinfo']]$lp, decreasing=FALSE)[1:min(ncol(self$counts),n.odgenes)])]
      }
    },

    #' @description Return variance-normalized matrix for specified genes or a number of OD genes
    #'
    #' @param genes vector of gene names to explicitly return (default=NULL)
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' p2_object$getNormalizedExpressionMatrix()
    #' }
    #' 
    #' @return cell by gene matrix
    getNormalizedExpressionMatrix=function(genes=NULL, n.odgenes=NULL) {
      if (is.null(genes)) {
        genes <- self$getOdGenes(n.odgenes)
      }
      x <- self$counts[,genes]
      x@x <- x@x*rep(self$misc[['varinfo']][colnames(x),'gsf'], diff(x@p))
      return(x)
    },


    #' @description Calculate PCA reduction of the data
    #' 
    #' @param nPcs numeric Number of principal components (PCs) (default=20)
    #' @param type string Dataset view to reduce (counts by default, but can specify a name of an existing reduction) (default='counts')
    #' @param name string Name for the PCA reduction to be created (default='PCA')
    #' @param use.odgenes boolean Whether pre-calculated set of overdispersed genes should be used (default=TRUE)
    #' @param odgenes Explicitly specify a set of overdispersed genes to use for the reduction (default=NULL)
    #' @param center boolean Whether data should be centered prior to PCA (default=TRUE)
    #' @param cells optional subset of cells on which PCA should be run (default=NULL)
    #' @param fastpath boolean Use C implementation for speedup (default=TRUE)
    #' @param maxit numeric Maximum number of iterations (default=100). For more information, see 'maxit' parameter in irlba::irlba(). 
    #' @param var.scale boolean Apply scaling if using raw counts (default=TRUE). If type="counts", var.scale is TRUE by default.
    #' @param ... additional arguments forwarded to irlba::irlba
    #' @examples 
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=600)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts, log.scale=FALSE, min.cells.per.gene=30, n.cores=1)
    #' ## Normalize gene expression variance 
    #' p2_object$adjustVariance(plot=TRUE, gam.k=15)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=2e3)
    #' }
    #' 
    #' @return Invisible PCA result (the reduction itself is saved in self$reductions[[name]])"
    calculatePcaReduction=function(nPcs=20, type='counts', name='PCA', use.odgenes=TRUE, n.odgenes=NULL, 
      odgenes=NULL, center=TRUE, cells=NULL, fastpath=TRUE, maxit=100, verbose=TRUE, var.scale=(type == "counts"), ...) {

      if (type=='counts') {
        x <- self$counts
      } else {
        if (!type %in% names(self$reductions)) { 
          stop("Reduction ",type,' not found')
        }
        x <- self$reductions[[type]]
      }
      if ((use.odgenes || !is.null(n.odgenes)) && is.null(odgenes)) {
        if (is.null(self$misc[['odgenes']] )) { stop("Please run adjustVariance() first")}
        odgenes <- self$misc[['odgenes']]
        if (!is.null(n.odgenes)) {
          if (n.odgenes>length(odgenes)) {
            #warning("number of specified odgenes is higher than the number of the statistically significant sites, will take top ",n.odgenes,' sites')
            odgenes <- rownames(self$misc[['varinfo']])[(order(self$misc[['varinfo']]$lp,decreasing=FALSE)[1:min(ncol(self$counts),n.odgenes)])]
          } else {
            odgenes <- odgenes[1:n.odgenes]
          }
        }
      }
      if (!is.null(odgenes)) {
        x <- x[,odgenes]
        if (verbose) message('running PCA using ',length(odgenes),' OD genes .')
      } else { #all genes?
        if (verbose) message('running PCA all ',ncol(x),' genes .')
      }
      # apply scaling if using raw counts
      if (var.scale) {
        #x <- t(t(x)*misc[['varinfo']][colnames(x),'gsf'])
        x@x <- x@x*rep(self$misc[['varinfo']][colnames(x),'gsf'],diff(x@p))
      }
      if (verbose) message('.')
      
      if (!is.null(cells)) {
        # cell subset is just for PC determination
        nPcs <- min(min(length(cells),ncol(x))-1,nPcs)
        cm <- Matrix::colMeans(x[cells,])
        pcs <- irlba(x[cells,], nv=nPcs, nu=0, center=cm, right_only=FALSE,fastpath=fastpath,maxit=maxit,reorth=TRUE, ...)
      } else {
        nPcs <- min(min(nrow(x),ncol(x))-1,nPcs)
        if (center) {
          cm <- Matrix::colMeans(x)
          pcs <- irlba(x, nv=nPcs, nu=0, center=cm, right_only=FALSE,fastpath=fastpath,maxit=maxit,reorth=TRUE, ...)
        } else {
          pcs <- irlba(x, nv=nPcs, nu=0, right_only=FALSE,fastpath=fastpath,maxit=maxit,reorth=TRUE, ...)
        }
      }
      rownames(pcs$v) <- colnames(x)

      if (verbose) message('.')

      # adjust for centering!
      if (center) {
        pcs$center <- cm
        pcas <- as.matrix(t(as(t(x %*% pcs$v), "dgeMatrix") - t(cm %*% pcs$v)))
      } else {
        pcas <- as.matrix(x %*% pcs$v)
      }
      self$misc$PCA <- pcs
      if (verbose) message('.')
      #pcas <- scde::winsorize.matrix(pcas,0.05)
      # # control for sequencing depth
      # if(is.null(batch)) {
      #   mx <- model.matrix(x ~ d,data=data.frame(x=1,d=depth))
      # } else {
      #   mx <- model.matrix(x ~ d*b,data=data.frame(x=1,d=depth,b=batch))
      # }
      # # TODO: how to get rid of residual depth effects in the PCA-based clustering?
      # #pcas <- t(t(colLm(pcas,mx,returnResid=TRUE))+Matrix::colMeans(pcas))
      # pcas <- colLm(pcas,mx,returnResid=TRUE)
      rownames(pcas) <- rownames(x)
      colnames(pcas) <- paste0('PC', seq(ncol(pcas)))
      #pcas <- pcas[,-1]
      #pcas <- scde::winsorize.matrix(pcas,0.1)
      if (verbose) message(' done\n')
      self$reductions[[name]] <- pcas
      ## nIcs <- nPcs;
      ## a <- ica.R.def(t(pcas),nIcs,tol=1e-3,fun='logcosh',maxit=200,verbose=T,alpha=1,w.init=matrix(rnorm(nIcs*nPcs),nIcs,nPcs))
      ## reductions[['ICA']] <- as.matrix( x %*% pcs$v %*% a);
      ## colnames(reductions[['ICA']]) <- paste('IC',seq(ncol(reductions[['ICA']])),sep='');

      invisible(pcas)
    },

    #' @description Reset overdispersed genes 'odgenes' to be a superset of the standard odgene selection (guided by n.odgenes or alpha), 
    #'     and a set of recursively determined odgenes based on a given group (or a cluster info)
    #' 
    #' @param min.group.size integer Number of minimum cells for filtering out group size (default=30)
    #' @param od.alpha numeric The Type I error probability or the significance level for calculating overdispersed genes (default=1e-1). This is the criterion used to measure statistical significance, i.e. if the p-value < alpha, then it is statistically significant.
    #' @param use.odgenes boolean Whether pre-calculated set of overdispersed genes should be used (default=FALSE)
    #' @param odgenes Explicitly specify a set of overdispersed genes to use for the reduction (default=NULL) #' @param odgenes (default=NULL)
    #' @param n.odgene.multiplier numeric (default=1)
    #' @param gam.k integer The k used for the generalized additive model 'v ~ s(m, k =gam.k)' (default=10). If gam.k<2, linear regression is used 'lm(v ~ m)'.
    #' @param min.odgenes integer Minimum number of overdispersed genes to use (default=10)
    #' @param max.odgenes integer Maximum number of overdispersed genes to use (default=Inf)
    #' @param recursive boolean Whether to determine groups for which variance normalization will be rerun (default=TRUE)
    #' @examples
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=50, type='PCA', center=TRUE, distance='cosine')
    #' ## Call clusters based on kNN
    #' p2_object$getKnnClusters(method=infomap.community, type='PCA')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', M=20, perplexity=30, gamma=1/20)
    #' ## Perform differential expression
    #' p2_object$getDifferentialGenes(type='PCA',verbose=TRUE)
    #' p2_object$expandOdGenes(type='PCA')
    #' }
    #' 
    #' @return List of overdispersed genes
    expandOdGenes=function(type='counts', clusterType=NULL, groups=NULL , min.group.size=30, od.alpha=1e-1, 
      use.odgenes=FALSE, n.odgenes=NULL, odgenes=NULL, n.odgene.multiplier=1, gam.k=10,verbose=FALSE,n.cores=self$n.cores,
      min.odgenes=10,max.odgenes=Inf,recursive=TRUE) {
      # determine groups
      if (is.null(groups)) {
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        groups <- as.factor(groups[names(groups) %in% rownames(self$counts)])
        groups <- droplevels(groups)
      }

      # determine initial set of odgenes
      if ((use.odgenes || !is.null(n.odgenes)) && is.null(odgenes)) {
        if (is.null(self$misc[['varinfo']] )) { stop("Please run adjustVariance() first")}
        df <- self$misc$varinfo
        odgenes <- rownames(df)[!is.na(df$lpa) & df$lpa<log(od.alpha)]
        #odgenes <- misc[['odgenes']];
        if (!is.null(n.odgenes)) {
          if (n.odgenes>length(odgenes)) {
            #warning("number of specified odgenes is higher than the number of the statistically significant sites, will take top ",n.odgenes,' sites')
            odgenes <- rownames(self$misc[['varinfo']])[(order(self$misc[['varinfo']]$lp,decreasing=FALSE)[1:min(ncol(self$counts),n.odgenes)])]
          } else {
            odgenes <- odgenes[1:n.odgenes]
          }
        }
      }

      # filter out small groups
      if (min.group.size>1) { 
        groups[groups %in% levels(groups)[unlist(tapply(groups,groups,length))<min.group.size]] <- NA
        groups <- droplevels(groups) 
      }
      if (sum(!is.na(groups))<min.group.size) {
        warning("clustering specifies fewer cells than min.group.size")
        return(odgenes)
      }


      if (length(levels(groups))<2) {
        warning("cannot expand od genes based on a single group")
        return(odgenes)
      }

      # determine groups for which variance normalization will be rerun
      if (recursive) {
        if (verbose) message("recursive group enumeration ...")
        # derive cluster hierarchy

        # use raw counts to derive clustering
        z <- self$misc$rawCounts
        rowFac <- rep(-1,nrow(z))
        names(rowFac) <- rownames(z)
        rowFac[match(names(groups),rownames(z))] <- as.integer(groups)
        tc <- colSumByFac(z,as.integer(rowFac))[-1,,drop=FALSE]
        rownames(tc) <- levels(groups)
        d <- 1-cor(t(log10(tc/pmax(1,Matrix::rowSums(tc))*1e3+1)))
        hc <- stats::hclust(as.dist(d),method='average',members=unlist(tapply(groups,groups,length)))

        dlab <- function(l) {
          if (is.leaf(l)) {
            return(list(labels(l)))
          } else {
            return(c(list(labels(l)),dlab(l[[1]]),dlab(l[[2]])))
          }
        }

        # for each level in the cluster hierarchy, except for the top
        rgroups <- dlab(as.dendrogram(hc))[-1]
        rgroups <- c(list(levels(groups)),rgroups)
        if (verbose) message("done.\n")
      } else {
        rgroups <- lapply(levels(groups),I)
      }
      names(rgroups) <- unlist(lapply(rgroups,paste,collapse="+"))

      # run local variance normalization
      if (verbose) message("running local variance normalization ")
      # run variance normalization, determine PCs
      gpcs <- papply(rgroups,function(group) {
        cells <- names(groups)[groups %in% group]

        # variance normalization
        df <- self$adjustVariance(persist=FALSE, gam.k=gam.k, verbose=FALSE, cells=cells, n.cores=1)
        #if(!is.null(n.odgenes)) {
        #  odgenes <- rownames(df)[order(df$lp,decreasing=F)[1:n.odgenes]]
        #} else {
        df <- df[!is.na(df$lp),,drop=FALSE]
        df <- df[order(df$lp,decreasing=FALSE),,drop=FALSE]
        n.od <- min(max(sum(df$lpa<log(od.alpha)),min.odgenes),max.odgenes)
        if (n.od>0) {
          odgenes <- rownames(df)[1:min(n.od*n.odgene.multiplier,nrow(df))]
        } else {
          return(NULL)
        }
        sf <- df$gsf[match(odgenes,rownames(df))]
        return(list(sf=sf,cells=cells,odgenes=odgenes))
      },n.cores=n.cores, mc.preschedule=TRUE)
      if (verbose) message(" done\n")
      odg <- unique(unlist(lapply(gpcs,function(z) z$odgenes)))
      # TODO: consider gsf?
      odgenes <- unique(c(odgenes,odg))
      self$misc[['odgenes']] <- odgenes
      invisible(odgenes)
    },


    #' @description local PCA implementation
    #' 
    #' @param nPcs integer Number of principal components (default=5)
    #' @param k integer Number of components for kNN graph (default=30)
    #' @param b numeric Constant within exp(-b*(ncid/cldsd)^2), used for calculating cell relevance per cluster (default=1)
    #' @param a numeric Constant within "(1-exp(-a*(dsq)/(p$pcs$trsd^2)))*(pk /outerproduct pk)" (default=1)
    #' @param min.group.size integer Number of minimum cells for filtering out group size (default=30)
    #' @param name string Title (default='localPCA')
    #' @param od.alpha numeric Significance level for calculating overdispersed genes (default=1e-1). P-values will be filtered by <log(od.alpha).
    #' @param gam.k integer The k used for the generalized additive model 'v ~ s(m, k =gam.k)' (default=10). If gam.k<2, linear regression is used 'lm(v ~ m)'.
    #' @param min.odgenes integer Minimum number of overdispersed genes to use (default=5)
    #' @param take.top.odgenes boolean Take top overdispersed genes in decreasing order (default=FALSE)
    #' @param recursive boolean Whether to recursively determine groups for which variance normalization will be rerun (default=FALSE)
    #' @param euclidean boolean Whether to applied euclidean-based distance similarity during variance normalization (default=FALSE)
    #' @param perplexity integer Perplexity parameter within Rtsne::Rtsne() (default=k). Please see Rtsne for more details.
    #' @param return.pca boolean Whether to return the PCs (default=FALSE)
    #' @param skip.pca boolean If TRUE and return.pca=TRUE, will return a list of scale factors, cells, and overdispersed genes, i.e. list(sf=sf, cells=cells, odgenes=odgenes) (default=FALSE). Otherwise, ignored.
    #'
    #' @return localPcaKnn return here
    localPcaKnn=function(nPcs=5, type='counts', clusterType=NULL, groups=NULL,
      k=30, b=1, a=1, min.group.size=30, name='localPCA', od.alpha=1e-1, 
      n.odgenes=NULL, gam.k=10, verbose=FALSE, n.cores=self$n.cores, min.odgenes=5,
      take.top.odgenes=FALSE, recursive=TRUE, euclidean=FALSE, perplexity=k,
      return.pca=FALSE, skip.pca=FALSE) {

      if (type=='counts') {
        x <- self$counts
      } else {
        if (!type %in% names(self$reductions)) { 
          stop("Reduction ",type,' not found')
        }
        x <- self$reductions[[type]]
      }

      if (is.null(groups)){
        # look up the clustering based on a specified type
        if (is.null(clusterType)) {
          # take last-generated clustering
          groups <- self$clusters[[type]][[length(self$clusters[[type]])]]
          if (is.null(groups)) { 
            stop(paste("Please generate clusters for",type,"first"))
          }
        } else {
          groups <- self$clusters[[type]][[clusterType]]
          if (is.null(groups)) { 
            stop("Clustering ",clusterType," for type ", type," doesn't exist")
          }
        }
      } else {
        groups <- as.factor(groups[names(groups) %in% rownames(x)])
        groups <- droplevels(groups)
      }

      if (min.group.size>1) { 
        groups[groups %in% levels(groups)[unlist(tapply(groups,groups,length))<min.group.size]] <- NA
        groups <- droplevels(groups) 
      }
      if (sum(!is.na(groups))<min.group.size) { 
        stop("clustering specifies fewer cells than min.group.size") 
      }


      if (recursive) {
        if (verbose) message("recursive group enumeration ...")
        ## # derive cluster hierarchy
        ## rowFac <- rep(-1,nrow(x)); names(rowFac) <- rownames(x);
        ## rowFac[names(groups)] <- as.integer(groups);
        ## tc <- colSumByFac(x,as.integer(rowFac))[-1,]
        ## rownames(tc) <- levels(groups)
        ## #tc <- rbind("total"=Matrix::colSums(tc),tc)
        ## #d <- jsDist(t(((tc/pmax(1,Matrix::rowSums(tc)))))); rownames(d) <- colnames(d) <- rownames(tc)
        ## d <- 1-cor(t(tc))
        ## hc <- stats::hclust(as.dist(d),method='ward.D')

        # use raw counts to derive clustering
        z <- self$misc$rawCounts
        rowFac <- rep(-1,nrow(z))
        names(rowFac) <- rownames(z)
        rowFac[match(names(groups),rownames(z))] <- as.integer(groups)
        tc <- colSumByFac(z,as.integer(rowFac))[-1,,drop=FALSE]
        rownames(tc) <- levels(groups)
        d <- 1-cor(t(log10(tc/pmax(1,Matrix::rowSums(tc))*1e3+1)))
        hc <- stats::hclust(as.dist(d),method='average',members=unlist(tapply(groups,groups,length)))


        dlab <- function(l) {
          if (is.leaf(l)) {
            return(list(labels(l)))
          } else {
            return(c(list(labels(l)),dlab(l[[1]]),dlab(l[[2]])))
          }
        }

        # for each level in the cluster hierarchy, except for the top
        rgroups <- dlab(as.dendrogram(hc))[-1]
        rgroups <- c(list(levels(groups)),rgroups)
        if (verbose) message("done.\n")
      } else {
        rgroups <- lapply(levels(groups),I)
      }
      names(rgroups) <- unlist(lapply(rgroups, paste, collapse="+"))


      if (verbose) message("determining local PCs ")
      # run variance normalization, determine PCs
      gpcs <- papply(rgroups,function(group) {
        cells <- names(groups)[groups %in% group]

        # variance normalization
        df <- self$adjustVariance(persist=FALSE, gam.k=gam.k, verbose=FALSE, cells=cells, n.cores=1)
        if (!is.null(n.odgenes)) {
          odgenes <- rownames(df)[order(df$lp, decreasing=FALSE)[1:n.odgenes]]
        } else {
          odgenes <- rownames(df)[!is.na(df$lpa) & df$lpa<log(od.alpha)]
        }
        if (length(odgenes)<min.odgenes) {
          if (take.top.odgenes) {
            odgenes <- rownames(df)[order(df$lp, decreasing=FALSE)[1:min.odgenes]]
          } else {
            return(NULL)
          }
        }
        sf <- df$gsf[match(odgenes,rownames(df))]

        if (return.pca && skip.pca) {
          return(list(sf=sf, cells=cells, odgenes=odgenes))
        }


        y <- t(t(x[cells,odgenes])*sf)
        cm <- Matrix::colMeans(y)
        # PCA
        pcs <- irlba(y, nv=nPcs, nu=0, center=cm, right_only=FALSE,fastpath=TRUE,reorth=TRUE)
        rownames(pcs$v) <- colnames(y)
        pcs$center <- cm
        # row-randomize x to get a sense for the pcs
        m1 <- y
        if (euclidean) {
          #for (i in 1:nrow(m1)) m1[i,] <- m1[i,order(runif(length(m1[i,])))]
          m1@i <- sample(m1@i)
          rpcas <- t(t(m1 %*% pcs$v) - t(pcs$center %*% pcs$v))
          pcs$rsd <- apply(rpcas,2,sd)
          pcs$trsd <- sd(dist(rpcas))
        }

        # sample within-cluster distances (based on main PCA)

        pcas <- as.matrix(t(t(t(t(x[,odgenes])*sf) %*% pcs$v) - t(pcs$center %*% pcs$v)))
        if (verbose) message(".")
        return(list(pcs=pcs, sf=sf, df=df, cells=cells, pcas=pcas, odgenes=odgenes))
      },n.cores=n.cores)
      if (verbose) message(" done\n")
      if (return.pca){
        return(gpcs)
      }

      ivi <- unlist(lapply(gpcs,is.null))
      if (any(ivi)) {
        gpcs <- gpcs[!ivi]
        rgroups <- rgroups[!ivi]
      }


      # calculate cell relevance to each cluster (p_k,i matrix)
      # use global PCA distances
      if (verbose) message("calculating global distances ...")
      gcdist <- as.matrix(dist(gpcs[[1]]$pcas))
      if (verbose) message(" done.\n")

      # for each PCA
        # for each cell, determine p_k_i
        # for each PC
          # determine cell projections
            # subset genes
            # subtract center
            # scale, multiply
          # for each pair, determine cell distances
          # use cell distances to complete weight matrix
          # add to the w*d^2 and w matrices
      # normalize by the sqrt(sum(w))


      if (euclidean) {
        if (verbose) message("calculating local Euclidean distances .")
        dcs <- papply(gpcs,function(p) {
          pk <- rep(1,nrow(p$pcas))
          names(pk) <- rownames(p$pcas)
          nci <- setdiff(rownames(gcdist),p$cells)
          if (length(nci)>0) {
            # determine within cluster sd
            scells <- sample(p$cells,min(1e3,length(p$cells)))
            cldsd <- sd(as.numeric(gcdist[scells,scells]))
            ncid <- rowMeans(gcdist[nci,scells])
            pk[nci] <- exp(-b*(ncid/cldsd)^2)
          }
          dsq <- as.matrix(dist(p$pcas)^2)
          w <- (1-exp(-a*(dsq)/(p$pcs$trsd^2))) * (pk %o% pk)
          if (verbose) message(".")
          list(dsq=dsq,w=w)
        },n.cores=n.cores)
        if (verbose) message(".")
        d <- Reduce('+',lapply(dcs,function(x) x$dsq*x$w))
        if (verbose) message(".")
        d <- sqrt(d/Reduce('+',lapply(dcs,function(x) x$w)))
        diag(d) <- 0
        if (verbose) message(" done.\n")
      } else {
        # weighted correlation
        if (verbose) message("calculating local correlation distances .")
        dcs <- papply(gpcs,function(p) {
          pk <- rep(1,nrow(p$pcas))
          names(pk) <- rownames(p$pcas)
          nci <- setdiff(rownames(gcdist),p$cells)
          if (length(nci)>0) {
            # determine within cluster sd
            scells <- sample(p$cells,min(1e3,length(p$cells)))
            cldsd <- sd(as.numeric(gcdist[scells,scells]))
            ncid <- rowMeans(gcdist[nci,scells])
            pk[nci] <- exp(-b*(ncid/cldsd)^2)
          }
          x <- cov(t(p$pcas))*(ncol(p$pcas)-1)
          xc <- x / sqrt(diag(x) %o% diag(x)) # correlation
          w <- (1-exp(-a*(1-xc))) * (pk %o% pk)

          if (verbose) message(".")
          list(x=x,w=w)
        },n.cores=n.cores)
        if (verbose) message(".")
        # calculate sum_{k}_{w_k*v} matrix
        wm <- Reduce('+',lapply(dcs,function(z) z$w*diag(z$x)))
        d <- Reduce('+',lapply(dcs,function(z) z$x*z$w))
        d <- 1-d/sqrt(wm*t(wm))
        diag(d) <- 0
        if (verbose) message(" done.\n")
      }

      ## d <- dcs[[1]]$dsq
      ## d <- as.matrix(dist(r$reductions$PCA))
      ## knn <- apply(d,2,function(x) order(x,decreasing=F)[1:(k+1)])
      ## cat(".")
      ## m <- sparseMatrix(i=as.numeric(knn),p=c(0,(1:ncol(knn))*nrow(knn)),dims=rep(ncol(knn),2),x=rep(1,nrow(knn)*ncol(knn)))
      ## m <- m+t(m); # symmetrize
      ## diag(m) <- 0;
      ## rownames(m) <- colnames(m) <- rownames(d)
      ## x <- list(m=m)
      ## i <- 5; cl <- rep(NA,nrow(x$m)); names(cl) <- rownames(x$m); cl[rownames(x$m)[i]] <- 1; cl[which(x$m[,i]>0)] <- 2; cl <- as.factor(cl);
      ## r$plotEmbedding(type='PCA',embeddingType='tSNE',groups=cl,alpha=0.2,min.group.size=00,mark.clusters = TRUE, mark.cluster.cex=0.8,unclassified.cell.color=adjustcolor(1,alpha=0.1))

      ## i <- 5; cl <- 1/(dcs[[1]]$dsq[,i]+1e-6); names(cl) <- rownames(x$m);
      ## i <- 5; cl <- 1/(d[,i]+1e-6); names(cl) <- rownames(x$m);
      ## r$plotEmbedding(type='PCA',embeddingType='tSNE',colors=cl,alpha=0.2,min.group.size=00,mark.clusters = TRUE, mark.cluster.cex=0.8,unclassified.cell.color=adjustcolor(1,alpha=0.1))

      # kNN
      if (verbose) message("creating kNN graph .")
      knn <- apply(d,2,function(x) order(x,decreasing=FALSE)[1:(k+1)])
      if (verbose) message(".")
      #m <- sparseMatrix(i=as.numeric(knn),p=c(0,(1:ncol(knn))*nrow(knn)),dims=rep(ncol(knn),2),x=rep(1,nrow(knn)*ncol(knn)))
      m <- sparseMatrix(i=as.numeric(knn),p=c(0,(1:ncol(knn))*nrow(knn)),dims=rep(ncol(knn),2),x=d[as.integer(t(t(knn)+((1:ncol(knn))-1)*nrow(d)))])
      m <- m+t(m) # symmetrize
      diag(m) <- 0
      rownames(m) <- colnames(m) <- rownames(d)
      if (verbose) message(".")
      g <- graph_from_adjacency_matrix(m,mode='undirected',weighted=TRUE)
      if (verbose) message(".")
      self$graphs[[name]] <- g
      if (verbose) message(" done.\n")

      emb <- Rtsne::Rtsne(d,is_distance=TRUE, perplexity=perplexity, num_threads=n.cores)$Y
      rownames(emb) <- colnames(d)
      self$embeddings[[type]][[name]] <- emb

      # calculate cell-cell distance, considering weighting

      # getting total cell-cell distance

      invisible(list(d=d,m=m,gpcs=gpcs))
    },

    ## env - pathway to gene environment

    #' @description Test pathway overdispersion
    #' Note: this is a compressed version of the PAGODA1 approach in SCDE <https://hms-dbmi.github.io/scde/>
    #' 
    #' @param setenv Specific environment for pathway analysis
    #' @param min.pathway.size integer Minimum number of observed genes that should be contained in a valid gene set (default=10)
    #' @param max.pathway.size integer Maximum number of observed genes in a valid gene set (default=1e3)
    #' @param n.randomizations numeric Number of random gene sets (of the same size) to be evaluated in parallel with each gene set (default=5). (This can be kept at 5 or 10, but should be increased to 50-100 if the significance of pathway overdispersion will be determined relative to random gene set models.)
    #' @param score.alpha numeric Significance level of the confidence interval for determining upper/lower bounds (default=0.05)
    #' @param cells character vector Specific cells to investigate (default=NULL)
    #' @param adjusted.pvalues boolean Whether to use adjusted p-values (default=TRUE)
    #' @param z.score numeric Z-score to be used as a cutoff for statistically significant patterns (default=qnorm(0.05/2, lower.tail = FALSE))
    #' @param use.oe.scale boolean Whether the variance of the returned aspect patterns should be normalized using observed/expected value instead of the default chi-squared derived variance corresponding to overdispersion Z-score (default=FALSE)
    #' @param return.table boolean Whether to return a text table with results (default=FALSE)
    #' @param name string Title (default='pathwayPCA')
    #' @param correlation.distance.threshold numeric Similarity threshold for grouping interdependent aspects in pagoda.reduce.redundancy() (default=0.2)
    #' @param loading.distance.threshold numeric Similarity threshold for grouping interdependent aspects in pagoda.reduce.loading.redundancy() (default=0.2)
    #' @param top.aspects Restrict output to the top N aspects of heterogeneity (default=Inf)
    #' @param recalculate.pca boolean Whether to recalculate PCA (default=FALSE)
    #' @param save.pca boolean Whether to save the PCA results (default=TRUE). If TRUE, caches them in self$misc[['pwpca']].
    #'
    #' @return pathway output
    testPathwayOverdispersion=function(setenv, type='counts', max.pathway.size=1e3, min.pathway.size=10, 
      n.randomizations=5, verbose=FALSE, n.cores=self$n.cores, score.alpha=0.05, plot=FALSE, cells=NULL, adjusted.pvalues=TRUE,
      z.score = qnorm(0.05/2, lower.tail = FALSE), use.oe.scale = FALSE, return.table=FALSE, name='pathwayPCA',
      correlation.distance.threshold=0.2, loading.distance.threshold=0.01, top.aspects=Inf, recalculate.pca=FALSE, save.pca=TRUE) {
  
      if (!requireNamespace("scde", quietly=TRUE)){
        stop("You need to install package 'scde' to be able to use testPathwayOverdispersion().")
      }

      nPcs <- 1
      if (type=='counts') {
        x <- self$counts
        # apply scaling if using raw counts
        x@x <- x@x*rep(self$misc[['varinfo']][colnames(x),'gsf'],diff(x@p))
      } else {
        if (!type %in% names(self$reductions)) { stop("Reduction ",type,' not found')}
        x <- self$reductions[[type]]
      }
      if (!is.null(cells)) {
        x <- x[cells,]
      }

      proper.gene.names <- colnames(x)

      if (is.null(self$misc[['pwpca']]) || recalculate.pca) {
        if (verbose) {
          message("determining valid pathways")
        }

        # determine valid pathways
        gsl <- ls(envir = setenv)
        gsl.ng <- unlist(mclapply(sn(gsl), function(go) sum(unique(get(go, envir = setenv)) %in% proper.gene.names),mc.cores=n.cores,mc.preschedule=TRUE))
        gsl <- gsl[gsl.ng >= min.pathway.size & gsl.ng<= max.pathway.size]
        names(gsl) <- gsl

        if (verbose) {
          message("processing ", length(gsl), " valid pathways")
        }

        cm <- Matrix::colMeans(x)

        pwpca <- papply(gsl, function(sn) {
          lab <- proper.gene.names %in% get(sn, envir = setenv)
          if (sum(lab)<1) { 
            return(NULL)
          }
          pcs <- irlba(x[,lab], nv=nPcs, nu=0, center=cm[lab])
          pcs$d <- pcs$d/sqrt(nrow(x))
          pcs$rotation <- pcs$v
          pcs$v <- NULL

          # get standard deviations for the random samples
          ngenes <- sum(lab)
          z <- do.call(rbind,lapply(seq_len(n.randomizations), function(i) {
            si <- sample(ncol(x), ngenes)
            pcs <- irlba(x[,si], nv=nPcs, nu=0, center=cm[si])$d
          }))
          z <- z/sqrt(nrow(x))

          # local normalization of each component relative to sampled PC1 sd
          avar <- pmax(0, (pcs$d^2-mean(z[, 1]^2))/sd(z[, 1]^2))

          if (avar>0.5) {
            # flip orientations to roughly correspond with the means
            pcs$scores <- as.matrix(t(x[,lab] %*% pcs$rotation) - as.numeric((cm[lab] %*% pcs$rotation)))
            cs <- unlist(lapply(seq_len(nrow(pcs$scores)), function(i) sign(cor(pcs$scores[i,], colMeans(t(x[, lab, drop = FALSE])*abs(pcs$rotation[, i]))))))
            pcs$scores <- pcs$scores*cs
            pcs$rotation <- pcs$rotation*cs
            rownames(pcs$rotation) <- colnames(x)[lab]
          } # don't bother otherwise - it's not significant
          return(list(xp=pcs,z=z,n=ngenes))
        }, n.cores = n.cores,mc.preschedule=TRUE)
        if (save.pca) {
          self$misc[['pwpca']] <- pwpca
        }
      } else {
        if (verbose) {
          message("reusing previous overdispersion calculations")
          pwpca <- self$misc[['pwpca']]
        }
      }

      if (verbose) {
        message("scoring pathway od signifcance")
      }

      # score overdispersion
      true.n.cells <- nrow(x)

      pagoda.effective.cells <- function(pwpca, start = NULL) {
        n.genes <- unlist(lapply(pwpca, function(x) rep(x$n, nrow(x$z))))
        var <- unlist(lapply(pwpca, function(x) x$z[, 1]))
        if (is.null(start)) { start <- true.n.cells*2 } # start with a high value
        of <- function(p, v, sp) {
          sn <- p[1]
          vfit <- (sn+sp)^2/(sn*sn+1/2) -1.2065335745820*(sn+sp)*((1/sn + 1/sp)^(1/3))/(sn*sn+1/2)
          residuals <- (v-vfit)^2
          return(sum(residuals))
        }
        x <- nlminb(objective = of, start = c(start), v = var, sp = sqrt(n.genes-1/2), lower = c(1), upper = c(true.n.cells))
        return((x$par)^2+1/2)
      }
      n.cells <- pagoda.effective.cells(pwpca)

      vdf <- data.frame(do.call(rbind, lapply(seq_along(pwpca), function(i) {
        vars <- as.numeric((pwpca[[i]]$xp$d))
        cbind(i = i, var = vars, n = pwpca[[i]]$n, npc = seq(1:ncol(pwpca[[i]]$xp$rotation)))
      })))

      # fix p-to-q mistake in qWishartSpike
      qWishartSpikeFixed <- function (q, spike, ndf = NA, pdim = NA, var = 1, beta = 1, lower.tail = TRUE, log.p = FALSE)  {
        params <- RMTstat::WishartSpikePar(spike, ndf, pdim, var, beta)
        qnorm(q, mean = params$centering, sd = params$scaling, lower.tail, log.p)
      }

      # add right tail approximation to ptw, which gives up quite early
      pWishartMaxFixed <- function (q, ndf, pdim, var = 1, beta = 1, lower.tail = TRUE) {
        params <- RMTstat::WishartMaxPar(ndf, pdim, var, beta)
        q.tw <- (q - params$centering)/(params$scaling)
        p <- RMTstat::ptw(q.tw, beta, lower.tail, log.p = TRUE)
        p[p == -Inf] <- pgamma((2/3)*q.tw[p == -Inf]^(3/2), 2/3, lower.tail = FALSE, log.p = TRUE) + lgamma(2/3) + log((2/3)^(1/3))
        p
      }

      vshift <- 0
      ev <- 0

      vdf$var <- vdf$var-(vshift-ev)*vdf$n
      basevar <- 1
      vdf$exp <- RMTstat::qWishartMax(0.5, n.cells, vdf$n, var = basevar, lower.tail = FALSE)
      #vdf$z <- qnorm(pWishartMax(vdf$var, n.cells, vdf$n, log.p = TRUE, lower.tail = FALSE, var = basevar), lower.tail = FALSE, log.p = TRUE)
      vdf$z <- qnorm(pWishartMaxFixed(vdf$var, n.cells, vdf$n, lower.tail = FALSE, var = basevar), lower.tail = FALSE, log.p = TRUE)
      vdf$cz <- qnorm(bh.adjust(pnorm(as.numeric(vdf$z), lower.tail = FALSE, log.p = TRUE), log = TRUE), lower.tail = FALSE, log.p = TRUE)
      vdf$ub <- RMTstat::qWishartMax(score.alpha/2, n.cells, vdf$n, var = basevar, lower.tail = FALSE)
      vdf$ub.stringent <- RMTstat::qWishartMax(score.alpha/nrow(vdf)/2, n.cells, vdf$n, var = basevar, lower.tail = FALSE)

      if (plot) {
        test_pathway_par <- par(mfrow = c(1, 1), mar = c(3.5, 3.5, 1.0, 1.0), mgp = c(2, 0.65, 0))
        on.exit(par(test_pathway_par))
        un <- sort(unique(vdf$n))
        on <- order(vdf$n, decreasing = FALSE)
        pccol <- colorRampPalette(c("black", "grey70"), space = "Lab")(max(vdf$npc))
        plot(vdf$n, vdf$var/vdf$n, xlab = "gene set size", ylab = "PC1 var/n", ylim = c(0, max(vdf$var/vdf$n)), col = adjustcolor(pccol[vdf$npc],alpha=0.1),pch=19)
        lines(vdf$n[on], (vdf$exp/vdf$n)[on], col = 2, lty = 1)
        lines(vdf$n[on], (vdf$ub.stringent/vdf$n)[on], col = 2, lty = 2)
      }

      rs <- (vshift-ev)*vdf$n
      vdf$oe <- (vdf$var+rs)/(vdf$exp+rs)
      vdf$oec <- (vdf$var+rs)/(vdf$ub+rs)

      df <- data.frame(name = names(pwpca)[vdf$i], npc = vdf$npc, n = vdf$n, score = vdf$oe, z = vdf$z, adj.z = vdf$cz, stringsAsFactors = FALSE)
      if (adjusted.pvalues) {
        vdf$valid <- vdf$cz  >=  z.score
      } else {
        vdf$valid <- vdf$z  >=  z.score
      }

      if (!any(vdf$valid)) { 
        stop("No significantly overdispersed pathways found at z.score threshold of ",z.score) 
      }

      # apply additional filtering based on >0.5 sd above the local random estimate
      vdf$valid <- vdf$valid & unlist(lapply(pwpca,function(x) !is.null(x$xp$scores)))
      vdf$name <- names(pwpca)[vdf$i]

      if (return.table) {
        df <- df[vdf$valid, ]
        df <- df[order(df$score, decreasing = TRUE), ]
        return(df)
      }
      if (verbose) {
        message("compiling pathway reduction")
      }
      # calculate pathway reduction matrix

      # return scaled patterns
      xmv <- do.call(rbind, lapply(pwpca[vdf$valid], function(x) {
        xm <- x$xp$scores
      }))

      if (use.oe.scale) {
        xmv <- (xmv -rowMeans(xmv))* (as.numeric(vdf$oe[vdf$valid])/sqrt(apply(xmv, 1, var)))
        vdf$sd <- as.numeric(vdf$oe)
      } else {
        # chi-squared
        xmv <- (xmv-rowMeans(xmv)) * sqrt((qchisq(pnorm(vdf$z[vdf$valid], lower.tail = FALSE, log.p = TRUE), n.cells, lower.tail = FALSE, log.p = TRUE)/n.cells)/apply(xmv, 1, var))
        vdf$sd <- sqrt((qchisq(pnorm(vdf$z, lower.tail = FALSE, log.p = TRUE), n.cells, lower.tail = FALSE, log.p = TRUE)/n.cells))

      }
      rownames(xmv) <- paste("#PC", vdf$npc[vdf$valid], "# ", names(pwpca)[vdf$i[vdf$valid]], sep = "")
      rownames(vdf) <- paste("#PC", vdf$npc, "# ", vdf$name, sep = "")
      self$misc[['pathwayODInfo']] <- vdf

      # collapse gene loading
      if (verbose) {
        message("clustering aspects based on gene loading ... ",appendLF=FALSE)
      }
      tam2 <- pagoda.reduce.loading.redundancy(list(xv=xmv,xvw=matrix(1,ncol=ncol(xmv),nrow=nrow(xmv))),pwpca,NULL,plot=FALSE,distance.threshold=loading.distance.threshold,n.cores=n.cores)
      if (verbose) {
        message(nrow(tam2$xv)," aspects remaining")
      }
      if (verbose) {
        message("clustering aspects based on pattern similarity ... ",appendLF=FALSE)
      }
      tam3 <- pagoda.reduce.redundancy(tam2, distance.threshold=correlation.distance.threshold,top=top.aspects)
      if (verbose) {
        message(nrow(tam3$xv)," aspects remaining\n")
      }
      tam2$xvw <- tam3$xvw <- NULL # to save space
      tam3$env <- setenv

      # clean up aspect names, as GO ids are meaningless
      names(tam3$cnam) <- rownames(tam3$xv) <- paste0('aspect',1:nrow(tam3$xv))

      self$misc[['pathwayOD']] <- tam3
      self$reductions[[name]] <- tam3$xv
      invisible(tam3)
    },


    #' @description Return embedding
    #' 
    #' @param embeddingType string Type of embedding to construct (default='largeVis'). Possible values are: 'largeVis', 'tSNE', 'FR' (Fruchterman–Reingold), 'UMAP', 'UMAP_graph' 
    #' @param name string Name of the embedding (default=NULL). If NULL, the name = embeddingType.
    #' @param dims integer Parameter 'dims' Matrix::sparseMatrix(); a non-negative, integer, dimensions vector of length 2 (default=2). See Matrix package documentation for more details.
    #' @param M numeric (largeVis) The number of negative edges to sample for each positive edge (default=5). Parameter only used if embeddingType is 'largeVis'.
    #' @param gamma numeric (largeVis) The strength of the force pushing non-neighbor nodes apart (default=7). Parameter only used if embeddingType is 'largeVis'.
    #' @param perplexity numeric Parameter 'perplexity' within largeVis::buildWijMatrix() (default=50). Please see the largeVis documentation for more details.
    #' @param verbose boolean Whether to give verbose output (default=TRUE)
    #' @param sgd_batches numeric The number of edges to process during SGD (default=NULL). Passed to projectKNNs(). Defaults to a value set based on the size of the dataset. If the parameter given is
    #'     between \code{0} and \code{1}, the default value will be multiplied by the parameter.
    #' @param diffusion.steps integer Iteration steps to use. If 0, no steps are run. (default=0)
    #' @param diffusion.power numeric Factor to be used when calculating diffusion, (default=0.5)
    #' @param distance string 'pearson', 'spearman', 'euclidean', 'L2', 'JS' (default='pearson')
    #' @param n.sgd.cores numeric Number of cores to use (default=n.cores)
    #' @param ...  Additional parameters passed to embedding functions, Rtsne::Rtsne() if 'L2', uwot::umap() if 'UMAP', embedKnnGraphUmap() if 'UMAP_graph'
    #' @examples
    #' \donttest{
    #' ## Load pre-generated a dataset of 3000 bone marrow cells as matrix
    #' cm <- p2data::sample_BM1
    #' ## Perform QC, i.e. filter any cells that
    #  ##  don't fit the expected detected gene vs molecule count relationship
    #' counts <- gene.vs.molecule.cell.filter(cm,min.cell.size=500)
    #' rownames(counts) <- make.unique(rownames(counts))
    #' ## Generate Pagoda2 object 
    #' p2_object <- Pagoda2$new(counts,log.scale=TRUE, min.cells.per.gene=10, n.cores=1) 
    #' ## Normalize gene expression variance 
    #' p2_object$adjustVariance(plot=TRUE, gam.k=10)
    #' ## Reduce the dataset dimensions by running PCA
    #' p2_object$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    #' ## Generate a kNN graph of cells that will allow us to identify clusters of cells
    #' p2_object$makeKnnGraph(k=40, type='PCA', center=TRUE, distance='cosine')
    #' ## Call clusters based on kNN
    #' p2_object$getKnnClusters(method=infomap.community, type='PCA')
    #' ## Generate embedding of the data
    #' p2_object$getEmbedding(type='PCA', embeddingType = 'UMAP', M=30, perplexity=30, gamma=1/30)
    #' } 
    #'
    #' @return embedding stored in self$embedding
    getEmbedding=function(type='counts', embeddingType='largeVis', name=NULL, dims=2, M=1, gamma=1/M, perplexity=50, verbose=TRUE,
      sgd_batches=NULL, diffusion.steps=0, diffusion.power=0.5, distance='pearson', n.cores = self$n.cores, n.sgd.cores=n.cores, ... ) {
      
      if (dims<1) {
        stop("Dimensions parameter 'dims' must be >=1")
      }
      if (type=='counts') {
        x <- self$counts
      } else {
        if (!type %in% names(self$reductions)) { 
          stop("Reduction ",type,' not found')
        }
        x <- self$reductions[[type]]
      }
      if (is.null(name)) { 
        name <- embeddingType 
      }

      if (embeddingType=='largeVis') {
        edgeMat <- self$misc[['edgeMat']][[type]]
        if (is.null(edgeMat)){ 
          stop(paste0('KNN graph for type ',type,' not found. Please run makeKnnGraph with type=',type)) 
        }
        if (is.null(sgd_batches)){ 
          sgd_batches <- nrow(edgeMat)*1e3 
        }
        #edgeMat <- sparseMatrix(i=xn$s+1,j=xn$e+1,x=xn$rd,dims=c(nrow(x),nrow(x)))
        edgeMat <- (edgeMat + t(edgeMat))/2 # symmetrize
        #edgeMat <- sparseMatrix(i=c(xn$s,xn$e)+1,j=c(xn$e,xn$s)+1,x=c(xn$rd,xn$rd),dims=c(nrow(x),nrow(x)))
        # if(diffusion.steps>0) {
        #   Dinv <- Diagonal(nrow(edgeMat),1/colSums(edgeMat))
        #   Im <- Diagonal(nrow(edgeMat))
        #   W <- (Diagonal(nrow(edgeMat)) + edgeMat %*% Dinv)/2
        #   for(i in 1:diffusion.steps) {
        #     edgeMat <- edgeMat %*% W
        #   }
        # }
        #require(largeVis)
        #if(!is.null(seed)) { set.seed(seed) }
        if (!is.na(perplexity)) {
          wij <- buildWijMatrix(edgeMat, perplexity=perplexity, threads=n.cores)
        } else {
          wij <- edgeMat
        }

        if (diffusion.steps>0) {
          Dinv <- Diagonal(nrow(wij),1/colSums(wij))
          W <- Dinv %*% wij 
          W <- 
          #W <- (Diagonal(nrow(wij)) + W)/2
          #W <- (Diagonal(nrow(wij)) + sign(W)*(abs(W)^(diffusion.power)))/2

          #W <- sign(W)*(abs(W)^diffusion.power)
          #W <- (Diagonal(nrow(wij)) + W)/2
          for(i in 1:diffusion.steps) {
            wij <- wij %*% W
          }
          if (!is.na(perplexity)) {
            wij <- buildWijMatrix(wij, perplexity=perplexity, threads=n.cores)
          }
          
        }
        coords <- projectKNNs(wij = wij, M = M, dim=dims, verbose = verbose, sgd_batches = sgd_batches, gamma=gamma, seed=1, threads=n.cores, ...)
        colnames(coords) <- rownames(x)
        emb <- t(coords)
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType=='tSNE') {
        if (nrow(x)>4e4) {
          warning('Too many cells to pre-calculate correlation distances, switching to L2. Please consider using UMAP.')
          distance <- 'L2'
        }
        
        dup.ids <- which(duplicated(x))
        if (length(dup.ids) > 0) {
          max.vals <- abs(x[dup.ids,] * 0.01)
          x[dup.ids,] <- runif(length(x[dup.ids,]), -max.vals, max.vals)
        }
        
        if (distance=='L2') {
          if (verbose) message("running tSNE using ",n.cores," cores:\n")
          emb <- Rtsne::Rtsne(x, perplexity=perplexity, dims=dims, num_threads=n.cores, ... )$Y
        } else {
          if (verbose) message('calculating distance ... ')
          if (verbose) message('pearson ...')
          d <- 1-cor(t(x))
          if (verbose) message("running tSNE using ",n.cores," cores:\n")
          emb <- Rtsne::Rtsne(d, is_distance=TRUE, perplexity=perplexity, dims=dims, num_threads=n.cores, ... )$Y
        }
        rownames(emb) <- rownames(x)
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType=='FR') {
        g <- self$graphs[[type]]
        if (is.null(g)){ 
          stop(paste0("Generate kNN graph first (type=",type,")"))
        }
        emb <- layout.fruchterman.reingold(g, weights=E(g)$weight)
        rownames(emb) <- rownames(x)
        colnames(emb) <- c("D1","D2")
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType == "UMAP") {
        if (!requireNamespace("uwot", quietly=TRUE)){
          stop("You need to install package 'uwot' to be able to use UMAP embedding.")
        }
        
        distance <- switch(distance, pearson = "cosine", L2 = "euclidean", distance)
        
        emb <- uwot::umap(as.matrix(x), metric=distance, verbose=verbose, n_threads=n.cores, n_sgd_threads=n.sgd.cores, n_components=dims, ...)
        rownames(emb) <- rownames(x)
        self$embeddings[[type]][[name]] <- emb
      } else if (embeddingType == "UMAP_graph") {
        g <- self$graphs[[type]]
        if (is.null(g)){ 
          stop(paste0("generate kNN graph first (type=",type,")"))
        }
        emb <- embedKnnGraphUmap(g, verbose=verbose, n_threads=n.cores, n_sgd_threads=n.sgd.cores, n_components=dims, ...)
        self$embeddings[[type]][[name]] <- emb
      } else {
        stop('Unknown embeddingType ',embeddingType,' specified')
      }

      invisible(emb)
     }
  )

)