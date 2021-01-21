#' @importFrom parallel mclapply
NULL

#' Generates human GO annotation for the web object
#' 
#' @param gene.names a character vector of genes to include
#' @param n.cores numeric Number of cores to use (default=1)
#' @keywords internal
p2.generate.human.go.web <- function(gene.names, n.cores = 1) {
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    stop("Package \"org.Hs.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
  }

  p2.generate.go.web(gene.names = gene.names,  egALIAS2EG = org.Hs.eg.db::org.Hs.egALIAS2EG, 
                     egGO2ALLEGS = org.Hs.eg.db::org.Hs.egGO2ALLEGS, n.cores = n.cores)
}

#' Generates mouse (Mus musculus) GO annotation for the web object
#' 
#' @param gene.names a character vector of genes to include
#' @param n.cores numeric Number of cores to use (default=1)
#' @keywords internal
p2.generate.mouse.go.web <- function(gene.names, n.cores = 1) {
  if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
    stop("Package \"org.Mm.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
  }

  p2.generate.go.web(gene.names = gene.names,  egALIAS2EG = org.Mm.eg.db::org.Mm.egALIAS2EG, 
                     egGO2ALLEGS = org.Mm.eg.db::org.Mm.egGO2ALLEGS, n.cores = n.cores)
}

#' Generates zebrafish (Danio rerio) GO annotation for the web object
#' 
#' @param gene.names a character vector of genes to include
#' @param n.cores numeric Number of cores to use (default=1)
#' @keywords internal
p2.generate.dr.go.web <- function(gene.names, n.cores = 1) {
  if (!requireNamespace("org.Dr.eg.db", quietly = TRUE)) {
    stop("Package \"org.Dr.eg.db\" needed for this function to work. Please install it.", call. = FALSE)
  }

  p2.generate.go.web(gene.names = gene.names,  egALIAS2EG = org.Dr.eg.db::org.Dr.egALIAS2EG, egGO2ALLEGS = org.Dr.eg.db::org.Dr.egGO2ALLEGS, n.cores = n.cores)
}


#' Generates GO annotation for the web object for any species
#' 
#' @param gene.names a character vector of genes to include
#' @param egALIAS2EG (default=NULL)
#' @param egGO2ALLEGS (default=NULL)
#' @param n.cores numeric Number of cores to use (default=1)
#' @keywords internal
p2.generate.go.web  <- function (gene.names, egALIAS2EG = NULL, egGO2ALLEGS = NULL, n.cores = 1) {
  if (!requireNamespace("GO.db", quietly = TRUE)) {
    stop("Package \"GO.db\" needed for this function to work. Please install it with `BiocManager::install('GO.db')`.", call. = FALSE)
  }

  if (is.null(egALIAS2EG)) {
    stop("egALIAS2EG cannot be null, it has to be an object like org.Hs.egALIAS2EG")
  }

  if (!is.character(gene.names)) {
    stop("gene.names needs to be a character vector of gene names")
  }

  if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
    stop("Package \"AnnotationDbi\" needed for this function to work. Please install it with `BiocManager::install('AnnotationDbi')`.", call. = FALSE)
  }

  ids <- unlist(mclapply(AnnotationDbi::mget(gene.names, egALIAS2EG, ifnotfound = NA), function(x) x[1], mc.cores = n.cores))
  rids <- names(ids)
  names(rids) <- ids
  go.env <- AnnotationDbi::eapply(egGO2ALLEGS, function(x) as.character(na.omit(rids[x])))
  go.env <- go.env[unlist(lapply(go.env, length)) > 5]

  ## Put the GO Term annotation generated in a format suitable for the web interface
  nms <- names(go.env)
  names(nms) <- nms
  geneSets <- lapply(nms, function(x) {
    list(
      properties = list(
        locked = TRUE,
        genesetname = x,
        shortdescription = GO.db::GOTERM[[x]]@Term
      ),
      genes = c(go.env[[x]])
    )
  })

  invisible(geneSets)
}

#' Generates GO annotation for the web object from the GO environment used for enrichment analysis
#'
#' @param go.env GO enviroment generated with p2.generate.go
#' @keywords internal
p2.generate.go.web.fromGOEnv <- function(go.env){
  go.env <- as.list(go.env)

  ## Put the GO Term annotation generated in a format suitable for the web interface
  nms <- names(go.env)
  names(nms) <- nms
  geneSets <- lapply(nms, function(x) {
    list(
      properties = list(
        locked = TRUE,
        genesetname = x,
        shortdescription = GO.db::GOTERM[[x]]@Term
      ),
      genes = c(go.env[[x]])
    )
  })

  invisible(geneSets)
}
