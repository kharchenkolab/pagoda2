#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
#' @import GO.db
#' @import BiocGenerics
#' @import AnnotationDbi
#' @import parallel
NULL

#' Generates human go annotation for the web object
#' @description generates a humna go annotation for the pagoda2 web object
#' @param gene.names a character vector of genes to include
#' @export p2.generate.human.go.web
p2.generate.human.go.web <- function(gene.names) {
  require(org.Hs.eg.db)
  p2.generate.go.web(gene.names = gene.names,  egALIAS2EG = org.Hs.egALIAS2EG, egGO2ALLEGS = org.Hs.egGO2ALLEGS);
}

#' Generates mouse go annotation for the web object
#' @description generates a humna go annotation for the pagoda2 web object
#' @param gene.names a character vector of genes to include
#' @export p2.generate.mouse.go.web
p2.generate.mouse.go.web <- function(gene.names) {
  require(org.Mm.eg.db)
  p2.generate.go.web(gene.names = gene.names,  egALIAS2EG = org.Mm.egALIAS2EG, egGO2ALLEGS = org.Mm.egGO2ALLEGS);
}


#' Generates human go annotation for the web object for any species
#' @description generates a humna go annotation for the pagoda2 web object
#' @param gene.names a character vector of genes to include
#' @export p2.generate.human.go.web
p2.generate.go.web <- function(gene.names, egALIAS2EG = NULL, egGO2ALLEGS = NULL) {
  require(GO.db)
  require(BiocGenerics)
  require(AnnotationDbi)
  require(parallel)

  if (is.null(egALIAS2EG)) {
    stop('egALIAS2EG cannot be null, it has to be an object like org.Hs.egALIAS2EG');
  }

  if (is.null(org.Hs.egGO2ALLEGS)) {
    stop('org.Hs.egGO2ALLEGS cannot be null it has to be an object like org.Hs.egGO2ALLEGS');
  }

  if (!is.character(gene.names)) {
    stop("gene.names needs to be a character vector of gene names");
  }

  ids <- unlist(mclapply(BiocGenerics::mget(gene.names, , ifnotfound = NA), function(x) x[1]))

  # Swap names and ids
  rids <- names(ids)
  names(rids) <- ids

  # Get go environment
  go.env <- AnnotationDbi::eapply(egGO2ALLEGS, function(x) as.character(na.omit(rids[x])))

  # Filter for go terms with at least 5 genes
  go.env <- go.env[unlist(lapply(go.env, length)) > 5]

  # TODO make this parallel
  geneSets <- lapply(names(go.env), function(x) {
    list(
      properties = list(
        locked = T,
        genesetname = x,
        shortdescription = GO.db::GOTERM[[x]]@Term
      ),
      genes = c(go.env[[x]])
    )
  })

  # Name the genesets
  names(geneSets) <- names(go.env)

  # return geneSets
  invisible(geneSets)
}

