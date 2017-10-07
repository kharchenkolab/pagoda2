#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
#' @import GO.db
# @import BiocGenerics
# @import AnnotationDbi
#' @import parallel
NULL

#' Generates human go annotation for the web object
#' @description generates a humna go annotation for the pagoda2 web object
#' @param gene.names a character vector of genes to include
#' @export p2.generate.human.go.web
p2.generate.human.go.web <- function(gene.names, n.cores = 1) {
  require(org.Hs.eg.db)
  p2.generate.go.web(gene.names = gene.names,  egALIAS2EG = org.Hs.egALIAS2EG, egGO2ALLEGS = org.Hs.egGO2ALLEGS, n.cores = n.cores);
}

#' Generates mouse go annotation for the web object
#' @description generates a humna go annotation for the pagoda2 web object
#' @param gene.names a character vector of genes to include
#' @export p2.generate.mouse.go.web
p2.generate.mouse.go.web <- function(gene.names, n.cores = 1) {
  require(org.Mm.eg.db)
  p2.generate.go.web(gene.names = gene.names,  egALIAS2EG = org.Mm.egALIAS2EG, egGO2ALLEGS = org.Mm.egGO2ALLEGS, n.cores = n.cores);
}


#' Generates human go annotation for the web object for any species
#' @description generates a humna go annotation for the pagoda2 web object
#' @param gene.names a character vector of genes to include
#' @export p2.generate.human.go.web
p2.generate.go.web  <-                                                                                                                                                                                   
  function (gene.names, egALIAS2EG = NULL, egGO2ALLEGS = NULL, n.cores = 1)                                                                                                                            
  {                                                                                                                                                                                                        
    require(GO.db)                                                                                                                                                                                       
    require(BiocGenerics)                                                                                                                                                                                
    require(AnnotationDbi)                                                                                                                                                                               
    require(parallel)                                                                                                                                                                                    
    if (is.null(egALIAS2EG)) {                                                                                                                                                                           
      stop("egALIAS2EG cannot be null, it has to be an object like org.Hs.egALIAS2EG")                                                                                                                 
    }                                                                                                                                                                                                    
    if (is.null(org.Hs.egGO2ALLEGS)) {                                                                                                                                                                   
      stop("org.Hs.egGO2ALLEGS cannot be null it has to be an object like org.Hs.egGO2ALLEGS")                                                                                                         
    }                                                                                                                                                                                                    
    if (!is.character(gene.names)) {                                                                                                                                                                     
      stop("gene.names needs to be a character vector of gene names")                                                                                                                                  
    }                                                                                                                                                                                                    
    
    ids <- unlist(mclapply(mget(myGeneNames, org.Hs.egALIAS2EG, ifnotfound = NA), function(x) x[1], mc.cores = n.cores))                                                                                 
    rids <- names(ids)                                                                                                                                                                                   
    names(rids) <- ids                                                                                                                                                                                   
    go.env <- AnnotationDbi::eapply(org.Hs.egGO2ALLEGS, function(x) as.character(na.omit(rids[x])))                                                                                                      
    go.env <- go.env[unlist(lapply(go.env, length)) > 5]                                                                                                                                                 
    
    ## Put the GO Term annotation generated in a format suitable for the web interface                                                                                                                   
    nms <- names(go.env);                                                                                                                                                                                
    names(nms) <- nms;                                                                                                                                                                                   
    geneSets <- lapply(names(go.env), function(x) {                                                                                                                                                      
      list(                                                                                                                                                                                            
        properties = list(                                                                                                                                                                           
          locked = T,                                                                                                                                                                              
          genesetname = x,                                                                                                                                                                         
          shortdescription = GOTERM[[x]]@Term                                                                                                                                                      
        ),                                                                                                                                                                                           
        genes = c(go.env[[x]])                                                                                                                                                                       
      )                                                                                                                                                                                                
    })                                                                                                                                                                                                   
    
    invisible(geneSets)                                                                                                                                                                                         
  }   
