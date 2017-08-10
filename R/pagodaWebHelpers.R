#' @export p2.generate.human.go.web
p2.generate.human.go.web <- function(myGeneNames) {
  require(org.Hs.eg.db)
  require(GO.db)
  require(BiocGenerics)
  require(AnnotationDbi)
  require(parallel)

  ids <- unlist(mclapply(BiocGenerics::mget(myGeneNames, org.Hs.egALIAS2EG, ifnotfound = NA), function(x) x[1]))
  rids <- names(ids)
  names(rids) <- ids

  go.env <- AnnotationDbi::eapply(org.Hs.egGO2ALLEGS, function(x) as.character(na.omit(rids[x])))

  go.env <- go.env[unlist(lapply(go.env, length)) > 5]

  # TODO make this parallel with mcmapply
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

  names(geneSets) <- names(go.env)

  geneSets
}
