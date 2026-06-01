.onLoad <- function(libname, pkgname) {
  Pagoda2$from <- pagoda2From
  Pagoda2$from10x <- pagoda2From10x
  Pagoda2$from10xH5 <- pagoda2From10xH5
  Pagoda2$fromAnnData <- pagoda2FromAnnData
  Pagoda2$fromH5Seurat <- pagoda2FromH5Seurat
  Pagoda2$fromLoom <- pagoda2FromLoom
}
