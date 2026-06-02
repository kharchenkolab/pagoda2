.pagoda2_global_variables <- c(
  "component",
  "curve",
  "percent",
  "qc_gene_molecule_fitted",
  "qc_gene_molecule_lower",
  "qc_gene_molecule_upper",
  "qc_log_genes",
  "qc_log_molecules",
  "qc_pass",
  "value"
)
utils::globalVariables(.pagoda2_global_variables)

.onLoad <- function(libname, pkgname) {
  Pagoda2$from <- pagoda2From
  Pagoda2$from10x <- pagoda2From10x
  Pagoda2$from10xH5 <- pagoda2From10xH5
  Pagoda2$fromAnnData <- pagoda2FromAnnData
  Pagoda2$fromH5Seurat <- pagoda2FromH5Seurat
  Pagoda2$fromLoom <- pagoda2FromLoom
}
