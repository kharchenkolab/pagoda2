# Phase A prep: build the in-memory baseline once and persist shared artifacts (raw dgCMatrix,
# normalization recipe, varinfo/odgenes from a SINGLE adjustVariance fit, grouping, op4 genes).
# All backends reuse these so the comparison is storage-only, never a re-fit.
suppressMessages({library(pagoda2); library(Matrix)})
H5 <- "/tmp/marrow_raw.h5ad"; OUT <- "/tmp/bench"
t0 <- proc.time()
## Read the anndata h5ad directly with hdf5r (no BPCells). X is a CSR cells x genes matrix, whose
## (data, indices=gene, indptr=cell) slots ARE the CSC of the genes x cells transpose -> build it directly.
read_h5ad_gxc <- function(h5) {
  f <- hdf5r::H5File$new(h5, "r"); on.exit(f$close_all())
  sh <- hdf5r::h5attributes(f[["X"]])[["shape"]]        # c(ncells, ngenes)
  pick <- function(grp) { nm <- names(f[[grp]]); fld <- if ("_index" %in% nm) "_index" else "index"; f[[paste0(grp, "/", fld)]]$read() }
  new("dgCMatrix", i = as.integer(f[["X/indices"]]$read()), p = as.integer(f[["X/indptr"]]$read()),
      x = as.numeric(f[["X/data"]]$read()), Dim = c(as.integer(sh[2]), as.integer(sh[1])),
      Dimnames = list(pick("var"), pick("obs")))
}
raw_gxc <- read_h5ad_gxc(H5)                            # genes x cells, materialized once for the Mem build
cat(sprintf("materialized raw %d x %d (%.0fM nnz) in %.1fs\n", nrow(raw_gxc), ncol(raw_gxc),
            length(raw_gxc@x)/1e6, (proc.time()-t0)[3]))

p2 <- Pagoda2$new(raw_gxc, modelType="plain", log.scale=TRUE, n.cores=8, verbose=FALSE)
p2$adjustVariance(plot=FALSE, verbose=FALSE, n.cores=8)
recipe <- p2$getMatrixView("analysis")[c("model","depthScale","depth","log.scale")]
varinfo <- p2$misc$varinfo; odgenes <- p2$misc$odgenes
cat(sprintf("adjustVariance: %d genes, %d odgenes\n", nrow(varinfo), length(odgenes)))

grp.tab <- read.delim("/tmp/marrow_grp.tsv", stringsAsFactors=FALSE)
grp <- factor(grp.tab$grp[match(rownames(p2$rawCounts), grp.tab$cell)])
genes20 <- head(rownames(varinfo)[order(varinfo$lp)], 20)   # 20 most overdispersed genes (deterministic)

saveRDS(as(p2$rawCounts, "dgCMatrix"), file.path(OUT,"raw_cxg.rds"))   # cells x genes (Mem backend source)
saveRDS(recipe,  file.path(OUT,"recipe.rds"))
saveRDS(varinfo, file.path(OUT,"varinfo.rds"))
saveRDS(odgenes, file.path(OUT,"odgenes.rds"))
saveRDS(grp,     file.path(OUT,"grp.rds"))
saveRDS(genes20, file.path(OUT,"genes20.rds"))
cat(sprintf("saved artifacts to %s in %.1fs total\n", OUT, (proc.time()-t0)[3]))
cat("raw_cxg.rds size:", round(file.size(file.path(OUT,"raw_cxg.rds"))/1e6), "MB\n")
