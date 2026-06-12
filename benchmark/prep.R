# Phase A prep: build the in-memory baseline once and persist shared artifacts (raw dgCMatrix,
# normalization recipe, varinfo/odgenes from a SINGLE adjustVariance fit, grouping, op4 genes).
# All backends reuse these so the comparison is storage-only, never a re-fit.
suppressMessages({library(pagoda2); library(Matrix); library(BPCells)})
H5 <- "/tmp/marrow_raw.h5ad"; OUT <- "/tmp/bench"
t0 <- proc.time()
m <- open_matrix_anndata_hdf5(H5)                       # genes x cells, on disk
raw_gxc <- as(m, "dgCMatrix")                           # materialize once (genes x cells) for the Mem build
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
