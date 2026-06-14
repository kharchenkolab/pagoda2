# Prep artifacts for the LARGE Tabula Muris Senis (droplet) benchmark: 245,389 cells x 20,138 genes.
# Reads the h5ad X via hdf5r (no BPCells), fits a single adjustVariance, derives a real grouping from
# obs, and persists the shared artifacts. The lstar-zarr store already exists (scale_stream.lstar.zarr,
# field "X", streaming-converted by lstar::convert_anndata) — no per-op kernel, same data as the golden.
suppressMessages({library(pagoda2); library(Matrix)})
H5  <- Sys.getenv("DATASET", "/tmp/scale_245389.h5ad")
OUT <- Sys.getenv("BENCH_DIR", "/tmp/bench_tms")
GRP <- Sys.getenv("GRP_COL", "cell_ontology_class")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
t0 <- proc.time()

read_h5ad_gxc <- function(h5) {                          # X (CSR cells x genes) -> CSC genes x cells
  f <- hdf5r::H5File$new(h5, "r"); on.exit(f$close_all())
  sh <- hdf5r::h5attributes(f[["X"]])[["shape"]]
  pick <- function(grp) { nm <- names(f[[grp]]); fld <- if ("_index" %in% nm) "_index" else "index"; f[[paste0(grp, "/", fld)]]$read() }
  g <- new("dgCMatrix", i = as.integer(f[["X/indices"]]$read()), p = as.integer(f[["X/indptr"]]$read()),
           x = as.numeric(f[["X/data"]]$read()), Dim = c(as.integer(sh[2]), as.integer(sh[1])),
           Dimnames = list(pick("var"), pick("obs")))
  g
}
read_obs_factor <- function(h5, col) {                   # anndata categorical (codes/categories) or plain
  f <- hdf5r::H5File$new(h5, "r"); on.exit(f$close_all())
  p <- paste0("obs/", col)
  ok <- tryCatch({ f[[p]]; TRUE }, error = function(e) FALSE); if (!ok) return(NULL)
  o <- f[[p]]
  if (inherits(o, "H5Group") && all(c("codes", "categories") %in% names(o))) {
    codes <- o[["codes"]]$read(); cats <- as.character(o[["categories"]]$read()); factor(cats[codes + 1L], levels = cats)
  } else factor(as.character(o$read()))
}

raw_gxc <- read_h5ad_gxc(H5)
cat(sprintf("read X %d x %d (%.0fM nnz) in %.1fs\n", nrow(raw_gxc), ncol(raw_gxc), length(raw_gxc@x)/1e6, (proc.time()-t0)[3]))

p2 <- Pagoda2$new(raw_gxc, modelType = "plain", log.scale = TRUE, n.cores = 8, verbose = FALSE,
                  min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0)   # keep all genes/cells in store order
p2$adjustVariance(plot = FALSE, verbose = FALSE, n.cores = 8)
recipe  <- p2$getMatrixView("analysis")[c("model", "depthScale", "depth", "log.scale")]
varinfo <- p2$misc$varinfo; odgenes <- p2$misc$odgenes
cat(sprintf("adjustVariance: %d genes, %d odgenes  (%.1fs)\n", nrow(varinfo), length(odgenes), (proc.time()-t0)[3]))

grp <- read_obs_factor(H5, GRP)
if (is.null(grp) || nlevels(grp) < 2) { set.seed(1); grp <- factor(sample(paste0("grp", 1:18), ncol(raw_gxc), replace = TRUE)) }
grp <- grp[match(rownames(p2$rawCounts), colnames(raw_gxc))]
cat(sprintf("grouping '%s': %d levels\n", GRP, nlevels(grp)))
genes20 <- head(rownames(varinfo)[order(varinfo$lp)], 20)

saveRDS(as(p2$rawCounts, "dgCMatrix"), file.path(OUT, "raw_cxg.rds"))   # cells x genes (Mem source)
saveRDS(recipe,  file.path(OUT, "recipe.rds"));  saveRDS(varinfo, file.path(OUT, "varinfo.rds"))
saveRDS(odgenes, file.path(OUT, "odgenes.rds")); saveRDS(grp, file.path(OUT, "grp.rds"))
saveRDS(genes20, file.path(OUT, "genes20.rds"))
cat(sprintf("saved to %s in %.1fs;  raw_cxg.rds %.0f MB\n", OUT, (proc.time()-t0)[3], file.size(file.path(OUT,"raw_cxg.rds"))/1e6))
