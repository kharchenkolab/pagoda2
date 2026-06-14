# Build per-cell-count artifacts for the scaling study by subsampling the 245k TMS data (so HVG/recipe
# are controlled; only the cell count varies). For each N: raw_cxg.rds + recipe (depth subset) + a chunked
# CSC lstar store; varinfo/odgenes/grp shared. The top point (245389) reuses the existing /tmp/bench_tms.
.libPaths(c(.libPaths(), "/home/pkharchenko/p21/lstar/.Rlib"))
suppressMessages({library(Matrix); library(lstar)})
big <- "/tmp/bench_tms"
raw <- readRDS(file.path(big, "raw_cxg.rds"))
recipe <- readRDS(file.path(big, "recipe.rds")); varinfo <- readRDS(file.path(big, "varinfo.rds"))
odgenes <- readRDS(file.path(big, "odgenes.rds")); genes20 <- readRDS(file.path(big, "genes20.rds"))
grp <- readRDS(file.path(big, "grp.rds"))
sizes <- as.integer(strsplit(Sys.getenv("SCALE_SIZES", "5000,20000,40000,80000,160000"), ",")[[1]])
for (N in sizes) {
  set.seed(1); idx <- sort(sample(nrow(raw), N))
  d <- file.path("/tmp/bench_scale", as.character(N)); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  rawN <- raw[idx, , drop = FALSE]
  saveRDS(rawN, file.path(d, "raw_cxg.rds"), compress = FALSE)        # uncompressed: fast save/load
  recN <- recipe; recN$depth <- recipe$depth[rownames(rawN)]
  saveRDS(recN, file.path(d, "recipe.rds")); saveRDS(varinfo, file.path(d, "varinfo.rds"))
  saveRDS(odgenes, file.path(d, "odgenes.rds")); saveRDS(genes20, file.path(d, "genes20.rds"))
  saveRDS(grp[idx], file.path(d, "grp.rds"))
  ds <- list(kind = "sample",
    axes = list(cells = list(labels = rownames(rawN), origin = "observed", role = "observation"),
                genes = list(labels = colnames(rawN), origin = "observed", role = "feature")),
    fields = list(counts = list(values = rawN, role = "measure", span = c("cells", "genes"),
                  state = "raw", encoding = "csc",
                  provenance = list(facet = "RNA", feature_axis = "genes", model = "plain", defaultReduction = "PCA"))))
  class(ds) <- "lstar_dataset"
  st <- file.path(d, "store.lstar.zarr"); unlink(st, recursive = TRUE)
  lstar::lstar_write(ds, st, chunk_elems = 2000000L)
  cat(sprintf("N=%d: raw %.0f MB + chunked store\n", N, file.size(file.path(d, "raw_cxg.rds")) / 1e6))
}
