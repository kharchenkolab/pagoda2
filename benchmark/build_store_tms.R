# Build a gene-major CSC lstar store for the TMS benchmark via the standard lstar::lstar_write.
# lstar's streaming convert_anndata preserves the h5ad's CSR (cell-major) layout, but the fused per-gene
# reducers (stream_col_stats / lstar_stream_col_sum_by_group) require CSC (gene-major). An R dgCMatrix is
# column-compressed, so a cells x genes matrix is already gene-contiguous -> lstar encodes it gene-major.
# (A gene-major / transpose option on the streaming converter would let this be done streaming too.)
.libPaths(c(.libPaths(), "/home/pkharchenko/p21/lstar/.Rlib"))
suppressMessages({library(lstar); library(Matrix)})
DIR   <- Sys.getenv("BENCH_DIR", "/tmp/bench_tms")
STORE <- Sys.getenv("BENCH_STORE", "/tmp/tms245k_csc.lstar.zarr")
raw <- readRDS(file.path(DIR, "raw_cxg.rds"))            # cells x genes dgCMatrix (CSC = gene-contiguous)
unlink(STORE, recursive = TRUE)
ds <- list(kind = "sample",
  axes = list(
    cells = list(labels = rownames(raw), origin = "observed", role = "observation"),
    genes = list(labels = colnames(raw), origin = "observed", role = "feature")),
  fields = list(counts = list(values = raw, role = "measure", span = c("cells", "genes"),
                state = "raw", encoding = "csc",
                provenance = list(facet = "RNA", feature_axis = "genes", model = "plain", defaultReduction = "PCA"))))
class(ds) <- "lstar_dataset"
# chunk_elems is essential: a single monolithic chunk defeats block streaming. ~2M nnz/chunk (as the
# lstar streaming converter uses) lets stream_col_stats read gene blocks chunk-by-chunk and thread.
t <- system.time(lstar::lstar_write(ds, STORE, chunk_elems = 2000000L))
cat(sprintf("wrote %s (%d x %d, %.0fM nnz) in %.1fs\n", STORE, nrow(raw), ncol(raw), length(raw@x)/1e6, t[["elapsed"]]))
# sanity: the per-gene fused reducer must now run (needs gene-major CSC)
s <- lstar::stream_col_stats(STORE, "counts", block = 2048L, n_threads = 8L, lognorm = TRUE,
                             depth = as.numeric(readRDS(file.path(DIR, "recipe.rds"))$depth[rownames(raw)]),
                             depthScale = readRDS(file.path(DIR, "recipe.rds"))$depthScale, population = TRUE)
cat(sprintf("stream_col_stats OK: %d gene means\n", length(s$mean)))
