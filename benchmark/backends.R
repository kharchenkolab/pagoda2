## Disk-backed count backends for pagoda2.1 — one interface, two storage implementations: the in-memory
## golden (dgCMatrix + pagoda2 C++ view kernels) and the shipped lstar-zarr backing (chunked CSC store +
## pagoda2's own kernels driven per block). Both reuse pagoda2's view kernels / recipe so the normalized
## values are identical (storage is the only thing that varies).
suppressMessages({library(Matrix)})

## the "plain" view value is log1p(x * depthScale / depth_cell); recipe = {depthScale, depth, log.scale}
.view_kernel_args <- function(raw, recipe) {
  list(depth = as.numeric(recipe$depth[rownames(raw)]), depthScale = recipe$depthScale,
       normalize = TRUE, log.scale = isTRUE(recipe$log.scale),
       batch = integer(0), batchFactors = matrix(numeric(), 0, 0),
       winsorCaps = numeric(), preWinsorDepth = numeric(), postWinsorDepth = numeric())
}

## ---- Mem backend: in-memory dgCMatrix (cells x genes) + pagoda2 C++ view kernels (the golden) ----
backend_mem <- function(art) {
  raw <- art$raw_cxg                                   # cells x genes dgCMatrix
  list(
    name = "mem",
    col_mean_var = function(recipe, n.cores = 1) {
      a <- .view_kernel_args(raw, recipe)
      r <- pagoda2:::colMeanVarView(raw, NULL, a$depth, a$depthScale, a$normalize, a$log.scale,
                                    a$batch, a$batchFactors, a$winsorCaps, a$preWinsorDepth,
                                    a$postWinsorDepth, 0L, numeric(0), numeric(0), n.cores)
      data.frame(m = r$m, v = r$v, nobs = r$nobs, row.names = colnames(raw))
    },
    col_sum_by_group = function(recipe, groups, n.cores = 1) {
      a <- .view_kernel_args(raw, recipe)
      cols <- as.integer(groups)                        # 1..nlev, NA stays NA (kernel row 0 = <NA>)
      out <- pagoda2:::colSumByFacView(raw, cols, a$depth, a$depthScale, a$normalize, a$log.scale,
                                       a$batch, a$batchFactors, a$winsorCaps, a$preWinsorDepth,
                                       a$postWinsorDepth, 0L, numeric(0), numeric(0), n.cores)
      rownames(out) <- c("<NA>", levels(groups)); colnames(out) <- colnames(raw); out
    },
    materialize_block = function(recipe, genes, scale.variance = FALSE, varinfo = NULL) {
      sub <- raw[, genes, drop = FALSE]
      x <- pagoda2:::.pagoda2_materialize_view(sub, list(model = recipe$model %||% "plain",
            depth = recipe$depth, depthScale = recipe$depthScale, log.scale = recipe$log.scale,
            batchFactors = NULL, winsorCaps = NULL))
      if (scale.variance) x <- pagoda2:::.pagoda2_apply_variance_scaling(x, varinfo)
      x
    }
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- Zarr backend: lstar chunked CSC store + pagoda2's OWN view kernels, driven per gene block ----
## Demonstrates the intent: lstar provides ONE general primitive (lstar_read_block); the consumer
## (pagoda2) streams gene blocks off disk and applies its existing kernels -> identical math to Mem,
## with no per-op kernel living inside lstar.
backend_zarr <- function(art) {
  store <- art$store; field <- Sys.getenv("BENCH_FIELD", "counts"); BLK <- 2048L
  cn <- art$cell_names; gn <- art$gene_names; G <- length(gn)
  depth <- art$recipe$depth; ds <- art$recipe$depthScale; lg <- isTRUE(art$recipe$log.scale)
  va <- function(blk) list(depth = as.numeric(depth[rownames(blk)]), depthScale = ds,
        normalize = TRUE, log.scale = lg, batch = integer(0), batchFactors = matrix(numeric(), 0, 0),
        winsorCaps = numeric(), preWinsorDepth = numeric(), postWinsorDepth = numeric())
  blocks <- function() seq(0L, G - 1L, by = BLK)
  list(
    name = "zarr",
    col_mean_var = function(recipe, n.cores = 1) {
      # FUSED: one threaded C++ pass over the store applying the plain view (depth-normalize + log1p)
      # while reducing -- no per-block dgCMatrix, no C++->R marshalling of the data. population=TRUE
      # matches pagoda2's colMeanVarView variance convention (/nrows).
      depth_vec <- as.numeric(depth[cn])          # per-cell depth in store row order
      s <- lstar::stream_col_stats(store, field, block = BLK, n_threads = n.cores, lognorm = lg,
                                   depth = depth_vec, depthScale = ds, population = TRUE)
      data.frame(m = s$mean, v = s$var, nobs = s$nnz, row.names = gn)
    },
    col_sum_by_group = function(recipe, groups, n.cores = 1) {
      # FUSED pseudobulk: one threaded C++ pass, plain view applied inline, no per-block dgCMatrix.
      depth_vec <- as.numeric(depth[cn])                       # per-cell depth in store row order
      codes <- as.integer(groups); codes[is.na(codes)] <- 0L   # NA cells -> bucket 0 (the <NA> row)
      ng <- nlevels(groups) + 1L
      M <- lstar::lstar_stream_col_sum_by_group(store, field, codes, ng, lognorm = lg,
                  depth = depth_vec, depthScale = ds, block = BLK, n_threads = n.cores)
      rownames(M) <- c("<NA>", levels(groups)); colnames(M) <- gn; M
    },
    materialize_block = function(recipe, genes, scale.variance = FALSE, varinfo = NULL) {
      sub <- lstar::lstar_read_genes(store, field, genes, gn, cell_names = cn)   # cells x genes off disk
      x <- pagoda2:::.pagoda2_materialize_view(sub, list(model = "plain", depth = depth,
            depthScale = ds, log.scale = lg, batchFactors = NULL, winsorCaps = NULL))
      if (scale.variance) x <- pagoda2:::.pagoda2_apply_variance_scaling(x, varinfo)
      x
    }
  )
}

load_artifacts <- function(dir = Sys.getenv("BENCH_DIR", "/tmp/bench"), which = "mem") {
  art <- list(recipe = readRDS(file.path(dir, "recipe.rds")),
              varinfo = readRDS(file.path(dir, "varinfo.rds")),
              odgenes = readRDS(file.path(dir, "odgenes.rds")),
              grp = readRDS(file.path(dir, "grp.rds")),
              genes20 = readRDS(file.path(dir, "genes20.rds")))
  if (which == "mem")  art$raw_cxg <- readRDS(file.path(dir, "raw_cxg.rds"))
  if (which == "zarr") {
    art$store <- Sys.getenv("BENCH_STORE", "/tmp/marrow_csc.lstar.zarr")
    art$cell_names <- names(art$recipe$depth); art$gene_names <- rownames(art$varinfo)
  }
  art
}
