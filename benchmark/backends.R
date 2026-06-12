## Disk-backed count backends for pagoda2.1 — one interface, three storage implementations.
## Each backend exposes the four standard ops; all reuse pagoda2's own view kernels / recipe so the
## normalized values are identical across backends (storage is the only thing that varies).
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
                                    a$postWinsorDepth, n.cores)
      data.frame(m = r$m, v = r$v, nobs = r$nobs, row.names = colnames(raw))
    },
    col_sum_by_group = function(recipe, groups, n.cores = 1) {
      a <- .view_kernel_args(raw, recipe)
      cols <- as.integer(groups)                        # 1..nlev, NA stays NA (kernel row 0 = <NA>)
      out <- pagoda2:::colSumByFacView(raw, cols, a$depth, a$depthScale, a$normalize, a$log.scale,
                                       a$batch, a$batchFactors, a$winsorCaps, a$preWinsorDepth,
                                       a$postWinsorDepth, n.cores)
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

## ---- BPCells backend: on-disk IterableMatrix + BPCells streaming ops -------------------------------
backend_bpcells <- function(art) {
  stopifnot(requireNamespace("BPCells", quietly = TRUE))
  M <- art$bp                                          # genes x cells IterableMatrix (on disk)
  depth <- art$recipe$depth; ds <- art$recipe$depthScale; lg <- isTRUE(art$recipe$log.scale)
  cells <- colnames(M)
  view <- function(Mx) {                               # plain view as a transform graph (genes x cells)
    sf <- ds / as.numeric(depth[colnames(Mx)])
    Y <- BPCells::multiply_cols(Mx, sf)
    if (lg) Y <- log1p(Y)
    Y
  }
  list(
    name = "bpcells",
    col_mean_var = function(recipe, n.cores = 1) {
      Y <- view(M)                                       # genes are ROWS here
      n <- ncol(M)
      m <- BPCells::rowMeans(Y); v <- BPCells::rowVars(Y) * (n - 1) / n   # BPCells /(n-1) -> pagoda2 /n
      data.frame(m = as.numeric(m), v = as.numeric(v), nobs = NA_real_, row.names = rownames(M))
    },
    col_sum_by_group = function(recipe, groups, n.cores = 1) {
      Y <- view(M)
      pb <- BPCells::pseudobulk_matrix(Y, cell_groups = groups, method = "sum")  # genes x groups (?)
      pb
    },
    materialize_block = function(recipe, genes, scale.variance = FALSE, varinfo = NULL) {
      Y <- view(M[genes, , drop = FALSE])                # genes-subset x cells
      x <- Matrix::t(as(Y, "dgCMatrix"))                 # -> cells x genes
      if (scale.variance) x <- pagoda2:::.pagoda2_apply_variance_scaling(x, varinfo)
      x
    },
    native_svd = function(recipe, genes, varinfo, nPcs = 50) {  # BPCells streaming SVD (bonus)
      Y <- view(M[genes, , drop = FALSE])
      sf <- as.numeric(varinfo[genes, "gsf"]); Y <- BPCells::multiply_rows(Y, sf)
      BPCells::svds(Y, k = nPcs)
    }
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- Zarr backend: lstar chunked CSC store + pagoda2's OWN view kernels, driven per gene block ----
## Demonstrates the intent: lstar provides ONE general primitive (lstar_read_block); the consumer
## (pagoda2) streams gene blocks off disk and applies its existing kernels -> identical math to Mem,
## with no per-op kernel living inside lstar.
backend_zarr <- function(art) {
  store <- art$store; field <- "counts"; BLK <- 2048L
  cn <- art$cell_names; gn <- art$gene_names; G <- length(gn)
  depth <- art$recipe$depth; ds <- art$recipe$depthScale; lg <- isTRUE(art$recipe$log.scale)
  va <- function(blk) list(depth = as.numeric(depth[rownames(blk)]), depthScale = ds,
        normalize = TRUE, log.scale = lg, batch = integer(0), batchFactors = matrix(numeric(), 0, 0),
        winsorCaps = numeric(), preWinsorDepth = numeric(), postWinsorDepth = numeric())
  blocks <- function() seq(0L, G - 1L, by = BLK)
  list(
    name = "zarr",
    col_mean_var = function(recipe, n.cores = 1) {
      m <- numeric(G); v <- numeric(G); nobs <- numeric(G)
      for (a in blocks()) {
        b <- min(a + BLK, G)
        blk <- lstar::lstar_read_block(store, field, a, b, cell_names = cn, gene_names = gn)
        ar <- va(blk)
        r <- pagoda2:::colMeanVarView(blk, NULL, ar$depth, ar$depthScale, ar$normalize, ar$log.scale,
              ar$batch, ar$batchFactors, ar$winsorCaps, ar$preWinsorDepth, ar$postWinsorDepth, n.cores)
        idx <- (a + 1L):b; m[idx] <- r$m; v[idx] <- r$v; nobs[idx] <- r$nobs
      }
      data.frame(m = m, v = v, nobs = nobs, row.names = gn)
    },
    col_sum_by_group = function(recipe, groups, n.cores = 1) {
      cols <- as.integer(groups); out <- NULL
      for (a in blocks()) {
        b <- min(a + BLK, G)
        blk <- lstar::lstar_read_block(store, field, a, b, cell_names = cn, gene_names = gn)
        ar <- va(blk)
        o <- pagoda2:::colSumByFacView(blk, cols, ar$depth, ar$depthScale, ar$normalize, ar$log.scale,
              ar$batch, ar$batchFactors, ar$winsorCaps, ar$preWinsorDepth, ar$postWinsorDepth)
        out <- if (is.null(out)) o else cbind(out, o)
      }
      rownames(out) <- c("<NA>", levels(groups)); colnames(out) <- gn; out
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

load_artifacts <- function(dir = "/tmp/bench", which = "mem") {
  art <- list(recipe = readRDS(file.path(dir, "recipe.rds")),
              varinfo = readRDS(file.path(dir, "varinfo.rds")),
              odgenes = readRDS(file.path(dir, "odgenes.rds")),
              grp = readRDS(file.path(dir, "grp.rds")),
              genes20 = readRDS(file.path(dir, "genes20.rds")))
  if (which == "mem")  art$raw_cxg <- readRDS(file.path(dir, "raw_cxg.rds"))
  if (which == "bpcells") art$bp <- BPCells::open_matrix_anndata_hdf5("/tmp/marrow_raw.h5ad")
  if (which == "zarr") {
    art$store <- Sys.getenv("BENCH_STORE", "/tmp/marrow_csc.lstar.zarr")
    art$cell_names <- names(art$recipe$depth); art$gene_names <- rownames(art$varinfo)
  }
  art
}
