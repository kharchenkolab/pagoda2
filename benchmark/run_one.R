# Run ONE (backend, op) in an isolated process so /usr/bin/time -v captures true peak RSS (incl.
# off-heap C++ allocations). Prints: op wall seconds (proc.time) + a result fingerprint.
# Args: <backend: mem|zarr> <op: 1|2|3|4>
.libPaths(c(.libPaths(), "/home/pkharchenko/p21/lstar/.Rlib"))
suppressMessages({library(pagoda2); library(Matrix)})
source("/home/pkharchenko/p21/pagoda2/benchmark/backends.R")
a <- commandArgs(trailingOnly = TRUE); bk <- a[1]; op <- a[2]
art <- load_artifacts(which = bk)
b <- switch(bk, mem = backend_mem(art), zarr = backend_zarr(art))
rec <- art$recipe; vi <- art$varinfo; g20 <- art$genes20; grp <- art$grp
od <- head(art$odgenes, as.integer(Sys.getenv("BENCH_PCA_ODGENES", "2000")))  # cap HVGs used for PCA
nc <- as.integer(Sys.getenv("BENCH_NCORES", "8"))
fp <- function(x) sprintf("%.6e", sum(as.numeric(x), na.rm = TRUE))   # cheap result fingerprint
t <- proc.time()
res <- switch(op,
  "1"   = { r <- b$col_mean_var(rec, n.cores = nc); fp(c(r$m, r$v)) },
  "3"   = { r <- b$col_sum_by_group(rec, grp, n.cores = nc); fp(as.matrix(r)) },
  "4"   = { r <- b$materialize_block(rec, g20, scale.variance = TRUE, varinfo = vi); fp(as.matrix(r)) },
  "2"   = { x <- b$materialize_block(rec, od, scale.variance = TRUE, varinfo = vi)
            cm <- Matrix::colMeans(x); p <- irlba::irlba(x, nv = 50, nu = 0, center = cm, maxit = 100); fp(p$d) })
cat(sprintf("RESULT backend=%s op=%s wall=%.2f fp=%s\n", bk, op, (proc.time() - t)[3], res))
