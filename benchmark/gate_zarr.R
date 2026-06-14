# lstar-zarr disk backend vs the in-memory golden: correctness (bit-fidelity) + thread scaling of the
# fused reducers. BPCells dropped (was only a comparison probe); this is the backing we ship.
# APPEND .Rlib so the freshly-installed pagoda2 (default lib) wins and only lstar is sourced from .Rlib.
.libPaths(c(.libPaths(), "/home/pkharchenko/p21/lstar/.Rlib"))
suppressMessages({library(pagoda2); library(Matrix)})
stopifnot(requireNamespace("lstar", quietly = TRUE))
source("/home/pkharchenko/p21/pagoda2/benchmark/backends.R")
tt <- function(expr) { t <- proc.time(); v <- force(expr); list(v = v, s = (proc.time() - t)[3]) }
relmax <- function(a, b) { a<-as.numeric(a); b<-as.numeric(b); s<-max(abs(b)); if (s==0) max(abs(a-b)) else max(abs(a-b))/s }
ff <- function(...) { cat(sprintf(...)); flush(stdout()) }

am <- load_artifacts(which = "mem"); az <- load_artifacts(which = "zarr")
mem <- backend_mem(am); zr <- backend_zarr(az)
rec <- am$recipe; vi <- am$varinfo; od <- am$odgenes; g20 <- am$genes20; grp <- am$grp
ff("store: %s\n", az$store)
ff("%-22s %8s %8s %8s   %s\n", "op", "mem(s)", "zarr1(s)", "zarr8(s)", "max rel.diff vs mem golden")

## op1 variance/HVG
M <- tt(mem$col_mean_var(rec, n.cores = 8))
Z1 <- tt(zr$col_mean_var(rec, n.cores = 1)); Z8 <- tt(zr$col_mean_var(rec, n.cores = 8))
g <- rownames(M$v)
ff("%-22s %8.2f %8.2f %8.2f   mean %.1e  var %.1e  (zarr1==zarr8: %s)\n", "1 variance/HVG",
   M$s, Z1$s, Z8$s, relmax(Z8$v[g,"m"], M$v[g,"m"]), relmax(Z8$v[g,"v"], M$v[g,"v"]),
   isTRUE(all.equal(Z1$v, Z8$v)))

## op3 cluster pseudobulk
M3 <- tt(mem$col_sum_by_group(rec, grp, n.cores = 8))
Z31 <- tt(zr$col_sum_by_group(rec, grp, n.cores = 1)); Z38 <- tt(zr$col_sum_by_group(rec, grp, n.cores = 8))
ff("%-22s %8.2f %8.2f %8.2f   pseudobulk %.1e  (%d groups, zarr1==zarr8: %s)\n", "3 cluster pseudobulk",
   M3$s, Z31$s, Z38$s, relmax(Z38$v, M3$v[rownames(Z38$v), colnames(Z38$v)]), nrow(Z38$v) - 1L,
   isTRUE(all.equal(Z31$v, Z38$v)))

## op4 gene-block(20) with variance scaling
M4 <- tt(mem$materialize_block(rec, g20, scale.variance = TRUE, varinfo = vi))
Z4 <- tt(zr$materialize_block(rec, g20, scale.variance = TRUE, varinfo = vi))
ff("%-22s %8.2f %8.2f %8s   block %.1e\n", "4 gene-block(20)", M4$s, Z4$s, "-",
   relmax(as.matrix(Z4$v[rownames(M4$v), g20]), as.matrix(M4$v[, g20])))

## op2 PCA (materialize odgenes + irlba)
pca <- function(b) { x <- b$materialize_block(rec, od, scale.variance = TRUE, varinfo = vi)
  cm <- Matrix::colMeans(x); irlba::irlba(x, nv = 50, nu = 0, center = cm, maxit = 100) }
P  <- tt(pca(mem)); Q <- tt(pca(zr))
dc <- abs(diag(cor(P$v$v, Q$v$v)))[1:10]
ff("%-22s %8.2f %8.2f %8s   |cor| top10 PC min %.4f  singval %.1e\n", "2 PCA (materialize)",
   P$s, Q$s, "-", min(dc), relmax(Q$v$d, P$v$d))
