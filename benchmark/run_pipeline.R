# One full to-clusters pipeline (NO embedding, NO plots) in an isolated process: variance/HVG -> PCA
# (RSpectra centered) -> kNN graph -> leiden. Data-sourcing steps (variance, materialize) use the chosen
# backend (mem | zarr); kNN + leiden are backend-agnostic (operate on the reduction). Prints total wall;
# /usr/bin/time -v wraps this for peak RSS. Args: <backend: mem|zarr>
.libPaths(c(.libPaths(), "/home/pkharchenko/p21/lstar/.Rlib"))
suppressMessages({library(pagoda2); library(Matrix)})
source("/home/pkharchenko/p21/pagoda2/benchmark/backends.R")
bk <- commandArgs(trailingOnly = TRUE)[1]
nc <- as.integer(Sys.getenv("BENCH_NCORES", "8"))
art <- load_artifacts(which = bk)
b <- switch(bk, mem = backend_mem(art), zarr = backend_zarr(art))
rec <- art$recipe; vi <- art$varinfo; od <- head(art$odgenes, 2000L)
t0 <- proc.time()
v  <- b$col_mean_var(rec, n.cores = nc)                                   # 1. variance / HVG
X  <- as(b$materialize_block(rec, od, scale.variance = TRUE, varinfo = vi), "CsparseMatrix")
cm <- Matrix::colMeans(X)
pc <- pagoda2:::.pagoda2_truncated_svd(X, nv = 50L, center = cm)          # 2. PCA (RSpectra centered)
scores <- as.matrix(sweep(X %*% pc$v, 2, as.numeric(cm %*% pc$v)))
M <- pagoda2:::.pagoda2_knn_sparse(scores, 31L, n.cores = nc, distance = "angular") # 3. kNN graph
Matrix::diag(M) <- 0; M <- Matrix::drop0(M)
C <- Matrix::drop0(methods::as(M + Matrix::t(M), "CsparseMatrix"))
g <- igraph::graph_from_adjacency_matrix(C, mode = "undirected", weighted = TRUE)
cl <- leidenAlg::leiden.community(g)                                     # 4. leiden clustering
wall <- (proc.time() - t0)[3]
cat(sprintf("PIPELINE backend=%s ncells=%d ncores=%d wall=%.2f nclusters=%d\n",
            bk, nrow(scores), nc, wall, length(unique(cl$membership))))
