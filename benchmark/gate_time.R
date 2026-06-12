.libPaths(c("/home/pkharchenko/p21/lstar/.Rlib", .libPaths()))
suppressMessages({library(pagoda2); library(Matrix); library(BPCells)})
source("/home/pkharchenko/p21/pagoda2/benchmark/backends.R")
tt <- function(expr) { t <- proc.time(); val <- force(expr); list(v = val, s = (proc.time() - t)[3]) }
relmax <- function(a, b) { a<-as.numeric(a); b<-as.numeric(b); s<-max(abs(b)); if(s==0) max(abs(a-b)) else max(abs(a-b))/s }

am <- load_artifacts(which="mem"); ab <- load_artifacts(which="bpcells")
mem <- backend_mem(am); bp <- backend_bpcells(ab)
rec <- am$recipe; vi <- am$varinfo; od <- am$odgenes; g20 <- am$genes20
grp <- am$grp                                   # factor aligned to cells; has NAs
grp_nona <- factor(ifelse(is.na(grp), "__NA__", as.character(grp)))   # BPCells dislikes NA groups

cat(sprintf("%-22s %9s %9s   %s\n","op","mem(s)","bpc(s)","correctness vs mem"))
op <- function(label, memexpr, bpexpr, check) tryCatch({
  rm <- tt(memexpr); rb <- tt(bpexpr); msg <- check(rm$v, rb$v)
  cat(sprintf("%-22s %9.2f %9.2f   %s\n", label, rm$s, rb$s, msg)); invisible(list(rm$v, rb$v))
}, error=function(e) cat(sprintf("%-22s  ERROR: %s\n", label, conditionMessage(e))))

## op1 variance/HVG  (gene mean/var of normalized view)
op("1 variance/HVG",
   mem$col_mean_var(rec, n.cores=8), bp$col_mean_var(rec),
   function(M,B){ g<-rownames(M); sprintf("mean %.1e  var %.1e", relmax(B[g,"m"],M[g,"m"]), relmax(B[g,"v"],M[g,"v"])) })

## op3 cluster pseudobulk  -> align both to (gene x real-group)
op("3 cluster pseudobulk",
   mem$col_sum_by_group(rec, grp, n.cores=8), bp$col_sum_by_group(rec, grp_nona),
   function(M,B){
     Mg <- t(M[setdiff(rownames(M),"<NA>"), , drop=FALSE])      # gene x group
     Bg <- as.matrix(B); if (nrow(Bg)!=nrow(Mg)) Bg <- t(Bg)     # ensure gene x group
     gg <- intersect(colnames(Mg), colnames(Bg)); gn <- intersect(rownames(Mg), rownames(Bg))
     sprintf("pseudobulk %.1e (%d groups)", relmax(Bg[gn,gg], Mg[gn,gg]), length(gg)) })

## op4 gene-block(20) with variance scaling
op("4 gene-block(20)",
   mem$materialize_block(rec, g20, scale.variance=TRUE, varinfo=vi),
   bp$materialize_block(rec, g20, scale.variance=TRUE, varinfo=vi),
   function(M,B) sprintf("block %.1e", relmax(as.matrix(B[rownames(M),g20]), as.matrix(M[,g20]))))

## op2 PCA (materialize odgenes + irlba), Mem vs BPCells; + BPCells native svds
pca_mat <- function(b){ x<-b$materialize_block(rec, od, scale.variance=TRUE, varinfo=vi)
  cm<-Matrix::colMeans(x); irlba::irlba(x, nv=50, nu=0, center=cm, maxit=100) }
op("2 PCA (materialize)", pca_mat(mem), pca_mat(bp),
   function(M,B){ dc<-abs(diag(cor(M$v,B$v)))[1:10]; sprintf("|cor| top10 min %.4f  d %.1e", min(dc), relmax(B$d,M$d)) })
r2bs <- tryCatch(tt(bp$native_svd(rec, od, vi, 50)), error=function(e) list(s=NA, err=conditionMessage(e)))
cat(sprintf("%-22s %9s %9.2f   (BPCells native streaming SVD)%s\n","2 PCA (BPCells svds)","-",
            ifelse(is.null(r2bs$s),NA,r2bs$s), if(!is.null(r2bs$err)) paste0(" ERR:",r2bs$err) else ""))
