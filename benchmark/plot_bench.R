# Two-panel figure from run_grid.sh results: runtime (left) and peak memory (right) for the basic
# processing ops. Color = backend (in-memory vs lstar-zarr); linetype = cores (1 = dashed, 8 = solid).
dir <- Sys.getenv("BENCH_DIR", "/tmp/bench_tms")
out <- Sys.getenv("BENCH_PNG", file.path("benchmark", "tms245k_bench.png"))
d <- read.csv(file.path(dir, "results.csv"), stringsAsFactors = FALSE)
d$wall_s <- suppressWarnings(as.numeric(d$wall_s)); d$mem_gb <- as.numeric(d$maxrss_mb) / 1024

mem_col <- "#1f77b4"; zarr_col <- "#d62728"           # in-memory vs lstar-zarr
oplab <- c("1" = "variance/HVG", "2" = "PCA", "3" = "pseudobulk", "4" = "gene-block")
oporder <- c(1, 4, 3, 2)                               # rough pipeline order
xlab <- oplab[as.character(oporder)]
series <- list(
  list(bk = "mem",  nc = 1, col = mem_col,  lty = 2, lab = "in-memory, 1 core"),
  list(bk = "mem",  nc = 8, col = mem_col,  lty = 1, lab = "in-memory, 8 cores"),
  list(bk = "zarr", nc = 1, col = zarr_col, lty = 2, lab = "lstar-zarr, 1 core"),
  list(bk = "zarr", nc = 8, col = zarr_col, lty = 1, lab = "lstar-zarr, 8 cores"))
yv <- function(s, metric) sapply(oporder, function(o) { r <- d[d$backend == s$bk & d$ncores == s$nc & d$op == o, ]; if (nrow(r)) r[[metric]][1] else NA })

png(out, width = 1150, height = 500, res = 110)
par(mfrow = c(1, 2), mar = c(7, 4.6, 3.2, 1), oma = c(0, 0, 2, 0))
## panel 1 — runtime (log y, wide dynamic range across ops)
aw <- unlist(lapply(series, yv, metric = "wall_s")); aw <- aw[is.finite(aw) & aw > 0]
plot(NA, xlim = c(1, length(oporder)), ylim = range(aw), log = "y", xaxt = "n", xlab = "", ylab = "runtime (s, log scale)", main = "Runtime")
axis(1, at = seq_along(oporder), labels = xlab, las = 2, cex.axis = 0.9)
for (s in series) lines(seq_along(oporder), yv(s, "wall_s"), col = s$col, lty = s$lty, lwd = 2.2, type = "b", pch = 19)
legend("topleft", legend = sapply(series, `[[`, "lab"), col = sapply(series, `[[`, "col"),
       lty = sapply(series, `[[`, "lty"), lwd = 2.2, bty = "n", cex = 0.82)
## panel 2 — peak memory (linear GB, to show the footprint gap)
am <- unlist(lapply(series, yv, metric = "mem_gb")); am <- am[is.finite(am)]
plot(NA, xlim = c(1, length(oporder)), ylim = c(0, max(am) * 1.05), xaxt = "n", xlab = "", ylab = "peak memory (GB)", main = "Peak memory")
axis(1, at = seq_along(oporder), labels = xlab, las = 2, cex.axis = 0.9)
for (s in series) lines(seq_along(oporder), yv(s, "mem_gb"), col = s$col, lty = s$lty, lwd = 2.2, type = "b", pch = 19)
mtext("Tabula Muris Senis (droplet), 245,389 cells x 20,138 genes — in-memory vs lstar-zarr", outer = TRUE, cex = 1.0, font = 2)
dev.off()
cat("wrote", out, "\n")
