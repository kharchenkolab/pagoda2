# Scaling figure: total to-clusters pipeline runtime (left) and peak memory (right) vs cell count.
# Color = backend (blue in-memory, red lstar-zarr); linetype = cores (1 dashed, 8 solid). Log-log.
out <- Sys.getenv("BENCH_PNG", "benchmark/tms_scaling.png")
d <- read.csv(Sys.getenv("BENCH_OUT", "/tmp/bench_scale/results_scale.csv"), stringsAsFactors = FALSE)
d$wall_s <- as.numeric(d$wall_s); d$mem_gb <- as.numeric(d$maxrss_mb) / 1024
mem_col <- "#1f77b4"; zarr_col <- "#d62728"
series <- list(
  list(bk = "mem",  nc = 1, col = mem_col,  lty = 2, lab = "in-memory, 1 core"),
  list(bk = "mem",  nc = 8, col = mem_col,  lty = 1, lab = "in-memory, 8 cores"),
  list(bk = "zarr", nc = 1, col = zarr_col, lty = 2, lab = "lstar-zarr, 1 core"),
  list(bk = "zarr", nc = 8, col = zarr_col, lty = 1, lab = "lstar-zarr, 8 cores"))
xs <- sort(unique(d$ncells))
yv <- function(s, metric) sapply(xs, function(n) { r <- d[d$backend == s$bk & d$ncores == s$nc & d$ncells == n, ]; if (nrow(r)) r[[metric]][1] else NA })

png(out, width = 1150, height = 500, res = 110)
par(mfrow = c(1, 2), mar = c(4.6, 4.6, 3.2, 1), oma = c(0, 0, 2, 0))
aw <- unlist(lapply(series, yv, metric = "wall_s")); aw <- aw[is.finite(aw) & aw > 0]
plot(NA, xlim = range(xs), ylim = range(aw), log = "xy", xaxt = "n",
     xlab = "number of cells", ylab = "total pipeline time (s)", main = "Runtime to clusters")
axis(1, at = xs, labels = format(xs, big.mark = ",", scientific = FALSE), las = 2, cex.axis = 0.8)
for (s in series) lines(xs, yv(s, "wall_s"), col = s$col, lty = s$lty, lwd = 2.2, type = "b", pch = 19)
legend("topleft", legend = sapply(series, `[[`, "lab"), col = sapply(series, `[[`, "col"),
       lty = sapply(series, `[[`, "lty"), lwd = 2.2, bty = "n", cex = 0.82)
am <- unlist(lapply(series, yv, metric = "mem_gb")); am <- am[is.finite(am) & am > 0]
plot(NA, xlim = range(xs), ylim = range(am), log = "xy", xaxt = "n",
     xlab = "number of cells", ylab = "peak memory (GB)", main = "Peak memory")
axis(1, at = xs, labels = format(xs, big.mark = ",", scientific = FALSE), las = 2, cex.axis = 0.8)
for (s in series) lines(xs, yv(s, "mem_gb"), col = s$col, lty = s$lty, lwd = 2.2, type = "b", pch = 19)
mtext("To-clusters pipeline (variance -> PCA -> kNN -> leiden; no embedding) vs cell count, TMS Senis",
      outer = TRUE, cex = 0.95, font = 2)
dev.off()
cat("wrote", out, "\n")
