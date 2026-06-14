#!/usr/bin/env bash
# Scaling grid: total to-clusters pipeline time + peak RSS vs cell count, for {mem,zarr} x {1,8 cores}.
# Each run isolated under /usr/bin/time -v. N=245389 reuses the existing /tmp/bench_tms artifacts/store.
set -u
cd "$(dirname "$0")/.."
SIZES="${SCALE_SIZES_ALL:-5000 20000 40000 80000 160000 245389}"
OUT="${BENCH_OUT:-/tmp/bench_scale/results_scale.csv}"
mkdir -p "$(dirname "$OUT")"
echo "backend,ncores,ncells,wall_s,maxrss_mb" > "$OUT"
for N in $SIZES; do
  if [ "$N" = "245389" ]; then DIR=/tmp/bench_tms; STORE=/tmp/tms245k_csc.lstar.zarr
  else DIR=/tmp/bench_scale/$N; STORE=/tmp/bench_scale/$N/store.lstar.zarr; fi
  for bk in mem zarr; do
    for ncore in 1 8; do
      tf=$(mktemp)
      env OPENBLAS_NUM_THREADS=$ncore OMP_NUM_THREADS=$ncore BENCH_NCORES=$ncore \
          BENCH_DIR="$DIR" BENCH_STORE="$STORE" BENCH_FIELD=counts \
          /usr/bin/time -v Rscript benchmark/run_pipeline.R "$bk" >"$tf.out" 2>"$tf.err"
      wall=$(grep -oE 'wall=[0-9.]+' "$tf.out" | head -1 | cut -d= -f2)
      rss=$(grep -iE 'Maximum resident set size' "$tf.err" | grep -oE '[0-9]+' | head -1)
      mb=$(awk "BEGIN{printf \"%.0f\", ${rss:-0}/1024}")
      echo "${bk},${ncore},${N},${wall:-NA},${mb}" | tee -a "$OUT"
      rm -f "$tf" "$tf.out" "$tf.err"
    done
  done
done
echo "results -> $OUT"
