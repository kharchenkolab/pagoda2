#!/usr/bin/env bash
# Speed + peak-memory grid: {mem, zarr} x {1, 8 cores} x basic-processing ops, each in an isolated
# process under /usr/bin/time -v. Runtime = the op's own wall (run_one.R, excludes R startup + load);
# memory = process peak RSS (captures the in-memory matrix vs the streamed store). Writes results.csv.
set -u
cd "$(dirname "$0")/.."
DIR=${BENCH_DIR:-/tmp/bench_tms}
STORE=${BENCH_STORE:-/tmp/scale_stream.lstar.zarr}
FIELD=${BENCH_FIELD:-X}
OUT="${BENCH_OUT:-$DIR/results.csv}"
BACKENDS="${BENCH_BACKENDS:-mem zarr}"
echo "backend,ncores,op,wall_s,maxrss_mb" > "$OUT"
for bk in $BACKENDS; do
  for nc in 1 8; do
    for op in 1 2 3 4; do
      tf=$(mktemp)
      env OPENBLAS_NUM_THREADS=$nc OMP_NUM_THREADS=$nc OMP_THREAD_LIMIT=$nc \
          BENCH_NCORES=$nc BENCH_DIR="$DIR" BENCH_STORE="$STORE" BENCH_FIELD="$FIELD" \
          /usr/bin/time -v Rscript benchmark/run_one.R "$bk" "$op" >"$tf.out" 2>"$tf.err"
      wall=$(grep -oE 'wall=[0-9.]+' "$tf.out" | head -1 | cut -d= -f2)
      rss=$(grep -iE 'Maximum resident set size' "$tf.err" | grep -oE '[0-9]+' | head -1)
      mb=$(awk "BEGIN{printf \"%.0f\", ${rss:-0}/1024}")
      echo "${bk},${nc},${op},${wall:-NA},${mb}" | tee -a "$OUT"
      rm -f "$tf" "$tf.out" "$tf.err"
    done
  done
done
echo "results -> $OUT"
