#!/usr/bin/env bash
# Run the R7RS benchmark suite against Steel (JIT on/off) and optionally Guile
# and Chicken, and emit a CSV of results.
#
#   ./r7rs-benchmarks/run.sh --size small --config both --systems steel,guile,chicken
#
# Must be run from the Steel repo root (benchmark input paths are relative to it).

set -uo pipefail

STEEL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BENCH_DIR="${STEEL_ROOT}/r7rs-benchmarks"
UPSTREAM="${UPSTREAM:-${HOME}/code/r7rs-benchmarks}"
STEEL_BIN="${STEEL_BIN:-${STEEL_ROOT}/target/release/steel}"
GUILE_BIN="${GUILE_BIN:-guile}"
GUILD_BIN="${GUILD_BIN:-guild}"
CHICKEN_CSC="${CHICKEN_CSC:-csc}"

SIZES="small"
CONFIGS="nojit,jit"
SYSTEMS="steel"
BENCHES=""
REPEAT=1
TIMEOUT=300
JOBS=1
OUTDIR="${BENCH_DIR}/results"
TAG=""

usage() {
    cat <<EOF
Usage: run.sh [options]

  --size    small|full|both     input size (default: small)
  --config  nojit|jit|both      Steel JIT configuration (default: both)
  --systems steel,guile,chicken systems to run (default: steel)
  --benches a,b,c               benchmarks to run (default: contents of enabled.txt)
  --all                         run every .scm in r7rs-benchmarks/ (ignores enabled.txt)
  --repeat  N                   runs per benchmark (default: 1)
  --jobs    N                   benchmarks to run at once (default: 1).
                                Each benchmark's own runs stay serial and
                                adjacent, so its ratio columns still compare
                                like with like; absolute times inflate under
                                contention, so cross-benchmark comparison of
                                raw seconds gets noisier.
  --timeout SEC                 per-run timeout (default: 300)
  --out     DIR                 output directory (default: r7rs-benchmarks/results)
  --tag     NAME                label for this run's CSV file
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --size)    SIZES="$2"; shift 2 ;;
        --config)  CONFIGS="$2"; shift 2 ;;
        --systems) SYSTEMS="$2"; shift 2 ;;
        --benches) BENCHES="$2"; shift 2 ;;
        --all)     BENCHES="ALL"; shift ;;
        --repeat)  REPEAT="$2"; shift 2 ;;
        --jobs)    JOBS="$2"; shift 2 ;;
        --timeout) TIMEOUT="$2"; shift 2 ;;
        --out)     OUTDIR="$2"; shift 2 ;;
        --tag)     TAG="$2"; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "unknown option: $1" >&2; usage; exit 1 ;;
    esac
done

[[ "$SIZES"   == "both" ]] && SIZES="small,full"
[[ "$CONFIGS" == "both" ]] && CONFIGS="nojit,jit"

if [[ -z "$BENCHES" ]]; then
    if [[ -f "${BENCH_DIR}/enabled.txt" ]]; then
        BENCHES="$(grep -vE '^\s*(#|$)' "${BENCH_DIR}/enabled.txt" | tr '\n' ',')"
    else
        BENCHES="ALL"
    fi
fi
if [[ "$BENCHES" == "ALL" ]]; then
    # The shim modules are libraries, not benchmarks.
    BENCHES="$(cd "$BENCH_DIR" && ls *.scm \
        | grep -vE '^(common|mutable-lists|mutable-strings|mpair-struct|mstring-struct|native-read|native-io|char-compat)\.scm$' \
        | sed 's/\.scm$//' | tr '\n' ',')"
fi

mkdir -p "$OUTDIR"
TMPDIR_RUN="${OUTDIR}/tmp"
mkdir -p "$TMPDIR_RUN"

CSV="${OUTDIR}/results${TAG:+-$TAG}.csv"
echo "system,config,size,benchmark,run,status,reported_secs,wall_secs,note" > "$CSV"
# Per-run, so the CSV only ever collects this run's rows. A shared directory
# kept every earlier run's parts, and a --benches subset run concatenated stale
# rows for all the benchmarks it didn't run.
PARTS="$(mktemp -d "${TMPDIR_RUN}/parts.XXXXXX")"
trap 'rm -rf "$PARTS"' EXIT
LOGDIR="${OUTDIR}/logs"
mkdir -p "$LOGDIR"

if [[ ! -x "$STEEL_BIN" ]]; then
    echo "steel binary not found at $STEEL_BIN (cargo build --release)" >&2
    exit 1
fi

# Parse a completed run's output into: status, reported_secs, note
# Sets globals R_STATUS R_SECS R_NOTE
parse_output() {
    local log="$1" rc="$2"
    R_SECS=""
    R_NOTE=""
    local csvline
    csvline="$(grep -a -m1 '^+!CSVLINE!+' "$log" 2>/dev/null | tail -1)"

    if [[ $rc -eq 124 || $rc -eq 137 ]]; then
        R_STATUS="timeout"
        R_NOTE="exceeded ${TIMEOUT}s"
        return
    fi
    if grep -qa 'returned incorrect result' "$log" 2>/dev/null || [[ "$csvline" == *,INCORRECT ]]; then
        R_STATUS="wrong-result"
        R_NOTE="$(grep -a -m1 'returned incorrect result' "$log" | head -c 200 | tr ',\n' ';')"
        return
    fi
    if [[ -n "$csvline" ]]; then
        R_SECS="$(printf '%s' "$csvline" | awk -F, '{print $NF}')"
        if [[ $rc -ne 0 ]]; then
            R_STATUS="error-after-report"
            R_NOTE="exit $rc"
        else
            R_STATUS="ok"
        fi
        return
    fi
    R_STATUS="error"
    R_NOTE="exit $rc: $(grep -a -m1 -iE 'error|panic|Exception' "$log" | head -c 200 | tr ',\n' ';')"
}

record() {
    printf '%s,%s,%s,%s,%s,%s,%s,%s,"%s"\n' \
        "$1" "$2" "$3" "$4" "$5" "$R_STATUS" "$R_SECS" "$6" "$R_NOTE" >> "${PART_CSV:-$CSV}"
    printf '  %-8s %-6s %-6s %-12s run %s: %-16s %s\n' \
        "$1" "$2" "$3" "$4" "$5" "$R_STATUS" "${R_SECS:-}"
}

wall() { # $1 = start ns
    awk -v s="$1" -v e="$(date +%s%N)" 'BEGIN{printf "%.3f", (e-s)/1e9}'
}

# ---------------------------------------------------------------- Steel

run_steel() {
    local bench="$1" cfg="$2" size="$3" run="$4"
    local scm="${BENCH_DIR}/${bench}.scm"
    if [[ ! -f "$scm" ]]; then
        R_STATUS="missing"; R_SECS=""; R_NOTE="no ${bench}.scm"
        record steel "$cfg" "$size" "$bench" "$run" ""
        return
    fi
    local jit=true
    [[ "$cfg" == "nojit" ]] && jit=false
    local log="${LOGDIR}/steel-${cfg}-${size}-${bench}-${run}.log"
    local t0; t0="$(date +%s%N)"
    # The jit verifier is on by default; timing runs don't want to pay for it
    ( cd "$STEEL_ROOT" && \
      STEEL_JIT="$jit" STEEL_JIT_VERIFIER=false R7RS_BENCH_SIZE="$size" \
      timeout -k 5 "$TIMEOUT" "$STEEL_BIN" "r7rs-benchmarks/${bench}.scm" ) \
        > "$log" 2>&1
    local rc=$?
    local w; w="$(wall "$t0")"
    parse_output "$log" "$rc"
    record steel "$cfg" "$size" "$bench" "$run" "$w"
}

# ---------------------------------------------------------------- Guile

# Guile runs the *unmodified upstream* sources, fed the same input files we
# give Steel, with cwd = the Steel repo root so data-file paths resolve.
guile_program() {
    local bench="$1"
    local src="${UPSTREAM}/src/${bench}.scm"
    # local benchmarks lowercase mbrotz; upstream calls it mbrotZ
    [[ -f "$src" ]] || src="${UPSTREAM}/src/mbrotZ.scm"
    [[ -f "$src" ]] || return 1
    local out="${TMPDIR_RUN}/guile-${bench}.scm"
    cat "${UPSTREAM}/src/Guile-prelude.scm" \
        "$src" \
        "${UPSTREAM}/src/common.scm" \
        "${UPSTREAM}/src/common-postlude.scm" > "$out"

    # Upstream's bench script compiles with -O3 first; without it guile
    # interprets and is several times slower. Not timed, same as upstream.
    "$GUILD_BIN" compile -O3 "$out" >/dev/null 2>&1

    printf '%s' "$out"
}

run_guile() {
    local bench="$1" size="$2" run="$3"
    local input
    input="$(bench_input_path "$bench" "$size")"
    local prog
    prog="$(guile_program "$bench")"
    if [[ -z "$prog" || -z "$input" ]]; then
        R_STATUS="missing"; R_SECS=""
        R_NOTE="${prog:+}${prog:-no upstream src}${input:+}"
        [[ -z "$input" ]] && R_NOTE="no input file"
        record guile "-" "$size" "$bench" "$run" ""
        return
    fi
    local log="${LOGDIR}/guile-${size}-${bench}-${run}.log"
    local t0; t0="$(date +%s%N)"
    ( cd "$STEEL_ROOT" && timeout -k 5 "$TIMEOUT" "$GUILE_BIN" "$prog" < "$input" ) \
        > "$log" 2>&1
    local rc=$?
    local w; w="$(wall "$t0")"
    parse_output "$log" "$rc"
    record guile "-" "$size" "$bench" "$run" "$w"
}

# ---------------------------------------------------------------- Chicken

# Chicken compiles the unmodified upstream sources to a native executable,
# with the flags upstream's bench script uses. Compiling is not timed, same
# as upstream. Prints the executable's path, or nothing if it did not build.
chicken_program() {
    local bench="$1"
    local src="${UPSTREAM}/src/${bench}.scm"
    [[ -f "$src" ]] || src="${UPSTREAM}/src/mbrotZ.scm"
    [[ -f "$src" ]] || return 1
    local out="${TMPDIR_RUN}/chicken-${bench}"
    cat "${UPSTREAM}/src/Chicken-prelude.scm" \
        "$src" \
        "${UPSTREAM}/src/common.scm" \
        "${UPSTREAM}/src/common-postlude.scm" > "${out}.scm"

    "$CHICKEN_CSC" -R r7rs -X r7rs -block -O3 -C -O3 -d0 "${out}.scm" -o "$out" \
        > "${LOGDIR}/chicken-compile-${bench}.log" 2>&1 || return 1

    printf '%s' "$out"
}

run_chicken() {
    local bench="$1" size="$2" run="$3"
    local input
    input="$(bench_input_path "$bench" "$size")"
    local prog="${TMPDIR_RUN}/chicken-${bench}"
    # Built once and kept for the repeats.
    [[ -x "$prog" ]] || prog="$(chicken_program "$bench")"
    if [[ -z "$prog" || -z "$input" ]]; then
        R_STATUS="missing"; R_SECS=""
        R_NOTE="did not compile"
        [[ -z "$input" ]] && R_NOTE="no input file"
        record chicken "-" "$size" "$bench" "$run" ""
        return
    fi
    local log="${LOGDIR}/chicken-${size}-${bench}-${run}.log"
    local t0; t0="$(date +%s%N)"
    ( cd "$STEEL_ROOT" && timeout -k 5 "$TIMEOUT" "$prog" < "$input" ) \
        > "$log" 2>&1
    local rc=$?
    local w; w="$(wall "$t0")"
    parse_output "$log" "$rc"
    record chicken "-" "$size" "$bench" "$run" "$w"
}

# Resolve which input file a benchmark uses for a given size.
bench_input_path() {
    local bench="$1" size="$2"
    if [[ "$size" == "small" && -f "${BENCH_DIR}/small-inputs/${bench}.input" ]]; then
        printf '%s' "${BENCH_DIR}/small-inputs/${bench}.input"
    elif [[ -f "${BENCH_DIR}/inputs/${bench}.input" ]]; then
        printf '%s' "${BENCH_DIR}/inputs/${bench}.input"
    fi
}

# ---------------------------------------------------------------- main

echo "Steel:  $STEEL_BIN"
echo "Guile:  $($GUILE_BIN --version 2>/dev/null | head -1)"
echo "Chicken: $($CHICKEN_CSC -version 2>/dev/null | grep -m1 Version)"
echo "Output: $CSV"
echo

IFS=',' read -ra BENCH_LIST <<< "${BENCHES%,}"
IFS=',' read -ra SIZE_LIST  <<< "${SIZES%,}"
IFS=',' read -ra CFG_LIST   <<< "${CONFIGS%,}"
IFS=',' read -ra SYS_LIST   <<< "${SYSTEMS%,}"

# One benchmark, every system and repeat, serially. Keeping a benchmark's runs
# together is what lets its ratio columns survive being run alongside others.
run_bench() {
    local bench="$1" size="$2"
    PART_CSV="${PARTS}/${size}-${bench}.csv"
    : > "$PART_CSV"

    for sys in "${SYS_LIST[@]}"; do
        case "$sys" in
            steel)
                for cfg in "${CFG_LIST[@]}"; do
                    for ((r=1; r<=REPEAT; r++)); do
                        run_steel "$bench" "$cfg" "$size" "$r"
                        # A repeat of a timeout just spends the timeout again
                        [[ "$R_STATUS" == "timeout" ]] && break
                    done
                done ;;
            guile)
                for ((r=1; r<=REPEAT; r++)); do
                    run_guile "$bench" "$size" "$r"
                    [[ "$R_STATUS" == "timeout" ]] && break
                done ;;
            chicken)
                for ((r=1; r<=REPEAT; r++)); do
                    run_chicken "$bench" "$size" "$r"
                    [[ "$R_STATUS" == "timeout" ]] && break
                done ;;
            *) echo "unknown system: $sys" >&2 ;;
        esac
    done
}

for size in "${SIZE_LIST[@]}"; do
    for bench in "${BENCH_LIST[@]}"; do
        [[ -z "$bench" ]] && continue

        if (( JOBS <= 1 )); then
            echo "== ${bench} (${size}) =="
            run_bench "$bench" "$size"
            continue
        fi

        while (( $(jobs -rp | wc -l) >= JOBS )); do wait -n; done
        echo "== ${bench} (${size}) started =="
        run_bench "$bench" "$size" &
    done
    wait
done

cat "${PARTS}"/*.csv >> "$CSV" 2>/dev/null || true

echo
echo "Wrote $CSV"
