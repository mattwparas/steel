#!/usr/bin/env bash
# Run the R7RS benchmark suite against Steel (JIT on/off) and optionally Guile,
# and emit a CSV of results.
#
#   ./r7rs-benchmarks/run.sh --size small --config both --systems steel,guile
#
# Must be run from the Steel repo root (benchmark input paths are relative to it).

set -uo pipefail

STEEL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BENCH_DIR="${STEEL_ROOT}/r7rs-benchmarks"
UPSTREAM="${UPSTREAM:-${HOME}/code/r7rs-benchmarks}"
STEEL_BIN="${STEEL_BIN:-${STEEL_ROOT}/target/release/steel}"
GUILE_BIN="${GUILE_BIN:-guile}"
GUILD_BIN="${GUILD_BIN:-guild}"

SIZES="small"
CONFIGS="nojit,jit"
SYSTEMS="steel"
BENCHES=""
REPEAT=1
TIMEOUT=300
OUTDIR="${BENCH_DIR}/results"
TAG=""

usage() {
    cat <<EOF
Usage: run.sh [options]

  --size    small|full|both     input size (default: small)
  --config  nojit|jit|both      Steel JIT configuration (default: both)
  --systems steel,guile         systems to run (default: steel)
  --benches a,b,c               benchmarks to run (default: contents of enabled.txt)
  --all                         run every .scm in r7rs-benchmarks/ (ignores enabled.txt)
  --repeat  N                   runs per benchmark (default: 1)
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
        "$1" "$2" "$3" "$4" "$5" "$R_STATUS" "$R_SECS" "$6" "$R_NOTE" >> "$CSV"
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
    ( cd "$STEEL_ROOT" && \
      STEEL_JIT="$jit" R7RS_BENCH_SIZE="$size" \
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
echo "Output: $CSV"
echo

IFS=',' read -ra BENCH_LIST <<< "${BENCHES%,}"
IFS=',' read -ra SIZE_LIST  <<< "${SIZES%,}"
IFS=',' read -ra CFG_LIST   <<< "${CONFIGS%,}"
IFS=',' read -ra SYS_LIST   <<< "${SYSTEMS%,}"

for size in "${SIZE_LIST[@]}"; do
    for bench in "${BENCH_LIST[@]}"; do
        [[ -z "$bench" ]] && continue
        echo "== ${bench} (${size}) =="
        for sys in "${SYS_LIST[@]}"; do
            case "$sys" in
                steel)
                    for cfg in "${CFG_LIST[@]}"; do
                        for ((r=1; r<=REPEAT; r++)); do run_steel "$bench" "$cfg" "$size" "$r"; done
                    done ;;
                guile)
                    for ((r=1; r<=REPEAT; r++)); do run_guile "$bench" "$size" "$r"; done ;;
                *) echo "unknown system: $sys" >&2 ;;
            esac
        done
    done
done

echo
echo "Wrote $CSV"
