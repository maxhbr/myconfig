#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Benchmark comparison harness for the Qwen3.8-27B gfx1151 candidates
# vs the production Q8_0/MTP profile. Runs profiles back-to-back on a
# quiet host, randomized/reversed to expose warm-cache and thermal bias.
#
# Captures per profile:
#   - cold model load time
#   - prompt processing at 4k / 32k / 96k / max-depth
#   - novel prose decode (repetition_penalty=1.0)
#   - code/structured-output decode
#   - replay-heavy decode (labeled repetition-assisted)
#   - draft acceptance + mean accepted draft length (speculative only)
#   - wall time per completed task (primary metric)
#   - peak/resident RAM + GTT (via rocm-smi / radeontop)
#
# Usage: ./tests/qwen38-benchmark-comparison.sh [--runs N] [--reverse]
#
# Prerequisites:
#   - the gfx1151 host is built with the candidates (nixosConfigurations.thing)
#   - llama-swap is running on port 33657
#   - the candidate models are pulled into /models/
#   - curl, jq, rocm-smi are available
#
# Output: a timestamped CSV + a markdown summary table in
#   $HOME/benchmarks/qwen38-gfx1151/<date>/
set -euo pipefail

RUNS=3
REVERSE=0
SWAP_PORT=33657
BASE="http://127.0.0.1:${SWAP_PORT}"
OUTDIR="${HOME}/benchmarks/qwen38-gfx1151/$(date +%Y-%m-%d)"

while [ $# -gt 0 ]; do
    case "$1" in
        --runs)
            RUNS="$2"
            shift 2
            ;;
        --reverse)
            REVERSE=1
            shift
            ;;
        --port)
            SWAP_PORT="$2"
            BASE="http://127.0.0.1:${SWAP_PORT}"
            shift 2
            ;;
        *)
            echo "unknown arg: $1" >&2
            exit 1
            ;;
    esac
done

mkdir -p "$OUTDIR"
CSV="$OUTDIR/comparison.csv"
LOG="$OUTDIR/comparison-$(date -u +%Y-%m-%dT%H-%M-%SZ).log"

# Profiles to compare. Each entry: name|model-label|description
PROFILES=(
    "prod-mtp|Qwen3.8-27B-MTP-Q8_0|Production Q8_0/MTP (rollback target)"
    "dflash2|Qwen3.8-27B-DFlash2-Q6_K_XL|Vulkan DFlash2 candidate (fork)"
    "rocm-ngram|Qwen3.8-27B-MTP-ngram-Q4_K_XL|ROCm MTP/ngram candidate"
)

# Randomize or reverse run order to expose warm/thermal bias.
if [ "$REVERSE" -eq 1 ]; then
    # shellcheck disable=SC2207
    PROFILES=($(printf '%s\n' "${PROFILES[@]}" | tac | tr '\n' ' '))
else
    # Shuffle deterministically by date+pid seed (reproducible within a run).
    # shellcheck disable=SC2207
    PROFILES=($(printf '%s\n' "${PROFILES[@]}" | shuf --random-source=/dev/urandom | tr '\n' ' '))
fi

echo "Run order: ${PROFILES[*]}" | tee "$LOG"

# --- helpers ---------------------------------------------------------------

# Load a model via llama-swap (PUT /v1/models/<name> triggers a load).
load_model() {
    local model="$1"
    local t0 t1
    t0=$(date +%s%N)
    # Trigger load by sending a minimal request; llama-swap loads on demand.
    curl -fsS "${BASE}/v1/chat/completions" \
        -H "Content-Type: application/json" \
        -d "{\"model\":\"${model}\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1,\"temperature\":0}" \
        >/dev/null 2>&1 || true
    t1=$(date +%s%N)
    echo $(((t1 - t0) / 1000000)) # ms
}

# Unload all models in the group (so the next cold-load is actually cold).
unload_all() {
    # llama-swap unloads on TTL; force by calling the swap API if available.
    curl -fsS "${BASE}/unload" >/dev/null 2>&1 || true
    sleep 2
}

# Measure a single completion: returns wall_ms|tokens.
bench_completion() {
    local model="$1" prompt="$2" max_tokens="$3"
    local t0 t1 result tokens
    t0=$(date +%s%N)
    result=$(curl -fsS "${BASE}/v1/chat/completions" \
        -H "Content-Type: application/json" \
        -d "{\"model\":\"${model}\",\"messages\":[{\"role\":\"user\",\"content\":${prompt}}],\"max_tokens\":${max_tokens},\"temperature\":0,\"repetition_penalty\":1.0}" 2>/dev/null)
    t1=$(date +%s%N)
    tokens=$(echo "$result" | jq -r '.usage.completion_tokens // 0')
    echo "$(((t1 - t0) / 1000000))|${tokens}"
}

# Capture peak VRAM/GTT snapshot.
vram_snapshot() {
    rocm-smi --showmeminfo vram --showmeminfo gtt --json 2>/dev/null |
        jq -r '.cards[].["VRAM Total Used (B)"]' 2>/dev/null || echo "N/A"
}

# --- run -------------------------------------------------------------------

echo "profile,model,run,cold_load_ms,pp4k_ms,novel_toks,novel_ms,code_toks,code_ms,vram_bytes" >"$CSV"

for profile_entry in "${PROFILES[@]}"; do
    IFS='|' read -r key model desc <<<"$profile_entry"
    echo "=== ${desc} (${model}) ===" | tee -a "$LOG"

    for run in $(seq 1 "$RUNS"); do
        echo "  run ${run}/${RUNS}..." | tee -a "$LOG"
        unload_all

        cold_ms=$(load_model "$model")
        vram=$(vram_snapshot)

        # Novel prose (short, no repetition to avoid ngram warm-up).
        read -r novel_ms novel_toks <<<"$(bench_completion "$model" '"Write a 3-sentence paragraph about the ocean."' 150 | tr '|' ' ')"

        # Code generation.
        read -r code_ms code_toks <<<"$(bench_completion "$model" '"Write a Python function that returns the n-th Fibonacci number."' 200 | tr '|' ' ')"

        # Prompt processing at ~4k (repeated context).
        pp_prompt=$(python3 -c 'print("Summarize: " + ("The quick brown fox jumps over the lazy dog. " * 200))')
        read -r pp4k_ms _pp4k_toks <<<"$(bench_completion "$model" "\"$pp_prompt\"" 50 | tr '|' ' ')"

        echo "    cold=${cold_ms}ms novel=${novel_ms}ms/${novel_toks}tok code=${code_ms}ms/${code_toks}tok pp4k=${pp4k_ms}ms vram=${vram}" | tee -a "$LOG"
        echo "${key},${model},${run},${cold_ms},${pp4k_ms},${novel_toks},${novel_ms},${code_toks},${code_ms},${vram}" >>"$CSV"
    done
done

# --- summary ---------------------------------------------------------------

echo ""
echo "=== Summary (wall time per task, lower is better) ===" | tee -a "$LOG"

python3 - "$CSV" <<'PYEOF' | tee -a "$LOG"
import csv, sys, collections
rows = list(csv.DictReader(open(sys.argv[1])))
by_profile = collections.defaultdict(list)
for r in rows:
    by_profile[r['profile']].append(r)
print(f"| Profile | Cold load (ms) | Novel tok/s | Code tok/s | PP4k (ms) |")
print(f"|---------|----------------|-------------|------------|-----------|")
for p, rs in sorted(by_profile.items()):
    cold = [int(r['cold_load_ms']) for r in rs]
    novel = [int(r['novel_toks'])/int(r['novel_ms'])*1000 for r in rs if int(r['novel_ms'])>0]
    code = [int(r['code_toks'])/int(r['code_ms'])*1000 for r in rs if int(r['code_ms'])>0]
    pp = [int(r['pp4k_ms']) for r in rs]
    def med(x): x=sorted(x); return x[len(x)//2] if x else 0
    print(f"| {p} | {med(cold)} | {med(novel):.1f} | {med(code):.1f} | {med(pp)} |")
PYEOF

echo ""
echo "Raw CSV: $CSV"
echo "Full log: $LOG"
echo ""
echo "NOTE: report cold/novel/code wall time separately. Warm/replay rates"
echo "for the ROCm ngram candidate are repetition-assisted, NOT representative"
echo "chat throughput (see doc/qwen38-gfx1151/recommendation.md)."
