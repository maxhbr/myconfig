#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# litellm-smoke-test — smoke-test models deployed on a LiteLLM server
# (e.g. http://localhost:4000): list /v1/models, send one small chat
# completion per model, retry with exponential backoff while the
# backend ramps up (llama-swap / llama-server on-demand loading takes
# minutes for multi-GB models), and write a CSV report plus a log file
# (next to the CSV) that records every step — each model is logged
# before its first request so the log shows progress even when the
# host is OOM-killed mid-test.
#
# The run is incremental: models recorded in the state file by a
# previous run are not tested again; after each model completes its id
# is appended to the state file (default: the CSV path with .state).
# --force ignores the state file and tests every model.
set -euo pipefail

# Defaults overridable via environment: the --test-one child process
# inherits the parent's parsed configuration through exported variables.
BASE_URL="${LITELLM_SMOKE_TEST_URL:-http://localhost:4000}"
API_KEY="${LITELLM_API_KEY:-sk-smoke-test}"
CSV_OUT="${LITELLM_SMOKE_TEST_OUT:-litellm-smoke-test.csv}"
LOG_OUT="${LITELLM_SMOKE_TEST_LOG:-}"
STATE_OUT="${LITELLM_SMOKE_TEST_STATE:-}"
FORCE=0
ATTEMPTS="${LITELLM_SMOKE_TEST_ATTEMPTS:-4}"
TIMEOUT="${LITELLM_SMOKE_TEST_TIMEOUT:-300}"
BACKOFF="${LITELLM_SMOKE_TEST_BACKOFF:-15}"
BACKOFF_FACTOR=2
BACKOFF_CAP=120
CONCURRENCY=1
PROMPT="Reply with the single word: ok"
MAX_TOKENS=16

usage() {
    cat <<EOF
Usage: litellm-smoke-test [options]

Smoke-test models of a LiteLLM server. Discovers models via
GET \$BASE_URL/v1/models, then sends one minimal chat-completion request
per model, retrying with exponential backoff to give on-demand backends
(llama-swap, llama-server router) time to load the model into VRAM.

The run is incremental: models recorded in the state file by a previous
run are skipped, so only new models are probed. After each model
completes (ok or failed) its id is appended to the state file; use
--force to ignore the state file and retest everything.

Results are written to a CSV (default: ./litellm-smoke-test.csv) and a
log file next to it (default: same path with .log instead of the CSV
extension). Every step — including each model before it is first probed —
is appended to the log, so progress survives even if the host is
OOM-killed mid-run.

Options:
  -u, --url URL          LiteLLM base URL (default: http://localhost:4000)
  -o, --out FILE         CSV output path (default: litellm-smoke-test.csv)
  -l, --log FILE         Log output path (default: CSV path with .log)
  -s, --state FILE       State file of previously tested models
                         (default: CSV path with .state)
  -f, --force            Ignore the state file and test all models
  -a, --attempts N       Attempts per model (default: $ATTEMPTS)
  -t, --timeout SECONDS  Per-attempt request timeout (default: $TIMEOUT)
  -b, --backoff SECONDS  Initial backoff between attempts (default: $BACKOFF)
  -c, --concurrency N    Parallel requests (default: $CONCURRENCY; backends
                        are on-demand llama-swap targets, parallel model
                        loads can OOM the GPU host — keep at 1)
  -m, --max-tokens N     max_tokens of the probe request (default: $MAX_TOKENS)
  -h, --help             This help

Environment:
  LITELLM_API_KEY            Bearer token for the server (default: sk-smoke-test)
  LITELLM_SMOKE_TEST_STATE   State file path (default: CSV path with .state)
EOF
    exit 0
}

while (($#)); do
    case "$1" in
        -u | --url)
            shift
            BASE_URL="$1"
            ;;
        -o | --out)
            shift
            CSV_OUT="$1"
            ;;
        -l | --log)
            shift
            LOG_OUT="$1"
            ;;
        -s | --state)
            shift
            STATE_OUT="$1"
            ;;
        -f | --force)
            FORCE=1
            ;;
        -a | --attempts)
            shift
            ATTEMPTS="$1"
            ;;
        -t | --timeout)
            shift
            TIMEOUT="$1"
            ;;
        -b | --backoff)
            shift
            BACKOFF="$1"
            ;;
        -c | --concurrency)
            shift
            CONCURRENCY="$1"
            ;;
        -m | --max-tokens)
            shift
            MAX_TOKENS="$1"
            ;;
        --test-one)
            shift
            TEST_ONE_MODEL="$1"
            ;;
        -h | --help) usage ;;
        *)
            echo "ERROR: unknown option: $1" >&2
            usage
            ;;
    esac
    shift
done

log() {
    local line
    line="[$(date +%H:%M:%S)] $*"
    printf '%s\n' "$line" >&2
    [[ -z $LOG_OUT ]] || printf '%s\n' "$line" >>"$LOG_OUT"
}

# Internal per-model mode: `--test-one <model>` (parsed below into
# TEST_ONE_MODEL) runs test_one for a single model and exits. Used for
# --concurrency > 1, where a fresh process per model avoids exporting
# shell functions through xargs.

csv_quote() {
    # RFC 4180: quote when the value contains comma, quote, CR or LF;
    # embedded quotes are doubled.
    local s=$1
    if [[ $s == *[,\"]* ]]; then
        s="\"${s//\"/\"\"}\""
    fi
    printf '%s' "$s"
}

# ---------------------------------------------------------------- log setup
# Default: write the log next to the CSV (same directory, same basename,
# .log extension). Open the file immediately so even a crash before the
# first model leaves a trace of the invocation. Skipped for --test-one
# children, which inherit the parent's log path via the environment.
if [[ -z $LOG_OUT ]]; then
    LOG_OUT="$(dirname "$CSV_OUT")/$(basename "$CSV_OUT")"
    LOG_OUT="${LOG_OUT%.*}.log"
fi
if [[ -z $STATE_OUT ]]; then
    STATE_OUT="$(dirname "$CSV_OUT")/$(basename "$CSV_OUT")"
    STATE_OUT="${STATE_OUT%.*}.state"
fi
if [[ -z ${TEST_ONE_MODEL:-} ]]; then
    log "litellm-smoke-test starting: url=$BASE_URL csv=$CSV_OUT log=$LOG_OUT state=$STATE_OUT force=$FORCE attempts=$ATTEMPTS timeout=$TIMEOUT"
fi

# ---------------------------------------------------------------- models
if [[ -z ${TEST_ONE_MODEL:-} ]]; then
    log "querying $BASE_URL/v1/models ..."
    MODELS=$(curl -sfS --max-time 60 \
        -H "Authorization: Bearer $API_KEY" \
        "$BASE_URL/v1/models" |
        jq -r '.data[].id' | sort -u)

    if [[ -z $MODELS ]]; then
        echo "ERROR: no models found at $BASE_URL/v1/models" >&2
        exit 1
    fi

    MODEL_COUNT=$(wc -l <<<"$MODELS")
    log "found $MODEL_COUNT models"

    # Incremental mode: keep only the models not recorded in the state
    # file by a previous run; with --force, test everything.
    if ((FORCE)); then
        TO_TEST=$MODELS
    elif [[ -f $STATE_OUT && -s $STATE_OUT ]]; then
        TO_TEST=$(comm -23 <(printf '%s\n' "$MODELS") <(sort -u "$STATE_OUT"))
        [[ -n $TO_TEST ]] || TO_TEST=""
    else
        TO_TEST=$MODELS
    fi

    if [[ -z $TO_TEST ]]; then
        log "all $MODEL_COUNT models already tested (state: $STATE_OUT) — nothing to do; existing report left in place: $CSV_OUT"
        exit 0
    fi

    TESTED_COUNT=$(($(wc -l <<<"$TO_TEST")))
    SKIPPED=$((MODEL_COUNT - TESTED_COUNT))
    if ((SKIPPED > 0)); then
        log "skipping $SKIPPED previously tested model(s); testing $TESTED_COUNT model(s)"
    else
        log "testing $TESTED_COUNT model(s)"
    fi
fi

# ---------------------------------------------------------------- runner
# test_one <model> -> one CSV row on stdout:
#   model,ok,attempts,latency_seconds,http_code,error,reply
test_one() {
    local model=$1
    local attempt=1 wait=$BACKOFF
    local start end latency http_code body reply reasoning ok=0 error=""

    log "[$model] testing model"
    while ((attempt <= ATTEMPTS)); do
        log "[$model] attempt $attempt/$ATTEMPTS ..."
        body=$(mktemp)
        start=$(date +%s)
        http_code=$(curl -sS -o "$body" -w '%{http_code}' \
            --max-time "$TIMEOUT" \
            -H "Authorization: Bearer $API_KEY" \
            -H "Content-Type: application/json" \
            -d "$(jq -nc --arg m "$model" --argjson t "$MAX_TOKENS" --arg p "$PROMPT" \
                '{model: $m, max_tokens: $t, messages: [{role: "user", content: $p}]}')" \
            "$BASE_URL/v1/chat/completions" 2>/dev/null) || http_code="000"
        end=$(date +%s)
        latency=$((end - start))

        if [[ $http_code == "200" ]]; then
            # Reasoning models can emit only `reasoning_content` while the
            # visible `content` is still empty (finish_reason=length) — count
            # that as a successful round trip too.
            reply=$(jq -r '.choices[0].message.content // ""' "$body" 2>/dev/null || echo "")
            reasoning=$(jq -r '.choices[0].message.reasoning_content // ""' "$body" 2>/dev/null || echo "")
            if [[ -n $reply || -n $reasoning ]]; then
                ok=1
                [[ -n $reply ]] || reply="(reasoning only)"
                rm -f "$body"
                break
            fi
            error="empty completion"
        else
            reply=""
            error=$(jq -r '.error.message // .error // .detail // empty' "$body" 2>/dev/null | head -c 300)
            [[ -z $error ]] && error="http $http_code"
        fi
        rm -f "$body"

        if ((attempt < ATTEMPTS)); then
            log "[$model] failed (attempt $attempt: $error) — retrying in ${wait}s"
            sleep "$wait"
            wait=$((wait * BACKOFF_FACTOR))
            ((wait > BACKOFF_CAP)) && wait=$BACKOFF_CAP
        fi
        ((attempt += 1))
    done
    # The loop counter ends at ATTEMPTS+1 when every attempt failed.
    ((attempt > ATTEMPTS)) && attempt=$ATTEMPTS

    local status
    if ((ok)); then
        status="ok"
        [[ -z $error ]] || status="ok-on-retry"
    else
        status="failed"
    fi

    printf '%s,%s,%s,%s,%s,%s,%s\n' \
        "$(csv_quote "$model")" \
        "$status" \
        "$attempt" \
        "$latency" \
        "$http_code" \
        "$(csv_quote "$error")" \
        "$(csv_quote "$reply")"
}

# ---------------------------------------------------------------- run
# Internal per-model dispatch (see the option parser): run test_one for
# a single model and exit before any server discovery.
if [[ -n ${TEST_ONE_MODEL:-} ]]; then
    test_one "$TEST_ONE_MODEL"
    printf '%s\n' "$TEST_ONE_MODEL" >>"$STATE_OUT"
    exit 0
fi
{
    echo "model,status,attempts,latency_seconds,http_code,error,reply"
    if ((CONCURRENCY > 1)); then
        # The child needs the config; pass it through the environment.
        export \
            LITELLM_SMOKE_TEST_URL="$BASE_URL" \
            LITELLM_SMOKE_TEST_ATTEMPTS="$ATTEMPTS" \
            LITELLM_SMOKE_TEST_TIMEOUT="$TIMEOUT" \
            LITELLM_SMOKE_TEST_BACKOFF="$BACKOFF" \
            LITELLM_SMOKE_TEST_LOG="$LOG_OUT" \
            LITELLM_SMOKE_TEST_STATE="$STATE_OUT" \
            API_KEY PROMPT MAX_TOKENS BACKOFF_FACTOR BACKOFF_CAP
        # shellcheck disable=SC2016  # the $-expansion must happen in the child bash
        printf '%s\n' "$TO_TEST" | xargs -P "$CONCURRENCY" -I{} \
            env litellm_smoke_test="$(readlink -f "${BASH_SOURCE[0]}")" \
            bash -c '"$litellm_smoke_test" --test-one "$1"' _ {}
    else
        while IFS= read -r model; do
            test_one "$model"
            printf '%s\n' "$model" >>"$STATE_OUT"
        done <<<"$TO_TEST"
    fi
} | tee "$CSV_OUT"

# ---------------------------------------------------------------- summary
failed=$(grep -c ',failed,' "$CSV_OUT" || true)
log "done: $TESTED_COUNT of $MODEL_COUNT models tested ($SKIPPED skipped as previously tested), $failed failed — report: $CSV_OUT log: $LOG_OUT"
((failed == 0))
