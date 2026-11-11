#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Print the attribution trailer for a commit message produced by an AI agent
# harness, or exit non-zero with no output if neither model nor harness can be
# determined.
#
# Output (exactly one line, when successful):
#   supported by <model> in <harness>
#   supported by <model>            (harness unknown)
#   supported in <harness>          (model unknown)
#
# Both facts are detected from the current shell environment and agent state
# files — never from the conversation. An empty result or a literal `null`
# counts as "unknown"; the script never emits empty or placeholder values.
#
# Any arguments are accepted and ignored (calling it "for the directory"
# needs no special handling — the trailer does not depend on the cwd).

set -euo pipefail

harness=""
model=""

# --- harness ---------------------------------------------------------------
# pi sets PI_CODING_AGENT=true in its shell tool's environment;
# opencode sets OPENCODE=1.
if [ "${PI_CODING_AGENT:-}" = "true" ]; then
    harness="pi"
elif [ "${OPENCODE:-}" = "1" ]; then
    harness="opencode"
fi

# --- model -----------------------------------------------------------------
# pi exports PI_MODEL, freshly resolved for every shell command. Prefix
# PI_PROVIDER only when the model id carries no provider of its own.
if [ -n "${PI_MODEL:-}" ]; then
    model="$PI_MODEL"
    case "$model" in
        */*) : ;;
        *)
            if [ -n "${PI_PROVIDER:-}" ]; then
                model="$PI_PROVIDER/$model"
            fi
            ;;
    esac
fi

# opencode: the active model is not exported. The most recently selected
# model comes from the state file; fall back to the configured default.
if [ -z "$model" ] && [ "$harness" = "opencode" ] && command -v jq >/dev/null 2>&1; then
    f="${XDG_STATE_HOME:-$HOME/.local/state}/opencode/model.json"
    if [ -f "$f" ]; then
        model="$(jq -r '
            (.recent[0] // empty)
            | select(.providerID and .modelID)
            | "\(.providerID)/\(.modelID)"' "$f" 2>/dev/null || true)"
    fi
    if [ -z "$model" ] || [ "$model" = "null" ]; then
        f="${XDG_CONFIG_HOME:-$HOME/.config}/opencode/opencode.json"
        if [ -f "$f" ]; then
            model="$(jq -r '.model // empty' "$f" 2>/dev/null || true)"
        fi
    fi
fi

# --- assemble --------------------------------------------------------------
# Empty and placeholder values mean "unknown", not a trailer to print.
[ "$model" = "null" ] && model=""
[ "$model" = "null/null" ] && model=""
[ "$harness" = "null" ] && harness=""

trailer=""
if [ -n "$model" ] && [ -n "$harness" ]; then
    trailer="supported by $model in $harness"
elif [ -n "$model" ]; then
    trailer="supported by $model"
elif [ -n "$harness" ]; then
    trailer="supported in $harness"
fi

if [ -z "$trailer" ]; then
    echo "compute-commit-attribution-trailer: could not determine model or harness" >&2
    exit 1
fi

echo "$trailer"
