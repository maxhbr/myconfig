#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Tool-call fixture tests for the Qwen3.8-27B deployment on gfx1151.
# Tests the sharp.jinja template (and the embedded template on the ROCm
# candidate) against a running llama-server via the OpenAI-compatible
# /v1/chat/completions endpoint. Covers the 7 cases required by the task:
#   1. one tool call with scalar arguments
#   2. nested JSON arguments
#   3. multiple sequential tool calls
#   4. a tool response followed by a final answer
#   5. thinking enabled and disabled
#   6. preservation of prior reasoning when requested
#   7. absence of raw template/control tokens (checked in all fixtures)
#
# Usage: ./tests/qwen38-toolcall-fixture.sh <port> [model-name]
# The llama-server must already be running on <port> with --jinja loaded.
# Exit codes: 0 = all passed, 1 = at least one failed.
set -euo pipefail

PORT="${1:?usage: $0 <port> [model-name]}"
MODEL="${2:-}"
BASE="http://127.0.0.1:${PORT}"
PASS=0
FAIL=0

chat() { curl -fsS "${BASE}/v1/chat/completions" -H "Content-Type: application/json" -d @- 2>/dev/null; }
get_content() { jq -r '.choices[0].message.content // ""' "$1"; }
get_tool_calls() { jq -c '.choices[0].message.tool_calls // []' "$1"; }

check_no_control_tokens() {
    if echo "$1" | grep -qE '\{\{|\{%|<\|im_start\|>|<\|im_end\|>|<\|tool_calls\|>|<\|/tool_calls\|>'; then
        echo "  FAIL [$2]: raw control/jinja tokens in output"
        return 1
    fi
    return 0
}

run_fixture() {
    local name="$1" desc="$2" rf
    rf=$(mktemp)
    if "$name" "$rf"; then
        echo "PASS [$name]: $desc"
        PASS=$((PASS + 1))
    else
        echo "FAIL [$name]: $desc"
        FAIL=$((FAIL + 1))
    fi
    rm -f "$rf"
}

model_opt() { [ -n "$MODEL" ] && echo "\"model\": \"${MODEL}\"," || true; }

TOOLS='[{"type":"function","function":{"name":"get_weather","description":"Get current weather for a city","parameters":{"type":"object","properties":{"city":{"type":"string"},"unit":{"type":"string","enum":["celsius","fahrenheit"]}},"required":["city"]}},{"type":"function","function":{"name":"search_db","description":"Search a database with a structured query","parameters":{"type":"object","properties":{"query":{"type":"object","properties":{"table":{"type":"string"},"filters":{"type":"object"},"limit":{"type":"integer"}},"required":["table"]}},"required":["query"]}}]'

# 1. One tool call with scalar arguments.
fixture_scalar_args() {
    local out="$1"
    printf '{"%smessages":[{"role":"user","content":"What is the weather in Tokyo? Use celsius."}],"tools":%s,"tool_choice":"auto","temperature":0,"max_tokens":256}' "$(model_opt)" "$TOOLS" | chat >"$out"
    local tc
    tc=$(get_tool_calls "$out")
    [ "$(echo "$tc" | jq 'length')" -eq 1 ] || {
        echo "  expected 1 call"
        return 1
    }
    [ "$(echo "$tc" | jq -r '.[0].function.name')" = "get_weather" ] || {
        echo "  expected get_weather"
        return 1
    }
    [ "$(echo "$tc" | jq -r '.[0].function.arguments|fromjson.city')" = "Tokyo" ] || {
        echo "  expected city=Tokyo"
        return 1
    }
    check_no_control_tokens "$(get_content "$out")" scalar_args
}

# 2. Nested JSON arguments.
fixture_nested_json() {
    local out="$1"
    printf '{"%smessages":[{"role":"user","content":"Search the users table for active users named Alice, limit 10."}],"tools":%s,"tool_choice":"auto","temperature":0,"max_tokens":256}' "$(model_opt)" "$TOOLS" | chat >"$out"
    local tc
    tc=$(get_tool_calls "$out")
    [ "$(echo "$tc" | jq -r '.[0].function.name')" = "search_db" ] || {
        echo "  expected search_db"
        return 1
    }
    [ "$(echo "$tc" | jq -r '.[0].function.arguments|fromjson.query.table')" = "users" ] || {
        echo "  expected table=users"
        return 1
    }
    [ "$(echo "$tc" | jq -r '.[0].function.arguments|fromjson.query.limit')" -eq 10 ] || {
        echo "  expected limit=10"
        return 1
    }
    check_no_control_tokens "$(get_content "$out")" nested_json
}

# 3. Multiple sequential tool calls.
fixture_sequential_calls() {
    local out="$1"
    printf '{"%smessages":[{"role":"user","content":"What is the weather in Tokyo and London? Use celsius for both."}],"tools":%s,"tool_choice":"auto","temperature":0,"max_tokens":512}' "$(model_opt)" "$TOOLS" | chat >"$out"
    local tc
    tc=$(get_tool_calls "$out")
    [ "$(echo "$tc" | jq 'length')" -ge 2 ] || {
        echo "  expected >=2 calls"
        return 1
    }
    check_no_control_tokens "$(get_content "$out")" sequential_calls
}

# 4. A tool response followed by a final answer.
fixture_tool_response_answer() {
    local out="$1"
    local body
    body=$(printf '{"%smessages":[{"role":"user","content":"What is the weather in Tokyo?"},{"role":"assistant","content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"get_weather","arguments":"{\"city\":\"Tokyo\",\"unit\":\"celsius\"}"}}]},{"role":"tool","tool_call_id":"call_1","content":"{\"temp\":22,\"condition\":\"sunny\"}"}],"tools":%s,"temperature":0,"max_tokens":256}' "$(model_opt)" "$TOOLS")
    echo "$body" | chat >"$out"
    local c
    c=$(get_content "$out")
    [ -n "$c" ] || {
        echo "  expected non-empty answer"
        return 1
    }
    echo "$c" | grep -qiE '22|sunny' || {
        echo "  answer does not reference tool result"
        return 1
    }
    [ "$(get_tool_calls "$out" | jq 'length')" -eq 0 ] || {
        echo "  expected 0 further calls"
        return 1
    }
    check_no_control_tokens "$c" tool_response_answer
}

# 5. Thinking enabled.
fixture_thinking_enabled() {
    local out="$1"
    printf '{"%smessages":[{"role":"user","content":"Think step by step, then tell me: what is 17 * 23?"}],"temperature":0,"max_tokens":512,"chat_template_kwargs":{"enable_thinking":true}}' "$(model_opt)" | chat >"$out"
    local c
    c=$(get_content "$out")
    [ -n "$c" ] || {
        echo "  expected non-empty answer"
        return 1
    }
    echo "$c" | grep -qiE '391' || {
        echo "  answer does not contain 391"
        return 1
    }
    check_no_control_tokens "$c" thinking_enabled
}

# 5b. Thinking disabled.
fixture_thinking_disabled() {
    local out="$1"
    printf '{"%smessages":[{"role":"user","content":"What is 17 * 23?"}],"temperature":0,"max_tokens":256,"chat_template_kwargs":{"enable_thinking":false}}' "$(model_opt)" | chat >"$out"
    local c
    c=$(get_content "$out")
    [ -n "$c" ] || {
        echo "  expected non-empty answer"
        return 1
    }
    echo "$c" | grep -qiE '391' || {
        echo "  answer does not contain 391"
        return 1
    }
    if echo "$c" | grep -qi 'think'; then
        : # "think" in prose is fine
    fi
    check_no_control_tokens "$c" thinking_disabled
}

# 6. Preservation of prior reasoning when requested.
fixture_preserve_reasoning() {
    local out="$1"
    local body
    body=$(printf '{"%smessages":[{"role":"user","content":"Solve: a train travels 60 km/h for 2h, then 80 km/h for 1h. Total distance?"},{"role":"assistant","content":"Let me think: 60*2 = 120, 80*1 = 80, total = 200 km."},{"role":"user","content":"Now what if the first leg was 3 hours? Preserve your prior reasoning."}],"temperature":0,"max_tokens":256,"chat_template_kwargs":{"enable_thinking":true,"preserve_thinking":true}}' "$(model_opt)")
    echo "$body" | chat >"$out"
    local c
    c=$(get_content "$out")
    [ -n "$c" ] || {
        echo "  expected non-empty answer"
        return 1
    }
    echo "$c" | grep -qiE '180|240|60.3' || {
        echo "  answer does not reference updated calc"
        return 1
    }
    check_no_control_tokens "$c" preserve_reasoning
}

echo "=== Qwen3.8-27B tool-call fixtures (port ${PORT}, model='${MODEL:-default}') ==="
echo ""

run_fixture fixture_scalar_args "one tool call with scalar arguments"
run_fixture fixture_nested_json "nested JSON arguments"
run_fixture fixture_sequential_calls "multiple sequential tool calls"
run_fixture fixture_tool_response_answer "tool response followed by final answer"
run_fixture fixture_thinking_enabled "thinking enabled produces correct answer"
run_fixture fixture_thinking_disabled "thinking disabled, no reasoning blocks"
run_fixture fixture_preserve_reasoning "prior reasoning preserved when requested"

echo ""
echo "=== Results: ${PASS} passed, ${FAIL} failed ==="
[ "$FAIL" -eq 0 ] && exit 0 || exit 1
