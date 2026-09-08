#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Run `ccusage --json` against the monitored user's coding-agent session
# directories and write Prometheus textfile metrics (consumed by the
# node_exporter textfile collector).
#
# All site-specific values are injected via pkgs.replaceVars substitutions
# before the script is installed:
#
#   textfileDir    directory to write ccusage.prom into
#   monitoredUser  value of the `user` label, and the user whose
#                  session directories are read
#   userHome       HOME presented to ccusage (session dir discovery:
#                  ~/.claude, ~/.config/opencode, ...)
#   lookbackDays   how many days of history to emit (0 = all time)
#   timeoutSeconds maximum runtime for a single ccusage invocation
#
# The script runs as root (the systemd unit sets ProtectHome=read-only),
# so it can read the monitored user's home while atomically writing the
# .prom file into the root-owned textfile directory. XDG_CACHE_HOME is
# redirected into the unit's PrivateTmp so ccusage's pricing cache never
# lands in the user's home.
#
# ccusage is invoked with `-O` (--offline): the nixpkgs package has the
# LiteLLM pricing table baked in at build time (see
# modules/myconfig.ai/programs.ccusage), so no network access is needed
# and the unit carries no network-online dependency.
#
# Metrics written (all carry user="<monitoredUser>"; `period` is the
# YYYY-MM-DD day, `model` the model name from ccusage's breakdowns):
#
#   * ccusage_scrape_success{user}              1 if the last run succeeded
#   * ccusage_scrape_timestamp_seconds{user}    unix time of the last run
#   * ccusage_daily_total_tokens{user,period}   tokens, all models summed
#   * ccusage_daily_input_tokens{user,period}
#   * ccusage_daily_output_tokens{user,period}
#   * ccusage_daily_cache_creation_tokens{user,period}
#   * ccusage_daily_cache_read_tokens{user,period}
#   * ccusage_daily_cost_usd{user,period}       LiteLLM-estimated cost
#   * ccusage_model_input_tokens{user,period,model}
#   * ccusage_model_output_tokens{user,period,model}
#   * ccusage_model_cache_creation_tokens{user,period,model}
#   * ccusage_model_cache_read_tokens{user,period,model}
#   * ccusage_model_cost_usd{user,period,model}
#
# Series churn: period labels older than the lookback window drop out of
# the file on every refresh, so the series count stays bounded at
# roughly `lookbackDays * (models + 1)` per host.
#
# shellcheck disable=SC2154  # variables are substituted by Nix at build time

set -euo pipefail

target="@textfileDir@/ccusage.prom"
tmp="$(mktemp "@textfileDir@/.ccusage.prom.XXXXXX")"
response="$(mktemp)"
trap 'rm -f "$tmp" "$response"' EXIT

user="@monitoredUser@"
export HOME="@userHome@"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp/ccusage-cache}"

now_ts="$(date +%s)"

since_args=()
# The lookback is substituted by Nix at build time, so shellcheck's
# constant-expression warnings do not apply.
# shellcheck disable=SC2050,SC2170
if [ "@lookbackDays@" -gt 0 ]; then
    since_args=(--since "$(date --date="-@lookbackDays@ days" +%Y-%m-%d)")
fi

# -----------------------------------------------------------------------
# On any failure, still emit scrape_success=0 with a fresh timestamp so
# dashboards / alerts can detect the stale data (same contract as the
# weather exporter).
write_failure() {
    {
        printf '# HELP ccusage_scrape_success 1 if the most recent ccusage run succeeded, 0 otherwise.\n'
        printf '# TYPE ccusage_scrape_success gauge\n'
        printf 'ccusage_scrape_success{user="%s"} 0\n' "$user"
        printf '# HELP ccusage_scrape_timestamp_seconds Unix time when the most recent ccusage run finished.\n'
        printf '# TYPE ccusage_scrape_timestamp_seconds gauge\n'
        printf 'ccusage_scrape_timestamp_seconds{user="%s"} %s\n' "$user" "$now_ts"
    } >"$tmp"
    chmod 0644 "$tmp"
    mv -f "$tmp" "$target"
}

if ! timeout "@timeoutSeconds@" ccusage --json -O "${since_args[@]}" >"$response" 2>/dev/null; then
    write_failure
    trap - EXIT
    echo "ccusage-exporter: ccusage invocation failed" >&2
    exit 0
fi

if ! jq -e '(.daily // null) | type == "array"' "$response" >/dev/null 2>&1; then
    write_failure
    trap - EXIT
    echo "ccusage-exporter: unexpected ccusage JSON output" >&2
    exit 0
fi

# -----------------------------------------------------------------------
# Emit metrics. HELP/TYPE headers are static; sample lines come from a
# single jq pass over the daily array.
{
    printf '# HELP ccusage_scrape_success 1 if the most recent ccusage run succeeded, 0 otherwise.\n'
    printf '# TYPE ccusage_scrape_success gauge\n'
    printf 'ccusage_scrape_success{user="%s"} 1\n' "$user"
    printf '# HELP ccusage_scrape_timestamp_seconds Unix time when the most recent ccusage run finished.\n'
    printf '# TYPE ccusage_scrape_timestamp_seconds gauge\n'
    printf 'ccusage_scrape_timestamp_seconds{user="%s"} %s\n' "$user" "$now_ts"

    printf '# HELP ccusage_daily_total_tokens Total tokens consumed on the given day, summed over all agents and models.\n'
    printf '# TYPE ccusage_daily_total_tokens gauge\n'
    printf '# HELP ccusage_daily_input_tokens Input tokens consumed on the given day, summed over all agents and models.\n'
    printf '# TYPE ccusage_daily_input_tokens gauge\n'
    printf '# HELP ccusage_daily_output_tokens Output tokens consumed on the given day, summed over all agents and models.\n'
    printf '# TYPE ccusage_daily_output_tokens gauge\n'
    printf '# HELP ccusage_daily_cache_creation_tokens Cache-creation tokens on the given day, summed over all agents and models.\n'
    printf '# TYPE ccusage_daily_cache_creation_tokens gauge\n'
    printf '# HELP ccusage_daily_cache_read_tokens Cache-read tokens on the given day, summed over all agents and models.\n'
    printf '# TYPE ccusage_daily_cache_read_tokens gauge\n'
    printf '# HELP ccusage_daily_cost_usd LiteLLM-estimated USD cost for the given day, summed over all agents and models.\n'
    printf '# TYPE ccusage_daily_cost_usd gauge\n'
    printf '# HELP ccusage_model_input_tokens Input tokens consumed on the given day for one model.\n'
    printf '# TYPE ccusage_model_input_tokens gauge\n'
    printf '# HELP ccusage_model_output_tokens Output tokens consumed on the given day for one model.\n'
    printf '# TYPE ccusage_model_output_tokens gauge\n'
    printf '# HELP ccusage_model_cache_creation_tokens Cache-creation tokens on the given day for one model.\n'
    printf '# TYPE ccusage_model_cache_creation_tokens gauge\n'
    printf '# HELP ccusage_model_cache_read_tokens Cache-read tokens on the given day for one model.\n'
    printf '# TYPE ccusage_model_cache_read_tokens gauge\n'
    printf '# HELP ccusage_model_cost_usd LiteLLM-estimated USD cost on the given day for one model.\n'
    printf '# TYPE ccusage_model_cost_usd gauge\n'

    jq -r --arg user "$user" '
        def esc: tostring | gsub("\""; "\\\"");

        (.daily // [])[] as $d |
        (
            "ccusage_daily_total_tokens{user=\"\($user)\",period=\"\($d.period)\"} \($d.totalTokens // 0)",
            "ccusage_daily_input_tokens{user=\"\($user)\",period=\"\($d.period)\"} \($d.inputTokens // 0)",
            "ccusage_daily_output_tokens{user=\"\($user)\",period=\"\($d.period)\"} \($d.outputTokens // 0)",
            "ccusage_daily_cache_creation_tokens{user=\"\($user)\",period=\"\($d.period)\"} \($d.cacheCreationTokens // 0)",
            "ccusage_daily_cache_read_tokens{user=\"\($user)\",period=\"\($d.period)\"} \($d.cacheReadTokens // 0)",
            "ccusage_daily_cost_usd{user=\"\($user)\",period=\"\($d.period)\"} \($d.totalCost // 0)"
        ),
        (
            $d.modelBreakdowns[]? |
            "ccusage_model_input_tokens{user=\"\($user)\",period=\"\($d.period)\",model=\"\(.modelName | esc)\"} \(.inputTokens // 0)",
            "ccusage_model_output_tokens{user=\"\($user)\",period=\"\($d.period)\",model=\"\(.modelName | esc)\"} \(.outputTokens // 0)",
            "ccusage_model_cache_creation_tokens{user=\"\($user)\",period=\"\($d.period)\",model=\"\(.modelName | esc)\"} \(.cacheCreationTokens // 0)",
            "ccusage_model_cache_read_tokens{user=\"\($user)\",period=\"\($d.period)\",model=\"\(.modelName | esc)\"} \(.cacheReadTokens // 0)",
            "ccusage_model_cost_usd{user=\"\($user)\",period=\"\($d.period)\",model=\"\(.modelName | esc)\"} \(.cost // 0)"
        )
    ' "$response"
} >"$tmp"

chmod 0644 "$tmp"
mv -f "$tmp" "$target"
trap - EXIT
