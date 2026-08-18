#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generates the model lists consumed by the shared.localModels.* /
# shared.litellm.* modules from the live model server `/models` endpoints:
#   - hosts/shared.localModels.{rtx5090,gfx1151}.nix  (inline model lists)
#   - hosts/shared.localModels.litellm.models.nix     (single source of
#     truth for the litellm model names, imported by both
#     hosts/shared.localModels.litellm.nix and hosts/shared.litellm.proxy.nix)
#
# For the litellm list, models whose backend advertises the loaded
# llama-server command line (its `--ctx-size`) are emitted as
# `{ name; contextWindow; }` attrsets; the rest stay as bare strings.
# See build_ctx_map / CTX_SOURCE_URLS below.

set -euo pipefail

cd "$(dirname "$0")/.."

declare -A SERVER_URLS=(
    [rtx5090]="https://rtx5090.thing.wg0.maxhbr.local/v1/models"
    [gfx1151]="https://gfx1151.thing.wg0.maxhbr.local/v1/models"
    [litellm]="http://thing.wg0.maxhbr.local:4000/models"
)

# Backends whose `/v1/models` embeds the loaded llama-server command line
# in `status.args` (and thus `--ctx-size`). Keyed by the provider prefix
# LiteLLM uses for that backend's models (e.g. `rtx5090:<model>`), so the
# scraped context sizes can be matched against the prefixed litellm model
# names. Backends absent here (or whose `/v1/models` omits the args, e.g.
# plain llama-swap) simply contribute no context sizes.
declare -A CTX_SOURCE_URLS=(
    [rtx5090]="https://rtx5090.thing.wg0.maxhbr.local/v1/models"
    [gfx1151]="https://gfx1151.thing.wg0.maxhbr.local/v1/models"
)

# The upstream LiteLLM's own model registry. Unlike CTX_SOURCE_URLS this
# reports the DECLARED budgets (from `myconfig.ai.localModels`, i.e. the
# llama-cpp module's `ctxSize`), so it works for every model regardless
# of whether llama-swap currently has it loaded. See build_budget_map.
LITELLM_INFO_URL="http://thing.wg0.maxhbr.local:4000/model/info"

# CTX_MAP[<prefix>:<model-or-alias>] = <ctx-size in tokens> and
# MAXOUT_MAP[<same key>] = <max output tokens>, populated by
# build_ctx_map / build_budget_map and consulted by write_nix_litellm.
declare -A CTX_MAP=()
declare -A MAXOUT_MAP=()

# Scrape `--ctx-size` for every model (and alias) each CTX_SOURCE_URLS
# backend exposes, keyed by the litellm provider prefix. Models without a
# `--ctx-size` arg (or on backends that don't publish the args) are left
# out and later emitted as bare strings (no contextWindow).
build_ctx_map() {
    local prefix url key ctx
    for prefix in "${!CTX_SOURCE_URLS[@]}"; do
        url="${CTX_SOURCE_URLS[$prefix]}"
        echo "Scraping context sizes from ${prefix} at $url ..." >&2
        while IFS=$'\t' read -r key ctx; do
            [[ -z $key ]] && continue
            CTX_MAP["${prefix}:${key}"]="$ctx"
        done < <(curl -sk --fail --max-time 30 "$url" | jq -r '
            .data[]
            | (.status.args // []) as $a
            | ($a | index("--ctx-size")) as $i
            | select($i != null)
            | ($a[$i + 1]) as $ctx
            | ([.id] + (.aliases // []))[]
            | "\(.)\t\($ctx)"
        ' || true)
    done
    echo "Scraped ${#CTX_MAP[@]} context-size entries" >&2
}

# Scrape the DECLARED budgets from the upstream LiteLLM's /model/info.
#
# This is the authoritative source and is preferred over build_ctx_map:
# `/v1/models` only carries `status.args` for models llama-swap has
# currently LOADED, so with a short `ttl` almost every model is
# "unloaded" at scrape time and contributes nothing (that is why most
# gfx1151 entries used to end up as bare strings). /model/info instead
# reflects what the Nix config declares, so it is stable.
#
# Keys match write_nix_litellm's model names because LiteLLM's
# `model_name` is already the prefixed form (e.g. `gfx1151:<model>`).
build_budget_map() {
    local key ctx maxout n_ctx=0 n_out=0
    echo "Scraping declared budgets from $LITELLM_INFO_URL ..." >&2
    while IFS=$'\t' read -r key ctx maxout; do
        [[ -z $key ]] && continue
        if [[ -n $ctx && $ctx != null ]]; then
            CTX_MAP["$key"]="$ctx"
            n_ctx=$((n_ctx + 1))
        fi
        if [[ -n $maxout && $maxout != null ]]; then
            MAXOUT_MAP["$key"]="$maxout"
            n_out=$((n_out + 1))
        fi
    done < <(curl -sk --fail --max-time 30 "$LITELLM_INFO_URL" | jq -r '
        .data[]
        | . as $e
        | [ .model_name
          , ((.litellm_params.max_input_tokens // .model_info.max_input_tokens) // "null")
          , ((.litellm_params.max_tokens // .model_info.max_output_tokens) // "null")
          ]
        | @tsv
    ' || true)
    echo "Scraped $n_ctx context-window and $n_out max-output entries" >&2
}

fetch_models() {
    local name="$1"
    local url="$2"
    echo "Querying ${name} at $url ..." >&2

    local models
    models=$(curl -sk --fail "$url" | jq -r '.data[].id' | LC_ALL=C sort -u)

    if [[ -z "$models" ]]; then
        echo "error: no models returned from $url" >&2
        exit 1
    fi

    local count
    count=$(echo "$models" | wc -l)
    echo "Found $count models for ${name}" >&2

    echo "$models"
}

write_nix_rtxgfx() {
    local output="$1"
    local name="$2"
    local url="$3"
    shift 3
    local models=("$@")

    local models_nix=""
    for m in "${models[@]}"; do
        models_nix+="    \"${m}\""$'\n'
    done

    cat >"$output" <<NIXEOF
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the ${name} model server running on \`thing\` as a localModels provider.
# The server listens on \`0.0.0.0:80\` on \`thing\` (firewall-restricted to
# wg0, see hosts/host.thing/default.nix), so peers reach it via the
# wg0 IP.
#
# Regenerate with: ./hosts/shared.localModels.update.sh
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  # Model IDs as exposed by \`curl ${url}\`.
  models = [
${models_nix}  ];
in
{
  config = {
    myconfig.ai.localModels = [
      {
        name = "${name}.thing.wg0";
        inherit models;
        host = "https://${name}.thing.wg0.maxhbr.local/v1";
        port = 80;
      }
    ];
  };
}
NIXEOF

    echo "Generated $output" >&2
}

# Writes the shared litellm model list (single source of truth). The
# wrapper modules hosts/shared.localModels.litellm.nix and
# hosts/shared.litellm.proxy.nix both `import` this file. Each entry is
# either a bare model-name string or, when a context size was scraped for
# it (see build_ctx_map / build_budget_map), a
# `{ name; contextWindow; maxOutputTokens?; }` attrset — both accepted
# by the consumers' `models` options.
write_nix_litellm() {
    local output="$1"
    local url="$2"
    shift 2
    local models=("$@")

    local models_nix=""
    local m ctx maxout
    for m in "${models[@]}"; do
        ctx="${CTX_MAP[$m]:-}"
        maxout="${MAXOUT_MAP[$m]:-}"
        if [[ -n $ctx || -n $maxout ]]; then
            models_nix+="  {"$'\n'
            models_nix+="    name = \"${m}\";"$'\n'
            [[ -n $ctx ]] && models_nix+="    contextWindow = ${ctx};"$'\n'
            [[ -n $maxout ]] && models_nix+="    maxOutputTokens = ${maxout};"$'\n'
            models_nix+="  }"$'\n'
        else
            models_nix+="  \"${m}\""$'\n'
        fi
    done

    cat >"$output" <<NIXEOF
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Single source of truth for the model names published by thing's
# LiteLLM proxy (the output of
#   curl ${url}
# ). Consumed by both:
#   - hosts/shared.localModels.litellm.nix   (direct localModels providers)
#   - hosts/shared.litellm.proxy.nix          (local LiteLLM proxy: f13, p14)
#
# Each entry is either a bare model-name string or a
# \`{ name; contextWindow; maxOutputTokens?; }\` attrset. Both numbers
# come from the upstream LiteLLM's \`/model/info\` (which reports what the
# llama-cpp module declares), falling back to the backend
# llama-server's \`--ctx-size\` for models that LiteLLM does not describe;
# the rest are bare strings.
#
# Regenerate with: ./hosts/shared.localModels.update.sh
[
${models_nix}]
NIXEOF

    echo "Generated $output" >&2
}

if [[ $# -eq 0 ]]; then
    mapfile -t TARGETS < <(printf '%s\n' "${!SERVER_URLS[@]}" | LC_ALL=C sort)
else
    IFS=' ' read -r -a TARGETS <<< "$*"
fi

for name in "${TARGETS[@]}"; do
    if [[ ! -v SERVER_URLS[$name] ]]; then
        echo "error: unknown target '$name', available: ${!SERVER_URLS[*]}" >&2
        exit 1
    fi
    url="${SERVER_URLS[$name]}"
    models_str=$(fetch_models "$name" "$url")
    mapfile -t models_arr <<< "$models_str"

    case "$name" in
        rtx5090|gfx1151)
            write_nix_rtxgfx "hosts/shared.localModels.${name}.nix" "$name" "$url" "${models_arr[@]}"
            ;;
        litellm)
            # Least specific first: the /v1/models `--ctx-size` scrape
            # only sees currently-loaded models, and build_budget_map
            # overwrites its keys with the declared values.
            build_ctx_map
            build_budget_map
            write_nix_litellm "hosts/shared.localModels.${name}.models.nix" "$url" "${models_arr[@]}"
            ;;
    esac
done

if [[ -x ./nixfmtall.sh ]]; then
    echo "Formatting..." >&2
    ./nixfmtall.sh 2>/dev/null || true
fi

echo "Done." >&2