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

# CTX_MAP[<prefix>:<model-or-alias>] = <ctx-size in tokens>, populated by
# build_ctx_map and consulted by write_nix_litellm.
declare -A CTX_MAP=()

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
# it (see build_ctx_map), a `{ name; contextWindow; }` attrset — both
# accepted by the consumers' `models` options.
write_nix_litellm() {
    local output="$1"
    local url="$2"
    shift 2
    local models=("$@")

    local models_nix=""
    local m ctx
    for m in "${models[@]}"; do
        ctx="${CTX_MAP[$m]:-}"
        if [[ -n $ctx ]]; then
            models_nix+="  {"$'\n'
            models_nix+="    name = \"${m}\";"$'\n'
            models_nix+="    contextWindow = ${ctx};"$'\n'
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
# \`{ name; contextWindow; }\` attrset. contextWindow (tokens) is scraped
# from the backend llama-server's \`--ctx-size\` for models whose backend
# publishes it; the rest are bare strings.
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
            build_ctx_map
            write_nix_litellm "hosts/shared.localModels.${name}.models.nix" "$url" "${models_arr[@]}"
            ;;
    esac
done

if [[ -x ./nixfmtall.sh ]]; then
    echo "Formatting..." >&2
    ./nixfmtall.sh 2>/dev/null || true
fi

echo "Done." >&2