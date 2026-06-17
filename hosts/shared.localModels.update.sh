#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generates hosts/shared.localModels.{rtx5090,gfx1151,litellm}.nix from the
# live model server `/models` endpoints.

set -euo pipefail

cd "$(dirname "$0")/.."

declare -A SERVER_URLS=(
    [rtx5090]="https://rtx5090.thing.wg0.maxhbr.local/v1/models"
    [gfx1151]="https://gfx1151.thing.wg0.maxhbr.local/v1/models"
    [litellm]="http://thing.wg0.maxhbr.local:4000/models"
)

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

write_nix_litellm() {
    local output="$1"
    local url="$2"
    shift 2
    local models=("$@")

    local models_nix=""
    for m in "${models[@]}"; do
        models_nix+="    \"${m}\""$'\n'
    done

    cat >"$output" <<NIXEOF
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the LiteLLM proxy running on \`thing\` as a localModels provider.
# LiteLLM aggregates the underlying per-GPU model server instances and
# prefixes each model name with the producer's provider name
# (e.g. \`rtx5090:...\` for the NVIDIA RTX 5090 instance,
# \`gfx1151:...\` for the AMD Radeon 8060S iGPU instance).
# LiteLLM listens on \`0.0.0.0:4000\` on \`thing\` (firewall-restricted to
# wg0, see hosts/host.thing/default.nix), so peers reach it directly via the
# wg0 IP — no Caddy in the path.
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
        name = "litellm.thing.wg0";
        inherit models;
        # Direct connection to LiteLLM on thing's wg0 IP (no Caddy proxy).
        host = myconfig.metadatalib.getWgIp "thing";
        port = 4000;
      }
      {
        name = "litellm.thing.vserver.wg0";
        inherit models;
        # Proxy connection via vserver.
        host = "litellm.thing.vserver.wg0.maxhbr.local";
        port = 80;
      }
    ];
  };
}
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
            write_nix_litellm "hosts/shared.localModels.${name}.nix" "$url" "${models_arr[@]}"
            ;;
    esac
done

if [[ -x ./nixfmtall.sh ]]; then
    echo "Formatting..." >&2
    ./nixfmtall.sh 2>/dev/null || true
fi

echo "Done." >&2