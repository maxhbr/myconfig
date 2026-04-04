#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generates hosts/shared.localModels.llama-swap.nix from the evaluated
# llama-swap model keys of a given host (default: thing).

set -euo pipefail

cd "$(dirname "$0")/.."

HOST="${1:-thing}"
OUTPUT="hosts/shared.localModels.llama-swap.nix"

echo "Evaluating llama-swap model keys for host '$HOST'..." >&2

# Extract model key names from the evaluated NixOS config
mapfile -t MODELS < <(
    nix eval ".#nixosConfigurations.${HOST}.config.services.llama-swap.settings.models" \
        --apply 'builtins.attrNames' \
        --json 2>/dev/null | jq -r '.[]'
)

if [[ ${#MODELS[@]} -eq 0 ]]; then
    echo "error: no models found for host '$HOST'" >&2
    exit 1
fi

# Extract the port
PORT=$(nix eval ".#nixosConfigurations.${HOST}.config.services.llama-swap.port" 2>/dev/null)
if [[ -z $PORT ]]; then
    echo "error: could not determine llama-swap port for host '$HOST'" >&2
    exit 1
fi

echo "Found ${#MODELS[@]} models on port $PORT" >&2

# Build the models list as Nix syntax
MODELS_NIX=""
for m in "${MODELS[@]}"; do
    MODELS_NIX+="      \"${m}\""$'\n'
done

# Write the file
cat >"$OUTPUT" <<NIXEOF
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  models = [
${MODELS_NIX}  ];
in
{
  config = {
    myconfig.ai.localModels = [
      {
        name = "llama-swap.${HOST}";
        inherit models;
        port = ${PORT};
        host = myconfig.metadatalib.getIp "${HOST}";
      }
      {
        name = "llama-swap.${HOST}.wg0";
        inherit models;
        port = ${PORT};
        host = myconfig.metadatalib.getWgIp "${HOST}";
      }
    ];
  };
}
NIXEOF

echo "Generated $OUTPUT" >&2

# Format the output
if [[ -x ./nixfmtall.sh ]]; then
    echo "Formatting..." >&2
    ./nixfmtall.sh 2>/dev/null
fi

echo "Done. Models:" >&2
printf '  %s\n' "${MODELS[@]}" >&2
