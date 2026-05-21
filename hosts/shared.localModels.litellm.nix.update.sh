#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generates hosts/shared.localModels.litellm.nix from the live LiteLLM
# `/models` endpoint exposed on `thing` via wg0 (port 4000).

set -euo pipefail

cd "$(dirname "$0")/.."

LITELLM_HOST="${LITELLM_HOST:-thing.wg0.maxhbr.local}"
LITELLM_PORT="${LITELLM_PORT:-4000}"
LITELLM_URL="${LITELLM_URL:-http://${LITELLM_HOST}:${LITELLM_PORT}/models}"
OUTPUT="hosts/shared.localModels.litellm.nix"

echo "Querying LiteLLM at $LITELLM_URL ..." >&2

# Fetch and extract model IDs (sorted, unique)
mapfile -t MODELS < <(
    curl -sk --fail "$LITELLM_URL" |
        jq -r '.data[].id' |
        LC_ALL=C sort -u
)

if [[ ${#MODELS[@]} -eq 0 ]]; then
    echo "error: no models returned from $LITELLM_URL" >&2
    exit 1
fi

echo "Found ${#MODELS[@]} models" >&2

# Build the models list as Nix syntax
MODELS_NIX=""
for m in "${MODELS[@]}"; do
    MODELS_NIX+="    \"${m}\""$'\n'
done

# Write the file
cat >"$OUTPUT" <<NIXEOF
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the LiteLLM proxy running on \`thing\` as a localModels provider.
# LiteLLM aggregates the underlying per-GPU model server instances and
# prefixes each model name with the producer's provider name
# (e.g. \`rtx5090:...\` for the NVIDIA RTX 5090 instance,
# \`gfx1151:...\` for the AMD Radeon 8060S iGPU instance).
# LiteLLM listens on \`0.0.0.0:${LITELLM_PORT}\` on \`thing\` (firewall-restricted to
# wg0, see hosts/host.thing/default.nix), so peers reach it directly via the
# wg0 IP — no Caddy in the path.
#
# Regenerate with: ./hosts/shared.localModels.litellm.nix.update.sh
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  # Model IDs as exposed by \`curl ${LITELLM_URL}\`.
  models = [
${MODELS_NIX}  ];
in
{
  config = {
    myconfig.ai.localModels = [
      {
        name = "litellm.thing.wg0";
        inherit models;
        # Direct connection to LiteLLM on thing's wg0 IP (no Caddy proxy).
        host = myconfig.metadatalib.getWgIp "thing";
        port = ${LITELLM_PORT};
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

echo "Generated $OUTPUT" >&2

# Format the output
if [[ -x ./nixfmtall.sh ]]; then
    echo "Formatting..." >&2
    ./nixfmtall.sh 2>/dev/null || true
fi

echo "Done. Models:" >&2
printf '  %s\n' "${MODELS[@]}" >&2
