# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the LiteLLM proxy running on `thing` as a localModels provider.
# LiteLLM aggregates the underlying per-GPU model server instances and
# prefixes each model name with the producer's provider name
# (e.g. `rtx5090:...` for the NVIDIA RTX 5090 instance,
# `gfx1151:...` for the AMD Radeon 8060S iGPU instance).
# LiteLLM listens on `0.0.0.0:4000` on `thing` (firewall-restricted to
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
  # Model IDs as exposed by `curl http://thing.wg0.maxhbr.local:4000/models`.
  # Single source of truth in ./shared.localModels.litellm.models.nix
  # (also consumed by ./shared.litellm.proxy.nix).
  models = import ./shared.localModels.litellm.models.nix;
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
    ];
  };
}
