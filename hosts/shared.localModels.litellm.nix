# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the LiteLLM proxy running on `thing` as a localModels provider.
# LiteLLM aggregates the underlying per-GPU model server instances and
# prefixes each model name with the producer's provider name
# (e.g. `rtx5090:...` for the NVIDIA RTX 5090 instance,
# `gfx1151:...` for the AMD Radeon 8060S iGPU instance).
# LiteLLM listens on `0.0.0.0:4000` on `thing` (firewall-restricted to wg0,
# see hosts/host.thing/default.nix), so peers reach it directly via the
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
  # Model IDs as exposed by `curl -k https://litellm.thing.wg0.maxhbr.local/models`.
  models = [
    "hermes"
    "hermes-fallback"
    "rtx5090:CUDA0:Qwen3.5-9B-Q5_K_M"
    "rtx5090:CUDA0:Qwen3.6-27B-Q8_0"
    "rtx5090:CUDA0:Qwen3.6-27B-Q8_0-tweaked"
    "rtx5090:CUDA0:Qwen3.6-27B-UD-Q4_K_XL"
    "rtx5090:CUDA0:Qwen3.6-27B-UD-Q5_K_XL"
    "rtx5090:CUDA0:Qwen3.6-27B-UD-Q6_K_XL"
    "rtx5090:CUDA0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "rtx5090:CUDA0:gemma-4-26B-A4B-it-Q8_K_XL"
    "rtx5090:CUDA0:gemma-4-31B-it-Q6_K_XL"
    "rtx5090:CUDA0:gemma-4-31B-it-UD-Q4_K_XL"
    "rtx5090:CUDA0:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "rtx5090:CUDA0:gemma-4-31B-it-UD-Q4_K_XL-nothink:mmproj"
    "rtx5090:CUDA0:gemma-4-31B-it-UD-Q4_K_XL:mmproj"
    "rtx5090:Qwen3.6-27B"
    "rtx5090:Qwen3.6-35B"
    "rtx5090:Qwen3.6-35B-A3B"
    "rtx5090:Qwen3.6-35B-A3B-UD"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K"
    "rtx5090:Vulkan0:Qwen3.5-9B-Q5_K_M"
    "rtx5090:Vulkan0:Qwen3.6-27B-Q8_0"
    "rtx5090:Vulkan0:Qwen3.6-27B-Q8_0-tweaked"
    "rtx5090:Vulkan0:Qwen3.6-27B-UD-Q4_K_XL"
    "rtx5090:Vulkan0:Qwen3.6-27B-UD-Q5_K_XL"
    "rtx5090:Vulkan0:Qwen3.6-27B-UD-Q6_K_XL"
    "rtx5090:Vulkan0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "rtx5090:Vulkan0:gemma-4-26B-A4B-it-Q8_K_XL"
    "rtx5090:Vulkan0:gemma-4-31B-it-Q6_K_XL"
    "rtx5090:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL"
    "rtx5090:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "rtx5090:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL-nothink:mmproj"
    "rtx5090:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL:mmproj"
    "rtx5090:Vulkan1:Qwen3.5-9B-Q5_K_M"
    "rtx5090:Vulkan1:Qwen3.6-27B-Q8_0"
    "rtx5090:Vulkan1:Qwen3.6-27B-Q8_0-tweaked"
    "rtx5090:Vulkan1:Qwen3.6-27B-UD-Q4_K_XL"
    "rtx5090:Vulkan1:Qwen3.6-27B-UD-Q5_K_XL"
    "rtx5090:Vulkan1:Qwen3.6-27B-UD-Q6_K_XL"
    "rtx5090:Vulkan1:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "rtx5090:Vulkan1:gemma-4-26B-A4B-it-Q8_K_XL"
    "rtx5090:Vulkan1:gemma-4-31B-it-Q6_K_XL"
    "rtx5090:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL"
    "rtx5090:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "rtx5090:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL-nothink:mmproj"
    "rtx5090:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL:mmproj"
    "rtx5090:gemma-4-26B-A4B-Q8"
    "rtx5090:gemma-4-31B-Q4"
    "rtx5090:gemma-4-31B-Q6"
    "rtx5090:gemma-4:31b-q4"
    "rtx5090:gemma-4:31b-q4-nothink"
    "rtx5090:hermes"
    "rtx5090:hermes-fallback"
    "rtx5090:opencode-fast"
    "rtx5090:opencode-fast-fallback"
    "rtx5090:sidekick"
    "gfx1151:Qwen3.6-27B"
    "gfx1151:Qwen3.6-27B-GGUF"
    "gfx1151:Qwen3.6-35B"
    "gfx1151:Qwen3.6-35B-A3B"
    "gfx1151:Qwen3.6-35B-A3B-UD"
    "gfx1151:Qwen3.6-35B-A3B-UD-Q5"
    "gfx1151:Qwen3.6-35B-A3B-UD-Q5_K"
    "gfx1151:Vulkan0:Qwen3.5-9B-Q5_K_M"
    "gfx1151:Vulkan0:Qwen3.6-27B-GGUF-BF16"
    "gfx1151:Vulkan0:Qwen3.6-27B-Q8_0"
    "gfx1151:Vulkan0:Qwen3.6-27B-Q8_0-tweaked"
    "gfx1151:Vulkan0:Qwen3.6-27B-UD-Q4_K_XL"
    "gfx1151:Vulkan0:Qwen3.6-27B-UD-Q5_K_XL"
    "gfx1151:Vulkan0:Qwen3.6-27B-UD-Q6_K_XL"
    "gfx1151:Vulkan0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "gfx1151:Vulkan0:gemma-4-26B-A4B-it-Q8_K_XL"
    "gfx1151:Vulkan0:gemma-4-31B-it-BF16"
    "gfx1151:Vulkan0:gemma-4-31B-it-Q6_K_XL"
    "gfx1151:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL"
    "gfx1151:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "gfx1151:Vulkan0:qwen3.5-122B-A10B-Q5_K_M"
    "gfx1151:gemma-4-26B-A4B-Q8"
    "gfx1151:gemma-4-31B"
    "gfx1151:gemma-4-31B-BF16"
    "gfx1151:gemma-4-31B-Q4"
    "gfx1151:gemma-4-31B-Q6"
    "gfx1151:gemma-4:31b-q4"
    "gfx1151:gemma-4:31b-q4-nothink"
    "gfx1151:hermes"
    "gfx1151:hermes-fallback"
    "gfx1151:opencode"
    "gfx1151:opencode-fallback"
    "gfx1151:opencode-fast"
    "gfx1151:opencode-fast-fallback"
    "gfx1151:opencode-slow"
    "gfx1151:qwen3.5-122B"
    "gfx1151:sidekick"
    "localhost:22545:localhost:22545"
    "localhost:22546:localhost:22546"
    "opencode"
    "opencode-fallback"
    "opencode-fast"
    "opencode-fast-fallback"
    "opencode-slow"
    "sidekick"
  ];
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
