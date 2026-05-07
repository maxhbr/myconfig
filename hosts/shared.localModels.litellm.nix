# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the LiteLLM proxy running on `thing` as a localModels provider.
# LiteLLM aggregates the underlying llama-swap instances and prefixes each
# model name with the producer's provider name (e.g. `llama-swap-33656:...`).
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
    "llama-swap-33656:CUDA0:Qwen3.5-9B-Q5_K_M"
    "llama-swap-33656:CUDA0:Qwen3.6-27B-Q8_0"
    "llama-swap-33656:CUDA0:Qwen3.6-27B-Q8_0-tweaked"
    "llama-swap-33656:CUDA0:Qwen3.6-27B-UD-Q4_K_XL"
    "llama-swap-33656:CUDA0:Qwen3.6-27B-UD-Q5_K_XL"
    "llama-swap-33656:CUDA0:Qwen3.6-27B-UD-Q6_K_XL"
    "llama-swap-33656:CUDA0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "llama-swap-33656:CUDA0:gemma-4-26B-A4B-it-Q8_K_XL"
    "llama-swap-33656:CUDA0:gemma-4-31B-it-Q6_K_XL"
    "llama-swap-33656:CUDA0:gemma-4-31B-it-UD-Q4_K_XL"
    "llama-swap-33656:CUDA0:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "llama-swap-33656:CUDA0:gemma-4-31B-it-UD-Q4_K_XL-nothink:mmproj"
    "llama-swap-33656:CUDA0:gemma-4-31B-it-UD-Q4_K_XL:mmproj"
    "llama-swap-33656:Qwen3.6-27B"
    "llama-swap-33656:Qwen3.6-35B"
    "llama-swap-33656:Qwen3.6-35B-A3B"
    "llama-swap-33656:Qwen3.6-35B-A3B-UD"
    "llama-swap-33656:Qwen3.6-35B-A3B-UD-Q5"
    "llama-swap-33656:Qwen3.6-35B-A3B-UD-Q5_K"
    "llama-swap-33656:Vulkan0:Qwen3.5-9B-Q5_K_M"
    "llama-swap-33656:Vulkan0:Qwen3.6-27B-Q8_0"
    "llama-swap-33656:Vulkan0:Qwen3.6-27B-Q8_0-tweaked"
    "llama-swap-33656:Vulkan0:Qwen3.6-27B-UD-Q4_K_XL"
    "llama-swap-33656:Vulkan0:Qwen3.6-27B-UD-Q5_K_XL"
    "llama-swap-33656:Vulkan0:Qwen3.6-27B-UD-Q6_K_XL"
    "llama-swap-33656:Vulkan0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "llama-swap-33656:Vulkan0:gemma-4-26B-A4B-it-Q8_K_XL"
    "llama-swap-33656:Vulkan0:gemma-4-31B-it-Q6_K_XL"
    "llama-swap-33656:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL"
    "llama-swap-33656:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "llama-swap-33656:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL-nothink:mmproj"
    "llama-swap-33656:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL:mmproj"
    "llama-swap-33656:Vulkan1:Qwen3.5-9B-Q5_K_M"
    "llama-swap-33656:Vulkan1:Qwen3.6-27B-Q8_0"
    "llama-swap-33656:Vulkan1:Qwen3.6-27B-Q8_0-tweaked"
    "llama-swap-33656:Vulkan1:Qwen3.6-27B-UD-Q4_K_XL"
    "llama-swap-33656:Vulkan1:Qwen3.6-27B-UD-Q5_K_XL"
    "llama-swap-33656:Vulkan1:Qwen3.6-27B-UD-Q6_K_XL"
    "llama-swap-33656:Vulkan1:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "llama-swap-33656:Vulkan1:gemma-4-26B-A4B-it-Q8_K_XL"
    "llama-swap-33656:Vulkan1:gemma-4-31B-it-Q6_K_XL"
    "llama-swap-33656:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL"
    "llama-swap-33656:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "llama-swap-33656:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL-nothink:mmproj"
    "llama-swap-33656:Vulkan1:gemma-4-31B-it-UD-Q4_K_XL:mmproj"
    "llama-swap-33656:gemma-4-26B-A4B-Q8"
    "llama-swap-33656:gemma-4-31B-Q4"
    "llama-swap-33656:gemma-4-31B-Q6"
    "llama-swap-33656:gemma-4:31b-q4"
    "llama-swap-33656:gemma-4:31b-q4-nothink"
    "llama-swap-33656:hermes"
    "llama-swap-33656:hermes-fallback"
    "llama-swap-33656:opencode-fast"
    "llama-swap-33656:opencode-fast-fallback"
    "llama-swap-33656:sidekick"
    "llama-swap-33657:Qwen3.6-27B"
    "llama-swap-33657:Qwen3.6-27B-GGUF"
    "llama-swap-33657:Qwen3.6-35B"
    "llama-swap-33657:Qwen3.6-35B-A3B"
    "llama-swap-33657:Qwen3.6-35B-A3B-UD"
    "llama-swap-33657:Qwen3.6-35B-A3B-UD-Q5"
    "llama-swap-33657:Qwen3.6-35B-A3B-UD-Q5_K"
    "llama-swap-33657:Vulkan0:Qwen3.5-9B-Q5_K_M"
    "llama-swap-33657:Vulkan0:Qwen3.6-27B-GGUF-BF16"
    "llama-swap-33657:Vulkan0:Qwen3.6-27B-Q8_0"
    "llama-swap-33657:Vulkan0:Qwen3.6-27B-Q8_0-tweaked"
    "llama-swap-33657:Vulkan0:Qwen3.6-27B-UD-Q4_K_XL"
    "llama-swap-33657:Vulkan0:Qwen3.6-27B-UD-Q5_K_XL"
    "llama-swap-33657:Vulkan0:Qwen3.6-27B-UD-Q6_K_XL"
    "llama-swap-33657:Vulkan0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "llama-swap-33657:Vulkan0:gemma-4-26B-A4B-it-Q8_K_XL"
    "llama-swap-33657:Vulkan0:gemma-4-31B-it-BF16"
    "llama-swap-33657:Vulkan0:gemma-4-31B-it-Q6_K_XL"
    "llama-swap-33657:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL"
    "llama-swap-33657:Vulkan0:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "llama-swap-33657:Vulkan0:qwen3.5-122B-A10B-Q5_K_M"
    "llama-swap-33657:gemma-4-26B-A4B-Q8"
    "llama-swap-33657:gemma-4-31B"
    "llama-swap-33657:gemma-4-31B-BF16"
    "llama-swap-33657:gemma-4-31B-Q4"
    "llama-swap-33657:gemma-4-31B-Q6"
    "llama-swap-33657:gemma-4:31b-q4"
    "llama-swap-33657:gemma-4:31b-q4-nothink"
    "llama-swap-33657:hermes"
    "llama-swap-33657:hermes-fallback"
    "llama-swap-33657:opencode"
    "llama-swap-33657:opencode-fallback"
    "llama-swap-33657:opencode-fast"
    "llama-swap-33657:opencode-fast-fallback"
    "llama-swap-33657:opencode-slow"
    "llama-swap-33657:qwen3.5-122B"
    "llama-swap-33657:sidekick"
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
