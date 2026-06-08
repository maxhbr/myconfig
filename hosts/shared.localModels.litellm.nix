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
  # Model IDs as exposed by `curl http://thing.wg0.maxhbr.local:4000/models`.
  models = [
    "Qwen3.6-27B"
    "Qwen3.6-35B"
    "Qwen3.6-35B-A3B"
    "Qwen3.6-35B-A3B-UD"
    "Qwen3.6-35B-A3B-UD-Q5"
    "Qwen3.6-35B-A3B-UD-Q5_K"
    "gemma-4-26B-Q4-QAT"
    "gemma-4-26B-Q6"
    "gemma-4-26B-Q8"
    "gemma-4-31B-Q4"
    "gemma-4-31B-Q4-QAT"
    "gemma-4-31B-Q5"
    "gemma-4-MoE"
    "gemma-4-dense"
    "gfx1151:MiniMax-M2.7-UD-IQ4_NL"
    "gfx1151:MiniMax-M2.7-UD-IQ4_NL-196k"
    "gfx1151:MiniMax-M2.7-UD-IQ4_NL-49k"
    "gfx1151:MiniMax-M2.7-UD-IQ4_XS"
    "gfx1151:MiniMax-M2.7-UD-IQ4_XS-196k"
    "gfx1151:MiniMax-M2.7-UD-IQ4_XS-49k"
    "gfx1151:MiniMax-M2.7-UD-Q3_K_S"
    "gfx1151:MiniMax-M2.7-UD-Q3_K_S-196k"
    "gfx1151:MiniMax-M2.7-UD-Q3_K_S-49k"
    "gfx1151:NVIDIA-Nemotron-3-Nano-Omni-Q8_0"
    "gfx1151:NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M"
    "gfx1151:Qwen3.5-9B-Q5_K_M"
    "gfx1151:Qwen3.6-27B"
    "gfx1151:Qwen3.6-27B-BF16"
    "gfx1151:Qwen3.6-27B-MTP-Q8_0"
    "gfx1151:Qwen3.6-27B-Q8_0"
    "gfx1151:Qwen3.6-27B-UD-Q4_K_XL"
    "gfx1151:Qwen3.6-27B-UD-Q5_K_XL"
    "gfx1151:Qwen3.6-27B-UD-Q6_K_XL"
    "gfx1151:Qwen3.6-35B"
    "gfx1151:Qwen3.6-35B-A3B"
    "gfx1151:Qwen3.6-35B-A3B-BF16"
    "gfx1151:Qwen3.6-35B-A3B-BF16-instruct-general"
    "gfx1151:Qwen3.6-35B-A3B-BF16-instruct-reasoning"
    "gfx1151:Qwen3.6-35B-A3B-BF16-thinking-coding"
    "gfx1151:Qwen3.6-35B-A3B-BF16-thinking-general"
    "gfx1151:Qwen3.6-35B-A3B-MTP-BF16"
    "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-instruct-general"
    "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-instruct-reasoning"
    "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-thinking-coding"
    "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-thinking-general"
    "gfx1151:Qwen3.6-35B-A3B-Q8_0"
    "gfx1151:Qwen3.6-35B-A3B-Q8_0-instruct-general"
    "gfx1151:Qwen3.6-35B-A3B-Q8_0-instruct-reasoning"
    "gfx1151:Qwen3.6-35B-A3B-Q8_0-thinking-coding"
    "gfx1151:Qwen3.6-35B-A3B-Q8_0-thinking-general"
    "gfx1151:Qwen3.6-35B-A3B-UD"
    "gfx1151:Qwen3.6-35B-A3B-UD-Q5"
    "gfx1151:Qwen3.6-35B-A3B-UD-Q5_K"
    "gfx1151:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_NL"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_NL-196k"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_NL-49k"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_XS"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_XS-196k"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_XS-49k"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-Q3_K_S"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-Q3_K_S-196k"
    "gfx1151:ROCm0:MiniMax-M2.7-UD-Q3_K_S-49k"
    "gfx1151:ROCm0:NVIDIA-Nemotron-3-Nano-Omni-Q8_0"
    "gfx1151:ROCm0:NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M"
    "gfx1151:ROCm0:Qwen3.5-9B-Q5_K_M"
    "gfx1151:ROCm0:Qwen3.6-27B-BF16"
    "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0"
    "gfx1151:ROCm0:Qwen3.6-27B-Q8_0"
    "gfx1151:ROCm0:Qwen3.6-27B-UD-Q4_K_XL"
    "gfx1151:ROCm0:Qwen3.6-27B-UD-Q5_K_XL"
    "gfx1151:ROCm0:Qwen3.6-27B-UD-Q6_K_XL"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-instruct-general"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-instruct-reasoning"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-thinking-coding"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-thinking-general"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-instruct-general"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-instruct-reasoning"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-thinking-coding"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-thinking-general"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-instruct-general"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-instruct-reasoning"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-thinking-coding"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-thinking-general"
    "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "gfx1151:ROCm0:gemma-4-26B-A4B-it-UD-Q6_K_XL"
    "gfx1151:ROCm0:gemma-4-26B-A4B-it-UD-Q8_K_XL"
    "gfx1151:ROCm0:gemma-4-26B-A4B-it-qat-q4_0"
    "gfx1151:ROCm0:gemma-4-31B-it-BF16"
    "gfx1151:ROCm0:gemma-4-31B-it-UD-Q4_K_XL"
    "gfx1151:ROCm0:gemma-4-31B-it-UD-Q5_K_XL"
    "gfx1151:ROCm0:gemma-4-31B-it-qat-q4_0"
    "gfx1151:ROCm0:qwen3.5-122B-A10B-Q5_K_M"
    "gfx1151:gemma-4-26B-A4B-it-UD-Q6_K_XL"
    "gfx1151:gemma-4-26B-A4B-it-UD-Q8_K_XL"
    "gfx1151:gemma-4-26B-A4B-it-qat-q4_0"
    "gfx1151:gemma-4-26B-Q4-QAT"
    "gfx1151:gemma-4-26B-Q6"
    "gfx1151:gemma-4-26B-Q8"
    "gfx1151:gemma-4-31B"
    "gfx1151:gemma-4-31B-BF16"
    "gfx1151:gemma-4-31B-Q4"
    "gfx1151:gemma-4-31B-Q4-QAT"
    "gfx1151:gemma-4-31B-Q5"
    "gfx1151:gemma-4-31B-it-BF16"
    "gfx1151:gemma-4-31B-it-UD-Q4_K_XL"
    "gfx1151:gemma-4-31B-it-UD-Q5_K_XL"
    "gfx1151:gemma-4-31B-it-qat-q4_0"
    "gfx1151:gemma-4-MoE"
    "gfx1151:gemma-4-dense"
    "gfx1151:hermes"
    "gfx1151:hermes-fallback"
    "gfx1151:opencode"
    "gfx1151:opencode-fallback"
    "gfx1151:opencode-fast-fallback"
    "gfx1151:opencode-slow"
    "gfx1151:qwen3.5-122B"
    "gfx1151:qwen3.5-122B-A10B-Q5_K_M"
    "gfx1151:sidekick"
    "hermes"
    "hermes-fallback"
    "localhost:22545:localhost:22545"
    "localhost:22546:localhost:22546"
    "opencode"
    "opencode-fallback"
    "opencode-fast"
    "opencode-fast-fallback"
    "opencode-slow"
    "rtx5090:Qwen3.5-9B-Q5_K_M"
    "rtx5090:Qwen3.6-27B"
    "rtx5090:Qwen3.6-27B-Q8_0"
    "rtx5090:Qwen3.6-27B-Q8_0-tweaked"
    "rtx5090:Qwen3.6-27B-UD-Q4_K_XL"
    "rtx5090:Qwen3.6-27B-UD-Q5_K_XL"
    "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-general-tasks"
    "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-instruct-general-tasks"
    "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-instruct-reasoning-tasks"
    "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-modded"
    "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-precise-coding-tasks"
    "rtx5090:Qwen3.6-27B-UD-Q6_K_XL"
    "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-131072"
    "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-65536"
    "rtx5090:Qwen3.6-35B"
    "rtx5090:Qwen3.6-35B-A3B"
    "rtx5090:Qwen3.6-35B-A3B-UD"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-general"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-reasoning"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-coding"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-general"
    "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-tweaked"
    "rtx5090:gemma-4-26B-A4B-it-UD-Q6_K_XL"
    "rtx5090:gemma-4-26B-A4B-it-UD-Q8_K_XL"
    "rtx5090:gemma-4-26B-A4B-it-qat-q4_0"
    "rtx5090:gemma-4-26B-A4B-it-qat-q4_0-mmproj"
    "rtx5090:gemma-4-26B-A4B-it-qat-q4_0-nothink"
    "rtx5090:gemma-4-26B-Q4-QAT"
    "rtx5090:gemma-4-26B-Q6"
    "rtx5090:gemma-4-26B-Q8"
    "rtx5090:gemma-4-31B-Q4"
    "rtx5090:gemma-4-31B-Q4-QAT"
    "rtx5090:gemma-4-31B-Q5"
    "rtx5090:gemma-4-31B-it-UD-Q4_K_XL"
    "rtx5090:gemma-4-31B-it-UD-Q4_K_XL-mmproj"
    "rtx5090:gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "rtx5090:gemma-4-31B-it-UD-Q5_K_XL"
    "rtx5090:gemma-4-31B-it-qat-q4_0"
    "rtx5090:gemma-4-31B-it-qat-q4_0-mmproj"
    "rtx5090:gemma-4-31B-it-qat-q4_0-nothink"
    "rtx5090:gemma-4-MoE"
    "rtx5090:gemma-4-dense"
    "rtx5090:hermes"
    "rtx5090:hermes-fallback"
    "rtx5090:opencode-fast"
    "rtx5090:opencode-fast-fallback"
    "rtx5090:sidekick"
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
