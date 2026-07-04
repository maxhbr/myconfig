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
  models = [
    "gfx1151:InternScience-Agents-A1-Q8_0"
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
    "gfx1151:Ornith-1.0-35B-Q8_0"
    "gfx1151:Qwen-AgentWorld-35B-A3B-Q8_0"
    "gfx1151:Qwen3-235B-A22B-Instruct-Q2_K_L"
    "gfx1151:Qwen3.5-9B-Q5_K_M"
    "gfx1151:Qwen3.6-27B-MTP-Q8_0"
    "gfx1151:Qwen3.6-27B-MTP-Q8_0-general-tasks"
    "gfx1151:Qwen3.6-27B-MTP-Q8_0-instruct-general-tasks"
    "gfx1151:Qwen3.6-27B-MTP-Q8_0-instruct-reasoning-tasks"
    "gfx1151:Qwen3.6-27B-MTP-Q8_0-precise-coding-tasks"
    "gfx1151:Qwen3.6-27B-MTP-Q8_0-q8_0"
    "gfx1151:Qwen3.6-27B-Q8_0"
    "gfx1151:Qwen3.6-27B-Q8_0-general-tasks"
    "gfx1151:Qwen3.6-27B-Q8_0-instruct-general-tasks"
    "gfx1151:Qwen3.6-27B-Q8_0-instruct-reasoning-tasks"
    "gfx1151:Qwen3.6-27B-Q8_0-precise-coding-tasks"
    "gfx1151:Qwen3.6-27B-Q8_0-q8_0"
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
    "gfx1151:ROCm0:InternScience-Agents-A1-Q8_0"
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
    "gfx1151:ROCm0:Ornith-1.0-35B-Q8_0"
    "gfx1151:ROCm0:Qwen-AgentWorld-35B-A3B-Q8_0"
    "gfx1151:ROCm0:Qwen3-235B-A22B-Instruct-Q2_K_L"
    "gfx1151:ROCm0:Qwen3.5-9B-Q5_K_M"
    "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0"
    "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-general-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-instruct-general-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-instruct-reasoning-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-precise-coding-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-q8_0"
    "gfx1151:ROCm0:Qwen3.6-27B-Q8_0"
    "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-general-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-instruct-general-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-instruct-reasoning-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-precise-coding-tasks"
    "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-q8_0"
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
    "gfx1151:ROCm0:gemma-4-31B-it-BF16"
    "gfx1151:ROCm0:qwen3.5-122B-A10B-Q5_K_M"
    "gfx1151:gemma-4-31B-it-BF16"
    "gfx1151:hermes"
    "gfx1151:opencode"
    "gfx1151:opencode-fallback"
    "gfx1151:opencode-fast"
    "gfx1151:opencode-slow-fallback"
    "gfx1151:qwen3.5-122B-A10B-Q5_K_M"
    "gfx1151:sidekick"
    "hermes"
    "localhost:22545:localhost:22545"
    "localhost:22546:localhost:22546"
    "opencode"
    "opencode-fallback"
    "opencode-fast"
    "opencode-slow-fallback"
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
