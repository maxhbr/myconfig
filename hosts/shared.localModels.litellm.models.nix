# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Single source of truth for the model names published by thing's
# LiteLLM proxy (the output of
#   curl http://thing.wg0.maxhbr.local:4000/models
# ). Consumed by both:
#   - hosts/shared.localModels.litellm.nix   (direct localModels providers)
#   - hosts/shared.litellm.proxy.nix          (local LiteLLM proxy: f13, p14)
#
# Each entry is either a bare model-name string or a
# `{ name; contextWindow; }` attrset. contextWindow (tokens) is scraped
# from the backend llama-server's `--ctx-size` for models whose backend
# publishes it; the rest are bare strings.
#
# Regenerate with: ./hosts/shared.localModels.update.sh
[
  "gfx1151:Hy3-Q2_K_L"
  "gfx1151:InternScience-Agents-A1-Q4_K_M"
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
  "gfx1151:Qwen3-235B-A22B-Instruct-Q2_K_L"
  "gfx1151:Qwen3.5-9B-Q5_K_M"
  "gfx1151:Qwen3.6-27B-MTP-Q8_0"
  "gfx1151:Qwen3.6-27B-MTP-Q8_0-general-tasks"
  "gfx1151:Qwen3.6-27B-MTP-Q8_0-instruct-general-tasks"
  "gfx1151:Qwen3.6-27B-MTP-Q8_0-instruct-reasoning-tasks"
  "gfx1151:Qwen3.6-27B-MTP-Q8_0-precise-coding-tasks"
  "gfx1151:Qwen3.6-27B-MTP-Q8_0-q8_0"
  "gfx1151:Qwen3.6-27B-Q6_K-MTP"
  "gfx1151:Qwen3.6-27B-Q8_0"
  "gfx1151:Qwen3.6-27B-Q8_0-general-tasks"
  "gfx1151:Qwen3.6-27B-Q8_0-instruct-general-tasks"
  "gfx1151:Qwen3.6-27B-Q8_0-instruct-reasoning-tasks"
  "gfx1151:Qwen3.6-27B-Q8_0-precise-coding-tasks"
  "gfx1151:Qwen3.6-27B-Q8_0-q8_0"
  "gfx1151:Qwen3.6-27B-UD-Q4_K_XL"
  "gfx1151:Qwen3.6-27B-UD-Q5_K_XL"
  "gfx1151:Qwen3.6-27B-UD-Q6_K_XL"
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
  "gfx1151:Qwen3.6-35B-A3B-UD-Q5_K_XL"
  "gfx1151:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP"
  "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP"
  "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-general"
  "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-reasoning"
  "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-coding"
  "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-general"
  "gfx1151:Qwen3.8-27B-ABL-Q5_K_M"
  "gfx1151:Qwen3.8-27B-UD-Q4_K_XL"
  "gfx1151:Qwen3.8-27B-UD-Q5_K_XL"
  "gfx1151:Qwen3.8-27B-UD-Q6_K_XL"
  "gfx1151:Qwen3.8-27B-UD-Q8_K_XL"
  "gfx1151:Qwen3.8-27B-UD-Q8_K_XL-general-tasks"
  "gfx1151:Qwen3.8-27B-UD-Q8_K_XL-instruct-general-tasks"
  "gfx1151:Qwen3.8-27B-UD-Q8_K_XL-instruct-reasoning-tasks"
  "gfx1151:Qwen3.8-27B-UD-Q8_K_XL-precise-coding-tasks"
  "gfx1151:ROCm0:Hy3-Q2_K_L"
  "gfx1151:ROCm0:InternScience-Agents-A1-Q4_K_M"
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
  "gfx1151:ROCm0:Qwen3-235B-A22B-Instruct-Q2_K_L"
  "gfx1151:ROCm0:Qwen3.5-9B-Q5_K_M"
  "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0"
  "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-general-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-instruct-general-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-instruct-reasoning-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-precise-coding-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-q8_0"
  "gfx1151:ROCm0:Qwen3.6-27B-Q6_K-MTP"
  "gfx1151:ROCm0:Qwen3.6-27B-Q8_0"
  "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-general-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-instruct-general-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-instruct-reasoning-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-precise-coding-tasks"
  "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-q8_0"
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
  "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP"
  "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP"
  "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-general"
  "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-reasoning"
  "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-coding"
  "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-general"
  "gfx1151:ROCm0:Qwen3.8-27B-ABL-Q5_K_M"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q4_K_XL"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q5_K_XL"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q6_K_XL"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q8_K_XL"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q8_K_XL-general-tasks"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q8_K_XL-instruct-general-tasks"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q8_K_XL-instruct-reasoning-tasks"
  "gfx1151:ROCm0:Qwen3.8-27B-UD-Q8_K_XL-precise-coding-tasks"
  "gfx1151:ROCm0:TheDrummer_Skyfall-31B-v4.2-Q6_K"
  "gfx1151:ROCm0:gemma-4-26B-A4B-it-UD-Q6_K_XL"
  "gfx1151:ROCm0:gemma-4-26B-A4B-it-UD-Q8_K_XL"
  "gfx1151:ROCm0:gemma-4-26B-A4B-it-qat-q4_0"
  "gfx1151:ROCm0:gemma-4-31B-it-BF16"
  "gfx1151:ROCm0:gemma-4-31B-it-UD-Q4_K_XL"
  "gfx1151:ROCm0:gemma-4-31B-it-UD-Q5_K_XL"
  "gfx1151:ROCm0:gemma-4-31B-it-qat-q4_0"
  "gfx1151:ROCm0:qwen3.5-122B-A10B-Q5_K_M"
  "gfx1151:TheDrummer_Skyfall-31B-v4.2-Q6_K"
  "gfx1151:gemma-4-26B-A4B-it-UD-Q6_K_XL"
  "gfx1151:gemma-4-26B-A4B-it-UD-Q8_K_XL"
  "gfx1151:gemma-4-26B-A4B-it-qat-q4_0"
  "gfx1151:gemma-4-31B-it-BF16"
  "gfx1151:gemma-4-31B-it-UD-Q4_K_XL"
  "gfx1151:gemma-4-31B-it-UD-Q5_K_XL"
  "gfx1151:gemma-4-31B-it-qat-q4_0"
  "gfx1151:hermes"
  "gfx1151:hermes-fallback"
  "gfx1151:opencode"
  "gfx1151:opencode-fallback"
  "gfx1151:opencode-fast"
  "gfx1151:opencode-fast-fallback"
  "gfx1151:opencode-slow-fallback"
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
  "opencode-slow-fallback"
  {
    name = "rtx5090:InternScience-Agents-A1-Q4_K_M";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:InternScience-Agents-A1-Q4_K_M-mmproj";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.5-9B-Q5_K_M";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.6-27B-NVFP4";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.6-27B-Q6_K-MTP";
    contextWindow = 131072;
  }
  {
    name = "rtx5090:Qwen3.6-27B-Q6_K-MTP-full-ctx";
    contextWindow = 184320;
  }
  "rtx5090:Qwen3.6-27B-UD-Q4_K_XL"
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q5_K_XL";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-instruct-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-instruct-reasoning-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-precise-coding-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q6_K_XL";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-general-tasks";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-instruct-general-tasks";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-instruct-reasoning-tasks";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-precise-coding-tasks";
    contextWindow = 196608;
  }
  "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL"
  {
    name = "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP";
    contextWindow = 184320;
  }
  {
    name = "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-instruct-general";
    contextWindow = 184320;
  }
  {
    name = "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-instruct-reasoning";
    contextWindow = 184320;
  }
  {
    name = "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-thinking-coding";
    contextWindow = 184320;
  }
  {
    name = "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-thinking-general";
    contextWindow = 184320;
  }
  "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-general"
  "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-reasoning"
  "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-coding"
  "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-general"
  {
    name = "rtx5090:Qwen3.8-27B-ABL-Q5_K_M";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-ABL-Q5_K_M-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-ABL-Q5_K_M-instruct-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-ABL-Q5_K_M-instruct-reasoning-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-ABL-Q5_K_M-precise-coding-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q4_K_XL";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q4_K_XL-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q4_K_XL-instruct-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q4_K_XL-instruct-reasoning-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q4_K_XL-precise-coding-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q5_K_XL";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q5_K_XL-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q5_K_XL-instruct-general-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q5_K_XL-instruct-reasoning-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q5_K_XL-precise-coding-tasks";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q6_K_XL";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q6_K_XL-general-tasks";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q6_K_XL-instruct-general-tasks";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q6_K_XL-instruct-reasoning-tasks";
    contextWindow = 196608;
  }
  {
    name = "rtx5090:Qwen3.8-27B-UD-Q6_K_XL-precise-coding-tasks";
    contextWindow = 196608;
  }
  "rtx5090:TheDrummer_Skyfall-31B-v4.2-Q6_K"
  {
    name = "rtx5090:gemma-4-26B-A4B-it-UD-Q6_K_XL";
    contextWindow = 262144;
  }
  "rtx5090:gemma-4-26B-A4B-it-UD-Q8_K_XL"
  {
    name = "rtx5090:gemma-4-26B-A4B-it-qat-q4_0";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:gemma-4-26B-A4B-it-qat-q4_0-mmproj";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:gemma-4-26B-A4B-it-qat-q4_0-nothink";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:gemma-4-31B-it-UD-Q4_K_XL";
    contextWindow = 65536;
  }
  {
    name = "rtx5090:gemma-4-31B-it-UD-Q4_K_XL-mmproj";
    contextWindow = 65536;
  }
  {
    name = "rtx5090:gemma-4-31B-it-UD-Q4_K_XL-nothink";
    contextWindow = 65536;
  }
  "rtx5090:gemma-4-31B-it-UD-Q5_K_XL"
  {
    name = "rtx5090:gemma-4-31B-it-qat-q4_0";
    contextWindow = 65536;
  }
  {
    name = "rtx5090:gemma-4-31B-it-qat-q4_0-mmproj";
    contextWindow = 65536;
  }
  {
    name = "rtx5090:gemma-4-31B-it-qat-q4_0-nothink";
    contextWindow = 65536;
  }
  "rtx5090:hermes"
  {
    name = "rtx5090:hermes-fallback";
    contextWindow = 262144;
  }
  {
    name = "rtx5090:opencode";
    contextWindow = 131072;
  }
  {
    name = "rtx5090:opencode-fallback";
    contextWindow = 262144;
  }
  "rtx5090:opencode-fast-fallback"
  {
    name = "rtx5090:sidekick";
    contextWindow = 262144;
  }
  "sidekick"
  "trustedtokens/Qwen/Qwen3.5-397B-A17B-FP8"
  "trustedtokens/Qwen/Qwen3.6-35B-A3B-FP8"
  "trustedtokens/deepseek-ai/DeepSeek-V4-Flash"
  "trustedtokens/deepseek-ai/DeepSeek-V4-Flash-0731"
  "trustedtokens/google/gemma-4-31B-it"
  "trustedtokens/openai/gpt-oss-120b"
  "trustedtokens/tngtech/DeepSeek-TNG-R1T2-Chimera"
  "trustedtokens/zai-org/GLM-5.2"
]
