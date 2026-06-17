# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the gfx1151 model server running on `thing` as a localModels provider.
# The server listens on `0.0.0.0:80` on `thing` (firewall-restricted to
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
  # Model IDs as exposed by `curl https://gfx1151.thing.wg0.maxhbr.local/v1/models`.
  models = [
    "MiniMax-M2.7-UD-IQ4_NL"
    "MiniMax-M2.7-UD-IQ4_NL-196k"
    "MiniMax-M2.7-UD-IQ4_NL-49k"
    "MiniMax-M2.7-UD-IQ4_XS"
    "MiniMax-M2.7-UD-IQ4_XS-196k"
    "MiniMax-M2.7-UD-IQ4_XS-49k"
    "MiniMax-M2.7-UD-Q3_K_S"
    "MiniMax-M2.7-UD-Q3_K_S-196k"
    "MiniMax-M2.7-UD-Q3_K_S-49k"
    "NVIDIA-Nemotron-3-Nano-Omni-Q8_0"
    "NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M"
    "Qwen3.5-9B-Q5_K_M"
    "Qwen3.6-27B-BF16"
    "Qwen3.6-27B-MTP-Q8_0"
    "Qwen3.6-27B-Q8_0"
    "Qwen3.6-27B-UD-Q4_K_XL"
    "Qwen3.6-27B-UD-Q5_K_XL"
    "Qwen3.6-27B-UD-Q6_K_XL"
    "Qwen3.6-35B-A3B-BF16"
    "Qwen3.6-35B-A3B-BF16-instruct-general"
    "Qwen3.6-35B-A3B-BF16-instruct-reasoning"
    "Qwen3.6-35B-A3B-BF16-thinking-coding"
    "Qwen3.6-35B-A3B-BF16-thinking-general"
    "Qwen3.6-35B-A3B-MTP-BF16"
    "Qwen3.6-35B-A3B-MTP-BF16-instruct-general"
    "Qwen3.6-35B-A3B-MTP-BF16-instruct-reasoning"
    "Qwen3.6-35B-A3B-MTP-BF16-thinking-coding"
    "Qwen3.6-35B-A3B-MTP-BF16-thinking-general"
    "Qwen3.6-35B-A3B-Q8_0"
    "Qwen3.6-35B-A3B-Q8_0-instruct-general"
    "Qwen3.6-35B-A3B-Q8_0-instruct-reasoning"
    "Qwen3.6-35B-A3B-Q8_0-thinking-coding"
    "Qwen3.6-35B-A3B-Q8_0-thinking-general"
    "Qwen3.6-35B-A3B-UD-IQ1_M"
    "Qwen3.6-35B-A3B-UD-Q2_K_XL"
    "Qwen3.6-35B-A3B-UD-Q3_K_XL"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "ROCm0:MiniMax-M2.7-UD-IQ4_NL"
    "ROCm0:MiniMax-M2.7-UD-IQ4_NL-196k"
    "ROCm0:MiniMax-M2.7-UD-IQ4_NL-49k"
    "ROCm0:MiniMax-M2.7-UD-IQ4_XS"
    "ROCm0:MiniMax-M2.7-UD-IQ4_XS-196k"
    "ROCm0:MiniMax-M2.7-UD-IQ4_XS-49k"
    "ROCm0:MiniMax-M2.7-UD-Q3_K_S"
    "ROCm0:MiniMax-M2.7-UD-Q3_K_S-196k"
    "ROCm0:MiniMax-M2.7-UD-Q3_K_S-49k"
    "ROCm0:NVIDIA-Nemotron-3-Nano-Omni-Q8_0"
    "ROCm0:NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M"
    "ROCm0:Qwen3.5-9B-Q5_K_M"
    "ROCm0:Qwen3.6-27B-BF16"
    "ROCm0:Qwen3.6-27B-MTP-Q8_0"
    "ROCm0:Qwen3.6-27B-Q8_0"
    "ROCm0:Qwen3.6-27B-UD-Q4_K_XL"
    "ROCm0:Qwen3.6-27B-UD-Q5_K_XL"
    "ROCm0:Qwen3.6-27B-UD-Q6_K_XL"
    "ROCm0:Qwen3.6-35B-A3B-BF16"
    "ROCm0:Qwen3.6-35B-A3B-BF16-instruct-general"
    "ROCm0:Qwen3.6-35B-A3B-BF16-instruct-reasoning"
    "ROCm0:Qwen3.6-35B-A3B-BF16-thinking-coding"
    "ROCm0:Qwen3.6-35B-A3B-BF16-thinking-general"
    "ROCm0:Qwen3.6-35B-A3B-MTP-BF16"
    "ROCm0:Qwen3.6-35B-A3B-MTP-BF16-instruct-general"
    "ROCm0:Qwen3.6-35B-A3B-MTP-BF16-instruct-reasoning"
    "ROCm0:Qwen3.6-35B-A3B-MTP-BF16-thinking-coding"
    "ROCm0:Qwen3.6-35B-A3B-MTP-BF16-thinking-general"
    "ROCm0:Qwen3.6-35B-A3B-Q8_0"
    "ROCm0:Qwen3.6-35B-A3B-Q8_0-instruct-general"
    "ROCm0:Qwen3.6-35B-A3B-Q8_0-instruct-reasoning"
    "ROCm0:Qwen3.6-35B-A3B-Q8_0-thinking-coding"
    "ROCm0:Qwen3.6-35B-A3B-Q8_0-thinking-general"
    "ROCm0:Qwen3.6-35B-A3B-UD-IQ1_M"
    "ROCm0:Qwen3.6-35B-A3B-UD-Q2_K_XL"
    "ROCm0:Qwen3.6-35B-A3B-UD-Q3_K_XL"
    "ROCm0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "ROCm0:TheDrummer_Skyfall-31B-v4.2-Q6_K"
    "ROCm0:gemma-4-26B-A4B-it-UD-Q6_K_XL"
    "ROCm0:gemma-4-26B-A4B-it-UD-Q8_K_XL"
    "ROCm0:gemma-4-26B-A4B-it-qat-q4_0"
    "ROCm0:gemma-4-31B-it-BF16"
    "ROCm0:gemma-4-31B-it-UD-Q4_K_XL"
    "ROCm0:gemma-4-31B-it-UD-Q5_K_XL"
    "ROCm0:gemma-4-31B-it-qat-q4_0"
    "ROCm0:qwen3.5-122B-A10B-Q5_K_M"
    "TheDrummer_Skyfall-31B-v4.2-Q6_K"
    "gemma-4-26B-A4B-it-UD-Q6_K_XL"
    "gemma-4-26B-A4B-it-UD-Q8_K_XL"
    "gemma-4-26B-A4B-it-qat-q4_0"
    "gemma-4-31B-it-BF16"
    "gemma-4-31B-it-UD-Q4_K_XL"
    "gemma-4-31B-it-UD-Q5_K_XL"
    "gemma-4-31B-it-qat-q4_0"
    "qwen3.5-122B-A10B-Q5_K_M"
  ];
in
{
  config = {
    myconfig.ai.localModels = [
      {
        name = "gfx1151.thing.wg0";
        inherit models;
        host = "https://gfx1151.thing.wg0.maxhbr.local/v1";
        port = 80;
      }
    ];
  };
}
