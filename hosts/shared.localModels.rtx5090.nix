# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the rtx5090 model server running on `thing` as a localModels provider.
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
  # Model IDs as exposed by `curl https://rtx5090.thing.wg0.maxhbr.local/v1/models`.
  models = [
    "InternScience-Agents-A1-Q4_K_M"
    "InternScience-Agents-A1-Q4_K_M-mmproj"
    "Qwen3.5-9B-Q5_K_M"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-instruct-general"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-instruct-reasoning"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-thinking-coding"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-thinking-general"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-general"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-reasoning"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-coding"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-general"
    "Qwen3.8-27B-ABL-Q5_K_M"
    "Qwen3.8-27B-ABL-Q5_K_M-general-tasks"
    "Qwen3.8-27B-ABL-Q5_K_M-instruct-general-tasks"
    "Qwen3.8-27B-ABL-Q5_K_M-instruct-reasoning-tasks"
    "Qwen3.8-27B-ABL-Q5_K_M-precise-coding-tasks"
    "Qwen3.8-27B-MTP-Q4_0"
    "Qwen3.8-27B-MTP-Q4_0-general-tasks"
    "Qwen3.8-27B-MTP-Q4_0-instruct-general-tasks"
    "Qwen3.8-27B-MTP-Q4_0-instruct-reasoning-tasks"
    "Qwen3.8-27B-MTP-Q4_0-precise-coding-tasks"
    "Qwen3.8-27B-Q6_K-MTP"
    "Qwen3.8-27B-Q6_K-MTP-full-ctx"
    "Qwen3.8-27B-UD-Q4_K_XL"
    "Qwen3.8-27B-UD-Q4_K_XL-general-tasks"
    "Qwen3.8-27B-UD-Q4_K_XL-instruct-general-tasks"
    "Qwen3.8-27B-UD-Q4_K_XL-instruct-reasoning-tasks"
    "Qwen3.8-27B-UD-Q4_K_XL-precise-coding-tasks"
    "Qwen3.8-27B-UD-Q5_K_XL"
    "Qwen3.8-27B-UD-Q5_K_XL-general-tasks"
    "Qwen3.8-27B-UD-Q5_K_XL-instruct-general-tasks"
    "Qwen3.8-27B-UD-Q5_K_XL-instruct-reasoning-tasks"
    "Qwen3.8-27B-UD-Q5_K_XL-precise-coding-tasks"
    "Qwen3.8-27B-UD-Q6_K_XL"
    "Qwen3.8-27B-UD-Q6_K_XL-general-tasks"
    "Qwen3.8-27B-UD-Q6_K_XL-instruct-general-tasks"
    "Qwen3.8-27B-UD-Q6_K_XL-instruct-reasoning-tasks"
    "Qwen3.8-27B-UD-Q6_K_XL-precise-coding-tasks"
    "TheDrummer_Skyfall-31B-v4.2-Q6_K"
    "default"
    "gemma-4-26B-A4B-it-UD-Q6_K_XL"
    "gemma-4-26B-A4B-it-UD-Q8_K_XL"
    "gemma-4-26B-A4B-it-qat-q4_0"
    "gemma-4-26B-A4B-it-qat-q4_0-mmproj"
    "gemma-4-26B-A4B-it-qat-q4_0-nothink"
    "gemma-4-31B-it-UD-Q4_K_XL"
    "gemma-4-31B-it-UD-Q4_K_XL-mmproj"
    "gemma-4-31B-it-UD-Q4_K_XL-nothink"
    "gemma-4-31B-it-UD-Q5_K_XL"
    "gemma-4-31B-it-qat-q4_0"
    "gemma-4-31B-it-qat-q4_0-mmproj"
    "gemma-4-31B-it-qat-q4_0-nothink"
  ];
in
{
  config = {
    myconfig.ai.localModels = [
      {
        name = "rtx5090.thing.wg0";
        inherit models;
        host = "https://rtx5090.thing.wg0.maxhbr.local/v1";
        port = 80;
      }
    ];
  };
}
