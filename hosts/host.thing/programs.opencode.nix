# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  config = {
    myconfig.ai.opencode.enable = true;
    home-manager.sharedModules = [
      {
        programs.opencode.settings = {
          # Default model: "opencode" alias -> qwen3.5-122B-A10B-Q5_K_M in the
          # containerized llama-swap on port 33657 (provider name "llama-swap-33657").
          model = "local-llama-swap-33657/opencode";
          # Fast/small model: "opencode-fast" alias -> Qwen3.6-35B-A3B-UD-Q5_K_XL
          # on the host-level llama-swap on port 33656 (provider name "llama-swap").
          small_model = "local-llama-swap/opencode-fast";
        };
      }
    ];
  };
}
