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
    myconfig.ai.aider.enable = true;
    home-manager.sharedModules = [
      {
        # Match the model picks from programs.opencode.nix.
        # Default model: "opencode" alias -> qwen3.5-122B-A10B-Q5_K_M in the
        # containerized llama-swap on port 33657 (provider name "llama-swap-33657").
        # Fast/small model: "opencode-fast" alias -> Qwen3.6-35B-A3B-UD-Q5_K_XL
        # on the host-level llama-swap on port 33656 (provider name "llama-swap").
        programs.aider-chat.settings = {
          model = "openai/local-llama-swap-33657--opencode";
          weak-model = "openai/local-llama-swap--opencode-fast";
        };
      }
    ];
  };
}
