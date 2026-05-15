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
          # Default model: "opencode" alias -> qwen3.5-122B-A10B-Q5_K_M on
          # the containerized router bound to the AMD iGPU
          # (provider name "gfx1151", port 33657).
          model = "local-gfx1151/opencode";
          # Fast/small model: "opencode-fast" alias -> Qwen3.6-35B-A3B-UD-Q5_K_XL
          # on the host-level router bound to the NVIDIA RTX 5090
          # (provider name "rtx5090", port 33656).
          small_model = "local-rtx5090/opencode-fast";
        };
      }
    ];
  };
}
