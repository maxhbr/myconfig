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
    myconfig.ai.dev.opencode.enable = true;
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
          #
          # `small_model` is the OpenCode 1 spelling; OpenCode 2 selects the
          # model of its built-in `title` agent instead, and the schema only
          # allows a models.dev id in `small_model` (so a local provider fails
          # validation there). Same value, current field.
          agents.title.model = "local-rtx5090/opencode-fast";
        };
      }
    ];
  };
}
