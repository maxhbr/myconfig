# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.difftastic;
in
{
  config = lib.mkIf cfg.enable {
    myconfig.ai.dev.sandboxTools.extraPackages = with pkgs; [ difftastic ];
    home-manager.sharedModules = [
      {
        programs.difftastic = {
          enable = true;
          git.enable = true;
        };
      }
    ];
  };
}
