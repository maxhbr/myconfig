# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.difftastic;
in
{
  config = lib.mkIf cfg.enable {
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
