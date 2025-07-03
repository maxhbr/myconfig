# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.go;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [ { programs.go.enable = true; } ];
  };
}
