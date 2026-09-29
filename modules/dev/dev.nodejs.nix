# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.nodejs;
in
{
  config = lib.mkIf cfg.enable {
    myconfig.ai.dev.sandboxTools.extraPackages = with pkgs; [ nodejs_latest ];
    home-manager.sharedModules = [ { home.packages = with pkgs; [ nodejs_latest ]; } ];
  };
}
