# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.python;
in
{
  options.myconfig.dev.python = with lib; {
    enable = mkEnableOption "myconfig.dev.python";
  };
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [ python3 ];
      }
    ];
  };
}
