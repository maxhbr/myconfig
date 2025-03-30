# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
{
  options.myconfig = with lib; {
    ai.enable = mkEnableOption "myconfig.ai";
  };
  config = lib.mkIf config.myconfig.ai.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        aichat
      ];
    }];
  };
}

