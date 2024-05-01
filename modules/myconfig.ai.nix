# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    ai.enable = mkEnableOption "ai";
  };
  config = (lib.mkIf cfg.ai.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        ollama
      ];
    }];
  });
}
