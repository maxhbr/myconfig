# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  options.myconfig = with lib; {
    desktop.wayland.newm = { enable = mkEnableOption "newm"; };
  };
  config = (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.newm.enable) {
    home-manager.sharedModules = [{
      xdg.configFile = { "newm/config.py".source = ./newm/config.py; };
      home.packages = with pkgs; [ newm pywm-fullscreen ];
    }];
    myconfig.desktop.wayland.greetdSettings = {
      newm_session = {
        command = "start-newm";
        inherit user;
      };
    };
  });
}
