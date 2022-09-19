# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    wayland.newm = {
      enable = mkEnableOption "newm";
    };
  };
  config = (lib.mkIf (cfg.wayland.enable && cfg.wayland.newm.enable) {
    home-manager.sharedModules = [{
      xdg.configFile = {
        "newm/config.py".source = ./newm/config.py;
      };
      home.packages = with pkgs; [
        newm
        pywm-fullscreen
      ];
    }];
    myconfig.wayland.greetdSettings = {
      newm_session = {
        command = "start-newm";
        user = "mhuber";
      };
    };
  });
}
