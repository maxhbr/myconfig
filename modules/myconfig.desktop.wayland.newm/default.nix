# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  pkg = pkgs.newm;
in {
  config = (lib.mkIf (cfg.desktop.wayland.enable
    && builtins.elem "newm" cfg.desktop.wayland.selectedSessions) {
      home-manager.sharedModules = [{
        xdg.configFile = { "newm/config.py".source = ./newm/config.py; };
        home.packages = [ pkg pkgs.pywm-fullscreen ];
      }];
      myconfig.desktop.wayland.sessions = {
        newm = { command = "${pkg}/bin/start-newm"; };
      };
      services.xserver.windowManager.session = lib.singleton {
        name = "newm";
        start = "${pkg}/bin/start-newm";
      };
    });
}
