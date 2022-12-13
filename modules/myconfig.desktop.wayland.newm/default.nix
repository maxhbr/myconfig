# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  pkg = pkgs.newm;
in {
  options.myconfig = with lib; {
    desktop.wayland.newm = { enable = mkEnableOption "newm"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.newm.enable) {
      home-manager.sharedModules = [{
        xdg.configFile = { "newm/config.py".source = ./newm/config.py; };
        home.packages = [ pkg pkgs.pywm-fullscreen ];
      }];
      myconfig.desktop.wayland.greetdSettings = {
        newm_session = {
          command = "${pkg}/bin/start-newm";
          inherit user;
        };
      };
      services.xserver.windowManager.session = lib.singleton {
        name = "newm";
        start = "${pkg}/bin/start-newm";
      };
    });
}
