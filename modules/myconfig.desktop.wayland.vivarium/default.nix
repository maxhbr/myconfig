# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  pkg = pkgs.vivarium;
in {
  options.myconfig = with lib; {
    desktop.wayland.vivarium = { enable = mkEnableOption "vivarium"; };
  };
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && cfg.desktop.wayland.vivarium.enable) {
      home-manager.sharedModules = [{
        xdg.configFile = { "vivarium/config.toml".source = ./config.toml; };
        home.packages = with pkgs; [ pkg ];
      }];

      myconfig.desktop.wayland.sessions = {
        vivarium = {
          command = "${pkg}/bin/vivarium";
        };
      };
    });
}
