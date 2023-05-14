# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let cfg = config.myconfig;
  user = myconfig.user;
in {
  options.myconfig = with lib; {
    desktop.wayland.hyprland = { enable = mkEnableOption "hyprland"; };
  };
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && cfg.desktop.wayland.hyprland.enable) {
      home-manager.sharedModules = [{
        xdg.configFile."hypr/hyprland.conf".source = ./hypr/hyprland.conf;
        wayland.windowManager.hyprland = {
          enable = true;
          extraConfig = builtins.readFile ./hypr/hyprland.conf;
        };
      }];
      myconfig.desktop.wayland.greetdSettings = {
        hyprland_session = {
          command = "${pkgs.hyprland}/bin/Hyprland";
          inherit user;
        };
      };
    });
}
