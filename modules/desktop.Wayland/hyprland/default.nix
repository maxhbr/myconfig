# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    wayland.hyprland = { enable = mkEnableOption "hyprland"; };
  };
  config = (lib.mkIf (cfg.wayland.enable && cfg.wayland.hyprland.enable) {
    home-manager.sharedModules = [{
      xdg.configFile."hypr/hyprland.conf".source = ./hypr/hyprland.conf;
      wayland.windowManager.hyprland.enable = true;
    }];
  });
}
