# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, inputs, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  options.myconfig = with lib; {
    desktop.wayland.hyprland = { enable = mkEnableOption "hyprland"; };
  };
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && cfg.desktop.wayland.hyprland.enable) {
      home-manager.sharedModules = [{
        home.packages = with pkgs; [
          hyprpaper
          hyprnome
          hyprdim
        ];
        # xdg.configFile."hypr/hyprpaper.conf".text = ''
        #   preload = 
        #   wallpaper = ${pkgs.hyprland}/share/backgrounds/hyprland.png
        # '';
        wayland.windowManager.hyprland = {
          enable = true;
          package = inputs.hyprland.packages.${pkgs.system}.hyprland;
          extraConfig = ''
          source = ${./hypr/hyprland.conf}
          exec-once = ${pkgs.writeShellScriptBin "autostart.sh" ''
            set -x
            ${cfg.desktop.wayland.autostartCommands}
            pkill waybar ; ${config.programs.waybar.package}/bin/waybar > /tmp/hyprland.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown
          ''}/bin/autostart.sh
          exec-once = ${pkgs.hyprdim}/bin/hyprdim

          bind = $mainMod, P, exec, ${pkgs.wofi}/bin/wofi --show drun

          bind = $mainMod, Comma, exec, ${pkgs.hyprnome}/bin/hyprnome --previous
          bind = $mainMod, Dot,   exec, ${pkgs.hyprnome}/bin/hyprnome
          bind = $mainMod SHIFT, Comma, exec, ${pkgs.hyprnome}/bin/hyprnome --previous --move
          bind = $mainMod SHIFT, Dot,   exec, ${pkgs.hyprnome}/bin/hyprnome --move
        '';
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
