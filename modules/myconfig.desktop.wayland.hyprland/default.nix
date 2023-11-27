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
        home.packages = with pkgs; [ hyprpaper hyprnome hyprdim ];
        # xdg.configFile."hypr/hyprpaper.conf".text = ''
        #   preload = 
        #   wallpaper = ${pkgs.hyprland}/share/backgrounds/hyprland.png
        # '';
        wayland.windowManager.hyprland = {
          enable = true;
          package = inputs.hyprland.packages.${pkgs.system}.hyprland;
          extraConfig = ''
            source = ${./hypr/hyprland.conf}
            exec-once = ${
              pkgs.writeShellScriptBin "autostart.sh" ''
                set -x
                ${cfg.desktop.wayland.autostartCommands}
                pkill waybar ; ${config.programs.waybar.package}/bin/waybar > /tmp/hyprland.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown
              ''
            }/bin/autostart.sh
            exec-once = ${pkgs.hyprdim}/bin/hyprdim
          '';
        };
        programs.waybar = {
          enable = lib.mkDefault true;
          settings.mainBar = {
            modules-left =  [ "hyprland/workspaces" ];
            modules-center = [ "hyprland/window" ];
            "hyprland/workspaces" = {
              "format" = "{icon}";
              "format-icons" = {
                "1" = "1:u";
                "2" = "2:i";
                "3" = "3:a";
                "4" = "4:e";
                "5" = "5:o";
                "6" = "6:s";
                "7" = "7:n";
                "8" = "8:r";
                "9" = "9:t";
                "10" = "0:d";
              };
              "persistent-workspaces" = {
                "*" = [ 1 2 3 4 5 6 7 8 9 10 ];
              };
              "on-scroll-up" = "${pkgs.hyprnome}/bin/hyprnome";
              "on-scroll-down" = "${pkgs.hyprnome}/bin/hyprnome --previous";
            };
            "hyprland/window" = {
              "max-length" = 200;
              "separate-outputs" = true;
            };
          };
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
