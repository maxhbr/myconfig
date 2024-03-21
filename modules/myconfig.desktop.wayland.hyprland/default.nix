# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, inputs, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  debug = false;
  hyprlandPkg = if debug
    then inputs.hyprland.packages.${pkgs.system}.hyprland.override { inherit debug; }
    else inputs.hyprland.packages.${pkgs.system}.hyprland;
in {
  options.myconfig = with lib; {
    desktop.wayland.hyprland = { enable = mkEnableOption "hyprland"; };
  };
  imports = [
    {
      programs.hyprland = {enable = false;};
    }
  ];
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && cfg.desktop.wayland.hyprland.enable) {
      home-manager.sharedModules = [({config, ...}: let
        hyprctl = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl";
        hyprctl-scripts = with pkgs; [
          (writeShellScriptBin "hyprctl-animations-off" ''
            ${hyprctl} keyword animations:enabled false
            ${hyprctl} keyword decoration:blur:enabled false
            ${hyprctl} keyword decoration:inactive_opacity 1
            ${hyprctl} keyword decoration:dim_inactive false
            ${hyprctl} keyword decoration:dim_special 1
            '')
          (writeShellScriptBin "hyprctl-animations-on"  "${hyprctl} keyword animations:enabled true")
          (writeShellScriptBin "hyprctl-create-headless" ''
            ${hyprctl} output create headless
            echo "remove with:"
            echo "  hyprctl output remove HEADLESS-2"
          '')
        ];
      in {
        home.sessionVariables = lib.mkIf debug {
          ASAN_OPTIONS = "log_path=/tmp/asan.log";
        };
        home.packages = with pkgs; [ 
          hyprpaper
          hyprnome
          hyprpicker
        ] ++ hyprctl-scripts;
        # xdg.configFile."hypr/hyprpaper.conf".text = ''
        #   preload = 
        #   wallpaper = ${pkgs.hyprland}/share/backgrounds/hyprland.png
        # '';
        wayland.windowManager.hyprland = {
          enable = true;
          package = hyprlandPkg;
          extraConfig = ''
            $notifycmd = ${pkgs.libnotify}/bin/notify-send -h string:x-canonical-private-synchronous:hypr-cfg -u low
            source = ${./hypr}/hyprland.conf

            exec-once = ${
              pkgs.writeShellScriptBin "autostart.sh" ''
                set -x
                exec &> >(tee -a /tmp/hyprland.''${XDG_VTNR}.''${USER}.autostart.log)
                ${cfg.desktop.wayland.autostartCommands}
                pkill waybar ; waybar > /tmp/hyprland.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown
              ''
            }/bin/autostart.sh
          '';
        };
        programs.waybar = {
          enable = lib.mkDefault true;
          settings.mainBar = {
            position = lib.mkForce "left";
            height = lib.mkForce null;
            modules-left =  [ "hyprland/workspaces" "hyprland/submap" ];
            modules-center = lib.mkForce []; # modules-center = [ "hyprland/window" ];
            "hyprland/workspaces" = {
              "format" = "{icon}";
              "format-icons" = {
                "1"  = "u<sub> 1 </sub>";
                "2"  = "i<sub> 2 </sub>";
                "3"  = "a<sub> 3 </sub>";
                "4"  = "e<sub> 4 </sub>";
                "5"  = "o<sub> 5 </sub>";
                "6"  = "s<sub> 6 </sub>";
                "7"  = "n<sub> 7 </sub>";
                "8"  = "r<sub> 8 </sub>";
                "9"  = "t<sub> 9 </sub>";
                "10" = "d<sub> 0 </sub>";
              };
              # "persistent-workspaces" = {
              #   "*" = [ 1 2 3 4 5 6 7 8 9 10 ];
              # };
              "on-scroll-up" = "${pkgs.hyprnome}/bin/hyprnome";
              "on-scroll-down" = "${pkgs.hyprnome}/bin/hyprnome --previous";
              "show-special" = true;
            };
            "hyprland/window" = {
              "max-length" = 200;
              "rewrite" = {
                "(.*) — Mozilla Firefox" = "FF: $1";
                "(.*) - Visual Studio Code" = "code: $1";
                "(.*) - fish" = "> [$1]";
              };
              "separate-outputs" = true;
            };
            "hyprland/submap" = {
                "format" = "submap: {}";
                "max-length" = 8;
                "tooltip" = false;
                "rotate" = 90;
            };
            "pulseaudio"."rotate" = 90;
            "bluetooth"."rotate" = 90;
            "network"."rotate" = 90;
            "custom/isvpn"."rotate" = 90;
            "custom/platform_profile"."rotate" = 90;
            "custom/test_for_missing_tb_changing"."rotate" = 90;
            # "cpu"."rotate" = 90;
            # "memory"."rotate" = 90;
            "backlight"."rotate" = 90;
            "battery"."rotate" = 90;
            "wlr/taskbar"."rotate" = 90;
            # "group/hardware"."rotate" = 90;
            "idle_inhibitor"."rotate" = 90;
            "custom/audio_idle_inhibitor"."rotate" = 90;
            "tray"."rotate" = 90;
            "clock"."rotate" = 90;
          };
        };
      })];
      myconfig.desktop.wayland.greetdSettings = {
        hyprland_session = {
          command = "${hyprlandPkg}/bin/Hyprland";
          inherit user;
        };
      };
    });
}
