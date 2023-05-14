# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  wallpaperCmdString = ''
    output "*" bg ${pkgs.my-wallpapers}/share/background.png fill
  '';
in {
  options.myconfig = with lib; {
    desktop.wayland.sway = { enable = mkEnableOption "sway"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.sway.enable) {
      environment = {
        etc = {
          "sway/config".source = ./sway/config;
          "sway/config.d/zoom.us.conf".source = ./sway/config.d/zoom.us.conf;
          "sway/config.d/dex.conf".source =
            pkgs.writeText "dex.conf" "exec ${pkgs.dex}/bin/dex --autostart";
          "sway/config.d/background.conf".source =
            pkgs.writeText "background.conf" wallpaperCmdString;
        };
      };
      home-manager.sharedModules = [{ programs.waybar.enable = true; }];

      programs.sway = {
        enable = true;
        extraPackages = with pkgs;
          [
            autotiling
            swaylock
            swayidle
            swaybg
            swayws
            dmenu
            sway-launcher-desktop
            (writeShellScriptBin "foot-sway-launcher-desktop" ''
              ${foot}/bin/foot --title=launcher --app-id=launcher -e sway-launcher-desktop
            '')
            (writeShellScriptBin "my-set-background" ''
              ${sway}/bin/swaymsg ${wallpaperCmdString}
            '')
            (pkgs.writeScriptBin "sway-run-or-raise"
              (builtins.readFile ./sway-run-or-raise))
            (writeShellScriptBin "sway-foot-neomutt" ''
              exec sway-run-or-raise foot-neomutt
            '')
            # swaymonad
            i3-wk-switch # https://github.com/tmfink/i3-wk-switch
          ];
        wrapperFeatures.gtk = true;

        extraSessionCommands = ''
          export XDG_CURRENT_DESKTOP=sway
          export XKB_DEFAULT_LAYOUT=${
            config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"
          }
          export XKB_DEFAULT_VARIANT=${
            config.environment.sessionVariables."XKB_DEFAULT_VARIANT"
          }
          export XDG_SESSION_TYPE=${
            config.environment.sessionVariables."XDG_SESSION_TYPE"
          }
          export SDL_VIDEODRIVER=${
            config.environment.sessionVariables."SDL_VIDEODRIVER"
          }
          export QT_QPA_PLATFORM=${
            config.environment.sessionVariables."QT_QPA_PLATFORM"
          }
          export QT_WAYLAND_DISABLE_WINDOWDECORATION=${
            config.environment.sessionVariables."QT_WAYLAND_DISABLE_WINDOWDECORATION"
          }
          export _JAVA_AWT_WM_NONREPARENTING=${
            config.environment.sessionVariables."_JAVA_AWT_WM_NONREPARENTING"
          }
        '';
      };

      myconfig.desktop.wayland.greetdSettings = {
        sway_session = {
          command = "sway";
          user = "mhuber";
        };
      };
    });
}
