# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  wayfirePackage = pkgs.callPackage ./wrapper.nix {
    wayfire = pkgs.wayfire-unstable;
    withBaseWrapper = true;
    extraPaths = with pkgs; [ wayfire-unstable ];
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=wayfire
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
    withGtkWrapper = true;
    extraOptions = [ ];
  };
in {
  options.myconfig = with lib; {
    desktop.wayland.wayfire = { enable = mkEnableOption "wayfire"; };
  };
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && cfg.desktop.wayland.wayfire.enable) {
      home-manager.sharedModules = [{
        xdg.configFile = { "wayfire.ini".source = ./wayfire.ini; };
        home.packages = with pkgs; [ wayfirePackage ];
      }];
      myconfig.desktop.wayland.sessions = {
        wayfire = { command = "${wayfirePackage}/bin/wayfire"; };
      };
    });
}
