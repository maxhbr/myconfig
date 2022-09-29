# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  riverPackage = pkgs.callPackage ./wrapper.nix {
    river-unwrapped = pkgs.river;
    withBaseWrapper = true;
    extraPaths = cfg.desktop.wayland.commonPackages
      ++ (with pkgs; [ rivercarro ristate swaybg ]);
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=river
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
    desktop.wayland.river = { enable = mkEnableOption "river"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.river.enable) {
      home-manager.sharedModules = [{
        xdg.configFile = { "river/init".source = ./river/init; };
        home.packages = with pkgs; [ riverPackage ];
      }];
      myconfig.desktop.wayland.greetdSettings = {
        river_session = {
          command = "${riverPackage}/bin/river";
          user = "mhuber";
        };
      };
    });
}
