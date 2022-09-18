# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
in {
  config = (lib.mkIf (cfg.wayland.enable && config.programs.sway.enable) {
    environment = {
      etc."sway/config".source = ./sway/config;
    };
    home-manager.sharedModules = [{
      # home.file = { ".config/sway/config".source = ./sway/config; };
      programs.waybar.enable = true;
    }];

    programs.sway = {
      extraPackages = with pkgs;
        [ autotiling swaylock swayidle xwayland st dmenu ]
        ++ cfg.wayland.commonPackages;
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

    myconfig.wayland.greetdSettings = {
      sway_session = {
        command = "sway";
        user = "mhuber";
      };
    };
  });
}
