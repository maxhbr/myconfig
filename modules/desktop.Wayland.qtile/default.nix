# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  qtilePackage = pkgs.callPackage ./wrapper.nix {
    withBaseWrapper = true;
    extraPaths = cfg.wayland.commonPackages;
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=qtile
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
    wayland.qtile = {
      enable = mkEnableOption "qtile";
    };
  };
  config = (lib.mkIf (cfg.wayland.enable && cfg.wayland.qtile.enable) {
    home-manager.sharedModules = [{
      xdg.configFile = {
        "qtile/config.py".source = ./qtile/config.py;
      };
      home.packages = with pkgs; [ qtilePackage ];
    }];

    #services.xserver.windowManager.qtile = {
    #  enable = true;
    #  # package = qtilePackage;
    #};
    #services.xserver.displayManager.sessionPackages = [
    #  qtilePackage
    #];
    myconfig.wayland.greetdSettings = {
      qtile_session = {
        command = "${qtilePackage}/bin/qtile start -b wayland";
        user = "mhuber";
      };
    };
  });
}
