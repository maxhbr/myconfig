# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  qtilePackage = pkgs.callPackage ./wrapper.nix {
    withBaseWrapper = true;
    extraPaths = cfg.desktop.wayland.commonPackages;
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
  startQtile = pkgs.writeShellScriptBin "start-qtile" ''
    ${qtilePackage}/bin/qtile start \
      -b wayland \
      -l DEBUG
  '';
in {
  options.myconfig = with lib; {
    desktop.wayland.qtile = { enable = mkEnableOption "qtile"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.qtile.enable) {
      home-manager.sharedModules = [{
        xdg.configFile = { "qtile/config.py".source = ./qtile/config.py; };
        home.packages = with pkgs; [ qtilePackage startQtile ];
      }];

      #services.xserver.windowManager.qtile = {
      #  enable = true;
      #  # package = qtilePackage;
      #};
      #services.xserver.displayManager.sessionPackages = [
      #  qtilePackage
      #];
      myconfig.desktop.wayland.greetdSettings = {
        qtile_session = {
          command = "${startQtile}/bin/start-qtile";
          inherit user;
        };
      };
    });
}
