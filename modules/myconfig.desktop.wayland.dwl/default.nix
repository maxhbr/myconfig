# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# todo:
#  - https://github.com/jordanisaacs/dwl-flake and https://github.com/jordanisaacs/dotfiles
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  # dwlPackage = pkgs.callPackage ./wrapper.nix {
  #   dwl-unwrapped = pkgs.dwl;
  #   conf = ./config.h;
  #   withBaseWrapper = true;
  #   extraPaths = [ ]; # cfg.wayland.commonPackages;
  #   extraSessionCommands = ''
  #     export XDG_CURRENT_DESKTOP=dwl
  #     export XKB_DEFAULT_LAYOUT=${
  #       config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"
  #     }
  #     export XKB_DEFAULT_VARIANT=${
  #       config.environment.sessionVariables."XKB_DEFAULT_VARIANT"
  #     }
  #     export XDG_SESSION_TYPE=${
  #       config.environment.sessionVariables."XDG_SESSION_TYPE"
  #     }
  #     export SDL_VIDEODRIVER=${
  #       config.environment.sessionVariables."SDL_VIDEODRIVER"
  #     }
  #     export QT_QPA_PLATFORM=${
  #       config.environment.sessionVariables."QT_QPA_PLATFORM"
  #     }
  #     export QT_WAYLAND_DISABLE_WINDOWDECORATION=${
  #       config.environment.sessionVariables."QT_WAYLAND_DISABLE_WINDOWDECORATION"
  #     }
  #     export _JAVA_AWT_WM_NONREPARENTING=${
  #       config.environment.sessionVariables."_JAVA_AWT_WM_NONREPARENTING"
  #     }
  #   '';
  #   withGtkWrapper = true;
  #   extraOptions = [ ];
  # };
  dwlPackage = pkgs.nixos-unstable.dwl.override { conf = ./config.h; };
in {
  options.myconfig = with lib; {
    desktop.wayland.dwl = { enable = mkEnableOption "dwl"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.dwl.enable) {
      # nixpkgs.overlays = [
      #   (
      #     final: prev:
      #     {
      #       dwl = prev.dwl.override { conf = ./config.h; };
      #     }
      #   )
      # ];

      home-manager.sharedModules =
        [{ home.packages = [ dwlPackage ]; }];
      services.xserver.windowManager.session = lib.singleton {
        name = "dwl";
        start = "${dwlPackage}/bin/dwl";
      };
    });
}
