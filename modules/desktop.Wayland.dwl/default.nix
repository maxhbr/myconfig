# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  dwlPackage = pkgs.callPackage ./wrapper.nix {
    dwl-unwrapped = pkgs.dwl;
    conf = ./config.h;
    withBaseWrapper = true;
    extraPaths = cfg.wayland.commonPackages;
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=dwl
      export XKB_DEFAULT_LAYOUT=${config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"}
      export XKB_DEFAULT_VARIANT=${config.environment.sessionVariables."XKB_DEFAULT_VARIANT"}
      export XDG_SESSION_TYPE=${config.environment.sessionVariables."XDG_SESSION_TYPE"}
      export SDL_VIDEODRIVER=${config.environment.sessionVariables."SDL_VIDEODRIVER"}
      export QT_QPA_PLATFORM=${config.environment.sessionVariables."QT_QPA_PLATFORM"}
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=${config.environment.sessionVariables."QT_WAYLAND_DISABLE_WINDOWDECORATION"}
      export _JAVA_AWT_WM_NONREPARENTING=${config.environment.sessionVariables."_JAVA_AWT_WM_NONREPARENTING"}
    '';
    withGtkWrapper = true;
    extraOptions = [ ];
  };
in {
  config = (lib.mkIf (cfg.wayland.enable) {
    home-manager.sharedModules = [{ home.packages = with pkgs; [ dwlPackage ]; }];
    myconfig.wayland.greetdSettings = {
      dwl_session = {
        command = "${dwlPackage}/bin/dwl";
        user = "mhuber";
      };
    };
  });
}
