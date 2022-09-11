# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  riverPackage = pkgs.callPackage ./wrapper.nix {
    river-unwrapped = pkgs.river;
    withBaseWrapper = true;
    extraPaths = cfg.wayland.commonPackages ++ (with pkgs; [
      rivercarro
      ristate
      (writeShellScriptBin "myswayidle" ''
        set -euo pipefail
        ${swayidle}/bin/swayidle -w \
          timeout 300 '${config.security.wrapperDir}/physlock' \
          before-sleep '${config.security.wrapperDir}/physlock'
      '')
      (writeShellScriptBin "myphyslock"
        "exec '${config.security.wrapperDir}/physlock'")
    ]);
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=river
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
    home-manager.sharedModules = [
      ./home-manager.waybar.nix
      {
        xdg.configFile = {
          "river/init".source = ./river/init;
          "way-displays/cfg.yaml".source = ./way-displays/cfg.yaml;
        };
        home.packages = with pkgs; [ riverPackage ];
        programs.waybar.enable = true;
      }
    ];
    myconfig.wayland.greetdSettings = {
      river_session = {
        command = "${riverPackage}/bin/river";
        user = "mhuber";
      };
    };
  });
}
