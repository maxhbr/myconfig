# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  config = (lib.mkIf (cfg.desktop.wayland.enable && builtins.elem "niri" cfg.desktop.wayland.sessions) {
    home-manager.sharedModules = [
      ({ config, ... }: {
        xdg.configFile = {
          "niri/config.kdl".source = ./config.kdl;
        };
      })
    ];
    myconfig.desktop.wayland.greetdSettings = {
      niri_session = {
        command = "${pkgs.niri}/bin/niri-session";
        inherit user;
      };
    };
  });
}
