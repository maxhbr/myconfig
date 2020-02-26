# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  resetXrandr = with pkgs; writeScriptBin "resetXrandr" ''
    #!${stdenv.shell}
    ${pkgs.systemd}/bin/systemctl --user start redshift
    ${pkgs.xorg.xrandr}/bin/xrandr --output DP-2 --brightness 1
  '';

in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        autorandr
      ];
      home.file = {
        ".config/autorandr/postswitch.d/resetXrandr".source = "${resetXrandr}/bin/resetXrandr";
      };
    };
    services.autorandr.enable = true;
  };
}
