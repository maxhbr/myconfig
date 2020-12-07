# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, lib, config, ... }:
let
  user = config.myconfig.user;
in {
  imports = [
    ./my-wallpapers
    ./fonts.nix
  ];
  config = {
    programs.light.enable = true;
    services.avahi.enable =
      true; # https://github.com/NixOS/nixpkgs/issues/49630
    services.actkbd = {
      enable = true;
      bindings = [
        {
          keys = [ 224 ];
          events = [ "key" ];
          command = "/run/current-system/sw/bin/light -U 10";
        }
        {
          keys = [ 225 ];
          events = [ "key" ];
          command = "/run/current-system/sw/bin/light -A 10";
        }
      ];
    };
  };
}
