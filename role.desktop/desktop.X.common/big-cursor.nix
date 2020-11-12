# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let user = config.myconfig.user;
in {
  config = {
    home-manager.users."${user}" = {
      xsession.pointerCursor = {
        package = pkgs.vanilla-dmz;
        size = 128;
        name = "Vanilla-DMZ";
        defaultCursor = "left_ptr"; # or "left_ptr";
      };
    };
    environment = { systemPackages = with pkgs; [ cursor ]; };
    nixpkgs.overlays = [
      (final: prev: {
        # set default cursor theme when installed
        cursor = prev.writeTextDir "share/icons/default/index.theme" ''
          [icon theme]
          Inherits=Vanilla-DMZ
          Size=128
        '';
      })
    ];
  };
}
