# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
{
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [{
      home.pointerCursor = {
        package = pkgs.vanilla-dmz;
        size = 128;
        name = "Vanilla-DMZ";
        x11 = {
          enable = true;
          defaultCursor = "left_ptr";
        };
        gtk.enable = true;
      };
    }];
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
    environment.systemPackages = with pkgs; [ cursor ];
  };
}
