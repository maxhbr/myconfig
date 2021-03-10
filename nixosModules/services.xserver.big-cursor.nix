# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let user = config.myconfig.user;
in {
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.imports = [{
      xsession.pointerCursor = {
        package = pkgs.vanilla-dmz;
        size = 128;
        name = "Vanilla-DMZ";
        defaultCursor = "left_ptr"; # or "left_ptr";
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
  });
}
