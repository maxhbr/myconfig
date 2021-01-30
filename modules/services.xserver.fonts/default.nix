# see:
#  - Issue: https://github.com/NixOS/nixpkgs/issues/77807
#  - PR: https://github.com/NixOS/nixpkgs/pull/77820
#  - Commit: https://github.com/NixOS/nixpkgs/pull/77820/files#diff-fc007d3d4c4340216a3194b74ea4cdc0
#
# This can be removed, once the packages is listed
# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let user = config.myconfig.user;
in {
  config = (lib.mkIf config.services.xserver.enable {
    nixpkgs.overlays = [
      (self: super: {
        dejavu_nerdfont = super.callPackage ./pkgs/dejavu-nerdfont.nix { };
      })
    ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;

      fonts = with pkgs; [ dejavu_fonts dejavu_nerdfont corefonts inconsolata ];
      fontconfig.defaultFonts.monospace =
        [ "DejaVu Sans Mono Nerd Font Complete Mono" ];
    };
    home-manager.users."${user}" = {
      home.file = {
        ".fontconfig/fonts.conf".text = ''
          <?xml version="1.0"?>
          <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
          <fontconfig>
              <match target="font" >
                  <edit mode="assign" name="rgba" >
                      <const>rgb</const>
                  </edit>
              </match>
              <match target="font" >
                  <edit mode="assign" name="hinting">
                      <bool>true</bool>
                  </edit>
              </match>
              <match target="font" >
                  <edit mode="assign" name="hintstyle">
                      <const>hintslight</const>
                  </edit>
              </match>
              <match target="font">
                  <edit mode="assign" name="lcdfilter">
                      <const>lcddefault</const>
                  </edit>
              </match>
          </fontconfig>
        '';
      };
    };
  });
}
