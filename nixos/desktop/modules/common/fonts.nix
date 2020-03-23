# see:
#  - Issue: https://github.com/NixOS/nixpkgs/issues/77807
#  - PR: https://github.com/NixOS/nixpkgs/pull/77820
#  - Commit: https://github.com/NixOS/nixpkgs/pull/77820/files#diff-fc007d3d4c4340216a3194b74ea4cdc0
#
# This can be removed, once the packages is listed
# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    nixpkgs.overlays = [(self: super: {
      dejavu_nerdfont = super.callPackage ./pkgs/dejavu-nerdfont.nix { };
    })];

    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;

      fonts = with pkgs; [
        dejavu_fonts dejavu_nerdfont
        corefonts
        inconsolata
      ];
      fontconfig.defaultFonts.monospace = [
        "DejaVu Sans Mono Nerd Font Complete Mono"
      ];
    };
  };
}
