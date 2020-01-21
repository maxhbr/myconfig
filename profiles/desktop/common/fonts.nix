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
      jetbrains-mono = super.callPackage (super.fetchurl {
        url = "https://raw.githubusercontent.com/marsam/nixpkgs/35bdbf487bf98538f2ad31b61329482a192d6697/pkgs/data/fonts/jetbrains-mono/default.nix";
        sha256 = "176d7prq83s37w25yrr6f31vihj9ywrq1z18nw9695nvxnlcf5sy";
      }) {};
      dejavu_nerdfont = super.callPackage ./pkgs/dejavu-nerdfont.nix { };
    })];

    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;

      fonts = with pkgs; [
        dejavu_fonts dejavu_nerdfont
        corefonts
        inconsolata
        jetbrains-mono
      ];
      fontconfig.defaultFonts.monospace = [
        "DejaVu Sans Mono Nerd Font Complete Mono"
      ];
    };
  };
}
