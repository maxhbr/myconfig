# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

let
  background = pkgs.callPackage ./background { inherit pkgs stdenv; };
  maxhbr = {
    inherit background;
    slim-theme = pkgs.callPackage ./background/slim-theme { inherit pkgs stdenv background; };
  };
in {
  inherit maxhbr;
  nixSrc = /home/mhuber/myconfig/nix;
  # nixosSrc = pkgs.callPackage ./nixos/packageNixconfig.nix { inherit pkgs stdenv; };
  nixosSrc = /home/mhuber/myconfig/nixos;
  overlays = [(self: super: { inherit maxhbr; })];
}
