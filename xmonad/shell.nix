# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myxmonadEnv";
  buildInputs = [ gmp
                  x11
                  xorg.libXinerama
                  xorg.libXext
                  xorg.libX11
                  xorg.libXrandr
                  xorg.libXft
                  xorg.libXrender
                  ];
}
