# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> { }, ghc ? pkgs.ghc }:
with (import <nixpkgs> { });

haskell.lib.buildStackProject {
  inherit ghc;
  name = "maxhbr-xmonad-config";
  version = "1.0";
  isLibrary = true;
  isExecutable = true;
  src = builtins.filterSource (path: type:
    let basename = baseNameOf path;
    in if type == "symlink" then
      builtins.match "^result(|-.*)$" basename == null
    else
      builtins.match "^((|..*).(sw[a-z]|hi|o)|.*~)$" basename == null) ./.;
  configureFlags = [ "-W -fwarn-unused-imports -fno-warn-missing-signatures" ];
  buildInputs = [
    gmp
    xlibsWrapper
    libffi
    xorg.libXinerama
    xorg.libXext
    xorg.libX11
    xorg.libXrandr
    xorg.libXft
    xorg.libXrender
    xorg.libXScrnSaver
  ];
}
