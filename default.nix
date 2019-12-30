# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
funs: pkgs: let
  callPackage = funs.lib.callPackageWith pkgs;

  background = callPackage ./background {
    inherit pkgs;
  };
  scripts = callPackage ./scripts {
    inherit background pkgs;
  };
  my-xmonad = funs.haskellPackages.callPackage ./xmonad {
    inherit scripts pkgs;
  };
in {
  myconfig = {
    nixos-config = import ./nixos;
    inherit
      scripts
      my-xmonad
      background;
  };
}
