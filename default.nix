# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  packageSources = {dir, name, pattern ? "*"}:
    super.stdenv.mkDerivation {
      # version = "0.1";
      inherit name;

      src = dir;
      installPhase = ''
        mkdir -p $out
        cp -r ${pattern} $out
      '';
    };
in {
  maxhbr = rec {
    nixosSrc = super.callPackage (packageSources { dir = ./nixos; name = "nixosSrc"; });
    nixSrc = super.callPackage (packageSources { dir = ./nix; name = "nixSrc"; });
    dotfiles = super.callPackage (packageSources { dir = ./dotfiles; name = "dotfiles"; });
    scripts = super.callPackage (packageSources { dir = ./dotfiles; name = "scripts"; pattern = "*.{sh,pl}" });
  };
}
