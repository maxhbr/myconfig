# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on: https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb

{ pkgs ? import <nixpkgs> {}
}: with pkgs;
# Manifest to make sure imperative nix-env doesn't work (otherwise it will overwrite the profile, removing all packages other than the newly-installed one).
writeTextFile {
  name = "break-nix-env-manifest";
  destination = "/manifest.nix";
  text = ''
    throw ''\''
      Your user environment is aktively broken, to prevent imperative usage of it.
    ''\''
  '';
}
