# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super: let
  very-stable = import ../nixpkgs-very-stable {
    config = import ../nixpkgs-config.nix;
  };
in {
  inherit very-stable;
}
