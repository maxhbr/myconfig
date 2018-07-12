# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super: let
  unstable = import ../nixpkgs-unstable {
    config = import ../nixpkgs-config.nix;
  };
in {
  inherit unstable;
}
