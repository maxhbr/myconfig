# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super: let
  unstable = import ../nixpkgs-unstable {
    config = import ../nixpkgs-config.nix;
  };
  # unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
  # unstable = import unstableTarball {
  #   config = import ../nixpkgs-config.nix;
  # };
in {
  inherit unstable;
}
