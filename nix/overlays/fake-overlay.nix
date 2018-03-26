# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# stolen from: https://github.com/NixOS/nixpkgs/issues/25264#issuecomment-298178317
let
  system = builtins.currentSystem;

  pkgs = import <nixpkgs> {
    inherit system;
    inherit (ccc.config.nixpkgs) config overlays;
  };

  lib = pkgs.stdenv.lib;

  ccc = import <nixpkgs/nixos/lib/eval-config.nix> {
    inherit system pkgs;
    modules = [ /etc/nixos/configuration.nix ];
  };
in
  lib.foldl' lib.composeExtensions (self: super: {}) ccc.config.nixpkgs.overlays
