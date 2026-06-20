# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Aggregator for the pure helper libraries used by the llama-cpp module.
#
# Called from `default.nix` with the module's `lib` and `pkgs`. Returns
# an attrset that bundles the device helpers, variant expansion and
# script generators so module files don't have to import each piece
# individually.
{
  lib,
  pkgs,
  diffusionLlamaCpp ? null,
}:
let
  devices = import ./devices.nix { inherit lib pkgs; };
  variants = import ./variants.nix { inherit lib; };
  scripts = import ./scripts.nix {
    inherit
      lib
      pkgs
      devices
      diffusionLlamaCpp
      ;
  };
  router = import ./router.nix {
    inherit
      lib
      pkgs
      devices
      diffusionLlamaCpp
      ;
  };
in
{
  inherit
    devices
    variants
    scripts
    router
    ;
}
