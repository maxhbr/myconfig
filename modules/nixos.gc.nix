# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# See:
# - https://nixos.wiki/wiki/Storage_optimization

{ config, lib, ... }:
let
  cfg = config.myconfig.nix.gc;
in
{
  options.myconfig.nix.gc = {
    minFree = lib.mkOption {
      type = lib.types.int;
      default = 1 * 1024 * 1024 * 1024;
      description = "Bytes of free disk space below which Nix runs a GC during builds.";
    };
    maxFree = lib.mkOption {
      type = lib.types.int;
      default = 2 * 1024 * 1024 * 1024;
      description = "Bytes of free disk space Nix tries to free up to when an emergency GC triggers.";
    };
  };
  config = {
    nix.gc = {
      automatic = true;
      dates = lib.mkDefault "weekly";
      options = lib.mkDefault "--delete-older-than 30d";
    };

    nix.extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      min-free = ${toString cfg.minFree}
      max-free = ${toString cfg.maxFree}
    '';
  };
}
