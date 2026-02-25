# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# See:
# - https://nixos.wiki/wiki/Storage_optimization

{ pkgs, ... }:
{
  config = {
    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    nix.extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      min-free = ${toString (1 * 1024 * 1024 * 1024)}
      max-free = ${toString (2 * 1024 * 1024 * 1024)}
    '';
  };
}
