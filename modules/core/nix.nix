# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
{
  config = {
    nix = {
      useSandbox = true;
      readOnlyStore = true;

      trustedBinaryCaches = [ "https://cache.nixos.org" ];
      binaryCaches = [ "https://cache.nixos.org" ];

      extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      '';
    };
  };
}
