# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
let
  user = config.myconfig.user;
  nixpkgsConfig = { allowUnfree = true; };
in {
  config = {
    nixpkgs.config = nixpkgsConfig;
    home-manager.users."${user}" = {
      nixpkgs.config = nixpkgsConfig;
      home.file = {
        ".config/nixpkgs/config.nix" = {
          text = ''
            {
              allowBroken = true;
              allowUnfree = ${
                lib.boolToString config.nixpkgs.config.allowUnfree
              };
            }
          '';
        };
      };
    };
    nix = rec {
      # see: https://nixos.wiki/wiki/Flakes
      package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes

        gc-keep-outputs = true
        gc-keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      '';

      useSandbox = true;
      readOnlyStore = true;

      autoOptimiseStore = true;
      optimise.automatic = true;

      allowedUsers = [ "@wheel" "@builders" "${user}" ];
      trustedUsers = [ "root" "@wheel" "@builders" "${user}" ];

      trustedBinaryCaches = [
        "https://cache.nixos.org"
        "https://maxhbr.cachix.org"
        "https://nixfmt.cachix.org"
      ];
      binaryCaches = trustedBinaryCaches;
      binaryCachePublicKeys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "maxhbr.cachix.org-1:wwfYm+B6HaXyFey300cmuSQnvwULS0VU1VtOGXDyxCo="
        "nixfmt.cachix.org-1:uyEQg16IhCFeDpFV07aL+Dbmh18XHVUqpkk/35WAgJI="
      ];
    };
  };
}
