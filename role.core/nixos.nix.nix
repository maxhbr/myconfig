# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }: {
  config = {
    home-manager.users.mhuber = {
      nixpkgs.config.allowUnfree = true;
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
      package = pkgs.nixUnstable;

      useSandbox = true;
      readOnlyStore = true;

      autoOptimiseStore = true;
      optimise.automatic = true;

      allowedUsers = [ "@wheel" "@builders" "mhuber" ];
      trustedUsers = [ "root" "@wheel" "@builders" "mhuber" ];

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

      extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      ''; # experimental-features = nix-command flakes
    };
  };
}
