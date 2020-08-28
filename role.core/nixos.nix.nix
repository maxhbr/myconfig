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
            allowUnfree = ${lib.boolToString config.nixpkgs.config.allowUnfree};
            permittedInsecurePackages = [
              "dcraw-9.28.0"
              # - CVE-2018-19655
              # - CVE-2018-19565
              # - CVE-2018-19566
              # - CVE-2018-19567
              # - CVE-2018-19568
              "jasper-2.0.16"
              # - Numerous CVE unsolved upstream
              # - See: https://github.com/NixOS/nixpkgs/pull/57681#issuecomment-475857499
              # - See: https://github.com/mdadams/jasper/issues/208
            ];
          }
          '';
        };
      };
    };
    nix = rec {
      useSandbox = true;
      readOnlyStore = true;

      autoOptimiseStore = true;
      optimise.automatic = true;

      allowedUsers = [ "@wheel" "@builders" "mhuber" ];
      trustedUsers = [ "root" "@wheel" "@builders" "mhuber" ];

      trustedBinaryCaches = [ "https://cache.nixos.org" "https://maxhbr.cachix.org" ];
      binaryCaches = trustedBinaryCaches;
      binaryCachePublicKeys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" "maxhbr.cachix.org-1:wwfYm+B6HaXyFey300cmuSQnvwULS0VU1VtOGXDyxCo=" ];

      extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      '';
    };
  };
}
