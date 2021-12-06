# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, ... }: {
  imports = [{ # support for nix-flakes
    # see:
    # - https://nixos.wiki/wiki/Flakes
    # - https://www.tweag.io/blog/2020-05-25-flakes/
    config = {
      home-manager.sharedModules = [{
        programs.fish = {
          shellAbbrs = {
            nix-flake =
              "nix --experimental-features 'nix-command flakes' flake";
          };
        };
      }];
    };
  }];
  config = {
    nixpkgs.config = { allowUnfree = true; };
    home-manager.sharedModules = [{
      imports = [{
        programs.fish = {
          shellAbbrs = {
            why-depends-nixos = "nix why-depends /run/current-system";
          };
          functions = {
            nixse = "nix search nixpkgs";
            nixTest =
              "NIXPKGS_ALLOW_UNFREE=1 nix-shell '<nixpkgs>' --fallback --run fish -p";
            # see: https://github.com/NixOS/nixpkgs/issues/51368#issuecomment-704678563
            nix-closure-size =
              "${pkgs.nix}/bin/nix-store -q --size (nix-store -qR (readlink -e $argv) ) | awk '{ a+=$1 } END { print a }' | ${pkgs.coreutils}/bin/numfmt --to=iec-i";
            # see: https://nixos.wiki/wiki/Nix_command/path-info
            nix-closure-sizes =
              "nix path-info -rS (readlink -e $argv) | sort -nk2";
            nix-most-recently-added =
              "${pkgs.nix}/bin/nix path-info --json --all | ${pkgs.jq}/bin/jq -r 'sort_by(.registrationTime)[-11:-1][].path'";
            nix-list-big-closures =
              "${pkgs.nix}/bin/nix path-info --json --all -S | ${pkgs.jq}/bin/jq 'map(select(.closureSize > 1e9)) | sort_by(.closureSize) | map([.path, .closureSize])'";
          };
        };
      }];
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
    }];
    nix = rec {
      extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      '';

      useSandbox = true;
      readOnlyStore = true;

      autoOptimiseStore = true;
      optimise.automatic = true;

      allowedUsers = [ "@wheel" "@builders" "${myconfig.user}" ];
      trustedUsers = [ "root" ] ++ allowedUsers;

      trustedBinaryCaches = [
        "https://cache.nixos.org"
        # "https://maxhbr.cachix.org"
        "https://nixfmt.cachix.org"
      ];
      binaryCaches = trustedBinaryCaches;
      binaryCachePublicKeys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        # "maxhbr.cachix.org-1:wwfYm+B6HaXyFey300cmuSQnvwULS0VU1VtOGXDyxCo="
        "nixfmt.cachix.org-1:uyEQg16IhCFeDpFV07aL+Dbmh18XHVUqpkk/35WAgJI="
      ];
    };
  };
}
