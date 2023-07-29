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
    nixpkgs.config = {
      allowUnfree = true;
      segger-jlink.acceptLicense = true;
    };
    home-manager.sharedModules = [{
      home.packages = with pkgs; [ nix-tree ];
      imports = [{
        programs.fish = {
          shellAbbrs = {
            why-depends-nixos = "nix why-depends /run/current-system";
            nixse = "nix search nixpkgs";
          };
          functions = {
            nixTest =
              "NIXPKGS_ALLOW_UNFREE=1 nix-shell '<nixpkgs>' --fallback --run fish -p $argv";
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
    boot.readOnlyNixStore = true;
    nix = {
      extraOptions = ''
        keep-outputs = true
        keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      '';

      settings = rec {
        sandbox = true;
        auto-optimise-store = true;

        allowed-users = [ "@wheel" "@builders" "${myconfig.user}" ];
        trusted-users = [ "root" ] ++ allowed-users;

        trusted-substituters = [
          "https://cache.nixos.org"
          # "https://maxhbr.cachix.org"
          "https://nixfmt.cachix.org"
        ];
        substituters = trusted-substituters;
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          # "maxhbr.cachix.org-1:wwfYm+B6HaXyFey300cmuSQnvwULS0VU1VtOGXDyxCo="
          "nixfmt.cachix.org-1:uyEQg16IhCFeDpFV07aL+Dbmh18XHVUqpkk/35WAgJI="
        ];

      };
      optimise.automatic = true;
    };

    # see: https://github.com/NixOS/nixpkgs/issues/54707#issuecomment-1132907191
    systemd = {
      services.nix-daemon = { environment.TMPDIR = "/nix/tmp"; };
      tmpfiles.rules = [ "d /nix/tmp 0755 root root 1d" ];
    };
  };
}
