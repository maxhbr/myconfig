# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  getJson = channel:
    let
      jsonFile = ./. + ("/" + channel + ".json");
    in if builtins.pathExists jsonFile then
      builtins.fromJSON (builtins.readFile jsonFile)
    else
      throw "missing json for ${channel}, looked at ${jsonFile}";
  mkPkgsPath = channel:
    let json = getJson channel;
    in pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      inherit (json) rev sha256;
    };
  mkPkgsFromPath = pkgsPath:
    import pkgsPath { config = config.nixpkgs.config; };
  mkPkgs = channel: mkPkgsFromPath (mkPkgsPath channel);

  ustablePath = mkPkgsPath "nixpkgs-unstable";
  nixos2003Path = mkPkgsPath "nixos-20.03-small";
  nixos2009Path = mkPkgsPath "nixos-20.09-small";

  # see: https://discourse.nixos.org/t/my-painpoints-with-flakes/9750/14
  # maybe not necessary if configuration uses flakes
  mkRegistry = channel:
    let
      json = getJson channel;
    in {
      from = {
        id = channel;
        type = "indirect";
      };
      to = {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = json.rev;
        type = "github";
      };
    };

in {
  config = {
    nixpkgs.overlays = [
      (self: super: {
        unstable = super.unstable or { } // mkPkgsFromPath ustablePath;
        nixos-unstable = super.nixos-unstable or { } // mkPkgs "nixos-unstable";
        nixos-unstable-small = super.nixos-unstable-small or { }
          // mkPkgs "nixos-unstable-small";
        nixos-2003-small = super.unstable or { }
          // mkPkgsFromPath nixos2003Path;
        nixos-2009-small = super.unstable or { }
          // mkPkgsFromPath nixos2009Path;
      })
    ];
    nix = {
      nixPath = [
        ("nixpkgs=" + ../../../nixpkgs)
        ("nixpkgs-unstable=" + ustablePath)
        ("nixos-20.03-small=" + nixos2003Path)
        ("nixos-20.09-small=" + nixos2009Path)
        "nixos-config=/dev/null"
      ];
      registry = {
        # self.flake = inputs.self;
        nixpkgs-unstable = mkRegistry "nixpkgs-unstable";
      };
    };
  };
}
