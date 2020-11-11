# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  mkPkgsPath = channel:
    let
      jsonFile = ./. + channel + ".json";
      revFile = ./. + channel + ".rev";
    in if builtins.pathExists jsonFile then
      let json = builtins.fromJSON (builtins.readFile jsonFile);
      in builtins.fetchGit {
        name = "nixos-unstable-fix";
        url = "https://github.com/nixos/nixpkgs/";
        inherit (json) rev sha256;
      }
    else if builtins.pathExists revFile then
      let rev = builtins.readFile revFile;
      in builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      }
    else
      let
        fallbackUrl = "http://nixos.org/channels/" + channel
          + "/nixexprs.tar.xz";
      in builtins.fetchTarball fallbackUrl;
  mkPkgsFromPath = pkgsPath:
    import pkgsPath { config = config.nixpkgs.config; };
  mkPkgs = channel: mkPkgsFromPath (mkPkgsPath channel);
  ustablePath = mkPkgsPath "nixpkgs-unstable";
  nixos2003Path = mkPkgsPath "nixos-20.03-small";
  nixos2009Path = mkPkgsPath "nixos-20.09-small";
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
    home-manager.users."${user}" = {
      nixpkgs.overlays = config.nixpkgs.overlays;
    };
    nix.nixPath = [
      ("nixpkgs=" + ../../../nixpkgs)
      ("nixpkgs-unstable=" + ustablePath)
      ("nixos-20.03-small=" + nixos2003Path)
      ("nixos-20.09-small=" + nixos2009Path)
      "nixos-config=/dev/null"
    ];
  };
}
