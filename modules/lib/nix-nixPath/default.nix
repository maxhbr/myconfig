# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  mkPkgsPath = channel:
    let
      jsonFile = ./. + ("/" + channel + ".json");
      revFile = ./. + ("/" + channel + ".rev");
    in if builtins.pathExists jsonFile then
      let json = builtins.fromJSON (builtins.readFile jsonFile);
      in pkgs.fetchFromGitHub {
        owner = "nixos";
        repo = "nixpkgs";
        inherit (json) rev sha256;
      }
    else if builtins.pathExists revFile then
      let rev = builtins.readFile revFile;
      in builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      }
    else
      throw "missing pinning for ${channel}, looked for ${jsonFile} and ${revFile}";
      # let
      #   fallbackUrl = "http://nixos.org/channels/" + channel
      #     + "/nixexprs.tar.xz";
      # in builtins.fetchTarball fallbackUrl;
  mkPkgsFromPath = pkgsPath:
    import pkgsPath { config = config.nixpkgs.config; };
  mkPkgs = channel: mkPkgsFromPath (mkPkgsPath channel);

  # see: https://discourse.nixos.org/t/my-painpoints-with-flakes/9750/14
  # maybe not necessary if configuration uses flakes
  mkRegistry = channel:
    let
      rev =
        let
          jsonFile = ./. + ("/" + channel + ".json");
          revFile = ./. + ("/" + channel + ".rev");
        in if builtins.pathExists jsonFile then
          let json = builtins.fromJSON (builtins.readFile jsonFile);
          in json.rev
        else if builtins.pathExists revFile then
          builtins.readFile revFile
        else
          throw "missing rev for ${channel}";
    in {
      from = {
        id = channel;
        type = "indirect";
      };
      to = {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = rev;
        type = "github";
      };
    };

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
    nix = {
      nixPath = [
        ("nixpkgs=" + ../../../nixpkgs)
        ("nixpkgs-unstable=" + ustablePath)
        ("nixos-20.03-small=" + nixos2003Path)
        ("nixos-20.09-small=" + nixos2009Path)
        "nixos-config=/dev/null"
      ];
      # registry = {
      #   # self.flake = inputs.self;
      #   nixpkgs-unstable = mkRegistry "nixpkgs-unstable";
      # };
    };
  };
}
