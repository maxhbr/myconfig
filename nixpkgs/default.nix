# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
let
  loadNixpkgs = { jsonFile, fallbackUrl }:
    if builtins.pathExists jsonFile
    then let
        json = builtins.fromJSON (builtins.readFile jsonFile);
      in builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${json.rev}.tar.gz";
           inherit (json) sha256;
         }
    else builtins.fetchTarball fallbackUrl;

  nixpkgs = loadNixpkgs {
    jsonFile = ./nixos-18.03.json;
    fallbackUrl = http://nixos.org/channels/nixos-18.03/nixexprs.tar.xz;
  };
  unstableNixpkgs = loadNixpkgs {
    jsonFile = ./nixos-unstable.json;
    fallbackUrl = http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz;
  };
in
  { overlays ? [] , ... }@args:
  let

    config = pkgs: {
      allowUnfree = true;
      mplayer.useUnfreeCodecs = true;
      # virtualbox.enableExtensionPack = true;
    };

    allOverlays = let
        baseOverlays = [
          (self: super: {
            unstable = import (unstableNixpkgs + "/pkgs/top-level") (args //
              {
                inherit config;
                localSystem = { system = builtins.currentSystem;};
              }
            );
          })
          (import ../.)
        ];
        overlaysFromFolders = let
            path = ./overlays;
            content = builtins.readDir path;
          in map (n: import (path + ("/" + n)))
               (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                 (builtins.attrNames content));
      in
        overlays ++ baseOverlays ++ overlaysFromFolders;
  in
    import (nixpkgs + "/pkgs/top-level") ({
      inherit config;
      localSystem = { system = builtins.currentSystem; };
      overlays = allOverlays;
    }) // {
      # expose some internals to be used in nixos configuration
      # TODO: that should not be necessary
      myconfig-misc = {
        inherit
          nixpkgs unstableNixpkgs
          config;
        overlays = allOverlays;
      };
    }
