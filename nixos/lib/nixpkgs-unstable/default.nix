# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see: https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs

## generate json file:
#
#  rev=$(curl -L -s "https://nixos.org/channels/${channel}/git-revision")
#  tarball="https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz"
#  prefetchOutput=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
#  hash=$(echo "$prefetchOutput" | head -1)
#  path=$(echo "$prefetchOutput" | tail -1)
#  echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","path":"'$path'"}' > ./${channel}.json
#

## generate rev file with:
#
#  curl -L -s "https://nixos.org/channels/${channel}/git-revision" > ./${channel}.rev
#
{ pkgs, config, ... }:
let
  mkPkgs = channel: let
      jsonFile = ./. + channel + ".json";
      revFile = ./. + channel + ".rev";
    in import (if builtins.pathExists jsonFile
      then let
             json = builtins.fromJSON (builtins.readFile jsonFile);
           in builtins.fetchGit {
             name = "nixos-unstable-fix";
             url = https://github.com/nixos/nixpkgs/;
             inherit (json) rev sha256;
           }
      else if builtins.pathExists revFile
           then let
               rev = builtins.readFile revFile;
             in builtins.fetchTarball {
                  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
                }
           else let
               fallbackUrl = "http://nixos.org/channels/" + channel + "/nixexprs.tar.xz";
             in builtins.fetchTarball fallbackUrl)
    { config = config.nixpkgs.config; };
in {
  config = {
    nixpkgs.overlays = [(self: super: {
      nixos-unstable = super.nixos-unstable or {} // mkPkgs "nixos-unstable";
      nixos-unstable-small = super.nixos-unstable-small or {} // mkPkgs "nixos-unstable-small";
      unstable = super.unstable or {} // mkPkgs "nixpkgs-unstable";
      nixos-2003 = super.unstable or {} // mkPkgs "nixos-20.03";
      nixos-2003-small = super.unstable or {} // mkPkgs "nixos-20.03-small";
    })];
  };
}
