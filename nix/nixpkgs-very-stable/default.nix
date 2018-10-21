# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

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

let
  channel = "nixpkgs-18.09";
  jsonFile = ./. + (channel + ".json");
  revFile = ./. + (channel + ".rev");
  fallbackUrl = "http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz";
  nixpkgs = if builtins.pathExists jsonFile
    then let
           json = builtins.fromJSON (builtins.readFile jsonFile);
         in builtins.fetchTarball {
              url = "https://github.com/NixOS/nixpkgs/archive/${json.rev}.tar.gz";
              inherit (json) sha256;
            }
    else if builtins.pathExists revFile
         then let
                rev = builtins.readFile revFile;
              in builtins.fetchTarball {
                   url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
                 }
         else builtins.fetchTarball fallbackUrl;
in
  { overlays ? [] , ... }@args:
  import (nixpkgs + "/pkgs/top-level") (args // {
    localSystem = { system = builtins.currentSystem; };
    config = import ../nixpkgs-config.nix;
    overlays = (import ../nixpkgs-overlays.nix) ++ overlays;
  })
