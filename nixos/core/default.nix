# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, hostName, hostId,
  otherImports ? [],
  otherOverlays ? [],
  ... }:

{
  imports = otherImports ++ [
    ./base.nix
    ../roles
  ] ++ (if builtins.pathExists (../machines + "/${hostName}.nix")
        then [(../machines + "/${hostName}.nix")]
        else []);

  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";

  nixpkgs.config = import ../../nix/nixpkgs-config.nix;
  nixpkgs.overlays = otherOverlays ++ [(self: super: {
    unstable = import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { inherit (super) config; };
  })];

  nix.nixPath = [
    # "nixpkgs=channel:nixos-18.03"
    # "unstable=channel:nixos-unstable"
    "nixpkgs=channel:nixos-unstable"
    "nixpkgs-overlays=/etc/nix/overlays"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  # nixpkgs.overlays = nixpkgs.config.overlays;
  boot.kernel.sysctl = {
    # "fs.inotify.max_user_watches" = 524288;
    "vm.swappiness" = 1;
  };
}
