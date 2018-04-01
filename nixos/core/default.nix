# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, hostName, hostId,
  otherImports ? [],
  ... }:

{
  imports = otherImports ++ [
    ./base.nix
    ../roles
    (../machines + "/${hostName}.nix")
  ];

  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";

  nixpkgs.config = import ../../nix/nixpkgs-config.nix;
  nixpkgs.overlays = [(self: super: {
    unstable = import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { inherit (super) config; };
  })] ;

  nix.nixPath = [
    # "nixpkgs=http://nixos.org/channels/nixos-17.09/nixexprs.tar.xz"
    # "unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz"
    "nixpkgs=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz"
    "nixpkgs-overlays=/etc/nix/overlays"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  # nixpkgs.overlays = nixpkgs.config.overlays;
  boot.kernel.sysctl = {
    # "fs.inotify.max_user_watches" = 524288;
    "vm.swappiness" = 1;
  };
}
