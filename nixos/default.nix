# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee /etc/nixos/hostid
  hostId = "${builtins.readFile /etc/nixos/hostid}";

  mypkgs = import <nixpkgs> {};

in
  import ./core {
    system.copySystemConfiguration = true;
    inherit config hostName hostId;
    otherImports = [ /etc/nixos/hardware-configuration.nix ]
      ++ (if builtins.pathExists /etc/nixos/configuration.old.nix
          then [/etc/nixos/configuration.old.nix]
          else []);
  } // {
    environment.etc = {
      nixos-orig.source = ./.;
    };

    nixpkgs = {
      config = import ../nix/nixpkgs-config.nix;
      overlays = import ../nix/all-overlays.nix;
    };
  } // (if builtins.pathExists ../nix/nixPath.nix
        then { nix.nixPath = import ../nix/nixPath.nix; }
        else {})
