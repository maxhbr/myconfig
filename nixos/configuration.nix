# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
  hostId = "${builtins.readFile /etc/nixos/hostid}";

in import ./core {
  system.copySystemConfiguration = true;
  inherit config hostName hostId;
  otherImports = [ /etc/nixos/hardware-configuration.nix ];
} // {  environment.etc = {
    nixos-orig.source = ./.;
  };
}
