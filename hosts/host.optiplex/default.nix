# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./wifi.nix
    # hardware:
    ../../hardware/efi.nix
    # spin down the connected USB HDD
    ../../hardware/hdd-spinndown.nix
  ];
  config = {
    myconfig = {
      headless.enable = true;
      observability.client.enable = true;
    };

    boot.initrd.systemd.enable = true;

    networking.hostName = "optiplex";
    # Random 8 hex digits (only needs to be stable and unique on this LAN,
    # e.g. for ZFS host-identification purposes; not otherwise meaningful).
    networking.hostId = "2c43d66f";

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It's perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "26.11"; # Did you read the comment?
  };
}
