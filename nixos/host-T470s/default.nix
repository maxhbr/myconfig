# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, lib, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports = [
    ./hardware-configuration.nix
    ../hardware/notebook-generic.nix
    ../dev.nix
    # hardware:
    ../hardware/efi.nix
    # modules
    ../modules/service.postgresql.nix
  ] ++ importall ./imports;

  config = {
    networking.hostName = "T470s";
    networking.hostId = "8bf4efff";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/23fb2575-3ac8-41cd-acc2-29f6fee22702";
      preLVM = true;
      allowDiscards = true;
    };

    services = {
      logind.lidSwitch = lib.mkForce "ignore";
      logind.lidSwitchDocked = lib.mkForce "ignore";
    };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "20.09"; # Did you read the comment?
  };
}
