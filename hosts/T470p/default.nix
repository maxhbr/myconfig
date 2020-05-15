# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../hardware/notebook-generic.nix
    ../../roles/dev.nix
    # hardware:
    ../../hardware/efi.nix
    # modules
    ../../modules/service.postgresql.nix
  ] ++ importall ./imports;

  config = {
    networking.hostName = "T470p";
    networking.hostId = "8865d9be";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/98375f3f-0366-421f-9ac6-fda1b5a2d5fe";
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
