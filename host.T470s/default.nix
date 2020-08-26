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
  ];

  config = {
    networking.hostName = "T470s";
    networking.hostId = "8bf4efff";

    networking.firewall = {
      allowedUDPPorts = [ 5432 ];
      allowedTCPPorts = [ 5432 ];
    };

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/23fb2575-3ac8-41cd-acc2-29f6fee22702";
      preLVM = true;
      allowDiscards = true;
    };

    services = {
      logind.lidSwitch = lib.mkForce "ignore";
      logind.lidSwitchDocked = lib.mkForce "ignore";
      postgresql = {
        enable = true;
        package = pkgs.postgresql_10;
        enableTCPIP = true;
        authentication = pkgs.lib.mkOverride 10 ''
          local all all trust
          host all all ::1/128 trust
          host all all 10.199.203.3/24 trust
        '';
      };
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
