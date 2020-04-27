# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports = [
    ./nixPath.nix
    ./hardware-configuration.nix
    ../dev.nix
    # hardware:
    ../hardware/x1extremeG2.nix
    ../hardware/efi.nix
    ./backup-hdd.nix
    ./foto-hdd.nix
    # modules
    ../modules/work
    ../modules/misc-desktop-tools.nix
    ## fun
    ../modules/imagework
    ../modules/smarthome.nix
    ../gaming.nix
  ] ++ importall ./imports;

  config = {
    networking.hostName = "x1extremeG2";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/2118a468-c2c3-4304-b7d3-32f8e19da49f";
      preLVM = true;
      allowDiscards = true;
    };

    home-manager.users.mhuber = {
      home.file = {
        ".config/autorandr/" = {
          source = ./autorandr;
          recursive = true;
        };
      };
    };
  };
}
