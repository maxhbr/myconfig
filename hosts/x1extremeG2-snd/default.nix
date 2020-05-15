# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../roles/dev.nix
    # hardware:
    ../../hardware/x1extremeG2.nix
    ../../hardware/efi.nix
    ../../modules/misc-desktop-tools.nix
    ## fun
    ../../modules/imagework
    ../../modules/smarthome.nix
    ../../roles/gaming.nix
  ];

  config = {
    networking.hostName = "x1extremeG2-snd";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/0b3b85df-65ef-4b2b-8073-a62c5a7346dc";
      preLVM = true;
      allowDiscards = true;
    };

    # option definitions
    boot.kernel.sysctl = {
      "vm.swappiness" = 1;
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
