# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../dev
    # hardware:
    ./x1extremeG2.nix
    ../hardware/efi.nix
    # modules
    ./modules/work
    # ./modules/dnsmasq.nix
    ## fun
    ./modules/imagework
    ./modules/smarthome.nix
    ./modules/gaming
  ];

  config = {
    networking.hostName = "x1extremeG2";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/2118a468-c2c3-4304-b7d3-32f8e19da49f";
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
