# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../../hardware/grub.nix
    {
      boot.initrd.supportedFilesystems = [
        "btrfs"
        "luks"
      ];
      services.btrfs.autoScrub = {
        enable = true;
      };
    }
    (myconfig.metadatalib.fixIp "enp2s0")
  ];
  config = {
    myconfig = {
      desktop.enable = false;
      headless.enable = true;
    };

    networking.hostName = "nuc";
    networking.hostId = "29d93123";
  };
}
