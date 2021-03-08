# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../modules
    ../hardware/grub.nix
    {
      boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];
      services.btrfs.autoScrub = { enable = true; };
    }
  ];
  config = {
    myconfig = {
      desktop.enable = true;
      headless.enable = true;
    };

    networking.hostName = "nuc";
    networking.hostId = "29d93123";
  };
}
