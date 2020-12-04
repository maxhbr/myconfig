# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../hardware/grub.nix
    {
      boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];
      services.btrfs.autoScrub = { enable = true; };
    }
    ../role.headless
  ];
  config = {
    networking.hostName = "nuc";
    networking.hostId = "29d93123";
  };
}
