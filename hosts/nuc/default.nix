# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports =
    [ ./hardware-configuration.nix
      { boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];
        services.btrfs.autoScrub =
          { enable = true;
          };
      }
      ../../roles/headless.nix
      ../../modules/service.monitoring.nix
      ../../modules/virtualization.docker
      ../../modules/service.syncthing.nix
      # hardware:
      ../../hardware/grub.nix
      # configuration
      ./nuccam.nix
    ];
  config = {
    networking.hostName = "nuc";
    networking.hostId = "29d93123";
  };
}
