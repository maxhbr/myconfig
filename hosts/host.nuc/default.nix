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
    ./smart-home
    ./services.caddy.nix
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
    (myconfig.metadatalib.fixIp "enp0s20u2")
    {
      # programs.mosh.enable = lib.mkDefault true;
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
    }
  ];
  config = {
    myconfig = {
      desktop.enable = false;
      headless.enable = true;
      smart-home.enable = true;
    };

    networking.hostName = "nuc";
    networking.hostId = "29d93123";
  };
}
