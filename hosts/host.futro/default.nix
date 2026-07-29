# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Fujitsu Futro S740 thin client (Intel Celeron J4105, 4GB DDR4, 8GB M.2).
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
    (myconfig.metadatalib.fixIp "enp2s0")
    {
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
    }
  ];

  config = {
    # Use the systemd-boot EFI boot loader.
    boot.loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.enable = false;
    };

    myconfig = {
      desktop.enable = false;
      headless.enable = true;
    };

    networking.hostName = "futro";
    networking.hostId = "fdb6854c";
    networking.networkmanager.enable = true;

    system.stateVersion = lib.mkForce "26.11";
  };
}
