# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./wifi.nix
  ];

  config = {
    networking.hostName = "odroid";
    # `head -c 8 /etc/machine-id`-style stable 32-bit host id.
    networking.hostId = "0d201d02";
    networking.networkmanager.enable = true;

    # zfs is pulled in via base.nix's supportedFilesystems; explicitly preserve
    # the legacy default so the evaluation warning is silenced without changing
    # import behavior (mirrors host.pi3a).
    boot.zfs.forceImportRoot = true;

    swapDevices = [
      {
        device = "/swapfile";
        priority = 0;
        size = 1024;
      }
    ];

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?

    # node_exporter + vmagent + TFA Dostmann AirCO2NTROL Mini CO2 sensor
    myconfig.observability = {
      client = {
        enable = true;
        co2Exporter.enable = true;
      };
    };
  };
}
