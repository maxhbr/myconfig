# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  config = {
    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          desktop = "hyprland";
          hyprland.enable = true;
        };
      };
    };

    services.hardware.bolt.enable = true;

    networking.hostId = "12345678";
    boot.kernelPackages = lib.mkForce pkgs.linuxPackages;

    boot.initrd.supportedFilesystems = [ "luks" ];
  };
}
