# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  config = {
    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          desktop = "dwl";
          dwl.enable = true;
        };
      };
      #   virtualisation.enable = true;
      #   dev = {
      #     haskell.enable = true;
      #     network.enable = true;
      #     compliance.enable = true;
      #   };
    };
    # virtualisation.docker.enable = true;
    # virtualisation.libvirtd.enable = true;
    # virtualisation.virtualbox.host.enable = true;

    services.hardware.bolt.enable = true;

    networking.hostId = "12345678";
    boot.kernelPackages = lib.mkForce pkgs.linuxPackages;

    boot.initrd.supportedFilesystems = [ "luks" ];
  };
}
