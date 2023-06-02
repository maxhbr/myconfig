# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  config = {
    myconfig = {
      desktop.enable = true;
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

    networking.hostId = "12345678";
    boot.kernelPackages = lib.mkForce pkgs.linuxPackages;

    boot.initrd.supportedFilesystems = [ "luks" ];
  };
}
