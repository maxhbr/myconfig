# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  imports = [
    ./hardware/T470p.nix
    ./hardware/efi.nix
    ./hardware/exfat.nix
    ./hardware/steamcontroller.nix
    ./hardware/pulseaudio.nix
    # modules
    ../modules/desktop/xmonad.nix
    ../modules/desktop/sway.nix
    # ../modules/desktop/xfce.nix
    ../modules/desktop/games
    ../modules/mail.nix
    ../modules/virtualization
    ../modules/service/openssh.nix
    ../modules/service/syncthing.nix
  ];

  config = {
    boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
    swapDevices = [ {
      device = "/swapfile";
      size = 20480;
    }];

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices = [{
      device = "/dev/disk/by-uuid/fc9ecff5-e0c5-4cff-bb5c-08a745c76e3c";
      name = "crypted";
      preLVM = true;
      allowDiscards = true;
    }];

    services.xserver.displayManager.slim.autoLogin = true;
  };
}
