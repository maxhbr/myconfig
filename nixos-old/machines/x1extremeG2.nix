# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ./hardware/x1extremeG2.nix
    ./hardware/efi.nix
    ./hardware/exfat.nix
    ./hardware/steamcontroller.nix
    ./hardware/pulseaudio.nix
  ];

  myconfig.active-roles = [
    "xmonad" "xfce" "sway" # "vnc"
    "mail"
    "work" "virtualization" "dev"
    "imagework"
    "games" # "wine"
    "iot"
    "openssh"
  ];

  # swapDevices = [ {
  #   device = "/swapfile";
  #   size = 20480;
  # }];

  boot.initrd.supportedFilesystems = [ "luks" ];
  boot.initrd.luks.devices = [{
    device = "/dev/disk/by-uuid/2118a468-c2c3-4304-b7d3-32f8e19da49f";
    name = "crypted";
    preLVM = true;
    allowDiscards = true;
  }];

  services.xserver.displayManager.slim.autoLogin = true;
}
