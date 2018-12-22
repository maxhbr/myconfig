# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ./hardware/T470p.nix
    ./hardware/efi.nix
    ./hardware/exfat.nix
    ./hardware/steamcontroller.nix
    ./hardware/pulseaudio.nix
  ];

  myconfig.active-roles = [
    "xmonad" "xfce" # "vnc"
    "mail"
    "work" "virtualization" "dev"
    "imagework"
    "games"
  ];

  boot.initrd.supportedFilesystems = [ "luks" ];
  boot.initrd.luks.devices = [{
    device = "/dev/disk/by-uuid/fc9ecff5-e0c5-4cff-bb5c-08a745c76e3c";
    name = "crypted";
    preLVM = true;
    allowDiscards = true;
  }];

  services.xserver.displayManager.slim.autoLogin = true;
}
