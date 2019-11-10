# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware/x1extremeG2.nix
    ./hardware/efi.nix
    ./hardware/exfat.nix
    ./hardware/steamcontroller.nix
    ./hardware/pulseaudio.nix
    # modules
    ../modules/desktop/xmonad.nix
    ../modules/desktop/games
    ../modules/desktop/sway.nix
    # ../modules/desktop/xfce.nix
    ../modules/mail.nix
    ../modules/virtualization
    ../modules/service/openssh.nix
    ../modules/service/syncthing.nix
  ];

  config = {
    boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices = [{
      device = "/dev/disk/by-uuid/2118a468-c2c3-4304-b7d3-32f8e19da49f";
      name = "crypted";
      preLVM = true;
      allowDiscards = true;
    }];
    services.xserver.displayManager.slim.autoLogin = true;
  };

  # myconfig.active-roles = [
  #   "xmonad" "xfce" "sway" # "vnc"
  #   "mail"
  #   "work" "virtualization" "dev"
  #   "imagework"
  #   "games" # "wine"
  #   "iot"
  #   "openssh"
  # ];
}
