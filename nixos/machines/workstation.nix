# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ./hardware/grub.nix
    ./hardware/quadroFX4800.nix
    ./hardware/steamcontroller.nix
    ./hardware/pulseaudio.nix
  ];
  myconfig.active-roles = [
    "xmonad" "xfce"
    "virtualization" "dev"
    "imagework"
    "games" "wine"
    "vsftp" "openssh"
  ];

  services.xserver.displayManager.slim.autoLogin = true;
  # services.xserver.windowManager.default = pkgs.lib.mkForce "xfce";
}
