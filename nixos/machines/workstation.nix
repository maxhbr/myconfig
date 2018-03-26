# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ./hardware/grub.nix
    ./hardware/quadroFX4800.nix
    ./hardware/steamcontroller.nix
  ];
  myconfig.active-roles = [
    "desktop" "xmonad" "xfce" "vnc"
    "mail" "irc"
    "work" "virtualization" "dev"
    "imagework"
    "games" "wine"
    "vsftp" "openssh"
  ];
}
