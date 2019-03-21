# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ./hardware/T470p.nix
    ./hardware/efi.nix
    ./hardware/exfat.nix
    ./hardware/pulseaudio.nix
  ];

  myconfig.active-roles = [
    "xmonad"
    "work" "virtualization" "dev"
    "imagework"
  ];

}
