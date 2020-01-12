# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../modules/hardware/grub.nix
    ../modules/service/openssh.nix
    ../modules/service/wireguard-server
    ../modules/auto-upgrade.nix
    ../modules/gc.nix
  ];

  config = {
    system.autoUpgrade.allowReboot = true;
  };
}
