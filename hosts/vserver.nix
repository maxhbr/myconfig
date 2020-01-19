# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./minimal.nix
    # hardware:
    ./hardware/grub.nix
    # configuration
    ../modules/server
    ../modules/server/service.wireguard-server
  ];

  config = {
    system.autoUpgrade.allowReboot = true;
  };
}
