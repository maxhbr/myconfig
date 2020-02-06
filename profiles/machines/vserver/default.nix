# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    # hardware:
    ../../hardware/grub.nix
    # configuration
    ../../server
    ../../server/service.wireguard-server
  ];

  config = {
    system.autoUpgrade.allowReboot = true;
  };
}
