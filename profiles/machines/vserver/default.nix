# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    # hardware:
    ./hardware/grub.nix
    # configuration
    ../profiles/server
    ../profiles/server/service.wireguard-server
  ];

  config = {
    system.autoUpgrade.allowReboot = true;
  };
}
