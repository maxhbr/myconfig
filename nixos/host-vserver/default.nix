# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../imports.nix
    ../headless
    # hardware:
    ../hardware/grub.nix
    # configuration
    ./modules/service.wireguard-server
  ];
  config = {
    networking.hostName = "vserver";
  };
}
