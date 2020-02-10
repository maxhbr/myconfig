# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../headless
    # hardware:
    ../hardware/grub.nix
    # configuration
    ./modules/server/service.wireguard-server
  ];
}
