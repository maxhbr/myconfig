# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../roles/headless.nix
    # hardware:
    ../../hardware/grub.nix
    # configuration
    ./service.wireguard-server
    ../../modules/virtualization.docker
  ];
  config =
    { networking.hostName = "vserver";
      networking.hostId = "49496f29";
    };
}
