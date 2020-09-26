# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../role.headless
    # hardware:
    ../hardware/grub.nix
    # configuration
    ./service.wireguard-server
    ../role.dev/virtualization.docker
  ];
  config = {
    networking.hostName = "vserver";
    networking.hostId = "49496f29";

    services.netdata.enable = lib.mkForce false;
  };
}
