# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, lib, ... }: {
  imports = [
    ../modules
    ./hardware-configuration.nix
    # hardware:
    ../hardware/grub.nix
    # configuration
    ./service.wireguard-server
  ];
  config = {
    myconfig = { headless.enable = true; };

    networking.hostName = "vserver";
    networking.hostId = "49496f29";

    virtualisation.docker.enable = true;

    services.netdata.enable = lib.mkForce false;

    # stk
    networking.firewall.allowedUDPPorts = [ 2759 2757 ];
    networking.firewall.allowedTCPPorts = [ 2759 2757 ];

  };
}
