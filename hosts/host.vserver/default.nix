# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    # hardware:
    ../../hardware/grub.nix
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

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?
  };
}
