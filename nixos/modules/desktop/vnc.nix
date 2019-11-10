# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./X.nix
  ];

  config = {
    environment.systemPackages = with pkgs; [
      x11vnc
    ];
    networking.firewall.allowedUDPPorts = [ 5900 ];
    networking.firewall.allowedTCPPorts = [ 5900 ];
  };
}
