# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:

{
  imports = [
    ./dnsmasq.nix
    ./extrahosts
  ];
  config = {
    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };
  };
}
