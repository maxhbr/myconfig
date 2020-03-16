# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./extrahosts
  ];
  config = {
    environment.interactiveShellInit = ''
      myPorts() { /run/wrappers/bin/sudo ${pkgs.iproute}/bin/ss -tulpen; }
      killPort() { kill $(${pkgs.lsof}/bin/lsof -t -i:$1); }
    '';
    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };
  };
}
