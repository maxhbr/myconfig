# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./desktop.X.common
  ];

  config = {
    services.xrdp.enable = true;
    services.xrdp.defaultWindowManager = "${pkgs.icewm}/bin/icewm";
    networking.firewall.allowedTCPPorts = [ 3389 ];
  };
}
