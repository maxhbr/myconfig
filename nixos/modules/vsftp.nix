# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  config = {
    networking.firewall.allowedTCPPorts = [ 9136 ];
    networking.firewall.allowedUDPPorts = [ 9136 ];
    services.vsftpd = {
      enable = true;
      userlist = [ "mhuber" ];
      userlistEnable = true;
      localUsers = true;
      extraConfig = ''
        listen_port=9136
      '';
    };
  };
}
