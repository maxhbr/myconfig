# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, myconfig, ... }:
let
  port = 9136;
in {
  config = (lib.mkIf config.services.vsftpd.enable {
    networking.firewall.allowedTCPPorts = [ port ];
    networking.firewall.allowedUDPPorts = [ port ];
    services.vsftpd = {
      userlist = [ myconfig.user ];
      userlistEnable = true;
      localUsers = true;
      extraConfig = ''
        listen_port=${toString port}
      '';
    };
  });
}
