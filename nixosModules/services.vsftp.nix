# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let
  user = config.myconfig.user;
  port = 9136;
in {
  config = (lib.mkIf config.services.vsftpd.enable {
    networking.firewall.allowedTCPPorts = [ port ];
    networking.firewall.allowedUDPPorts = [ port ];
    services.vsftpd = {
      userlist = [ user ];
      userlistEnable = true;
      localUsers = true;
      extraConfig = ''
        listen_port=${toString port}
      '';
    };
  });
}
