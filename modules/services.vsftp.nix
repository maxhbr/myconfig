# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# based on https://github.com/jakubgs/nixos-config/blob/master/roles/vsftpd.nix
{ config, lib, pkgs, myconfig, ... }:
let
  listenPort = 9226;
  pasvPorts = {
    min = 51000;
    max = 51001;
  };
in {
  config = (lib.mkIf config.services.vsftpd.enable {
    # User
    users = {
      extraUsers.ftpuser = {
        createHome = true;
        isSystemUser = true;
        hashedPassword =
          "$6$yfi/TifAgZVWZN8n$pwEKwnLFSQ.P7xfhgwJ1hz7SxMCLvip/lQv0jEz74UqcwF16omIVvwpWXX6QErhm8Lr5lB1ACEUisLkI/mTyZ/";
        group = "ftpuser";
        extraGroups = [ "ftp" ];
        home = "/var/ftp";
        homeMode = "777";
      };
      extraUsers."${myconfig.user}" = { extraGroups = [ "ftp" "ftpuser" ]; };
      groups.ftpuser = { };
    };

    # Service - WARNING: Open to public!
    services.vsftpd = {
      writeEnable = true;
      localUsers = true;
      chrootlocalUser = true;
      allowWriteableChroot = true;
      userlistEnable = true;
      userlist = [ "ftpuser" ];
      extraConfig = ''
        local_umask=0000
        listen_port=${builtins.toString listenPort}
        pasv_enable=YES
        pasv_min_port=${builtins.toString pasvPorts.min}
        pasv_max_port=${builtins.toString pasvPorts.max}
      '';
    };

    # Firewall
    networking.firewall.allowedTCPPorts = [ listenPort ];
    networking.firewall.allowedTCPPortRanges = [{
      from = pasvPorts.min;
      to = pasvPorts.max;
    }];
  });
}
