# Copyright 2024 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# https://nixos.wiki/wiki/Radicale
{ pkgs, config, lib, myconfig, ... }:
let user = myconfig.user;
in {
  config = lib.mkIf config.services.radicale.enable {
    services.radicale = {
      settings = {
        server.hosts = [ "0.0.0.0:5232" "[::]:5232" ];
        auth = {
          type = "htpasswd";
          # htpasswd_filename = "/etc/radicale_users.htpasswd";
          # htpasswd_encryption = "bcrypt";
        };
        storage = { filesystem_folder = "/var/lib/radicale/collections"; };
      };
      rights = {
        root = {
          user = ".+";
          collection = "";
          permissions = "R";
        };
        principal = {
          user = ".+";
          collection = "{user}";
          permissions = "RW";
        };
        calendars = {
          user = ".+";
          collection = "{user}/[^/]+";
          permissions = "RW";
        };
      };
    };
    networking.firewall.allowedTCPPorts = [ 5232 ];
    networking.firewall.allowedUDPPorts = [ 5232 ];
  };
}

