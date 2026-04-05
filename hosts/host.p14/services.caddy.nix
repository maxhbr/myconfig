# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:

{
  services.caddy = {
    enable = true;
    virtualHosts = {
      "${config.networking.hostName}.wg0.maxhbr.local" = {
        listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
        hostName = "${config.networking.hostName}.wg0.maxhbr.local";
        serverAliases = [
          "${config.networking.hostName}.wg0"
          (myconfig.metadatalib.getWgIp "${config.networking.hostName}")
        ];
        extraConfig = ''
          handle_path /grafana/* {
            reverse_proxy http://localhost:3000
          }
          reverse_proxy http://localhost:8123
        '';
      };
    };
  };

  networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals config.services.caddy.enable [
    443
  ];
}
