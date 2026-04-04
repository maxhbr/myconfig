# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:

{
  options.myconfig.homeassistant = with lib; {
    enable = mkEnableOption "Home Assistant native installation";
  };

  config = lib.mkIf config.myconfig.homeassistant.enable {
    services.home-assistant = {
      enable = true;
      package = pkgs.home-assistant;
      config = {
        home = {
          name = "Home";
          latitude = 48.1351;
          longitude = 11.5820;
          elevation = 520;
          unit_system = "metric";
          currency = "EUR";
        };
        frontend = { };
        http = {
          use_x_forwarded_for = true;
          trusted_proxies = [
            "127.0.0.1"
            "10.0.0.0/8"
            "172.16.0.0/12"
            "192.168.0.0/16"
          ];
        };
        group = { };
      };
    };

    systemd.services.home-assistant = {
      serviceConfig = {
        PrivateDevices = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = "/var/lib/home-assistant";
      };
    };

    users.groups.homeassistant = { };
    users.users.homeassistant = {
      group = "homeassistant";
      home = "/var/lib/home-assistant";
      isSystemUser = true;
      createHome = true;
    };

    myconfig.persistence.directories = [ "/var/lib/home-assistant" ];

    services.deconz = {
      enable = true;
      group = "homeassistant";
    };
  };
}
