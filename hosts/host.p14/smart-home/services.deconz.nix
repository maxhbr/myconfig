# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  options.myconfig.smart-home = with lib; {
    deconz.enable = mkEnableOption "enable deconz service";
  };

  config = lib.mkIf config.myconfig.smart-home.deconz.enable {
    environment.systemPackages = with pkgs; [
      config.services.deconz.package
    ];

    services.deconz = {
      enable = true; # Whether to enable deCONZ, a Zigbee gateway for use with ConBee/RaspBee hardware (https://phoscon.d…
      # package # The deconz package to use
      device = "/dev/ttyACM0"; # Force deCONZ to use a specific USB device (e.g. /dev/ttyACM0)

      allowRestartService = true; # Whether to enable killing/restarting processes
      allowRebootSystem = false; # Whether to enable rebooting the system
      allowSetSystemTime = false; # Whether to enable setting the system time

      # extraArgs # Extra command line arguments for deCONZ, see https://github.com/dresden-elektronik/deconz-rest-plu…
      # listenAddress # Pin deCONZ to the network interface specified through the provided IP address
      httpPort = 8124; # TCP port for the web server
      wsPort = 8135; # TCP port for the WebSocket

      openFirewall = false;# Whether to enable opening up the service ports in the firewall
    };

  };
}

