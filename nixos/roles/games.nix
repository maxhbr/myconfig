# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
{
  options = {
    myconfig.roles.games = {
      enable = (lib.mkEnableOption "Games role") && config.services.xserver.enable;
    };
  };

  config = lib.mkIf config.myconfig.roles.games.enable {
    environment.systemPackages = with pkgs.unstable; [
      steam
    ];

    hardware.opengl = {
      driSupport = true;
      driSupport32Bit = true;
    };
    networking.firewall.allowedUDPPorts = [ 27031 27036 ];
    networking.firewall.allowedTCPPorts = [ 27036 27037 ];
  };
}
