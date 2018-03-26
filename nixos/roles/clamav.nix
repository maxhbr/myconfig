# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
{
  options = {
    myconfig.roles.clamav = {
      enable = lib.mkEnableOption "Clamav role";
    };
  };

  config = lib.mkIf config.myconfig.roles.clamav.enable {
    environment.systemPackages = with pkgs; [
      clamav
    ];
    services.clamav = {
      daemon.enable = true;
      updater.enable = true;
    };
  };
}
