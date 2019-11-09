# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.openssh = {
      enable = lib.mkEnableOption "Openssh role";
    };
  };

  config = lib.mkIf config.myconfig.roles.openssh.enable {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      forwardX11 = true;
    };
  };
}
