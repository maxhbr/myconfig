# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.taskserver = {
      enable = lib.mkEnableOption "Taskserver role";
    };
  };

  config = lib.mkIf config.myconfig.roles.taskserver.enable {
    services.taskserver = {
      enable = true;
      fqdn = "server";
      listenHost = "::";
      organisations.my-company.users = [ "mhuber" ];
    };
    # networking.firewall.allowedTCPPorts = [ ];
    environment.systemPackages = with pkgs; [
      taskwarrior
    ];
  };
}
