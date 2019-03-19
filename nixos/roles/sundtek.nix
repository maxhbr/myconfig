# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  options = {
      enable = lib.mkEnableOption "Sundtek role";
    };
  };

  imports = [
################################################################################
    { # sundtek
      config = lib.mkIf config.myconfig.roles.sundtek.enable {
        environment.systemPackages = with pkgs; [
          sundtek
        ];
      };
    }
  ];
}
