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
  imports = [
    ./services.deconz.nix
    ./services.home-assistant.nix
  ];

  options.myconfig.smart-home = with lib; {
    enable = mkEnableOption "smart home things";
  };

  config = lib.mkIf config.myconfig.smart-home.enable {
    myconfig.smart-home = {
      home-assistant.enable = true;
      deconz.enable = true;
    };
  };
}
