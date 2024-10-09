# Copyright 2022 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.printing.enable = mkEnableOption "myconfig.desktop.printing" // {
      default = false;
      example = false;
    };
  };
  config = (lib.mkIf ( config.myconfig.desktop.enable &&  config.myconfig.desktop.printing.enable ) {
    services.printing = {
      enable = true;
      drivers = with pkgs; [ gutenprint hplipWithPlugin ];
      # add hp-printer with:
      # $ nix run nixpkgs.hplipWithPlugin -c sudo hp-setup
    };
    programs.system-config-printer.enable = config.services.printing.enable;
  });
}
