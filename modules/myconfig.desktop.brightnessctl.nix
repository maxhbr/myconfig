# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.myconfig.desktop.brightnessctl;
in
{
  options.myconfig.desktop.brightnessctl = with lib; {
    enable = mkEnableOption "myconfig.desktop.brightnessctl";
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.brightnessctl ];
    services.actkbd = {
      enable = lib.mkDefault true;
      bindings = [
        {
          keys = [ 224 ]; # XF86MonBrightnessDown
          events = [ "key" ];
          command = "${pkgs.brightnessctl}/bin/brightnessctl set 10%-";
        }
        {
          keys = [ 225 ]; # XF86MonBrightnessUp
          events = [ "key" ];
          command = "${pkgs.brightnessctl}/bin/brightnessctl set +10%";
        }
      ];
    };
  };
}
