# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{lib, pkgs, config, ...}:
{ 
  config = lib.mkIf config.programs.light.enable {
    services.actkbd = {
      enable = lib.mkDefault true;
      bindings = [
        {
          keys = [ 224 ]; # XF86MonBrightnessDown
          events = [ "key" ];
          command = "${pkgs.light}/bin/light  -T 0.7"; # or -U 10";
        }
        {
          keys = [ 225 ]; # XF86MonBrightnessUp
          events = [ "key" ];
          command = "${pkgs.light}/bin/light -A 10";
        }
      ];
    };
  };
}
