# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: 
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { river.enable = mkEnableOption "river"; };
  config = (lib.mkIf cfg.river.enable {
    home-manager.sharedModules = [{
      home.file = { ".config/river/init".source = ./river/init; };
      home.packages = with pkgs; [
        river foot
      ];
    }];
    environment = {
      loginShellInit = ''
        [[ -z $DISPLAY && $XDG_VTNR -eq 5 ]] && {
          export XKB_DEFAULT_LAYOUT=de
          export XKB_DEFAULT_VARIANT=neo
          exec river
        }
      '';
    };
  });
}
