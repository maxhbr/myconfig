# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: 
let cfg = config.myconfig;
    otherWlPackages = with pkgs; [
        foot
        # https://github.com/riverwm/river/wiki/Recommended-Software
        ## Output configuration
        wlopm wlr-randr kanshi way-displays
        ## Program Launchers
        bemenu fuzzel
        ## Other
        ristate
        wayshot
      ];
in {
  options.myconfig = with lib; { river.enable = mkEnableOption "river"; };
  config = (lib.mkIf cfg.river.enable {
    home-manager.sharedModules = [{
      home.file = { ".config/river/init".source = ./river/init; };
      home.packages = with pkgs; [
        river
      ];
    }];
    environment = {
      loginShellInit = ''
        [[ -z $DISPLAY && $XDG_VTNR -eq 5 ]] && {
          export XKB_DEFAULT_LAYOUT=de
          export XKB_DEFAULT_VARIANT=neo
          export PATH=$PATH:${lib.makeBinPath otherWlPackages}
          while true; do
            river
          done
        }
      '';
    };
  });
}
