# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: 
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { qtile.enable = mkEnableOption "qtile"; };
  config = (lib.mkIf cfg.qtile.enable  {
    services.xserver.windowManager.qtile.enable = true;
    environment = {
      loginShellInit = ''
        [[ -z $DISPLAY && $XDG_VTNR -eq 4 ]] && {
          export XKB_DEFAULT_LAYOUT=de
          export XKB_DEFAULT_VARIANT=neo
          exec qtile -b wayland
        }
      '';
    };
  });
}
