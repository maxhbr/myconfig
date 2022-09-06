# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: 
let cfg = config.myconfig;
    dwlPackage = pkgs.callPackage ./wrapper.nix {
      dwl-unwrapped = pkgs.dwl;
      withBaseWrapper = true;
      conf = ./config.h;
      extraPaths= with pkgs; [
        foot
        ## Output configuration
        wlopm wlr-randr kanshi way-displays
        ## Program Launchers
        bemenu fuzzel
        ## Other
        ristate
        wayshot
      ];
      extraSessionCommands = ''
        export XKB_DEFAULT_LAYOUT=de
        export XKB_DEFAULT_VARIANT=neo
      '';
      withGtkWrapper = true;
      extraOptions = [];
    };
in {
  options.myconfig = with lib; { dwl.enable = mkEnableOption "dwl"; };
  config = (lib.mkIf cfg.dwl.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        dwlPackage
      ];
    }];
    services.xserver.displayManager.sessionPackages = [ dwlPackage ];
  });
}
