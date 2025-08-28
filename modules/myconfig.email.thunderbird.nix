let 
  hm = { config, lib, pkgs, ...  }:
    {
      config = {
        programs.thunderbird.enable = true;
        myconfig.desktop.wayland.launcherCommands = [ "thunderbird" ];
      };
    };
in 
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = lib.mkIf (cfg.email.enable && (builtins.elem "thunderbird" cfg.email.clients)) {
    home-manager.sharedModules = [ hm ];
  };
}