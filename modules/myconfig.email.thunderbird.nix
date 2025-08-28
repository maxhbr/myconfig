let 
  hm = { config, lib, pkgs, ...  }:
    {
      config = {
        programs.thunderbird = {
          enable = true;
          profiles = {
            "default" = {
              isDefault = true;
            };
          };
        };
        myconfig.desktop.wayland.launcherCommands = [ "thunderbird" ];
        myconfig.persistence.cache-directories = [
          ".cache/thunderbird"
        ];
        # myconfig.persistence.directories = [
        #   ".mozilla"
        # ];
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
  config = lib.mkIf (cfg.desktop.enable && cfg.email.enable && (builtins.elem "thunderbird" cfg.email.clients)) {
    home-manager.sharedModules = [ hm ];
  };
}