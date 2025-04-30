{ config, lib, pkgs, ... }:
{
  options.myconfig = with lib; {
    ai.koboldcpp = {
      enable = mkEnableOption "myconfig.ai.koboldcpp";
    };
  };
  config = lib.mkIf config.myconfig.ai.koboldcpp.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        koboldcpp
      ];
    }];
  };
}