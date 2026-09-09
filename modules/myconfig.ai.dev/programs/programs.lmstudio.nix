{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.dev.lmstudio = {
      enable = mkEnableOption "myconfig.ai.dev.lmstudio";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.lmstudio.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [ lmstudio ];
        myconfig.persistence.directories = [ ".config/LM Studio" ];
        myconfig.persistence.cache-directories = [ ".lmstudio" ];
      }
    ];
  };
}
