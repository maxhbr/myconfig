{ config, lib, pkgs, ... }: {
  options.myconfig = with lib; {
    ai.lmstudio = { enable = mkEnableOption "myconfig.ai.lmstudio"; };
  };
  config = lib.mkIf config.myconfig.ai.lmstudio.enable {
    home-manager.sharedModules = [{ home.packages = with pkgs; [ lmstudio ]; }];
  };
}
