{ config, lib, pkgs, ... }: {
  options.myconfig = with lib; {
    ai.coding.enable = mkEnableOption "myconfig.ai.coding ";
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.coding.enable) {
    home-manager.sharedModules =
      [{ home.packages = with pkgs; [ aider-chat code-cursor zed ]; }];
  };
}

