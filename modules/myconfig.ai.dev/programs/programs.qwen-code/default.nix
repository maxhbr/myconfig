{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.dev.qwen-code = {
      enable = mkEnableOption "myconfig.ai.dev.qwen-code";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.qwen-code.enable {
    home-manager.sharedModules = [
      {
        home.packages = [ pkgs.qwen-code ];
      }
    ];
  };
}
