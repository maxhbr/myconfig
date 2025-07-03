{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.inference-cpp = {
      enable = mkEnableOption "myconfig.ai.inference-cpp";
    };
  };
  config = lib.mkIf config.myconfig.ai.inference-cpp.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          koboldcpp
          llama-cpp
        ];
      }
    ];
  };
}
