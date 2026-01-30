{
  config,
  lib,
  pkgs,
  ...
}:

let
  callLib = file: import file { inherit lib pkgs; };
in
{
  options.myconfig = with lib; {
    ai.qwen-code = {
      enable = mkEnableOption "myconfig.ai.qwen-code";
    };
  };
  config = lib.mkIf config.myconfig.ai.qwen-code.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          qwen-code
          (callLib ../fns/sandboxed-app.nix {
            name = "qwen-code";
            pkg = qwen-code;
            readOnlyConfigDirs = [ ".config/qwen-code" ];
          })
        ];
      }
    ];
  };
}
