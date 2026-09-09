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
    ai.dev.qwen-code = {
      enable = mkEnableOption "myconfig.ai.dev.qwen-code";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.qwen-code.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          qwen-code
          (callLib ../../fns/bubblewrap-simple-app.nix {
            name = "qwen-code";
            pkg = qwen-code;
            readOnlyConfigDirs = [
              ".config/qwen-code"
              ".config/mcp"
            ];
          })
        ];
      }
    ];
  };
}
