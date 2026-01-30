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
    ai.claude-code = {
      enable = mkEnableOption "myconfig.ai.claude-code";
    };
  };
  config = lib.mkIf config.myconfig.ai.claude-code.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          claude-code
          (callLib ../fns/sandboxed-app.nix {
            name = "claude-code";
            pkg = claude-code;
            readOnlyConfigDirs = [ ".config/claude-code" ];
          })
        ];
      }
    ];
  };
}
