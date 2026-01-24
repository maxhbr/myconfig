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
    ai.codex = {
      enable = mkEnableOption "myconfig.ai.codex";
    };
  };
  config = lib.mkIf config.myconfig.ai.codex.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          codex
          (callLib ../fns/sandboxed-app.nix {
            name = "codex";
            pkg = codex;
            readOnlyConfigDirs = [ ".config/codex" ];
          })
        ];
        myconfig.persistence.cache-directories = [
          ".codex"
        ];
      }
    ];
  };
}