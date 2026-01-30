{
  config,
  lib,
  pkgs,
  ...
}:

let
  callLib = file: import file { inherit lib pkgs; };
  opencodeBwrap = callLib ../fns/sandboxed-app.nix {
    name = "opencode";
    pkg = pkgs.opencode;
    readOnlyConfigDirs = [ ".config/opencode" ];
  };
in
{
  options.myconfig = with lib; {
    ai.opencode = {
      enable = mkEnableOption "myconfig.ai.opencode";
    };
  };
  config = lib.mkIf config.myconfig.ai.opencode.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          opencode
          opencodeBwrap
          (writeShellApplication {
            name = "opencode-tmp";
            runtimeInputs = [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe opencodeBwrap} "$@"
            '';
          })
        ];
      }
    ];
  };
}
