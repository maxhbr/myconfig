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
    writableDirs = [ ".config/opencode" ];
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
          (writeShellApplication {
            name = "opencode-worktree";
            runtimeInputs = [
              git
              coreutils
            ];
            text = ''
              if [ ! -d .git ]; then
                echo "Error: Not in a git repository root"
                exit 1
              fi

              timestamp=$(date +%s)
              dirname=$(basename "$(pwd)")
              worktree_name="''${dirname}-opencode-''${timestamp}"
              branch_name="opencode-''${timestamp}"

              git checkout -b "''${branch_name}" || exit 1
              git worktree add "../''${worktree_name}" "''${branch_name}" || exit 1
              cd "../''${worktree_name}" && exec ${lib.getExe opencodeBwrap} "$@"
            '';
          })
        ];
      }
    ];
  };
}
