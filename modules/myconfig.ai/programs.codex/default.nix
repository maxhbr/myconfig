{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.myconfig = with lib; {
    ai.codex = {
      enable = mkEnableOption "myconfig.ai.codex";
    };
  };
  config = lib.mkIf config.myconfig.ai.codex.enable {
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          callLib = file: import file { inherit lib pkgs; };
          codexBwrap = callLib ../fns/sandboxed-app.nix {
            name = "codex";
            pkg = config.programs.codex.package;
            writableDirs = [ ".config/codex" ];
          };
        in
        {
          programs.codex = {
            enable = true;
            enableMcpIntegration = true;
          };
          home.packages = with pkgs; [
            codexBwrap
            (pkgs.writeShellApplication {
              name = "codex-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe codexBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "codex-worktree";
              runtimeInputs = with pkgs; [
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
                worktree_name="''${dirname}-codex-''${timestamp}"
                branch_name="codex-''${timestamp}"

                git worktree add -b "''${branch_name}" "../''${worktree_name}" || exit 1
                cd "../''${worktree_name}" && exec ${lib.getExe codexBwrap} "$@"
              '';
            })
          ];
        }
      )
    ];
  };
}
