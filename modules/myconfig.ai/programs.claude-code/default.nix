{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.myconfig = with lib; {
    ai.claude-code = {
      enable = mkEnableOption "myconfig.ai.claude-code";
    };
  };
  config = lib.mkIf config.myconfig.ai.claude-code.enable {
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
          claudeCodeBwrap = callLib ../fns/sandboxed-app.nix {
            name = "claude-code";
            pkg = config.programs.claude-code.package;
            writableDirs = [ ".config/claude-code" ];
          };
        in
        {
          programs.claude-code = {
            enable = true;
            enableMcpIntegration = true;
          };
          home.packages = with pkgs; [
            claudeCodeBwrap
            (pkgs.writeShellApplication {
              name = "claude-code-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe claudeCodeBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "claude-code-worktree";
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
                worktree_name="''${dirname}-claude-code-''${timestamp}"
                branch_name="claude-code-''${timestamp}"

                git worktree add -b "''${branch_name}" "../''${worktree_name}" || exit 1
                cd "../''${worktree_name}" && exec ${lib.getExe claudeCodeBwrap} "$@"
              '';
            })
          ];
        }
      )
    ];
  };
}
