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
    myconfig.ai.skills.playwright.enable = lib.mkDefault true;
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          jail,
          ...
        }:
        let
          callLib = file: import file { inherit lib pkgs; };
          callJailLib = file: import file { inherit lib pkgs jail; };
          claudeCodeBwrap = callLib ../fns/sandboxed-app.nix {
            name = "claude-code";
            pkg = config.programs.claude-code.package;
            writableDirs = [
              ".claude"
              ".config/claude-code"
              ".config/mcp"
            ];
          };
          jail-app = callJailLib ../fns/jail-app.nix;
          # `jailed-claude` is an alternative to `claudeCodeBwrap` that uses
          # the jail.nix library instead of a hand-rolled bubblewrap wrapper.
          # See `../fns/jail-app.nix` for the shared defaults.
          jailed-claude = jail-app {
            name = "jailed-claude";
            pkg = config.programs.claude-code.package;
            userDataDirs = [
              ".claude"
              ".config/claude-code"
              ".config/mcp"
            ];
            userDataFiles = [
              ".claude.json"
            ];
          };
        in
        {
          myconfig.persistence.directories = [ ".claude" ];
          programs.mcp.enable = true;
          programs.claude-code = {
            enable = true;
            enableMcpIntegration = true;
          };
          home.packages = with pkgs; [
            claudeCodeBwrap
            jailed-claude
            (pkgs.writeShellApplication {
              name = "claude-code-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe claudeCodeBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "jailed-claude-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe jailed-claude} "$@"
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
            (pkgs.writeShellApplication {
              name = "jailed-claude-worktree";
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
                worktree_name="''${dirname}-claude-''${timestamp}"
                branch_name="claude-''${timestamp}"

                git worktree add -b "''${branch_name}" "../''${worktree_name}" || exit 1
                cd "../''${worktree_name}" && exec ${lib.getExe jailed-claude} "$@"
              '';
            })
          ];
        }
      )
    ];
  };
}
