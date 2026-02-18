{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.opencode = {
      enable = mkEnableOption "myconfig.ai.opencode";
    };
  };
  config = lib.mkIf config.myconfig.ai.opencode.enable {
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
          opencodeBwrap = callLib ../fns/sandboxed-app.nix {
            name = "opencode";
            pkg = config.programs.opencode.package;
            writableDirs = [
              ".config/opencode"
              ".config/mcp"
            ];
          };
        in
        {
          programs.opencode = {
            enable = true;
            enableMcpIntegration = true;
            web.enable = true;
            ## TODO: overwriting with `lib.mkForce` does not work here
            # settings = {
            #   permission = lib.mkForce {
            #     "bash" = {
            #       "*" = "ask";
            #       "head *" = "allow";
            #       "go build *" = "allow";
            #       "go test *" = "allow";
            #       "go generate *" = "allow";
            #       "go fmt *" = "allow";
            #       "go vet *" = "allow";
            #       "npm run dev *" = "allow";
            #       "npm run build *" = "allow";
            #       "npm run lint *" = "allow";
            #       "npm test *" = "allow";
            #       "ls *" = "allow";
            #       "grep *" = "allow";
            #       "rg *" = "allow";
            #       "find *" = "allow";
            #       "mkdir *" = "allow";
            #     };
            #     "edit" = "ask";
            #   };
            # };
            agents = {
              code-reviewer = ''
                # Code Reviewer Agent

                You are a senior software engineer specializing in code reviews.
                Focus on code quality, security, and maintainability.

                ## Guidelines
                - Review for potential bugs and edge cases
                - Check for security vulnerabilities
                - Ensure code follows best practices
                - Suggest improvements for readability and performance
              '';
            };
            commands = {
              commit = ''
                # Commit Command

                Create a git commit with proper message formatting.
                Usage: /commit [message]
              '';
            };
          };
          home.packages = [
            opencodeBwrap
            (pkgs.writeShellApplication {
              name = "opencode-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe opencodeBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "opencode-worktree";
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
                worktree_name="''${dirname}-opencode-''${timestamp}"
                branch_name="opencode-''${timestamp}"

                git worktree add -b "''${branch_name}" "../''${worktree_name}" || exit 1
                cd "../''${worktree_name}" && exec ${lib.getExe opencodeBwrap} "$@"
              '';
            })
          ];
        }
      )
    ];
  };
}
