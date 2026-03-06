# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.myconfig = with lib; {
    ai.github-copilot-cli = {
      enable = mkEnableOption "myconfig.ai.github-copilot-cli";
    };
  };
  config = lib.mkIf config.myconfig.ai.github-copilot-cli.enable {
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
          ghCopilotCliBwrap = callLib ../fns/sandboxed-app.nix {
            name = "github-copilot-cli";
            pkg = pkgs.github-copilot-cli;
            writableDirs = [
              ".config/github-copilot"
            ];
          };
        in
        {
          myconfig.persistence.directories = [
            ".config/.copilot"
            ".local/state/.copilot"
          ];
          home.packages = [
            ghCopilotCliBwrap
            (pkgs.writeShellApplication {
              name = "github-copilot-cli-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe ghCopilotCliBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "github-copilot-cli-worktree";
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
                worktree_name="''${dirname}-github-copilot-cli-''${timestamp}"
                branch_name="github-copilot-cli-''${timestamp}"

                git worktree add -b "''${branch_name}" "../''${worktree_name}" || exit 1
                cd "../''${worktree_name}" && exec ${lib.getExe ghCopilotCliBwrap} "$@"
              '';
            })
          ];
        }
      )
    ];
  };
}
