{
  config,
  lib,
  pkgs,
  ...
}:

let
  osconfig = config;
  callLib = file: import file { inherit lib pkgs; };
  # Built at the NixOS scope (home-manager uses `useGlobalPkgs`, so
  # `pkgs.codex` is the same package `programs.codex.package` defaults to) so
  # the workmux launcher and its named-agent registration can be assembled
  # here and merged into `myconfig.ai.workmux.agents`.
  codexBwrap = callLib ../fns/sandboxed-app.nix {
    name = "codex";
    pkg = pkgs.codex;
    writableDirs = [ ".config/codex" ];
  };
  mkWorkmuxWorktree = callLib ../fns/workmux-worktree.nix;
  # `codex-worktree` is now a thin workmux wrapper: it requires tmux and runs
  # `workmux add --agent codex`, which launches the codex sandbox in the new
  # worktree pane (the launcher exposes the worktree's shared git dir to the
  # sandbox via the WORKTREE_* env vars supported by `sandboxed-app.nix`).
  codexWorktree = mkWorkmuxWorktree {
    name = "codex-worktree";
    agentName = "codex";
    agentType = "codex";
    innerPkg = codexBwrap;
    workmuxPkg = osconfig.myconfig.ai.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
in
{
  options.myconfig = with lib; {
    ai.codex = {
      enable = mkEnableOption "myconfig.ai.codex";
    };
  };
  config = lib.mkIf config.myconfig.ai.codex.enable {
    myconfig.ai.workmux.agents.codex = codexWorktree.agent;
    home-manager.sharedModules = [
      {
        programs.codex = {
          enable = true;
          enableMcpIntegration = true;
        };
        home.packages = [
          codexBwrap
          (pkgs.writeShellApplication {
            name = "codex-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe codexBwrap} "$@"
            '';
          })
          codexWorktree.wrapper
        ];
      }
    ];
  };
}
