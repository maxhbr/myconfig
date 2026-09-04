{
  config,
  lib,
  pkgs,
  jail,
  ...
}:

let
  osconfig = config;
  callLib = file: import file { inherit lib pkgs; };
  callJailLib =
    file:
    import file {
      inherit
        lib
        pkgs
        jail
        osconfig
        ;
    };
  jail-app = callJailLib ../fns/bubblewrap-app.nix;
  mkWorkmuxWorktree = callLib ../fns/workmux-worktree.nix;

  # Make the `workmux` binary available inside the sandboxes (for the
  # `workmux set-window-status` status hooks and `workmux merge`/`remove` from
  # a worktree pane) whenever workmux is enabled.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.workmux.enable osconfig.myconfig.ai.workmux.package;

  # home-manager uses `useGlobalPkgs`, so `pkgs.claude-code` is the same
  # package `programs.claude-code.package` defaults to. Building the wrappers
  # at the NixOS scope lets us register the workmux named agents from here.
  claudeCodeBwrap = callLib ../fns/bubblewrap-simple-app.nix {
    name = "claude-code";
    pkg = pkgs.claude-code;
    writableDirs = [
      ".claude"
      ".config/claude-code"
      ".config/mcp"
    ];
    extraRuntimeInputs = workmuxDevTools;
  };
  # `agent-bubblewrap-claude` is an alternative to `claudeCodeBwrap` that uses the
  # jail.nix library instead of a hand-rolled bubblewrap wrapper. See
  # `../fns/bubblewrap-app.nix` for the shared defaults.
  agent-bubblewrap-claude = jail-app {
    name = "agent-bubblewrap-claude";
    pkg = pkgs.claude-code;
    userDataDirs = [
      ".claude"
      ".config/claude-code"
      ".config/mcp"
    ];
    userDataFiles = [
      ".claude.json"
    ];
    extraDevTools = workmuxDevTools;
  };
  # Worktree variant of `agent-bubblewrap-claude`: additionally binds the linked main
  # repository read-only and remounts its shared `.git` read-write, resolved
  # at runtime from the WORKTREE_* env vars set by the workmux launcher.
  agent-bubblewrap-claude-worktree-inner = jail-app {
    name = "agent-bubblewrap-claude-worktree-inner";
    pkg = pkgs.claude-code;
    userDataDirs = [
      ".claude"
      ".config/claude-code"
      ".config/mcp"
    ];
    userDataFiles = [
      ".claude.json"
    ];
    extraDevTools = workmuxDevTools;
    extraReadOnlyEnvPaths = [ "WORKTREE_MAIN_REPO" ];
    extraReadWriteEnvPaths = [ "WORKTREE_GIT_DIR" ];
  };

  claudeCodeWorktree = mkWorkmuxWorktree {
    name = "claude-code-worktree";
    agentName = "claude";
    agentType = "claude";
    innerPkg = claudeCodeBwrap;
    workmuxPkg = osconfig.myconfig.ai.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
  agentBubblewrapClaudeWorktree = mkWorkmuxWorktree {
    name = "agent-bubblewrap-claude-worktree";
    agentName = "agent-bubblewrap-claude";
    agentType = "claude";
    innerPkg = agent-bubblewrap-claude-worktree-inner;
    workmuxPkg = osconfig.myconfig.ai.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
in
{
  options.myconfig = with lib; {
    ai.claude-code = {
      enable = mkEnableOption "myconfig.ai.claude-code";
    };
  };
  config = lib.mkIf config.myconfig.ai.claude-code.enable {
    myconfig.ai.skills.playwright.enable = lib.mkDefault true;
    # The default `claude-code-worktree` maps to the `claude` named agent; the
    # jailed variant registers its own `agent-bubblewrap-claude` agent.
    myconfig.ai.workmux.agents.claude = claudeCodeWorktree.agent;
    myconfig.ai.workmux.agents.agent-bubblewrap-claude = agentBubblewrapClaudeWorktree.agent;
    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [ ".claude" ];
        programs.mcp.enable = true;
        programs.claude-code = {
          enable = true;
          enableMcpIntegration = true;
        };
        home.packages = [
          claudeCodeBwrap
          agent-bubblewrap-claude
          (pkgs.writeShellApplication {
            name = "claude-code-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe claudeCodeBwrap} "$@"
            '';
          })
          (pkgs.writeShellApplication {
            name = "agent-bubblewrap-claude-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe agent-bubblewrap-claude} "$@"
            '';
          })
          claudeCodeWorktree.wrapper
          agentBubblewrapClaudeWorktree.wrapper
        ];
      }
    ];
  };
}
