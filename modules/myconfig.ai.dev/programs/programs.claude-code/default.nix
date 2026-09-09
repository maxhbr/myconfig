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
  jail-app = callJailLib ../../fns/bubblewrap-app.nix;
  mkWorkmuxWorktree = callLib ../../fns/workmux-worktree.nix;
  mkNonInteractiveClaudeCode = callLib ./noninteractive-startup.nix;

  # The package every tier below runs: upstream `claude-code`, but with a
  # launcher that seeds the onboarding markers into the global config first,
  # so Claude Code starts straight into the session against the proxy given by
  # `$ANTHROPIC_AUTH_TOKEN` + `$ANTHROPIC_BASE_URL` instead of the first-run
  # wizard. See ./noninteractive-startup.nix for the details and the rationale.
  claude-code = mkNonInteractiveClaudeCode { package = pkgs.claude-code; };

  # Credentials for the proxy live ONLY in the host environment (never in the
  # store), so the jailed wrappers — which start from an empty environment —
  # have to forward them explicitly. `try-fwd-env` skips a variable that is
  # unset, so a host without a proxy still starts a working jail.
  anthropicFwdEnv = [
    "ANTHROPIC_AUTH_TOKEN"
    "ANTHROPIC_BASE_URL"
    "ANTHROPIC_API_KEY"
  ];

  # Inside the sandboxes the filesystem is already confined to the working
  # directory (plus the agent's own state), so the per-project trust dialog
  # ("Is this a project you trust?") is pure friction there: it would be asked
  # again on every launch, since the answer is stored in a `~/.claude.json`
  # the sandbox either discards or shares across all projects. Upstream's
  # `CLAUDE_CODE_SANDBOXED` marker is exactly the supported way to say "this
  # workspace is already contained". It is deliberately NOT set for the plain
  # host `claude`, where the trust gate stays in place.
  sandboxedEnv = {
    CLAUDE_CODE_SANDBOXED = "1";
  };

  # Make the `workmux` binary available inside the sandboxes (for the
  # `workmux set-window-status` status hooks and `workmux merge`/`remove` from
  # a worktree pane) whenever workmux is enabled.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.dev.workmux.enable osconfig.myconfig.ai.dev.workmux.package;

  # home-manager uses `useGlobalPkgs`, so `pkgs.claude-code` is the same
  # package `programs.claude-code.package` defaults to. Building the wrappers
  # at the NixOS scope lets us register the workmux named agents from here.
  claudeCodeBwrap = callLib ../../fns/bubblewrap-simple-app.nix {
    name = "claude-code";
    pkg = claude-code;
    writableDirs = [
      ".claude"
      ".config/claude-code"
      ".config/mcp"
    ];
    extraRuntimeInputs = workmuxDevTools;
    # This wrapper gives the agent a `tmpfs` $HOME, so the global config is
    # re-created (and re-seeded by the launcher) on every launch.
    envVars = sandboxedEnv;
  };
  # `agent-bubblewrap-claude` is an alternative to `claudeCodeBwrap` that uses the
  # jail.nix library instead of a hand-rolled bubblewrap wrapper. See
  # `../../fns/bubblewrap-app.nix` for the shared defaults.
  agent-bubblewrap-claude = jail-app {
    name = "agent-bubblewrap-claude";
    pkg = claude-code;
    userDataDirs = [
      ".claude"
      ".config/claude-code"
      ".config/mcp"
    ];
    userDataFiles = [
      ".claude.json"
    ];
    extraDevTools = workmuxDevTools;
    extraFwdEnv = anthropicFwdEnv;
    extraRuntimeEnv = sandboxedEnv;
  };
  # Worktree variant of `agent-bubblewrap-claude`: additionally binds the linked main
  # repository read-only and remounts its shared `.git` read-write, resolved
  # at runtime from the WORKTREE_* env vars set by the workmux launcher.
  agent-bubblewrap-claude-worktree-inner = jail-app {
    name = "agent-bubblewrap-claude-worktree-inner";
    pkg = claude-code;
    userDataDirs = [
      ".claude"
      ".config/claude-code"
      ".config/mcp"
    ];
    userDataFiles = [
      ".claude.json"
    ];
    extraDevTools = workmuxDevTools;
    extraFwdEnv = anthropicFwdEnv;
    extraRuntimeEnv = sandboxedEnv;
    extraReadOnlyEnvPaths = [ "WORKTREE_MAIN_REPO" ];
    extraReadWriteEnvPaths = [ "WORKTREE_GIT_DIR" ];
  };

  claudeCodeWorktree = mkWorkmuxWorktree {
    name = "claude-code-worktree";
    agentName = "claude";
    agentType = "claude";
    innerPkg = claudeCodeBwrap;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
  agentBubblewrapClaudeWorktree = mkWorkmuxWorktree {
    name = "agent-bubblewrap-claude-worktree";
    agentName = "agent-bubblewrap-claude";
    agentType = "claude";
    innerPkg = agent-bubblewrap-claude-worktree-inner;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
in
{
  options.myconfig = with lib; {
    ai.dev.claude-code = {
      enable = mkEnableOption "myconfig.ai.dev.claude-code";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.claude-code.enable {
    myconfig.ai.dev.skills.playwright.enable = lib.mkDefault true;
    # The default `claude-code-worktree` maps to the `claude` named agent; the
    # jailed variant registers its own `agent-bubblewrap-claude` agent.
    myconfig.ai.dev.workmux.agents.claude = claudeCodeWorktree.agent;
    myconfig.ai.dev.workmux.agents.agent-bubblewrap-claude = agentBubblewrapClaudeWorktree.agent;
    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [ ".claude" ];
        programs.mcp.enable = true;
        programs.claude-code = {
          enable = true;
          enableMcpIntegration = true;
          # Plain `claude` on the host gets the same non-interactive launcher
          # as the sandboxed tiers.
          package = claude-code;
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
