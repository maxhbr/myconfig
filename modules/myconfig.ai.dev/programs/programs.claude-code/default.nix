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

  # --- mysbx integration (../../mysbx) -------------------------------------
  #
  # The same three-piece wiring pi/opencode/rtk use
  # (../../programs/programs.{pi-coding-agent,opencode,rtk}), plus the
  # trust gate:
  #
  #   1. the CLI on the sandbox PATH -> `myconfig.ai.dev.mysbx.extraTools`
  #      (the podman backends get it via their own `agentPackagesByFlag`
  #      default in ../../mysbx/podman.nix, keyed on this module's
  #      `enable`).
  #   2. the *configuration* visible inside the sandbox -> read-only mounts
  #      from the shared `mkSandboxConfig` helper
  #      (../../mysbx/nix/sandbox-config.nix), one per deployed child of
  #      `.claude` (`childSubtrees`), so the tmpfs `.claude` stays writable
  #      and the state-dirs below it are not covered. `.claude` holds everything
  #      `programs.claude-code` renders (settings.json, hooks/, agents/,
  #      commands/, skills/) because `configDir` keeps its upstream default
  #      `~/.claude`. Skills flow through `home.file` in the
  #      skills-are-attrs default path, and via the agent-skills framework's
  #      `extraFiles` when that manages them — the one exception to
  #      "home.file is the source of truth", same as pi/opencode.
  #   3. the trust dialog -> `CLAUDE_CODE_SANDBOXED=1` in the generated
  #      layer's `[env]`, the supported marker that the workspace is
  #      already contained (same purpose as `sandboxedEnv` above; an
  #      `[env]` entry because mysbx `--clearenv` drops everything not
  #      explicitly set).
  #
  # NOT mounted: `~/.claude.json` (the global config file — besides the
  # onboarding markers it accumulates host project paths and session state;
  # the `claude-code` launcher above re-seeds a fresh one inside) and
  # `~/.claude/plugins` (marketplace config managed by Claude Code itself).
  # Credentials come from the environment, never a mounted file.
  #
  # The state that survives a run (`state-dirs`, ../../mysbx/docs/design/
  # config.md D15): `projects/` holds the session transcripts, `todos/` the
  # task lists, `statsig/` the client telemetry cache and `shell-snapshots/`
  # the shell snapshot files — everything under `~/.claude` EXCEPT the
  # read-only config mounts above and `~/.claude.json` (a state entry is
  # always a directory, so that FILE cannot be persisted this way; the
  # launcher re-seeds its onboarding markers on every launch).
  #
  # Home Manager uses `useGlobalPkgs`, so the shared helper is imported with
  # the NixOS scope's `pkgs`, like in ../../programs/programs.opencode.
  sandboxConfigLib = import ../../mysbx/nix/sandbox-config.nix {
    inherit lib;
    runCommand = pkgs.runCommand;
  };

  # Read from the user whose config layer mysbx generates, like
  # `hmHomeFile` in ../../programs/programs.opencode (laziness and
  # non-circularity arguments verbatim: the value feeds
  # `…mysbx.config.mounts`, rendered into `home.file` keyed below
  # `.config/mysbx`, a prefix no subtree below matches).
  hmHomeFile = config.home-manager.users.mhuber.home.file;
  hmHomeDirectory = config.home-manager.users.mhuber.home.homeDirectory;

  # MCP server definitions need NO separate mount: unlike opencode (which
  # reads the shared `~/.config/mcp/mcp.json`), Claude Code consumes them
  # through the generated `hm` plugin the home-manager module deploys INSIDE
  # `~/.claude/skills/` (its `.mcp.json`) — covered by the `.claude` mount.

  # Skills managed by the agent-skills framework are rsynced at ACTIVATION
  # time, not linked from `home.file` — pass the `claude` target's bundle
  # through `extraFiles` so the sandbox tree matches the host (same pattern
  # as `mysbxPiSkillsBundle`/`mysbxOpencodeSkillsBundle`).
  mysbxClaudeSkillsBundle =
    let
      hmSkills = config.home-manager.users.mhuber.programs.agent-skills or null;
    in
    if hmSkills == null || !hmSkills.enable then
      { }
    else if !(hmSkills.targetBundlePaths ? claude) then
      { }
    else
      { ".claude/skills" = hmSkills.targetBundlePaths.claude; };

  mysbxClaudeMounts =
    (sandboxConfigLib.mkSandboxConfig {
      homeFile = hmHomeFile;
      # The deployed children of `.claude`, not `.claude` itself: a
      # read-only mount of the whole dir would cover the state-dirs below.
      subtrees = sandboxConfigLib.childSubtrees {
        homeFile = hmHomeFile;
        homeDirectory = hmHomeDirectory;
        extraFiles = mysbxClaudeSkillsBundle;
        dir = ".claude";
      };
      name = "claude-code-sandbox-config";
      homeDirectory = hmHomeDirectory;
      extraFiles = mysbxClaudeSkillsBundle;
    }).mounts;

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
    # The default `claude-code-worktree` maps to the `claude` named agent; the
    # jailed variant registers its own `agent-bubblewrap-claude` agent.
    myconfig.ai.dev.workmux.agents.claude = claudeCodeWorktree.agent;
    myconfig.ai.dev.workmux.agents.agent-bubblewrap-claude = agentBubblewrapClaudeWorktree.agent;

    # mysbx tier integration (see `mysbxClaudeMounts` above). Gated on mysbx
    # being enabled too — the two features are independent, and the mounts
    # would otherwise be generated for a host that has no mysbx config file
    # to carry them. `CLAUDE_CODE_SANDBOXED` is set in the generated layer's
    # `[env]` (NOT `forwardedEnvVars`: it is a constant marker, not a host
    # credential), reaching every backend like the pi `PI_JAIL_MARKER` entry.
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ claude-code ];
      config.mounts = mysbxClaudeMounts;
      config.env.CLAUDE_CODE_SANDBOXED = "1";
      config.stateDirs = [
        ".claude/projects"
        ".claude/todos"
        ".claude/statsig"
        ".claude/shell-snapshots"
      ];
    };

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
