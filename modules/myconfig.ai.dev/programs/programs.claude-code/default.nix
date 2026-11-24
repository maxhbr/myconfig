{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  osconfig = config;
  callLib = file: import file { inherit lib pkgs; };
  mkWorkmuxWorktree = callLib ../../fns/workmux-worktree.nix;
  mkNonInteractiveClaudeCode = callLib ./noninteractive-startup.nix;

  # The package the wrappers below run: upstream `claude-code`, but with a
  # launcher that seeds the onboarding markers into the global config first,
  # so Claude Code starts straight into the session against the proxy given by
  # `$ANTHROPIC_AUTH_TOKEN` + `$ANTHROPIC_BASE_URL` instead of the first-run
  # wizard. See ./noninteractive-startup.nix for the details and the rationale.
  claude-code = mkNonInteractiveClaudeCode { package = pkgs.claude-code; };

  # --- mysbx integration (../../../../mysbx) -------------------------------------
  #
  # The same three-piece wiring pi/opencode/rtk use
  # (../../programs/programs.{pi-coding-agent,opencode,rtk}), plus the
  # trust gate:
  #
  #   1. the CLI on the sandbox PATH -> `myconfig.ai.dev.mysbx.extraTools`
  #      (the podman backends get it via their own `agentPackagesByFlag`
  #      default in ../../../../mysbx/podman.nix, keyed on this module's
  #      `enable`).
  #   2. the *configuration* visible inside the sandbox -> read-only mounts
  #      from the shared `mkSandboxConfig` helper
  #      (../../../mysbx flake lib (nix/sandbox-config.nix)), one per deployed child of
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
  #      already contained (an `[env]` entry because mysbx `--clearenv`
  #      drops everything not explicitly set).
  #
  # NOT mounted: `~/.claude.json` (the global config file — besides the
  # onboarding markers it accumulates host project paths and session state;
  # the `claude-code` launcher above re-seeds a fresh one inside) and
  # `~/.claude/plugins` (marketplace config managed by Claude Code itself).
  # Credentials come from the environment, never a mounted file.
  #
  # The state that survives a run (`state-dirs`, ../../../../mysbx/docs/design/
  # config.md D15): `projects/` holds the session transcripts, `todos/` the
  # task lists, `statsig/` the client telemetry cache and `shell-snapshots/`
  # the shell snapshot files — everything under `~/.claude` EXCEPT the
  # read-only config mounts above and `~/.claude.json` (a state entry is
  # always a directory, so that FILE cannot be persisted this way; the
  # launcher re-seeds its onboarding markers on every launch).
  #
  # Home Manager uses `useGlobalPkgs`, so the shared helper is imported with
  # the NixOS scope's `pkgs`, like in ../../programs/programs.opencode.
  sandboxConfigLib = inputs.mysbx.lib.sandboxConfigLib {
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

  claudeCodeWorktree = mkWorkmuxWorktree {
    name = "claude-code-worktree";
    agentName = "claude";
    agentType = "claude";
    innerPkg = claude-code;
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
    # The `claude-code-worktree` wrapper maps to the `claude` named agent.
    myconfig.ai.dev.workmux.agents.claude = claudeCodeWorktree.agent;

    # mysbx tier integration (see `mysbxClaudeMounts` above). Gated on mysbx
    # being enabled too — the two features are independent, and the mounts
    # would otherwise be generated for a host that has no mysbx config file
    # to carry them. `CLAUDE_CODE_SANDBOXED` is set in the generated layer's
    # `[env]` (NOT `forwardedEnvVars`: it is a constant marker, not a host
    # credential), reaching every backend like the pi `PI_JAIL_MARKER` entry.
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ claude-code ];
      # D32 agent registry entry: the kind is `claude` (the binary the
      # workmux tier above and the CLI both call), not the package name.
      agents.claude.command = lib.mkDefault "claude";
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
          package = claude-code;
        };
        home.packages = [ claudeCodeWorktree.wrapper ];
      }
    ];
  };
}
