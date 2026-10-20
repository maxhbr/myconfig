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
  # here and merged into `myconfig.ai.dev.workmux.agents`.
  # Make the `workmux` binary available inside the sandbox (for the
  # `workmux set-window-status` status hooks and `workmux merge`/`remove` from
  # a worktree pane) whenever workmux is enabled.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.dev.workmux.enable osconfig.myconfig.ai.dev.workmux.package;

  # --- mysbx integration (../../mysbx) -------------------------------------
  #
  # The same three-piece wiring pi/opencode/claude-code use
  # (../../programs/programs.{pi-coding-agent,opencode,claude-code}):
  #
  #   1. the `codex` binary on the sandbox PATH ->
  #      `myconfig.ai.dev.mysbx.extraTools` (the podman backends get it via
  #      their own `agentPackagesByFlag` default in ../../mysbx/podman.nix,
  #      keyed on this module's `enable`).
  #   2. codex's *configuration* visible inside the sandbox -> read-only
  #      mounts from the shared `mkSandboxConfig` helper
  #      (../../mysbx/nix/sandbox-config.nix), one per deployed child of
  #      `~/.codex` (`childSubtrees`), so the tmpfs `.codex` stays writable
  #      and the state-dirs below it are not covered. `~/.codex` (Home Manager's
  #      `configDir` default: `home.preferXdgDirectories` is off, so it is
  #      NOT `~/.config/codex`) holds everything `programs.codex` renders —
  #      config.toml, hooks.json, AGENTS.md, skills, rules, profiles.
  #      MCP server definitions need NO separate mount: codex consumes them
  #      through `mcp_servers` the home-manager module writes INTO
  #      `~/.codex/config.toml`, not the shared `~/.config/mcp/mcp.json`
  #      only opencode/qwen read. Skills flow through `home.file` in
  #      the skills-attrs default path, and via the agent-skills framework's
  #      `extraFiles` when that manages them — the one exception to
  #      "home.file is the source of truth", same as the other agents.
  #   3. codex's *state* persists across runs -> `state-dirs` entries
  #      (`…mysbx.config.stateDirs`, ../../mysbx/docs/design/config.md D15),
  #      backed by `<repo>.mysbx/state/` in the sidecar.
  #
  # NOT mounted: `~/.codex/auth.json` (the OAuth/API credential) and the
  # session/materialized-history trees — a sandboxed session starts
  # unauthenticated (it talks to the local LiteLLM / llama.cpp providers,
  # which need no credentials) and writes its state to the sidecar instead.
  #
  # The persisted subtrees of `~/.codex` (state-dirs): `sessions/` (the
  # rollout transcripts, `session_index.jsonl`), `archived_sessions/`,
  # `file_history/` (the file-edit snapshots the `/rewind` UI browses) and
  # `log/` (the `log/lifecycle.log` client log) — everything the CLI writes
  # at runtime EXCEPT `auth.json` (never persisted: credentials must not
  # reach the sidecar, and a fresh session re-authenticates from the
  # environment) and the config files above (read-only mounts, not state).
  # `.agents/skills` is NOT a state entry: it is the shared handcrafted-skills
  # tree this repo deploys, mounted read-only like pi does.
  #
  # The shared mysbx sandbox-config helper (the dereferenced-store-tree
  # machinery, bd myconfig-ooh); consumed like the entry scripts consume
  # ../../mysbx/nix/mux-entry-lib.nix.
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

  # Skills managed by the agent-skills framework are rsynced at ACTIVATION
  # time, not linked from `home.file` — pass the `codex` target's bundle
  # through `extraFiles` so the sandbox tree matches the host (same pattern
  # as `mysbxPiSkillsBundle`/`mysbxOpencodeSkillsBundle`).
  mysbxCodexSkillsBundle =
    let
      hmSkills = config.home-manager.users.mhuber.programs.agent-skills or null;
    in
    if hmSkills == null || !hmSkills.enable then
      { }
    else if !(hmSkills.targetBundlePaths ? codex) then
      { }
    else
      { ".codex/skills" = hmSkills.targetBundlePaths.codex; };

  mysbxCodexMounts =
    (sandboxConfigLib.mkSandboxConfig {
      homeFile = hmHomeFile;
      # The deployed children of `.codex`, not `.codex` itself: a
      # read-only mount of the whole dir would cover the state-dirs below.
      subtrees =
        sandboxConfigLib.childSubtrees {
          homeFile = hmHomeFile;
          homeDirectory = hmHomeDirectory;
          extraFiles = mysbxCodexSkillsBundle;
          dir = ".codex";
        }
        # The shared handcrafted-skills tree, discovered by codex (and pi)
        # from `~/.agents/skills/`. Mounted only when populated (same
        # `piHasHandcraftedSkills` gate as
        # ../../programs/programs.pi-coding-agent) AND only when the pi
        # module has not already mounted the very same tree: pi's mysbx
        # integration binds `~/.agents/skills` too, and while the CLI
        # treats equal mount dests as a harmless shadowing re-bind, a
        # single writer keeps the generated layer (and its checks) clean.
        ++ lib.optional (
          !(osconfig.myconfig.ai.dev.pi-coding-agent.enable or false)
          && (
            (config.myconfig.ai.dev.skills.handcrafted or { }) != { }
            || (config.myconfig.ai.dev.skills.enable or false)
          )
        ) ".agents/skills";
      name = "codex-sandbox-config";
      homeDirectory = hmHomeDirectory;
      extraFiles = mysbxCodexSkillsBundle;
    }).mounts;

  codexBwrap = callLib ../../fns/bubblewrap-simple-app.nix {
    name = "codex";
    pkg = pkgs.codex;
    writableDirs = [ ".config/codex" ];
    extraRuntimeInputs = workmuxDevTools;
  };
  mkWorkmuxWorktree = callLib ../../fns/workmux-worktree.nix;
  # `codex-worktree` is now a thin workmux wrapper: it requires tmux and runs
  # `workmux add --agent codex`, which launches the codex sandbox in the new
  # worktree pane (the launcher exposes the worktree's shared git dir to the
  # sandbox via the WORKTREE_* env vars supported by `bubblewrap-simple-app.nix`).
  codexWorktree = mkWorkmuxWorktree {
    name = "codex-worktree";
    agentName = "codex";
    agentType = "codex";
    innerPkg = codexBwrap;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
in
{
  options.myconfig = with lib; {
    ai.dev.codex = {
      enable = mkEnableOption "myconfig.ai.dev.codex";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.codex.enable {
    myconfig.ai.dev.workmux.agents.codex = codexWorktree.agent;

    # mysbx tier integration (see `mysbxCodexMounts` above). Gated on mysbx
    # being enabled too — the two features are independent, and the mounts
    # would otherwise be generated for a host that has no mysbx config file
    # to carry them. The state dirs replace what the bubblewrap jail tiers
    # bind rw (`writableDirs`): codex's sessions and history persist per
    # repository in the sidecar (../../mysbx/docs/design/config.md D15)
    # instead of dying with the tmpfs home.
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ pkgs.codex ];
      config.mounts = mysbxCodexMounts;
      config.stateDirs = [
        ".codex/sessions"
        ".codex/archived_sessions"
        ".codex/file_history"
        ".codex/log"
      ];
    };

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
