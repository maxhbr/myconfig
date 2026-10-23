# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Declarative agent skills management, backed by the vendored
# `agent-skills-nix` library (see `vendor/agent-skills-nix/`). When enabled,
# skills are discovered from flake-pinned sources (mattpocock/skills by
# default) and synced into the config directories of the agent harnesses that
# are enabled on this host via home-manager. The locally-defined
# ("handcrafted") skills registered by the individual skill modules are
# routed through the same framework (as `handcrafted-<name>` sources plus
# explicit skills), so every skills directory has exactly one owner — the
# framework sync. Only when `myconfig.ai.dev.skills.enable` is false do the
# handcrafted skills fall back to direct home-manager deployment.
#
# The upstream home-manager module exposes `programs.agent-skills.*`; this
# wrapper provides a `myconfig.ai.dev.skills.*` surface with sensible defaults
# while still allowing full pass-through configuration. Anything set in
# `myconfig.ai.dev.skills.{sources,skills,targets,excludePatterns}` is merged on
# top of the defaults, so hosts can add sources, narrow the skill selection,
# or retarget individual harnesses without touching this file.
{
  config,
  lib,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.skills;

  # The upstream home-manager module. It is a function of `{ inputs, lib }`
  # (captured here with nixpkgs `lib` and the flake `inputs`) returning the
  # actual HM module. `inputs` is also resolvable inside the module via
  # home-manager's `extraSpecialArgs`, which is how `source.input` references
  # such as `"mattpocock-skills"` are resolved.
  agentSkillsHmModule =
    import ../../../vendor/agent-skills-nix/modules/home-manager/agent-skills.nix
      { inherit inputs lib; };

  # Map myconfig.ai harness enable flags to agent-skills target names (see
  # `defaultTargets` in vendor/agent-skills-nix/lib/default.nix). Only enabled
  # harnesses get a target here; the rest keep the upstream opt-in default of
  # `enable = false`. Using `mkOverride 900` keeps these below plain user
  # overrides (priority 100) but above the upstream `mkDefault` (1000), so a
  # host can still force a target on/off via `myconfig.ai.dev.skills.targets`.
  #
  # pi-coding-agent maps to the `agents` target (`~/.agents/skills`, the
  # Agent Skills spec location pi discovers) instead of the `pi` target
  # (`~/.pi/agent/skills`): every sandbox tier stages or binds exactly
  # `~/.agents/skills`, and a second synced copy under `~/.pi/agent/skills`
  # would make pi discover every skill twice.
  harnessTargetDefaults =
    let
      when = cond: name: lib.optionalAttrs cond { ${name}.enable = lib.mkOverride 900 true; };
    in
    lib.optionalAttrs (config.myconfig.ai.dev ? opencode) (
      when (config.myconfig.ai.dev.opencode.enable or false) "opencode"
    )
    // lib.optionalAttrs (config.myconfig.ai.dev ? codex) (
      when (config.myconfig.ai.dev.codex.enable or false) "codex"
    )
    // lib.optionalAttrs (config.myconfig.ai.dev ? claude-code) (
      when (config.myconfig.ai.dev.claude-code.enable or false) "claude"
    )
    // lib.optionalAttrs (config.myconfig.ai.dev ? pi-coding-agent) (
      when (config.myconfig.ai.dev.pi-coding-agent.enable or false) "agents"
    );

  # Whether each agent harness is enabled on this host. Used to gate the
  # handcrafted-skill apply block below so a host without, say, pi-coding-agent
  # does not get `~/.agents/skills/` entries it will never load.
  harnessEnabled = {
    opencode = config.myconfig.ai.dev.opencode.enable or false;
    claude-code = config.myconfig.ai.dev.claude-code.enable or false;
    codex = config.myconfig.ai.dev.codex.enable or false;
    pi = config.myconfig.ai.dev.pi-coding-agent.enable or false;
  };

  # Registry of locally-defined ("handcrafted") skills, populated by the
  # individual skill modules (commit, playwright-cli, grafana-core). Each
  # entry maps a skill name to its source directory (the dir containing
  # `SKILL.md`). The apply block below deploys every entry to each enabled
  # agent harness — independently of the agent-skills sync framework, which
  # is opt-in via `myconfig.ai.dev.skills.enable`.
  handcrafted = config.myconfig.ai.dev.skills.handcrafted;
  # The same registry reshaped for the agent-skills framework: one source
  # per handcrafted skill (its directory is the source root), plus one
  # explicit skill selecting that root, so the deployed skill keeps its name
  # (`<name>`) in every target directory. The `handcrafted-` prefix namespaces
  # these sources away from host-configured ones (`cfg.sources`).
  handcraftedSources = lib.mapAttrs' (
    name: dir: lib.nameValuePair "handcrafted-${name}" { path = dir; }
  ) handcrafted;
  handcraftedExplicit = lib.mapAttrs (name: _: {
    from = "handcrafted-${name}";
    path = ".";
  }) handcrafted;
  # Registry of locally-defined pi prompt templates, populated by individual
  # skill modules when a workflow should also be reachable as a `/`-command
  # prompt template. Deployed to `~/.pi/agent/prompts/<name>.md` for every host
  # with pi-coding-agent enabled.
  handcraftedPrompts = config.myconfig.ai.dev.skills.handcraftedPrompts;
  # Registry of locally-defined pi sub-agent definitions, populated by
  # individual skill modules (research). Each entry maps an agent name to its
  # source `.md` file. Deployed to `~/.pi/agent/agents/<name>.md`, where the
  # pi subagent extension discovers agents. Unlike the upstream sample
  # agents (whose `model:` line is stripped before deployment), these are
  # deployed verbatim — preserving `model:` frontmatter so a handcrafted
  # agent can pin a fast/cheap model for its sub-agents. Only applied on
  # hosts with pi-coding-agent enabled.
  handcraftedAgents = config.myconfig.ai.dev.skills.handcraftedAgents;
in
{
  imports = [
    ./bro.nix
    ./commit.nix
    ./grafana-core.nix
    ./implement-and-review-and-commit.nix
    ./implement-and-review-and-commit-until-done.nix
    ./merge-worktree.nix
    ./playwright-cli.nix
    ./research.nix
    ./review.nix
    ./simple-english.nix
    ./workmux.nix
  ];

  options.myconfig.ai.dev.skills = with lib; {
    enable = mkEnableOption "agent skills management via home-manager (vendor/agent-skills-nix)";

    sources = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Additional skill sources merged on top of the default `mattpocock`
        source. Each entry follows the upstream `sourceType` shape, e.g.:

        ```nix
        myconfig.ai.dev.skills.sources.anthropic = {
          input = "anthropic-skills"; # flake input name
          subdir = "skills";
        };
        ```

        Override the default source by setting `sources.mattpocock`.
      '';
    };

    skills = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Skill selection merged on top of the default
        `{ enableAll = [ "mattpocock" ]; }`. Supports the upstream
        `skills.{enable, enableAll, explicit}` options. Note that the default
        `mattpocock` source is filtered to the recommended core categories
        (`engineering`, `productivity`); the non-promoted `misc`/`personal`,
        unfinished `in-progress`, and `deprecated` buckets are not discovered.
        For example, to opt into a specific subset instead of all discovered
        mattpocock skills:

        ```nix
        myconfig.ai.dev.skills.skills = {
          enableAll = lib.mkForce false;
          enable = [ "engineering/triage" ];
        };
        ```
      '';
    };

    targets = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Per-target overrides merged on top of the harness-derived defaults.
        By default a target is enabled for every agent harness that is
        enabled on this host (opencode, codex, claude, pi). Set entries here
        to force a target on/off or change its `dest`/`structure`.
      '';
    };

    excludePatterns = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        rsync exclude patterns forwarded to `programs.agent-skills`.
        Defaults to the upstream `defaultExcludePatterns` (`[ "/.system" ]`).
      '';
    };

    handcrafted = mkOption {
      type = types.attrsOf (types.either types.path types.str);
      default = { };
      description = ''
        Registry of locally-defined ("handcrafted") skills, keyed by skill
        name and valued by the source directory (a Nix path or a store-path
        string) that contains the skill's `SKILL.md`. Populated by the
        individual skill modules (`commit.nix`, `grafana-core.nix`, …).
        When `myconfig.ai.dev.skills.enable` is true, every entry is routed
        through the agent-skills framework (source `handcrafted-<name>` plus
        an explicit skill) and lands in each enabled harness's synced skills
        directory. When the framework is disabled, the registry is deployed
        directly instead: opencode, claude-code and codex via their
        `programs.<harness>.skills` option, and pi-coding-agent (which has no
        such option) to the shared `~/.agents/skills/` discovery directory.
      '';
    };

    handcraftedPrompts = mkOption {
      type = types.attrsOf (types.either types.path types.str);
      default = { };
      description = ''
        Registry of locally-defined pi prompt templates, keyed by template
        name and valued by the source `.md` file (a Nix path or a store-path
        string). Deployed to `~/.pi/agent/prompts/<name>.md`, where pi
        discovers global prompt templates (invoked via `/<name>` in the
        editor). Populated by individual skill modules alongside their
        `handcrafted` skill entry when the same workflow should also be
        reachable as a `/`-command prompt template. Only applied on hosts
        with pi-coding-agent enabled.
      '';
    };

    handcraftedAgents = mkOption {
      type = types.attrsOf (types.either types.path types.str);
      default = { };
      description = ''
        Registry of locally-defined pi sub-agent definitions, keyed by agent
        name and valued by the source `.md` file (a Nix path or a store-path
        string). Deployed to `~/.pi/agent/agents/<name>.md`, where the pi
        subagent extension discovers agents. Unlike the upstream sample
        agents (whose `model:` frontmatter is stripped before deployment),
        these are deployed verbatim — preserving `model:` lines so a
        handcrafted agent can pin a fast/cheap model for its sub-agents.
        Populated by individual skill modules (`research.nix`); only applied
        on hosts with pi-coding-agent enabled.
      '';
    };
  };

  config = lib.mkMerge [
    {
      # Direct deployment of the handcrafted registry — ONLY when the
      # agent-skills framework is disabled. With the framework enabled the
      # same skills flow through it (block below), which must remain the sole
      # writer of every synced skills directory: a second writer (these
      # home-file symlinks) makes the framework's `rsync --delete` refuse the
      # unmarked destination ("no .agent-skills-managed.json marker"). The
      # pi prompt templates and sub-agent definitions are not skills and stay
      # on home.file in both modes.
      home-manager.sharedModules = [
        {
          programs.opencode.skills = lib.mkIf (harnessEnabled.opencode && !cfg.enable) handcrafted;
          programs.claude-code.skills = lib.mkIf (harnessEnabled.claude-code && !cfg.enable) handcrafted;
          programs.codex.skills = lib.mkIf (harnessEnabled.codex && !cfg.enable) handcrafted;
          home.file = lib.mkIf harnessEnabled.pi (
            (lib.optionalAttrs (!cfg.enable) (
              lib.mapAttrs' (
                name: src:
                lib.nameValuePair ".agents/skills/${name}" {
                  source = src;
                  recursive = true;
                }
              ) handcrafted
            ))
            // (lib.mapAttrs' (
              name: src: lib.nameValuePair ".pi/agent/prompts/${name}.md" { source = src; }
            ) handcraftedPrompts)
            // (lib.mapAttrs' (
              name: src: lib.nameValuePair ".pi/agent/agents/${name}.md" { source = src; }
            ) handcraftedAgents)
          );
        }
      ];
    }
    (lib.mkIf cfg.enable {
      home-manager.sharedModules = [
        agentSkillsHmModule
        {
          programs.agent-skills = {
            enable = true;

            sources = lib.mkMerge [
              {
                mattpocock = {
                  input = lib.mkDefault "mattpocock-skills";
                  subdir = lib.mkDefault "skills";
                  # Reduce to the recommended core: mattpocock's repo groups
                  # skills into categories, but only `engineering` and
                  # `productivity` are promoted in the plugin / top-level
                  # README. The `misc` and `personal` buckets are explicitly
                  # "not promoted", `in-progress` is unfinished/excluded, and
                  # `deprecated` is no longer used. Discover only the two
                  # promoted categories so those non-recommended skills never
                  # sync into any harness.
                  filter.nameRegex = lib.mkDefault "(engineering|productivity)/.*";
                };
              }
              handcraftedSources
              cfg.sources
            ];

            skills = lib.mkMerge [
              {
                enableAll = lib.mkDefault [ "mattpocock" ];
                enable = lib.mkDefault [ ];
                explicit = lib.mkDefault handcraftedExplicit;
              }
              cfg.skills
            ];

            targets = lib.mkMerge [
              harnessTargetDefaults
              cfg.targets
            ];

            # The claude-code home-manager module deploys its generated MCP
            # plugin as `~/.claude/skills/claude-code-home-manager` via
            # `home.file` — a second writer inside the synced `claude` target
            # directory. Exclude it from the framework's `rsync --delete`
            # (same pattern as the upstream `/.system` exclusion) so the two
            # mechanisms coexist instead of deleting each other.
            excludePatterns = lib.mkMerge [
              [ "/claude-code-home-manager" ]
              (lib.mkIf (cfg.excludePatterns != [ ]) cfg.excludePatterns)
            ];
          };
        }
      ];
    })
  ];
}
