# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Declarative agent skills management, backed by the vendored
# `agent-skills-nix` library (see `vendor/agent-skills-nix/`). When enabled,
# skills are discovered from flake-pinned sources (mattpocock/skills by
# default) and synced into the config directories of the agent harnesses that
# are enabled on this host via home-manager.
#
# The upstream home-manager module exposes `programs.agent-skills.*`; this
# wrapper provides a `myconfig.ai.skills.*` surface with sensible defaults
# while still allowing full pass-through configuration. Anything set in
# `myconfig.ai.skills.{sources,skills,targets,excludePatterns}` is merged on
# top of the defaults, so hosts can add sources, narrow the skill selection,
# or retarget individual harnesses without touching this file.
{
  config,
  lib,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.skills;

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
  # host can still force a target on/off via `myconfig.ai.skills.targets`.
  harnessTargetDefaults =
    let
      when = cond: name: lib.optionalAttrs cond { ${name}.enable = lib.mkOverride 900 true; };
    in
    lib.optionalAttrs (config.myconfig.ai ? opencode) (
      when (config.myconfig.ai.opencode.enable or false) "opencode"
    )
    // lib.optionalAttrs (config.myconfig.ai ? codex) (
      when (config.myconfig.ai.codex.enable or false) "codex"
    )
    // lib.optionalAttrs (config.myconfig.ai ? claude-code) (
      when (config.myconfig.ai.claude-code.enable or false) "claude"
    )
    // lib.optionalAttrs (config.myconfig.ai ? pi-coding-agent) (
      when (config.myconfig.ai.pi-coding-agent.enable or false) "pi"
    );

  # Whether each agent harness is enabled on this host. Used to gate the
  # handcrafted-skill apply block below so a host without, say, pi-coding-agent
  # does not get `~/.agents/skills/` entries it will never load.
  harnessEnabled = {
    opencode = config.myconfig.ai.opencode.enable or false;
    claude-code = config.myconfig.ai.claude-code.enable or false;
    codex = config.myconfig.ai.codex.enable or false;
    pi = config.myconfig.ai.pi-coding-agent.enable or false;
  };

  # Registry of locally-defined ("handcrafted") skills, populated by the
  # individual skill modules (commit, playwright-cli, grafana-core). Each
  # entry maps a skill name to its source directory (the dir containing
  # `SKILL.md`). The apply block below deploys every entry to each enabled
  # agent harness — independently of the agent-skills sync framework, which
  # is opt-in via `myconfig.ai.skills.enable`.
  handcrafted = config.myconfig.ai.skills.handcrafted;
  # Registry of locally-defined pi prompt templates, populated by individual
  # skill modules when a workflow should also be reachable as a `/`-command
  # prompt template. Deployed to `~/.pi/agent/prompts/<name>.md` for every host
  # with pi-coding-agent enabled.
  handcraftedPrompts = config.myconfig.ai.skills.handcraftedPrompts;
  # Registry of locally-defined pi sub-agent definitions, populated by
  # individual skill modules (research). Each entry maps an agent name to its
  # source `.md` file. Deployed to `~/.pi/agent/agents/<name>.md`, where the
  # pi subagent extension discovers agents. Unlike the upstream sample
  # agents (whose `model:` line is stripped before deployment), these are
  # deployed verbatim — preserving `model:` frontmatter so a handcrafted
  # agent can pin a fast/cheap model for its sub-agents. Only applied on
  # hosts with pi-coding-agent enabled.
  handcraftedAgents = config.myconfig.ai.skills.handcraftedAgents;
in
{
  imports = [
    ./commit.nix
    ./grafana-core.nix
    ./playwright-cli.nix
    ./research.nix
    ./review.nix
  ];

  options.myconfig.ai.skills = with lib; {
    enable = mkEnableOption "agent skills management via home-manager (vendor/agent-skills-nix)";

    sources = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Additional skill sources merged on top of the default `mattpocock`
        source. Each entry follows the upstream `sourceType` shape, e.g.:

        ```nix
        myconfig.ai.skills.sources.anthropic = {
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
        `skills.{enable, enableAll, explicit}` options. For example, to opt
        into a specific subset instead of all mattpocock skills:

        ```nix
        myconfig.ai.skills.skills = {
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
        individual skill modules (`commit.nix`, `playwright-cli.nix`,
        `grafana-core.nix`); this module deploys every entry to each enabled
        agent harness — opencode, claude-code and codex via their
        `programs.<harness>.skills` option, and pi-coding-agent (which has no
        such option) directly to the shared `~/.agents/skills/` discovery
        directory.
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
      # Apply every handcrafted skill to each enabled agent harness.
      # opencode, claude-code and codex expose a `programs.<harness>.skills`
      # attrset that home-manager turns into per-skill symlinks under their
      # config directories. pi-coding-agent has no such option — it discovers
      # skills purely from the filesystem — so skills are written directly to
      # the shared `~/.agents/skills/` discovery directory (which pi reads and
      # which the agent-skills sync framework does not manage by default, so
      # this does not collide with the framework's `--delete` rsync on hosts
      # that opt into it below). This block runs whenever a skill is enabled,
      # independent of the sync framework (`myconfig.ai.skills.enable`).
      home-manager.sharedModules = [
        {
          programs.opencode.skills = lib.mkIf harnessEnabled.opencode handcrafted;
          programs.claude-code.skills = lib.mkIf harnessEnabled.claude-code handcrafted;
          programs.codex.skills = lib.mkIf harnessEnabled.codex handcrafted;
          home.file = lib.mkIf harnessEnabled.pi (
            (lib.mapAttrs' (
              name: src:
              lib.nameValuePair ".agents/skills/${name}" {
                source = src;
                recursive = true;
              }
            ) handcrafted)
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
                };
              }
              cfg.sources
            ];

            skills = lib.mkMerge [
              {
                enableAll = lib.mkDefault [ "mattpocock" ];
                enable = lib.mkDefault [ ];
                explicit = lib.mkDefault { };
              }
              cfg.skills
            ];

            targets = lib.mkMerge [
              harnessTargetDefaults
              cfg.targets
            ];

            excludePatterns = lib.mkIf (cfg.excludePatterns != [ ]) cfg.excludePatterns;
          };
        }
      ];
    })
  ];
}
