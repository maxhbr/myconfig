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
in
{
  imports = [
    ./commit.nix
    ./grafana-core.nix
    ./playwright-cli.nix
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
  };

  config = lib.mkIf cfg.enable {
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
  };
}
