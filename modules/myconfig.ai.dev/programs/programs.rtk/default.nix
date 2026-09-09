# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# rtk (https://github.com/rtk-ai/rtk) — a CLI proxy that filters and compresses
# the output of common dev commands before an agent reads it.
#
# Upstream expects a one-off, INTERACTIVE `rtk init [-g]` run that patches the
# user's home directory (agent hooks, plugins, awareness markdown). Nothing in
# this module ever executes `rtk` — every artefact `rtk init` would have
# created is materialized here as a plain home-manager file:
#
#   * `~/.config/rtk/config.toml` — the user settings file
#     (`src/core/config.rs`: `dirs::config_dir()/rtk/config.toml`), rendered
#     from the `settings` option below.
#   * `~/.pi/agent/extensions/rtk.ts` — the pi extension (`rtk init --agent pi`)
#   * `~/.config/opencode/plugins/rtk.ts` — the OpenCode plugin
#     (`rtk init --opencode`)
#   * a `PreToolUse` → `rtk hook claude` entry in claude-code's `settings.json`
#     (`rtk init -g`)
#
# The two plugin files are NOT part of the nixpkgs `rtk` output ($out only
# holds `bin/rtk`); they live in the `hooks/` directory of rtk's source tree,
# from where the binary embeds them via `include_str!`. `rtkHooks` below takes
# them from the very same pinned source, so the deployed plugins always match
# the deployed binary.
#
# No state file needs to be pre-created: rtk creates its tracking database
# (`~/.local/share/rtk/history.db`) lazily on first use, and a missing
# `config.toml` simply means "all defaults" — so the deployment is complete
# and read-only from the module's point of view.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.rtk;
  aiCfg = config.myconfig.ai;

  tomlFormat = pkgs.formats.toml { };

  # Agent-integration files shipped in rtk's source tree but not installed by
  # the package. Copied into a tiny derivation of their own so that only the
  # `hooks/` directory — not the whole rtk source tree — ends up in the
  # user's closure.
  rtkHooks = pkgs.runCommand "rtk-hooks-${cfg.package.version}" { } ''
    cp -r ${cfg.package.src}/hooks $out
  '';

  # Harness gates, mirroring `../../skills/default.nix`.
  piEnabled = aiCfg.pi-coding-agent.enable or false;
  opencodeEnabled = aiCfg.opencode.enable or false;
  claudeEnabled = aiCfg.claude-code.enable or false;
in
{
  options.myconfig = with lib; {
    ai.rtk = {
      enable = mkEnableOption "myconfig.ai.rtk";

      package = mkPackageOption pkgs "rtk" { };

      settings = mkOption {
        type = tomlFormat.type;
        default = { };
        example = literalExpression ''
          {
            hooks.exclude_commands = [ "curl" ];
            hooks.transparent_prefixes = [ "nix develop --command" ];
          }
        '';
        description = ''
          Content of `~/.config/rtk/config.toml` (`Config` in rtk's
          `src/core/config.rs`). Merged on top of the defaults set in
          `config` below.

          Only sections that are spelled out COMPLETELY may be set: rtk
          deserializes each section as a whole struct, so a partially
          written `[filters]`/`[limits]` table would drop the fields it
          omits. Sections left out entirely keep rtk's built-in defaults,
          which is why this module only writes the few it actually wants
          to pin.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Teach every enabled harness about rtk's meta commands (`rtk gain`,
    # `rtk discover`, `rtk proxy`) — the declarative counterpart of the
    # `RTK.md` awareness file `rtk init` would append to CLAUDE.md/AGENTS.md.
    # `../../skills/default.nix` deploys the registry to each enabled harness.
    myconfig.ai.skills.handcrafted.rtk = ./skills/rtk;

    myconfig.ai.rtk.settings = {
      # Opt out of upstream telemetry explicitly instead of relying on the
      # compiled-in default. `enabled` is the only required field of the
      # `[telemetry]` table; the consent fields default to unset.
      telemetry.enabled = false;
      # Local usage tracking (feeds `rtk gain`) — kept on, with rtk's own
      # 90-day retention stated explicitly.
      tracking = {
        enabled = true;
        history_days = 90;
      };
    };

    # mysbx tier integration (../../mysbx), following the pattern of
    # ../programs.opencode/default.nix: put `rtk` on the sandbox PATH so the
    # agents' rewritten commands resolve inside the sandbox too, mount the
    # generated config read-only under the sandbox home (`HOME` is
    # `/mysbx-home` there), and give rtk a per-repository state directory so
    # its tracking database survives the tmpfs home without any host
    # `~/.local` path entering the sandbox.
    myconfig.ai.mysbx = lib.mkIf aiCfg.mysbx.enable {
      extraTools = [ cfg.package ];
      config.mounts = [
        {
          # Always present: the `settings` defaults above are non-empty, so
          # home-manager always writes `~/.config/rtk/config.toml` (a missing
          # mount source is a hard error on every mysbx run).
          path = "~/.config/rtk";
          dest = "/mysbx-home/.config/rtk";
          mode = "ro";
        }
      ];
      config.stateDirs = [ ".local/share/rtk" ];
    };

    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];

        # `~/.local/share/rtk/history.db` (`dirs::data_local_dir()`), created
        # lazily by rtk itself — worth persisting so `rtk gain` keeps its
        # history across reboots on impermanence hosts.
        myconfig.persistence.directories = [ ".local/share/rtk" ];

        xdg.configFile = {
          "rtk/config.toml".source = tomlFormat.generate "rtk-config.toml" cfg.settings;
        }
        // lib.optionalAttrs opencodeEnabled {
          # `rtk init -g --opencode`
          "opencode/plugins/rtk.ts".source = "${rtkHooks}/opencode/rtk.ts";
        };

        # `rtk init -g --agent pi`. pi discovers `~/.pi/agent/extensions/*.ts`
        # automatically (see ../programs.pi-coding-agent/default.nix, which
        # deploys its extensions the same way).
        home.file = lib.mkIf piEnabled {
          ".pi/agent/extensions/rtk.ts".source = "${rtkHooks}/pi/rtk.ts";
        };

        # `rtk init -g` for claude-code: the native binary hook rewrites every
        # `Bash` tool call before it runs. Written through the home-manager
        # `settings` option rather than by patching `settings.json` in place.
        programs.claude-code.settings = lib.mkIf claudeEnabled {
          hooks.PreToolUse = [
            {
              matcher = "Bash";
              hooks = [
                {
                  type = "command";
                  command = "${lib.getExe cfg.package} hook claude";
                }
              ];
            }
          ];
        };
      }
    ];
  };
}
