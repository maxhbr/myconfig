# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# ccusage (https://github.com/ccusage/ccusage) — CLI tool that analyzes
# coding-agent token usage and costs from the local session JSONL files
# (Claude Code, Codex, OpenCode, pi, ...). The tool is packaged in nixpkgs
# (`pkgs/by-name/cc/ccusage/package.nix`, a Rust crate with the LiteLLM
# pricing table baked in at build time), so no extra flake input is needed:
# this module just consumes `pkgs.ccusage`.
#
# ccusage needs no account, no auth token and no `init` run, and it needs
# NO deployed config file either: it works out of the box with built-in
# defaults, and its optional JSON config is discovered by ccusage itself at
# runtime, in priority order (see docs/guide/config-files.md upstream):
#
#   1. `<project>/.ccusage/ccusage.json`
#   2. `~/.config/claude/ccusage.json`
#   3. `~/.claude/ccusage.json`
#
# Writing a user-level config into `~/.config/claude/` or `~/.claude/` from
# here would squat on Claude Code's own directories, so this module deploys
# no settings file at all — a per-project `.ccusage/ccusage.json` (the
# documented place for pricing overrides and report defaults) stays the
# user's own choice. Nothing in this module ever executes `ccusage`.
#
# Like `rtk`, `hunk` and `beads`, this module is auto-enabled by the
# `myconfig.ai` umbrella (`myconfig.ai.dev.ccusage.enable = lib.mkDefault true`
# in ../default.nix): analyzing agent token usage and cost is part of every
# agentic coding workflow and the cost is one small binary. The `enable`
# option itself still defaults to false, so a host without
# `myconfig.ai.enable` never gets ccusage, and a host can opt out with
# `myconfig.ai.dev.ccusage.enable = false;`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.ccusage;
in
{
  options.myconfig = with lib; {
    ai.dev.ccusage = {
      enable = mkEnableOption "myconfig.ai.dev.ccusage";

      package = mkPackageOption pkgs "ccusage" { };
    };
  };

  config = lib.mkIf cfg.enable {
    # Sandbox tiers: ccusage is what the human (and agents) use to analyze
    # token usage and cost of the sessions in the sandbox home, so it has to
    # exist where the agents run — inside the sandboxes, not only on the
    # host. Like `hunk` and `beads`, ccusage follows this module's enable
    # gate, the same way the agent CLIs are gated: a host without ccusage
    # keeps its sandbox closures unchanged.
    #
    # `myconfig.ai.dev.sandboxTools.extraPackages` reaches every tier that
    # consumes the shared list (the `agent-bubblewrap-*`/nono jails, the
    # `myconfig.ai.dev.microvm` guests, the `sandboxed-*` qemu runners and the
    # gVisor image); mysbx has its own `extraTools` extension point (see
    # ../programs.hunk and ../programs.rtk for the same pair of hooks).
    myconfig.ai.dev.sandboxTools.extraPackages = [ cfg.package ];

    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ cfg.package ];
    };

    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];
      }
    ];
  };
}
