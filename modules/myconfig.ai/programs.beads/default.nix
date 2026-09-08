# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# beads (https://github.com/gastownhall/beads) — a lightweight memory system
# for AI coding agents with graph-based issue tracking. The tool is packaged
# in nixpkgs (`pkgs/by-name/be/beads/package.nix`), so no extra flake input is
# needed: this module just consumes `pkgs.beads`.
#
# beads needs no account, no auth token and no init run: it works out of the
# box against the current repository by storing memory in a `.beads/` directory.
# Nothing in this module ever executes `beads`; every artefact is a plain
# home-manager file:
#
#   * the binary on the PATH of every sandbox tier — via the shared
#     `myconfig.ai.sandboxTools.extraPackages` so that every agentic coding
#     environment and sandbox has access to the memory system.
#
# Like `rtk` and `hunk`, this module is auto-enabled by the `myconfig.ai` umbrella
# (`myconfig.ai.beads.enable = lib.mkDefault true` in ../default.nix): memory
# and issue tracking for agent workflows is part of every agentic coding
# workflow and the cost is one small binary. The `enable` option itself still
# defaults to false, so a host without `myconfig.ai.enable` never gets beads,
# and a host can still opt out with `myconfig.ai.beads.enable = false;`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.beads;
in
{
  options.myconfig = with lib; {
    ai.beads = {
      enable = mkEnableOption "myconfig.ai.beads";

      package = mkPackageOption pkgs "beads" { };
    };
  };

  config = lib.mkIf cfg.enable {
    # beads is the memory system that agents use to track context and issues
    # across sessions. It needs to exist where the agents run — inside the
    # sandboxes, not only on the host. Unlike host-only tools, beads follows
    # this module's enable gate, the same way the agent CLIs are gated: a host
    # without beads keeps its sandbox closures unchanged.
    #
    # `myconfig.ai.sandboxTools.extraPackages` reaches every tier that
    # consumes the shared list (the `agent-bubblewrap-*`/nono jails, the
    # `myconfig.ai.microvm` guests, the `sandboxed-*` qemu runners and the
    # gVisor image).
    myconfig.ai.sandboxTools.extraPackages = [ cfg.package ];

    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];
      }
    ];
  };
}
