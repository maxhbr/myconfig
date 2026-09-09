# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev — umbrella for the AI *developer tooling* split out of
# myconfig.ai (beads bd: myconfig-e4j, option rename bd: myconfig-4so). This
# module re-homes the dev tooling part of the former
# modules/myconfig.ai/default.nix umbrella: the agent CLI programs
# (./programs/programs.*), the sandbox tiers, mysbx, workmux, skills and
# hermes-agent. All options defined by this tree live under
# `myconfig.ai.dev.*` and are gated behind `myconfig.ai.dev.enable` (which
# itself defaults on wherever `myconfig.ai.enable` is set, see below).
{
  config,
  myconfig,
  lib,
  pkgs,
  ...
}:
let
  callLib = file: import file { inherit lib pkgs; };
in
{
  options.myconfig.ai.dev.enable =
    lib.mkEnableOption "myconfig.ai.dev (AI developer tooling: agent CLIs, sandbox tiers, mysbx, workmux, skills, hermes-agent)";

  imports = [
    # dev tooling — the sandbox tiers were moved here by myconfig-e4j.3
    # (./sandboxes/); mysbx and workmux were moved here by myconfig-e4j.4.
    # The agent CLI programs were moved here by myconfig-e4j.2, and
    # skills/, fns/ and hermes-agent by myconfig-e4j.5
    ./sandboxes/myconfig.ai.jail.nix
    ./sandboxes/myconfig.ai.nono.nix
    ./sandboxes/myconfig.ai.nono-agent-sandbox.nix
    ./sandboxes/myconfig.ai.sandboxTools.nix
    ./sandboxes/myconfig.ai.qemu-agent-sandbox
    ./sandboxes/myconfig.ai.microvm
    ./sandboxes/myconfig.ai.gvisor-agent-sandbox
    ./mysbx
    ./hermes-agent
    ./programs/programs.agent-browser
    ./programs/programs.agent-of-empires
    ./programs/programs.aichat.nix
    ./programs/programs.alpaca.nix
    ./programs/programs.beads
    ./programs/programs.ccusage
    ./programs/programs.claude-code
    ./programs/programs.codex
    ./programs/programs.github-copilot-cli
    ./programs/programs.herdr.nix
    ./programs/programs.hunk
    ./programs/programs.llm.nix
    ./programs/programs.lmstudio.nix
    ./programs/programs.mcp.servers.nix
    ./programs/programs.opencode
    ./programs/programs.pi-coding-agent
    ./programs/programs.qwen-code
    ./programs/programs.rtk
    ./skills
    ./myconfig.ai.workmux
  ];

  # myconfig.ai.enable implies myconfig.ai.dev.enable (by default), so every
  # existing host keeps its AI dev tooling. A host can still opt out per host
  # with `myconfig.ai.dev.enable = false`. The wiring must stay OUTSIDE the
  # mkIf gate below, otherwise it would be circular.
  config = lib.mkMerge [
    { myconfig.ai.dev.enable = lib.mkIf config.myconfig.ai.enable (lib.mkDefault true); }

    (lib.mkIf config.myconfig.ai.dev.enable {
      myconfig.dev.python.enable = true;
      # workmux is a terminal-native companion to agentic coding; auto-enable
      # it whenever the AI tooling, the dev profile, and tmux are all active.
      # (ai is guaranteed by the surrounding mkIf.) Use mkDefault so a host
      # can still turn it off explicitly.
      myconfig.ai.dev.workmux.enable = lib.mkDefault (
        config.myconfig.dev.enable && config.programs.tmux.enable
      );
      # nono-agent-sandbox provides `agent-nono-*` wrappers (like `agent-bubblewrap-*`)
      # for running coding agents in the nono capability-based sandbox. Enable by
      # default whenever myconfig.ai is enabled, but allow hosts to override.
      myconfig.ai.dev.nono-agent-sandbox.enable = lib.mkDefault true;
      # rtk is a plain CLI proxy that shrinks command output before an agent
      # reads it (./programs/programs.rtk). It costs one small binary plus a handful of
      # generated config files and benefits every coding agent on the host, so
      # it is on by default wherever the AI tooling is; a host can still turn it
      # off explicitly.
      myconfig.ai.dev.rtk.enable = lib.mkDefault true;
      # hunk is the review-first diff viewer for agent-authored changesets
      # (./programs/programs.hunk). Reviewing what an agent wrote is part of every
      # agentic coding workflow, and the cost is one small binary plus a
      # generated config file, so it follows rtk and is on by default wherever
      # the AI tooling is. `gitIntegration` stays off, so nothing changes for
      # plain `git diff`. A host can still turn it off explicitly.
      myconfig.ai.dev.hunk.enable = lib.mkDefault true;
      # beads is the memory system for AI coding agents with graph-based issue
      # tracking (./programs/programs.beads). Memory and issue tracking for agent
      # workflows is part of every agentic coding workflow, and the cost is one
      # small binary, so it follows rtk and hunk and is on by default wherever
      # the AI tooling is. A host can still turn it off explicitly.
      myconfig.ai.dev.beads.enable = lib.mkDefault true;
      # agent-browser provides browser automation capabilities to AI agents
      # (./programs/programs.agent-browser). Browser automation is a core capability
      # for agentic coding workflows, so it follows rtk and hunk and is on by
      # default wherever the AI tooling is. A host can still turn it off
      # explicitly.
      myconfig.ai.dev.agent-browser.enable = lib.mkDefault true;
      # ccusage provides token usage and cost analysis for Claude Code
      # sessions (./programs/programs.ccusage). Analyzing agent token usage is part of
      # every agentic coding workflow, and the cost is one small binary, so
      # it follows rtk, hunk and beads and is on by default wherever the AI
      # tooling is. A host can still turn it off explicitly.
      myconfig.ai.dev.ccusage.enable = lib.mkDefault true;
      home-manager.sharedModules = [
        {
          home.packages =
            with pkgs;
            [
              (callLib ./fns/bubblewrap-simple-app.nix {
                name = "fish";
                pkg = fish;
              })
              (callLib ./fns/bubblewrap-simple-app.nix {
                name = "bash";
                pkg = bash;
              })
            ]
            ++ (with pkgs.python3Packages; [
              huggingface-hub
            ]);
          myconfig.persistence.cache-directories = [ ".cache/huggingface/" ];
        }
        {
          home.packages = with pkgs; [
            llmfit
          ];
        }
        {
          home.packages = with pkgs; [
            # sandboxing
            nono
            fence
            bubblewrap
          ];
        }
      ];
    })
  ];
}
