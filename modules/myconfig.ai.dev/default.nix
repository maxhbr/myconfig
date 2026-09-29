# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev — umbrella for the AI *developer tooling* split out of
# myconfig.ai (beads bd: myconfig-e4j, option rename bd: myconfig-4so). This
# module re-homes the dev tooling part of the former
# modules/myconfig.ai.llmops/default.nix umbrella: the agent CLI programs
# (./programs/programs.*), the sandbox tiers, mysbx, workmux, skills and
# hermes-agent. All options defined by this tree live under
# `myconfig.ai.dev.*` and are gated behind `myconfig.ai.dev.enable`. The
# `myconfig.ai.*` tree is now `myconfig.ai.llmops` (inference/services
# tooling) and is completely orthogonal to this tree — no implied
# enables in either direction.
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
    ./myconfig.ai.dev.litellm-forwarder.nix

    ./sandboxes/myconfig.ai.jail.nix
    ./sandboxes/myconfig.ai.nono.nix
    ./sandboxes/myconfig.ai.nono-agent-sandbox.nix
    ./sandboxes/myconfig.ai.sandboxTools.nix
    ./sandboxes/myconfig.ai.qemu-agent-sandbox
    ./sandboxes/myconfig.ai.microvm
    ./sandboxes/myconfig.ai.gvisor-agent-sandbox
    ./services.orca.nix
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

  # `myconfig.ai.dev` is orthogonal to `myconfig.ai.llmops`: hosts opt in
  # explicitly with `myconfig.ai.dev.enable = true` (the umbrella below then
  # defaults the individual programs on).
  config = lib.mkMerge [
    (lib.mkIf config.myconfig.ai.dev.enable {
      myconfig.dev.python.enable = true;
      myconfig.ai.dev = {
        opencode.enable = true;
        pi-coding-agent = {
          enable = true;
          litellmUrl = "http://localhost:4000";
          tokenSpeed.enable = true;
        };
        claude-code.enable = true;
        codex.enable = true;
        skills.enable = true;
        agent-of-empires.enable = true;
        gvisor-agent-sandbox = {
          enable = true;
          nix.enable = true;
        };
        mysbx = {
          enable = true;
          display.package = pkgs.waypipe;
          krun.nestedPodman.enable = lib.mkDefault config.virtualisation.podman.enable;
          krun.nix.enable = lib.mkDefault true;
          browser.enable = true;
        };
        workmux.enable = lib.mkDefault (config.myconfig.dev.enable && config.programs.tmux.enable);
        nono-agent-sandbox.enable = lib.mkDefault true;
        rtk.enable = lib.mkDefault true;
        hunk.enable = lib.mkDefault true;
        beads.enable = lib.mkDefault true;
        agent-browser.enable = lib.mkDefault true;
        ccusage.enable = lib.mkDefault true;
      };
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
