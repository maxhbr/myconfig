# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev — umbrella for the AI *developer tooling* split out of
# myconfig.ai (beads bd: myconfig-e4j, option rename bd: myconfig-4so). This
# module re-homes the dev tooling part of the former
# modules/myconfig.ai.llmops/default.nix umbrella: the agent CLI programs
# (./programs/programs.*), mysbx, workmux, skills and hermes-agent. All options defined by this tree live under
# `myconfig.ai.dev.*` and are gated behind `myconfig.ai.dev.enable`. The
# `myconfig.ai.*` tree is now `myconfig.ai.llmops` (inference/services
# tooling) and is completely orthogonal to this tree — no implied
# enables in either direction.
{
  config,
  myconfig,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.dev;
in
{
  options.myconfig.ai.dev = {
    enable = lib.mkEnableOption "myconfig.ai.dev (AI developer tooling: agent CLIs, mysbx, workmux, skills, hermes-agent)";

    defaultHugeModel = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Model name of the default huge (hosted flagship) model routed
        through the LiteLLM proxy. Used as the umbrella default for the
        agent tools' model options (pi-coding-agent's `defaultModel`,
        hermes' `model.default`). Null (default) leaves those unset, so
        hosts that want one set it explicitly.
      '';
    };
  };

  imports = [
    ./agentsMd
    ./myconfig.ai.dev.litellm-forwarder.nix

    ./myconfig.ai.dev.sandboxTools.nix
    ./services.orca.nix
    inputs.mysbx.nixosModules.mysbx
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
    ./programs/programs.thinkrail.nix
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
        agentsMd.enable = lib.mkDefault true;
        opencode.enable = true;
        pi-coding-agent = {
          enable = true;
          litellmUrl = "http://localhost:4000";
          defaultModel = lib.mkIf (cfg.defaultHugeModel != null) {
            provider = "${config.networking.hostName}-litellm";
            model = cfg.defaultHugeModel;
          };
          tokenSpeed.enable = true;
        };
        hermes = {
          model.default = lib.mkIf (cfg.defaultHugeModel != null) (lib.mkDefault cfg.defaultHugeModel);
        };
        claude-code.enable = true;
        codex.enable = true;
        skills.enable = true;
        agent-of-empires.enable = true;
        mysbx = {
          enable = true;
          display.package = pkgs.waypipe;
          krun.nestedPodman.enable = lib.mkDefault config.virtualisation.podman.enable;
          krun.nix.enable = lib.mkDefault true;
          browser.enable = true;
        };
        workmux.enable = lib.mkDefault (config.myconfig.dev.enable && config.programs.tmux.enable);
        rtk.enable = lib.mkDefault true;
        hunk.enable = lib.mkDefault true;
        beads.enable = lib.mkDefault true;
        agent-browser.enable = lib.mkDefault true;
        # The browser-automation skill and its CLI; on by default only here, so a
        # host without `myconfig.ai.dev` never pulls a browser closure.
        skills.playwright-cli.enable = lib.mkDefault true;
        ccusage.enable = lib.mkDefault true;
        # heavyweight prebuilt binary (~130 MB + FHS/bwrap closure), so opt-in
        # like lmstudio/hermes rather than default-on
        thinkrail.enable = lib.mkDefault false;
      };
      home-manager.sharedModules = [
        {
          home.packages = with pkgs.python3Packages; [
            huggingface-hub
          ];
          myconfig.persistence.cache-directories = [ ".cache/huggingface/" ];
        }
        {
          home.packages = with pkgs; [
            llmfit
          ];
        }
      ];
    })
  ];
}
