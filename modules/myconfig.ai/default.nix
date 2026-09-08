# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  myconfig,
  lib,
  pkgs,
  ...
}:
let
  user = myconfig.user;
  nixpkgsConfig = config.nixpkgs.config;
  callLib = file: import file { inherit lib pkgs; };
in
{
  imports = [
    ./myconfig.localModels.nix
    ./myconfig.ai.pull_models.nix
    ./myconfig.ai.llama-cpp
    ./myconfig.ai.jail.nix
    ./myconfig.ai.nono.nix
    ./myconfig.ai.nono-agent-sandbox.nix
    ./myconfig.ai.sandboxTools.nix
    ./myconfig.ai.qemu-agent-sandbox
    ./myconfig.ai.microvm
    ./myconfig.ai.gvisor-agent-sandbox
    ./mysbx
    ./comfyui.nix
    ./container.Kokoro-FastAPI.nix
    ./container.crawl4ai.nix
    ./container.headroom.nix
    ./container.lobe-chat.nix
    ./container.nlm-ingestor.nix
    ./container.open-webui.nix
    ./hermes-agent
    ./programs.agent-of-empires
    ./programs.aichat.nix
    ./programs.alpaca.nix
    ./programs.beads
    ./programs.claude-code
    ./programs.codex
    ./programs.github-copilot-cli
    ./programs.herdr.nix
    ./programs.hunk
    ./programs.llm.nix
    ./programs.lmstudio.nix
    ./programs.mcp.servers.nix
    ./programs.opencode
    ./programs.pi-coding-agent
    ./programs.qwen-code
    ./programs.rtk
    ./services.litellm.nix
    ./litellm.proxy.nix
    ./services.open-webui.nix
    ./services.orca.nix
    ./services.searxng.nix
    ./services.tabby.nix
    ./skills
    ./myconfig.ai.workmux
  ];
  options.myconfig.ai.enable = lib.mkEnableOption "myconfig.ai";
  config = lib.mkIf config.myconfig.ai.enable {
    myconfig.ai.aichat.enable = true;
    myconfig.ai.llm.enable = true;
    myconfig.dev.python.enable = true;
    # workmux is a terminal-native companion to agentic coding; auto-enable
    # it whenever the AI tooling, the dev profile, and tmux are all active.
    # (ai is guaranteed by the surrounding mkIf.) Use mkDefault so a host
    # can still turn it off explicitly.
    myconfig.ai.workmux.enable = lib.mkDefault (
      config.myconfig.dev.enable && config.programs.tmux.enable
    );
    # nono-agent-sandbox provides `agent-nono-*` wrappers (like `agent-bubblewrap-*`)
    # for running coding agents in the nono capability-based sandbox. Enable by
    # default whenever myconfig.ai is enabled, but allow hosts to override.
    myconfig.ai.nono-agent-sandbox.enable = lib.mkDefault true;
    # rtk is a plain CLI proxy that shrinks command output before an agent
    # reads it (./programs.rtk). It costs one small binary plus a handful of
    # generated config files and benefits every coding agent on the host, so
    # it is on by default wherever the AI tooling is; a host can still turn it
    # off explicitly.
    myconfig.ai.rtk.enable = lib.mkDefault true;
    # hunk is the review-first diff viewer for agent-authored changesets
    # (./programs.hunk). Reviewing what an agent wrote is part of every
    # agentic coding workflow, and the cost is one small binary plus a
    # generated config file, so it follows rtk and is on by default wherever
    # the AI tooling is. `gitIntegration` stays off, so nothing changes for
    # plain `git diff`. A host can still turn it off explicitly.
    myconfig.ai.hunk.enable = lib.mkDefault true;
    # beads is the memory system for AI coding agents with graph-based issue
    # tracking (./programs.beads). Memory and issue tracking for agent
    # workflows is part of every agentic coding workflow, and the cost is one
    # small binary, so it follows rtk and hunk and is on by default wherever
    # the AI tooling is. A host can still turn it off explicitly.
    myconfig.ai.beads.enable = lib.mkDefault true;
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
    services.udev.extraRules = ''
      SUBSYSTEM=="accel", GROUP="render", MODE="0660"
    '';
    users.users."${user}" = {
      extraGroups = [ "render" ];
    };
  };
}
