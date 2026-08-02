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
    ./myconfig.ai.microvm
    ./comfyui.nix
    ./container.Kokoro-FastAPI.nix
    ./container.crawl4ai.nix
    ./container.headroom.nix
    ./container.lobe-chat.nix
    ./container.nlm-ingestor.nix
    ./container.open-webui.nix
    ./hermes-agent
    ./programs.aichat.nix
    ./programs.alpaca.nix
    ./programs.claude-code
    ./programs.codex
    ./programs.github-copilot-cli
    ./programs.herdr.nix
    ./programs.llm.nix
    ./programs.lmstudio.nix
    ./programs.mcp.servers.nix
    ./programs.opencode
    ./programs.pi-coding-agent
    ./programs.qwen-code
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
    home-manager.sharedModules = [
      {
        home.packages =
          with pkgs;
          [
            (callLib ./fns/sandboxed-app.nix {
              name = "fish";
              pkg = fish;
            })
            (callLib ./fns/sandboxed-app.nix {
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
