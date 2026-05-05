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
    ./comfyui.nix
    ./container.Kokoro-FastAPI.nix
    ./container.SillyTavern.nix
    ./container.crawl4ai.nix
    ./container.lobe-chat.nix
    ./container.nlm-ingestor.nix
    ./container.open-webui.nix
    ./inference.cpp.nix
    ./programs.aichat.nix
    ./programs.alpaca.nix
    ./programs.claude-code
    ./programs.codex
    ./programs.github-copilot-cli
    ./programs.llm.nix
    ./programs.lmstudio.nix
    ./programs.mcp.servers.nix
    ./programs.opencode
    ./programs.pi-coding-agent
    ./programs.qwen-code
    ./programs.vllm.nix
    ./services.litellm.nix
    ./services.llama-swap.nix
    ./services.ollama.nix
    ./services.open-webui.nix
    ./services.searxng.nix
    ./services.tabby.nix
    ./skill.playwright-cli.nix
  ];
  options.myconfig.ai.enable = lib.mkEnableOption "myconfig.ai";
  config = lib.mkIf config.myconfig.ai.enable {
    myconfig.ai.aichat.enable = true;
    myconfig.ai.llm.enable = true;
    myconfig.dev.python.enable = true;
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
