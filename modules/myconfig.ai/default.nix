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
    ./programs.qwen-code
    ./programs.vllm.nix
    ./services.litellm.nix
    ./services.llama-swap.nix
    ./services.n8n.nix
    ./services.ollama.nix
    ./services.open-webui.nix
    ./services.searxng.nix
    ./services.tabby.nix
    ./skill.playwright-cli.nix
  ];
  options.myconfig = with lib; {
    ai.enable = mkEnableOption "myconfig.ai";
    ai.localModels = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            name = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Provider alias for the local server instance (defaults to '<host>:<port>')";
            };
            models = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Model names served by this local server instance (defaults to [name] or ['<host>:<port>'])";
            };
            port = mkOption {
              type = types.int;
              description = "Port the local server is listening on";
            };
            host = mkOption {
              type = types.str;
              default = "localhost";
              description = "Host the local server is listening on";
            };
          };
        }
      );
      default = [ ];
      description = "List of local model server instances (e.g. llama-cpp) available for AI tools";
    };
  };
  config = lib.mkIf config.myconfig.ai.enable {
    myconfig.ai.aichat.enable = true;
    myconfig.ai.llm.enable = true;
    myconfig.dev.python.enable = true;
    systemd.tmpfiles.rules = [
      "d /run/myconfig 0755 root root - -"
      (
        let
          localModelsJson = builtins.toJSON config.myconfig.ai.localModels;
        in
        "f /run/myconfig/localModels.json 0644 root root - ${localModelsJson}"
      )
    ];
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
