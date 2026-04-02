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
    ./programs.aichat.nix
    ./programs.mcp.servers.nix
    ./programs.llm.nix
    ./services.ollama.nix
    ./services.open-webui.nix
    ./services.tabby.nix
    ./services.llama-server.nix
    ./container.open-webui.nix
    ./container.nlm-ingestor.nix
    ./container.SillyTavern.nix
    ./container.lobe-chat.nix
    ./container.Kokoro-FastAPI.nix
    ./inference.cpp.nix
    ./programs.lmstudio.nix
    ./programs.alpaca.nix
    ./programs.vllm.nix
    ./programs.opencode
    ./programs.codex
    ./programs.qwen-code
    ./programs.claude-code
    ./programs.github-copilot-cli
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
