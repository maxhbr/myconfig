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
    ./ai-coding.nix
    ./services.ollama.nix
    ./services.open-webui.nix
    ./services.tabby.nix
    ./container.open-webui.nix
    ./container.nlm-ingestor.nix
    ./container.SillyTavern.nix
    ./container.litellm.nix
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
  ];
  options.myconfig = with lib; {
    ai.enable = mkEnableOption "myconfig.ai";
  };
  config = lib.mkIf config.myconfig.ai.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          aichat
          (callLib ./fns/sandboxed-app.nix {
            name = "fish";
            pkg = fish;
          })
          (callLib ./fns/sandboxed-app.nix {
            name = "bash";
            pkg = bash;
          })
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
