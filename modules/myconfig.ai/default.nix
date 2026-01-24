# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
let
  nixpkgsConfig = config.nixpkgs.config;
in
{
  imports = [
    ./ai-coding.nix
    ./services.ollama.nix
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
    ./programs.opencode
  ];
  options.myconfig = with lib; {
    ai.enable = mkEnableOption "myconfig.ai";
  };
  config = lib.mkIf config.myconfig.ai.enable {
    home-manager.sharedModules = [ { home.packages = with pkgs; [ aichat ]; } ];
  };
}
