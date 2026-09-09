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
in
{
  # NOTE: the AI *developer tooling* part of the former umbrella imports
  # (agent CLI programs, sandbox tiers, mysbx, workmux, skills,
  # hermes-agent) moved to ../myconfig.ai.dev/default.nix (bd: myconfig-e4j).
  imports = [
    ./myconfig.localModels.nix
    ./myconfig.ai.pull_models.nix
    ./myconfig.ai.llama-cpp
    ./comfyui.nix
    ./container.Kokoro-FastAPI.nix
    ./container.crawl4ai.nix
    ./container.headroom.nix
    ./container.lobe-chat.nix
    ./container.nlm-ingestor.nix
    ./container.open-webui.nix
    ./services.litellm.nix
    ./litellm.proxy.nix
    ./services.open-webui.nix
    ./services.orca.nix
    ./services.searxng.nix
    ./services.tabby.nix
  ];
  options.myconfig.ai.enable = lib.mkEnableOption "myconfig.ai";
  config = lib.mkIf config.myconfig.ai.enable {
    myconfig.ai.dev.aichat.enable = true;
    myconfig.ai.dev.llm.enable = true;
    services.udev.extraRules = ''
      SUBSYSTEM=="accel", GROUP="render", MODE="0660"
    '';
    users.users."${user}" = {
      extraGroups = [ "render" ];
    };
  };
}
