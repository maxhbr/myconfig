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
    ./myconfig.ai.llmops.localModels.nix
    ./myconfig.ai.llmops.pull_models.nix
    ./myconfig.ai.llmops.llama-cpp
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
    ./services.searxng.nix
    ./services.tabby.nix
  ];
  options.myconfig.ai.llmops.enable = lib.mkEnableOption "myconfig.ai.llmops";
  config = lib.mkIf config.myconfig.ai.llmops.enable {
    # NOTE: `aichat` and `llm` are dev tools but historically followed the
    # broad AI flag; they keep following `llmops.enable` here.
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
