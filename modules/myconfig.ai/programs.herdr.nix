# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `herdr` is an "Agent multiplexer that lives in your terminal"
# (https://herdr.dev, nixpkgs `legacyPackages.x86_64-linux.herdr`). It is a
# companion to the agentic coding harnesses, so it is installed whenever at
# least one agentic coding agent is enabled on this host. The harness set
# mirrors `./skills/default.nix` (opencode, codex, claude-code,
# pi-coding-agent) and is extended with the remaining agentic terminal coding
# agents (`qwen-code`, `github-copilot-cli`).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  agenticCodingEnabled =
    (config.myconfig.ai.claude-code.enable or false)
    || (config.myconfig.ai.codex.enable or false)
    || (config.myconfig.ai.opencode.enable or false)
    || (config.myconfig.ai.pi-coding-agent.enable or false)
    || (config.myconfig.ai.qwen-code.enable or false)
    || (config.myconfig.ai.github-copilot-cli.enable or false);
in
{
  config = lib.mkIf agenticCodingEnabled {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [ herdr ];
      }
    ];
  };
}
