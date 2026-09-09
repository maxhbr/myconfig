# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A pi sub-agent definition (`research`) plus its `/research` prompt
# template. The agent is deployed to `~/.pi/agent/agents/research.md`, where
# the subagent extension discovers agents; the prompt is deployed to
# `~/.pi/agent/prompts/research.md`, reachable as the `/research` command.
# The research agent does read-only codebase investigation and returns a
# structured report for handoff. Unlike the upstream sample agents (whose
# `model:` line is stripped), this handcrafted agent keeps its `model:`
# frontmatter so research sub-agents run on a fast/cheap model. Registered
# in the `myconfig.ai.skills.handcraftedAgents` and `handcraftedPrompts`
# registries; `skills/default.nix` deploys both on every host with
# pi-coding-agent enabled.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.skills.research;
in
{
  options.myconfig.ai.skills.research = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.research";
  };

  config = lib.mkMerge [
    { myconfig.ai.skills.research.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      myconfig.ai.skills.handcraftedAgents.research = ./research/agent.md;
      # Expose the workflow as a pi prompt template (`/research`), deployed
      # to `~/.pi/agent/prompts/research.md` by `skills/default.nix`.
      myconfig.ai.skills.handcraftedPrompts.research = ./research/prompt.md;
    })
  ];
}
