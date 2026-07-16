# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A local "commit" skill that guides the agent through committing the current
# working-tree changes with a well-formed, conventional commit message derived
# from the diff and the repository's existing history.
#
# The skill is registered in the central `myconfig.ai.skills.handcrafted`
# registry; `skills/default.nix` deploys it to every enabled agent harness
# (opencode, claude-code, codex, and pi-coding-agent).
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.skills.commit;
  skillDir = ./commit;
in
{
  options.myconfig.ai.skills.commit = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.commit";
  };

  config = lib.mkMerge [
    # Enabled by default wherever the skills framework is in use; a host can
    # opt out with `myconfig.ai.skills.commit.enable = false;`.
    { myconfig.ai.skills.commit.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      # Register the skill source; `skills/default.nix` applies it to every
      # enabled agent harness via the `handcrafted` registry.
      myconfig.ai.skills.handcrafted.commit = skillDir;
      # Also expose the same workflow as a pi prompt template (`/commit`),
      # deployed to `~/.pi/agent/prompts/commit.md` by `skills/default.nix`.
      myconfig.ai.skills.handcraftedPrompts.commit = ./commit/prompt.md;
    })
  ];
}
