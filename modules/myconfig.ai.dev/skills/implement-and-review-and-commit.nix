# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A local "implement-and-review-and-commit" skill: a thin orchestrator that
# runs one full unit-of-work cycle for a task — implement, review (reusing the
# code-review skill / a focused self-review), fix, then commit via the local
# `commit` skill.
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
  cfg = config.myconfig.ai.skills.implement-and-review-and-commit;
  skillDir = ./implement-and-review-and-commit;
in
{
  options.myconfig.ai.skills.implement-and-review-and-commit = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.implement-and-review-and-commit";
  };

  config = lib.mkMerge [
    # Enabled by default wherever the skills framework is in use; a host can
    # opt out with `myconfig.ai.skills.implement-and-review-and-commit.enable = false;`.
    { myconfig.ai.skills.implement-and-review-and-commit.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      # Register the skill source; `skills/default.nix` applies it to every
      # enabled agent harness via the `handcrafted` registry.
      myconfig.ai.skills.handcrafted.implement-and-review-and-commit = skillDir;
    })
  ];
}
