# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A local "implement-and-review-and-commit-until-done" skill: an iterative
# driver that repeatedly takes the next concrete step toward a high-level goal
# by calling the `implement-and-review-and-commit` skill in a loop until the
# goal is met, producing one small reviewed+committed increment per iteration.
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
  cfg = config.myconfig.ai.skills.implement-and-review-and-commit-until-done;
  skillDir = ./implement-and-review-and-commit-until-done;
in
{
  options.myconfig.ai.skills.implement-and-review-and-commit-until-done = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.implement-and-review-and-commit-until-done";
  };

  config = lib.mkMerge [
    # Enabled by default wherever the skills framework is in use; a host can
    # opt out with
    # `myconfig.ai.skills.implement-and-review-and-commit-until-done.enable = false;`.
    { myconfig.ai.skills.implement-and-review-and-commit-until-done.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      # Register the skill source; `skills/default.nix` applies it to every
      # enabled agent harness via the `handcrafted` registry.
      myconfig.ai.skills.handcrafted.implement-and-review-and-commit-until-done = skillDir;
    })
  ];
}
