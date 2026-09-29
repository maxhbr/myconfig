# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A local "merge-worktree" skill: commit, rebase onto the local base branch,
# fast-forward the base and clean up a linked worktree in the herdr/workmux
# `<repo>__worktrees/<name>` layout, using plain git and the optional herdr
# CLI instead of `workmux merge`.
#
# Named `merge-worktree` because `workmux.nix` already registers `merge`.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.dev.skills.merge-worktree;
  skillDir = ./merge-worktree;
in
{
  options.myconfig.ai.dev.skills.merge-worktree = with lib; {
    enable = mkEnableOption "myconfig.ai.dev.skills.merge-worktree";
  };

  config = lib.mkMerge [
    { myconfig.ai.dev.skills.merge-worktree.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      myconfig.ai.dev.skills.handcrafted.merge-worktree = skillDir;
    })
  ];
}
