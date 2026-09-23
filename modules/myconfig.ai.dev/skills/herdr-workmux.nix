# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Dispatcher skill for parallel worktree agents when the caller is already
# inside a Herdr pane. workmux cannot open tmux windows there; this skill
# uses `herdr worktree` and `herdr agent` instead.
#
# Registered in `myconfig.ai.dev.skills.handcrafted`. `skills/default.nix`
# deploys it to every enabled agent harness. The skill itself refuses to
# run unless `HERDR_ENV=1`.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.dev.skills.herdr-workmux;
  skillDir = ./herdr-workmux;
in
{
  options.myconfig.ai.dev.skills.herdr-workmux = with lib; {
    enable = mkEnableOption "myconfig.ai.dev.skills.herdr-workmux";
  };

  config = lib.mkMerge [
    { myconfig.ai.dev.skills.herdr-workmux.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      myconfig.ai.dev.skills.handcrafted.herdr-workmux = skillDir;
    })
  ];
}
