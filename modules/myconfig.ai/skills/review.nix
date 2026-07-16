# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A pi prompt template (`/review`) that spawns a code-review sub-agent via
# `pi --print` rather than reviewing the code in the current session. The
# sub-agent does the reading; this session only launches it and reports the
# findings. Registered in the `myconfig.ai.skills.handcraftedPrompts`
# registry; `skills/default.nix` deploys it to `~/.pi/agent/prompts/review.md`
# on every host with pi-coding-agent enabled.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.skills.review;
in
{
  options.myconfig.ai.skills.review = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.review";
  };

  config = lib.mkMerge [
    # Enabled by default wherever pi-coding-agent is in use; a host can opt
    # out with `myconfig.ai.skills.review.enable = false;`.
    { myconfig.ai.skills.review.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      # Expose the workflow as a pi prompt template (`/review`), deployed to
      # `~/.pi/agent/prompts/review.md` by `skills/default.nix`.
      myconfig.ai.skills.handcraftedPrompts.review = ./review/prompt.md;
    })
  ];
}
