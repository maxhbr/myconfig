# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# workmux's own Claude-Code-style skills (https://workmux.raine.dev/guide/skills),
# vendored verbatim from the workmux flake input's `skills/` tree — the same
# artefacts `workmux setup --skills` would copy. They teach the agent how to
# drive the workmux CLI and its worktree/merge/rebase/coordinator workflows:
#
#   /workmux     — reference for the workmux CLI (worktrees, status, agents)
#   /merge       — commit, rebase, and merge the current branch
#   /rebase      — rebase with flexible target and smart conflict resolution
#   /worktree    — delegate tasks to parallel worktree agents
#   /coordinator — orchestrate multiple agents with full lifecycle control
#
# Each skill is registered in the central `myconfig.ai.dev.skills.handcrafted`
# registry; `skills/default.nix` then deploys it to every enabled agent
# harness (opencode, claude-code, codex, and pi-coding-agent). Installation
# only makes sense alongside the workmux CLI, so it is gated on
# `myconfig.ai.dev.workmux.enable` and defaults to on there.
{
  config,
  lib,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.skills.workmux;
  workmuxSrc = inputs.workmux;
  # The subset of upstream workmux skills we install. `open-pr` is
  # intentionally omitted (this setup does not use the PR-page workflow).
  skillNames = [
    "workmux"
    "merge"
    "rebase"
    "worktree"
    "coordinator"
  ];
in
{
  options.myconfig.ai.dev.skills.workmux = with lib; {
    enable = mkOption {
      type = types.bool;
      default = config.myconfig.ai.dev.workmux.enable;
      defaultText = literalExpression "config.myconfig.ai.dev.workmux.enable";
      description = ''
        Install workmux's own agent skills (`/workmux`, `/merge`, `/rebase`,
        `/worktree`, `/coordinator`) from the workmux flake input into every
        enabled agent harness. Defaults to on wherever `myconfig.ai.dev.workmux`
        is enabled; set to `false` to opt out.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Register each upstream skill directory (the dir containing its SKILL.md);
    # `skills/default.nix` applies the whole `handcrafted` registry to every
    # enabled agent harness.
    myconfig.ai.dev.skills.handcrafted = lib.genAttrs skillNames (name: "${workmuxSrc}/skills/${name}");
  };
}
