# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A local "commit" skill installed into every enabled agent harness that
# exposes a `programs.<harness>.skills` attrset (opencode, claude-code,
# codex). The skill guides the agent through committing the current
# working-tree changes with a well-formed, conventional commit message
# derived from the diff and the repository's existing history.
#
# Note: pi-coding-agent has no `programs.pi-coding-agent.skills` option
# (it only consumes skills via the agent-skills sync framework in
# skills/default.nix), so it is not wired here -- mirroring the
# grafana-core and playwright-cli skills.
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
      home-manager.sharedModules = [
        {
          programs.opencode.skills.commit = skillDir;
          programs.claude-code.skills.commit = skillDir;
          programs.codex.skills.commit = skillDir;
        }
      ];
    })
  ];
}
