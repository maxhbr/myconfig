# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A local "commit" skill that guides the agent through committing the current
# working-tree changes with a well-formed, conventional commit message derived
# from the diff and the repository's existing history.
#
# The skill is registered in the central `myconfig.ai.dev.skills.handcrafted`
# registry; `skills/default.nix` deploys it to every enabled agent harness
# (opencode, claude-code, codex, and pi-coding-agent).
#
# The skill's attribution-trailer logic lives in the standalone CLI
# `compute-commit-attribution-trailer.sh` next to the SKILL.md: the raw
# script is synced together with the skill, and the wrapped `writeShellApplication`
# build (jq on its PATH) is installed on the host and in every sandbox tier via
# `myconfig.ai.dev.sandboxTools.extraPackages`, so `compute-commit-attribution-trailer.sh`
# works wherever an agent runs.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.skills.commit;
  skillDir = ./commit;

  trailerScript = pkgs.writeShellApplication {
    name = "compute-commit-attribution-trailer.sh";
    runtimeInputs = [ pkgs.jq ];
    text = builtins.readFile (skillDir + "/compute-commit-attribution-trailer.sh");
  };
in
{
  options.myconfig.ai.dev.skills.commit = with lib; {
    enable = mkEnableOption "myconfig.ai.dev.skills.commit";
  };

  config = lib.mkMerge [
    # Enabled by default wherever the skills framework is in use; a host can
    # opt out with `myconfig.ai.dev.skills.commit.enable = false;`.
    { myconfig.ai.dev.skills.commit.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      # Register the skill source; `skills/default.nix` applies it to every
      # enabled agent harness via the `handcrafted` registry.
      myconfig.ai.dev.skills.handcrafted.commit = skillDir;
      # Also expose the same workflow as a pi prompt template (`/commit`),
      # deployed to `~/.pi/agent/prompts/commit.md` by `skills/default.nix`.
      myconfig.ai.dev.skills.handcraftedPrompts.commit = ./commit/prompt.md;

      # The trailer CLI: on PATH on the host ...
      home-manager.sharedModules = [
        {
          home.packages = [ trailerScript ];
        }
      ];
      # ... and in every sandbox tier, so agents that commit from inside a
      # sandbox resolve the same script (with jq) as on the host.
      myconfig.ai.dev.sandboxTools.extraPackages = [ trailerScript ];
    })
  ];
}
