# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The `/bro` skill (https://github.com/luchasarie/bro-skill): re-explain the
# previous assistant message in plain language, for when the reply was too
# dense or jargon-heavy. The skill source is fetched via nvfetcher (see
# `nvfetcher.toml` and `_sources/generated.nix`).
#
# Upstream keeps `SKILL.md` at the repository root next to a README, an
# installer script and examples, but the `handcrafted` registry expects a
# directory containing *only* the skill (it is symlinked/rsynced verbatim
# into each harness' skills directory). Hence the small `runCommand` below
# that extracts just `SKILL.md` into its own directory.
#
# The skill is registered in the central `myconfig.ai.skills.handcrafted`
# registry; `skills/default.nix` deploys it to every enabled agent harness
# (opencode, claude-code, codex, and pi-coding-agent). It is additionally
# registered as a pi prompt template so `/bro` is typable in pi.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.skills.bro;
  # `rev`+`hash` pin lives in the nvfetcher-generated `_sources/generated.nix`
  # (bumped by `nix run nixpkgs#nvfetcher` / a scheduled CI job), not in
  # flake.lock. See ../../../nvfetcher.toml.
  bro-skill = (pkgs.callPackage ../../../_sources/generated.nix { }).bro-skill.src;

  skillDir = pkgs.runCommand "bro-skill" { } ''
    mkdir -p $out
    cp ${bro-skill}/SKILL.md $out/SKILL.md
  '';

  # pi prompt template: same body, but with pi's `description:` frontmatter
  # (pi shows it in the `/`-command list) instead of the Agent-Skills
  # frontmatter. The body is everything after the upstream frontmatter block.
  promptFile = pkgs.runCommand "bro-prompt.md" { } ''
    {
      echo '---'
      echo 'description: Re-explain the last answer in plain language ("bro what" mode)'
      echo '---'
      awk 'BEGIN{n=0} /^---[ \t]*$/{n++; next} n>=2{print}' ${bro-skill}/SKILL.md
    } > $out
  '';
in
{
  options.myconfig.ai.skills.bro = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.bro";
  };

  config = lib.mkMerge [
    # Enabled by default wherever the skills framework is in use; a host can
    # opt out with `myconfig.ai.skills.bro.enable = false;`.
    { myconfig.ai.skills.bro.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      # Register the skill source; `skills/default.nix` applies it to every
      # enabled agent harness via the `handcrafted` registry.
      myconfig.ai.skills.handcrafted.bro = skillDir;
      # Also expose it as a pi prompt template (`/bro`), deployed to
      # `~/.pi/agent/prompts/bro.md` by `skills/default.nix`.
      myconfig.ai.skills.handcraftedPrompts.bro = promptFile;
    })
  ];
}
