# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A "simple-english" skill that guides the agent in writing or rewriting
# technical text to the rules of ASD-STE100 Simplified Technical English so
# it is clear, unambiguous, and free of AI slop. The skill sources are
# fetched via nvfetcher (see `nvfetcher.toml` and
# `_sources/generated.nix`); only the `skills/simple-english` subdirectory of
# the upstream repo is registered.
#
# The skill is registered in the central `myconfig.ai.skills.handcrafted`
# registry; `skills/default.nix` deploys it to every enabled agent harness
# (opencode, claude-code, codex, and pi-coding-agent).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.skills.simple-english;
  # `rev`+`hash` pin lives in the nvfetcher-generated `_sources/generated.nix`
  # (bumped by `nix run nixpkgs#nvfetcher` / a scheduled CI job), not in
  # flake.lock. See ../../../nvfetcher.toml.
  simple-english = (pkgs.callPackage ../../../_sources/generated.nix { }).simple-english.src;
  skillDir = "${simple-english}/skills/simple-english";
in
{
  options.myconfig.ai.skills.simple-english = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.simple-english";
  };

  config = lib.mkMerge [
    # Enabled by default wherever the skills framework is in use; a host can
    # opt out with `myconfig.ai.skills.simple-english.enable = false;`.
    { myconfig.ai.skills.simple-english.enable = lib.mkDefault true; }
    (lib.mkIf cfg.enable {
      # Register the skill source; `skills/default.nix` applies it to every
      # enabled agent harness via the `handcrafted` registry.
      myconfig.ai.skills.handcrafted.simple-english = skillDir;
    })
  ];
}
