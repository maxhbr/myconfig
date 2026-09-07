# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# hunk (https://github.com/modem-dev/hunk) — a review-first terminal diff
# viewer for agent-authored changesets: `hunk diff`, `hunk show`, `hunk log`,
# `hunk patch -`, plus a `hunk pager` mode that can replace git's pager.
#
# The tool is packaged in nixpkgs (`pkgs/by-name/hu/hunk/package.nix`, a
# bun-compiled standalone binary), so no extra flake input is needed: this
# module just consumes `pkgs.hunk`. Upstream also ships its own home-manager
# module (`nix/home-manager.nix` in the hunk flake); it is deliberately NOT
# imported here — its surface (`programs.hunk.*`) is small enough to mirror
# directly, which keeps the deployment inside the `myconfig.ai.*` namespace
# and avoids a second, redundant nixpkgs pin.
#
# hunk needs no account, no auth token and no `init` run: it works out of the
# box against the current repository, and a missing `~/.config/hunk/config.toml`
# simply means "all defaults". Nothing in this module ever executes `hunk`;
# every artefact is a plain home-manager file:
#
#   * `~/.config/hunk/config.toml` — the user config, rendered from `settings`
#     (see https://hunk.dev/docs/reference/config/).
#   * `core.pager = hunk pager` in the user's git config, gated behind
#     `gitIntegration` (off by default: it changes the output of *every*
#     `git diff`/`git show`, which is too invasive to opt in silently).
#   * the `hunk-review` agent skill shipped in the package
#     (`$out/share/skills/hunk/hunk-review`), registered in the handcrafted
#     skill registry of ../skills so every enabled agent harness learns how to
#     drive a live hunk review session.
#
# Default is OFF: hunk is an interactive reviewing tool, wanted per host
# (workstations with a terminal), not on servers — so hosts opt in explicitly
# with `myconfig.ai.hunk.enable = true;`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.hunk;

  tomlFormat = pkgs.formats.toml { };
in
{
  options.myconfig = with lib; {
    ai.hunk = {
      enable = mkEnableOption "myconfig.ai.hunk";

      package = mkPackageOption pkgs "hunk" { };

      gitIntegration = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Set `core.pager = "hunk pager"` in the user's git config, so that
          `git diff` and `git show` open in hunk. Off by default because it
          replaces the output of every git command that pages a diff,
          including in scripts run from an interactive shell.
        '';
      };

      settings = mkOption {
        type = tomlFormat.type;
        default = { };
        example = literalExpression ''
          {
            theme = "github-dark-default";
            mode = "split";
            wrap_lines = true;
          }
        '';
        description = ''
          Content of `~/.config/hunk/config.toml`
          (https://hunk.dev/docs/reference/config/). Merged on top of the
          defaults set in `config` below. Keys left out keep hunk's built-in
          defaults, and a per-repository `.hunk/config.toml` still wins over
          this file.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # The agent-facing half of hunk: the skill upstream tells the user to add
    # via `hunk skill path`. ../skills/default.nix deploys every handcrafted
    # entry to the agent harnesses enabled on this host.
    myconfig.ai.skills.handcrafted.hunk-review = "${cfg.package}/share/skills/hunk/hunk-review";

    myconfig.ai.hunk.settings = {
      # Follow the terminal instead of pinning a palette, and let hunk pick
      # split/stack from the terminal width.
      theme = "auto";
      mode = "auto";
      # Show agent annotations next to the code — the whole point of using
      # hunk together with the coding agents configured in this tree.
      agent_notes = true;
    };

    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];

        xdg.configFile."hunk/config.toml".source = tomlFormat.generate "hunk-config.toml" cfg.settings;

        programs.git.settings.core.pager = lib.mkIf cfg.gitIntegration "hunk pager";
      }
    ];
  };
}
