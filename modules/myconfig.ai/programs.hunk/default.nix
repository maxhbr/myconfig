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
#   * the binary on the PATH of every sandbox tier — via the shared
#     `myconfig.ai.sandboxTools.extraPackages` and, for mysbx, its
#     `extraTools` plus a read-only mount of the config above.
#
# Like `rtk`, this module is auto-enabled by the `myconfig.ai` umbrella
# (`myconfig.ai.hunk.enable = lib.mkDefault true` in ../default.nix):
# reviewing an agent's changeset is part of every agentic coding workflow and
# the cost is one small binary plus a generated config file. The `enable`
# option itself still defaults to false, so a host without `myconfig.ai
# .enable` never gets hunk, and a host can opt out with
# `myconfig.ai.hunk.enable = false;`.
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

    # Sandbox tiers: hunk is what the human (and the `hunk-review` skill
    # above) uses to review an agent's changeset, so it has to exist where
    # the changeset is produced — inside the sandboxes, not only on the
    # host. Unlike `tig` (unconditional in every tier's baseline toolset),
    # hunk follows this module's enable gate, the same way the agent CLIs
    # are gated: a host without hunk keeps its sandbox closures unchanged.
    #
    # `myconfig.ai.sandboxTools.extraPackages` reaches every tier that
    # consumes the shared list (the `agent-bubblewrap-*`/nono jails, the
    # `myconfig.ai.microvm` guests, the `sandboxed-*` qemu runners and the
    # gVisor image); mysbx has its own `extraTools` extension point (see
    # ../programs.rtk/default.nix for the same pair of hooks).
    myconfig.ai.sandboxTools.extraPackages = [ cfg.package ];

    myconfig.ai.mysbx = lib.mkIf config.myconfig.ai.mysbx.enable {
      extraTools = [ cfg.package ];
      config.mounts = [
        {
          # Always present while this module is enabled: `settings` above is
          # non-empty, so home-manager always writes the config file (a
          # missing mount source is a hard error on every mysbx run).
          # hunk needs nothing else — no auth, no state, no `init` run.
          path = "~/.config/hunk";
          dest = "/mysbx-home/.config/hunk";
          mode = "ro";
        }
      ];
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
