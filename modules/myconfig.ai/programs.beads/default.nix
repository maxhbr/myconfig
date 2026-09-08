# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# beads (https://github.com/gastownhall/beads) — a lightweight memory system
# for AI coding agents with graph-based issue tracking. The tool is
# packaged in nixpkgs (`pkgs/by-name/be/beads/package.nix`, a Go module), so
# no extra flake input is needed: this module just consumes `pkgs.beads`.
#
# beads needs no account, no auth token and no `init` run: it works out of
# the box against the current repository, and a missing
# `~/.config/beads/config.toml` simply means "all defaults". Nothing in this
# module ever executes `beads`; every artefact is a plain home-manager file:
#
#   * `~/.config/beads/config.toml` — the user config, rendered from `settings`
#     (see https://github.com/gastownhall/beads#configuration).
#   * the binary on the PATH of every sandbox tier — via the shared
#     `myconfig.ai.sandboxTools.extraPackages` and, for mysbx, its
#     `extraTools` plus a read-only mount of the config above.
#
# Like `rtk` and `hunk`, this module is auto-enabled by the `myconfig.ai`
# umbrella (`myconfig.ai.beads.enable = lib.mkDefault true` in ../default.nix):
# persistent memory and issue tracking is part of every agentic coding
# workflow and the cost is one small binary plus a generated config file.
# The `enable` option itself still defaults to false, so a host without
# `myconfig.ai.enable` never gets beads, and a host can opt out with
# `myconfig.ai.beads.enable = false;`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.beads;

  tomlFormat = pkgs.formats.toml { };

  # bd phones home anonymous usage metrics (command names + version + OS,
  # keyed by a machine-derived HMAC ID) to gastownhall-eventsapi.com by
  # default (`bd metrics off` only writes per-machine state in
  # `~/.config/bd/config.yaml`, which sandboxes and fresh hosts don't
  # inherit). `BD_DISABLE_METRICS=1` is bd's hard kill switch: it wins
  # over that per-machine config, so baking it into the wrapper makes the
  # opt-out declarative and identical on every host and in every sandbox
  # tier below — the same store path flows into all of them.
  # Two things to note about the nixpkgs expression
  # (pkgs/by-name/be/beads/package.nix):
  #   * it already wraps `$out/bin/bd` with `wrapProgram` (adding dolt to
  #     PATH), so `postInstall` is the right hook to extend;
  #   * `postInstall` is a plain string there, so string-concatenating a
  #     second `wrapProgram` call appends a new wrapper variable without
  #     touching the existing one (`--set-default` keeps a user-set
  #     `BD_DISABLE_METRICS=0` from the interactive shell working).
  package = cfg.package.overrideAttrs (prev: {
    postInstall = (prev.postInstall or "") + ''
      wrapProgram $out/bin/bd \
        --set-default BD_DISABLE_METRICS 1
    '';
  });
in
{
  options.myconfig = with lib; {
    ai.beads = {
      enable = mkEnableOption "myconfig.ai.beads";

      package = mkPackageOption pkgs "beads" { };

      settings = mkOption {
        type = tomlFormat.type;
        default = { };
        example = literalExpression ''
          {
            graph_dir = ".beads";
            auto_commit = true;
          }
        '';
        description = ''
          Content of `~/.config/beads/config.toml`
          (https://github.com/gastownhall/beads#configuration). Merged on top
          of the defaults set in `config` below. Keys left out keep beads's
          built-in defaults, and a per-repository `.beads/config.toml` still
          wins over this file.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    myconfig.ai.beads.settings = {
      # Default configuration for beads. Users can override these in their
      # own config files or via per-repository settings.
      graph_dir = ".beads";
      auto_commit = true;
    };

    # Sandbox tiers: beads is what the agent uses to maintain persistent
    # memory and track issues across sessions, so it has to exist where the
    # agent runs — inside the sandboxes, not only on the host. Like `hunk`,
    # beads follows this module's enable gate, the same way the agent CLIs
    # are gated: a host without beads keeps its sandbox closures unchanged.
    #
    # `myconfig.ai.sandboxTools.extraPackages` reaches every tier that
    # consumes the shared list (the `agent-bubblewrap-*`/nono jails, the
    # `myconfig.ai.microvm` guests, the `sandboxed-*` qemu runners and the
    # gVisor image); mysbx has its own `extraTools` extension point.
    myconfig.ai.sandboxTools.extraPackages = [ package ];

    myconfig.ai.mysbx = lib.mkIf config.myconfig.ai.mysbx.enable {
      extraTools = [ package ];
      config.mounts = [
        {
          # Always present while this module is enabled: `settings` above is
          # non-empty, so home-manager always writes the config file (a
          # missing mount source is a hard error on every mysbx run).
          # beads needs nothing else — no auth, no state, no `init` run.
          path = "~/.config/beads";
          dest = "/mysbx-home/.config/beads";
          mode = "ro";
        }
      ];
    };

    home-manager.sharedModules = [
      {
        home.packages = [ package ];

        xdg.configFile."beads/config.toml".source = tomlFormat.generate "beads-config.toml" cfg.settings;
      }
    ];
  };
}
