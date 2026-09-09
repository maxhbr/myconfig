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
# `~/.config/beads/config.toml` simply means "all defaults".
#
# The agent skill (bd's `beads` skill: when to use bd, `bd prime` as the
# workflow SSOT, claim/close/dependency etiquette) is generated at build
# time from the installed binary — same idiom as the herdr skill in
# ../programs.herdr.nix — by running `bd setup codex` in a scratch git repo
# and copying out `.agents/skills/beads/`. This keeps the skill in sync
# with the CLI version instead of being a stale vendored copy. It ships
# upstream alongside an `agents/openai.yaml` interface manifest, which is
# preserved. Registered in `myconfig.ai.dev.skills.handcrafted`, which
# ../skills/default.nix deploys to every enabled agent harness (opencode,
# claude-code, codex, pi via the shared `~/.agents/skills/`); implicitly
# enabled by this module — there is no separate enable flag.
#
# Like `rtk` and `hunk`, this module is auto-enabled by the `myconfig.ai`
# umbrella (`myconfig.ai.dev.beads.enable = lib.mkDefault true` in ../default.nix):
# persistent memory and issue tracking is part of every agentic coding
# workflow and the cost is one small binary plus a generated config file.
# The `enable` option itself still defaults to false, so a host without
# `myconfig.ai.enable` never gets beads, and a host can opt out with
# `myconfig.ai.dev.beads.enable = false;`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.beads;

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

  # The `beads` agent skill, generated at build time from the wrapped binary
  # (see the header comment): `bd setup codex` is the only code path that
  # materialises the skill; it writes `.agents/skills/beads/` into its
  # working directory, so run it in a throwaway git repo and copy the
  # result out. `--non-interactive`/TTY-less is irrelevant here — setup does
  # not prompt — but `$HOME` is pinned so the build cannot read or write
  # the builder user's real `~/.claude`/`~/.codex`.
  beadsSkillSrc =
    pkgs.runCommand "beads-skill"
      {
        nativeBuildInputs = [
          package
          pkgs.gitMinimal
        ];
        HOME = "/build/home";
      }
      ''
        mkdir -p $HOME $out
        cd "$(mktemp -d)"
        git init -q .
        bd setup codex >/dev/null
        cp -r .agents/skills/beads/. $out/
      '';
in
{
  options.myconfig = with lib; {
    ai.dev.beads = {
      enable = mkEnableOption "myconfig.ai.dev.beads";

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
    # Install the beads skill for every enabled agent harness; string form
    # (the derivation's outPath), same convention as the herdr skill
    # registration in ../programs.herdr.nix.
    myconfig.ai.dev.skills.handcrafted.beads = "${beadsSkillSrc}";

    myconfig.ai.dev.beads.settings = {
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
    # `myconfig.ai.dev.sandboxTools.extraPackages` reaches every tier that
    # consumes the shared list (the `agent-bubblewrap-*`/nono jails, the
    # `myconfig.ai.dev.microvm` guests, the `sandboxed-*` qemu runners and the
    # gVisor image); mysbx has its own `extraTools` extension point.
    myconfig.ai.dev.sandboxTools.extraPackages = [ package ];

    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
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
