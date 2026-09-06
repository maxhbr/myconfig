# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.mysbx — the `mysbx` sandboxing CLI (see ./README.md).
#
# `mysbx` is the successor experiment to the other sandboxing tiers in this
# repo (`myconfig.ai.jail`, `myconfig.ai.nono-agent-sandbox`,
# `myconfig.ai.gvisor-agent-sandbox`, `myconfig.ai.microvm`): a single CLI
# that owns the sidecar directory next to a repository and drives the
# underlying backend (bubblewrap first, containers/microvm later).
#
# Like the other sandbox tiers, this module is OFF by default and enabled
# explicitly per host — it is never switched on implicitly by the broad
# `myconfig.ai.enable`.
#
# The module also generates the *user* configuration layer
# (`~/.config/mysbx/config.toml`, see ./docs/design/config.md D6) from
# `myconfig.ai.mysbx.config`: the grant layer that pre-approves the host
# paths a sidecar may mount. Per-agent modules (e.g.
# ./../programs.pi-coding-agent/) are expected to extend
# `myconfig.ai.mysbx.config.mounts` with their own agent config files.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.mysbx;

  # Nix has no `builtins.toToml` (only `builtins.fromTOML`), so the
  # nixpkgs TOML generator is the equivalent: it renders `[[mounts]]`
  # arrays of tables and an `[env]` table, which is exactly the subset
  # ./mysbx-rs/src/toml.rs parses.
  tomlFormat = pkgs.formats.toml { };

  # Baseline grants: common agent-tooling host config, read-only.
  #
  # The paths are written with the `~/` prefix: mysbx expands it at run
  # time against the invoking user's `$HOME` (./docs/design/config.md D8),
  # so this module needs no home-directory lookup at all. That expansion
  # happens on the HOST, before bwrap is executed — `--clearenv` does not
  # pass `HOME` into the sandbox, but nothing inside the sandbox ever
  # resolves these paths, so the two facts do not collide.
  #
  # CAUTION: every path here MUST exist at runtime — mysbx canonicalizes
  # eagerly and a missing path is a hard error on *every* run
  # (./docs/design/config.md D8). Only directories this repo itself always
  # manages via home-manager are listed:
  #   - `.config/git`     ← ../../shell.git (unconditional `programs.git`)
  #   - `.config/ripgrep` ← ../../shell.programs.ripgrep.nix
  #   - `.config/bat`     ← ../../shell.programs.bat.nix
  #   - `.config/fish`    ← ../../programs.fish (only when fish is on)
  # Same list as the `configDirs` default of ../fns/bubblewrap-app.nix.
  baselineMounts =
    map
      (p: {
        path = p;
        mode = "ro";
      })
      (
        [
          "~/.config/git"
          "~/.config/ripgrep"
          "~/.config/bat"
        ]
        ++ lib.optional config.programs.fish.enable "~/.config/fish"
      );

  # `dest` is optional in the schema and there is no TOML null: a
  # `dest = null` key would be a type error in the strict parser, so it is
  # dropped instead of rendered.
  renderMount =
    m:
    {
      inherit (m) path mode;
    }
    // lib.optionalAttrs (m.dest != null) { inherit (m) dest; };

  userConfigToml = {
    inherit (cfg.config) network;
    mounts = map renderMount cfg.config.mounts;
    env = cfg.config.env;
  }
  // lib.optionalAttrs (cfg.config.backend != null) { inherit (cfg.config) backend; }
  // lib.optionalAttrs (cfg.config.gitDirs != [ ]) { git-dirs = cfg.config.gitDirs; };
in
{
  options.myconfig.ai.mysbx = with lib; {
    enable = mkEnableOption "myconfig.ai.mysbx";

    package = mkOption {
      type = types.package;
      # The wrapped package from ./nix/mysbx.nix: the crate's binary with
      # MYSBX_BWRAP / MYSBX_SHELL / MYSBX_TOOLS_PATH pinned to store paths.
      # The unwrapped crate build stays reachable as
      # `<package>.passthru.crate` (used by nix/checks.nix).
      default = pkgs.callPackage ./nix/mysbx.nix { };
      defaultText = literalExpression "pkgs.callPackage ./nix/mysbx.nix { }";
      description = ''
        The `mysbx` package to install (built from ./mysbx-rs in this repo).
      '';
    };

    config = mkOption {
      description = ''
        Content of the mysbx *user* configuration layer, generated into
        `~/.config/mysbx/config.toml` (./docs/design/config.md D6).

        This is the GRANT layer: a repo sidecar may only mount host paths
        at or below a path granted here, and may never upgrade `ro` to
        `rw` (D7). Modules may append to `mounts` — list definitions are
        merged by concatenation, so per-agent modules can add their own
        config files without replacing the baseline.
      '';
      default = { };
      type = types.submodule {
        options = {
          backend = mkOption {
            type = types.nullOr (types.enum [ "bubblewrap" ]);
            default = "bubblewrap";
            description = "Sandbox backend; `null` leaves the choice to the sidecar.";
          };
          network = mkOption {
            type = types.bool;
            default = true;
            description = "Share the host network; `false` is the deny switch.";
          };
          mounts = mkOption {
            description = ''
              Host paths granted into the sandbox. Each path is absolute,
              `~/...` (expanded against the invoking user's `$HOME` at run
              time) or relative to the generated config file's directory
              (`~/.config/mysbx/`) — ./docs/design/config.md D8.
            '';
            default = [ ];
            type = types.listOf (
              types.submodule {
                options = {
                  path = mkOption {
                    # No absolute-path check: `~/...` and paths relative to
                    # `~/.config/mysbx/` are valid too (D8). Only the empty
                    # string is rejected here — the Rust parser rejects it
                    # as well, but failing at eval time is cheaper.
                    type = types.addCheck types.str (p: p != "");
                    description = ''
                      Host path: absolute, `~/...` or relative to
                      `~/.config/mysbx/`. It must exist at run time — it is
                      canonicalized eagerly.
                    '';
                  };
                  mode = mkOption {
                    type = types.enum [
                      "ro"
                      "rw"
                    ];
                    default = "ro";
                    description = "Granted access mode.";
                  };
                  dest = mkOption {
                    type = types.nullOr (types.addCheck types.str (lib.hasPrefix "/"));
                    default = null;
                    # `dest` stays absolute-only: it is a path inside the
                    # sandbox, where neither `~/` nor "relative to the
                    # config file" means anything (D8).
                    description = "Absolute in-sandbox destination; `null` means the same path.";
                  };
                };
              }
            );
          };
          env = mkOption {
            type = types.attrsOf types.str;
            default = { };
            description = "Environment variables forwarded into the sandbox.";
          };
          gitDirs = mkOption {
            type = types.listOf (types.addCheck types.str (p: p != ""));
            default = [ ];
            example = [ "~/myconfig/myconfig/.git" ];
            description = ''
              Host directories approved as *external git metadata*: the
              targets a repository's `.git` FILE may point at when the
              repo is a linked worktree or a submodule
              (./docs/design/config.md, review-2 item 1).

              The `.git` file lives inside the repo and is therefore
              untrusted content (D3), so it grants nothing by itself:
              mysbx binds the metadata only when the resolved target is
              at or below an entry approved here or in the repo's
              sidecar — `mysbx init` records what it finds into a fresh
              sidecar, so this host-wide list is only needed to
              pre-approve checkout roots (e.g. `~/myconfig`).

              Entries take the same three forms as mount paths (D8) and
              must exist at run time: they are canonicalized eagerly.
            '';
          };
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Baseline grants; further definitions (from per-agent modules or the
    # host config) are concatenated onto this list.
    myconfig.ai.mysbx.config.mounts = baselineMounts;

    home-manager.sharedModules = [
      { home.packages = [ cfg.package ]; }
    ];

    # The generated user config is the grant layer of `mhuber`, so it is
    # written for that user only — not via `sharedModules`: an agent user
    # would expand the same `~/...` paths against its own home, granting
    # paths that were never reviewed for it.
    home-manager.users.mhuber = {
      xdg.configFile."mysbx/config.toml".source = tomlFormat.generate "mysbx-config.toml" userConfigToml;
    };
  };
}
