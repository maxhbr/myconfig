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
# `myconfig.ai.mysbx.config`: the host-wide layer, mounted into every
# sandbox of this user. Per-agent modules (e.g.
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

  # Baseline mounts: common agent-tooling host config, read-only.
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
  # Each entry also carries a `dest` under the sandbox home (review-2
  # item 6). Without one, the mount lands at its host path
  # (`/home/mhuber/.config/git`) — a path that exists nowhere in the
  # sandbox's own view: `HOME` is `/mysbx-home` (./docs/design/config.md
  # D14), so `git` looks in `/mysbx-home/.config/git` and finds nothing,
  # while the argv gains a `/home/…` destination the D14 invariant wants
  # to keep out of in-sandbox paths. Mapping `~/x` to `/mysbx-home/x`
  # keeps the configuration where every tool already looks for it.
  homeDest =
    p:
    if lib.hasPrefix "~/" p then
      "/mysbx-home/" + lib.removePrefix "~/" p
    else
      throw "myconfig.ai.mysbx: homeDest expects a `~/…` path, got `${p}`";

  baselineMounts =
    map
      (p: {
        path = p;
        dest = homeDest p;
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

  # `home-manager.users.mhuber` is only referenced under `mkIf cfg.enable`
  # below (Nix is lazy), and mysbx's own config block already writes
  # `home-manager.users.mhuber.xdg.configFile.…`, so the option path is
  # guaranteed to exist wherever this is evaluated.
  #
  # NOTE: the `~/.config/ripgrep` spelling below (both the mount list and
  # `baselineEnv`) assumes Home Manager's default `xdg.configHome`, i.e.
  # `~/.config`. HM writes its ripgreprc to
  # `${config.home-manager.users.mhuber.xdg.configHome}/ripgrep`; a host
  # that rewrites `xdg.configHome` would need the mount list AND the
  # variable adjusted together — otherwise the variable points at a
  # missing file (a hard `rg` failure). No myconfig host rewrites it;
  # revisit if one ever does.
  hmRipgrep = config.home-manager.users.mhuber.programs.ripgrep;

  # Baseline environment: regenerate inside the sandbox what the host
  # module layer activates through mechanisms other than files.
  #
  # `RIPGREP_CONFIG_PATH` (review-3 item 6): Home Manager's
  # `programs.ripgrep` writes `~/.config/ripgrep/ripgreprc` and points
  # `RIPGREP_CONFIG_PATH` at it — the file is mounted above, but the
  # VARIABLE is not in the forwarding allowlist (lib.rs), so `--clearenv`
  # kills it and `rg` inside the sandbox silently runs with defaults.
  # The same in-sandbox path Home Manager would compute is pinned here:
  # `homeDest` maps `~` to `/mysbx-home`, which is where the mount puts
  # the file. An [env] entry is the mysbx-native way to set it (config.md
  # D6); it is part of the user layer, so a sidecar may not override it
  # (D7) and the user may. Set exactly when Home Manager would write the
  # file AND the variable (`enable` + non-empty `arguments`): a variable
  # pointing at a missing file is a hard `rg` failure, and mounting the
  # directory alone does not guarantee the file.
  baselineEnv = lib.optionalAttrs (hmRipgrep.enable && hmRipgrep.arguments != [ ]) {
    RIPGREP_CONFIG_PATH = homeDest "~/.config/ripgrep/ripgreprc";
  };

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
      default = pkgs.callPackage ./nix/mysbx.nix { inherit (cfg) extraTools; };
      defaultText = literalExpression "pkgs.callPackage ./nix/mysbx.nix { inherit (cfg) extraTools; }";
      description = ''
        The `mysbx` package to install (built from ./mysbx-rs in this repo).
      '';
    };

    extraTools = mkOption {
      type = types.listOf types.package;
      default = [ ];
      example = literalExpression "[ pkgs.pi-coding-agent ]";
      description = ''
        Extra packages appended to the dev-tool closure that is baked
        into the sandbox `PATH` (`toolsEnv` in ./nix/mysbx.nix). This is
        the extension point for the agent modules that integrate with
        mysbx — ../programs.pi-coding-agent adds the `pi` binary here so
        it is callable inside every sandbox of this host.

        Anything listed here is on the PATH of every mysbx payload, so
        the same "security-relevant list, not packaging detail" rule as
        for the baseline closure applies.
      '';
    };

    config = mkOption {
      description = ''
        Content of the mysbx *user* configuration layer, generated into
        `~/.config/mysbx/config.toml` (./docs/design/config.md D6).

        Its `mounts` are mounted in every sandbox of this user. They
        do not bound what a repo sidecar may mount: the sidecar is a
        trusted layer too and declares its own mounts (D7); the two
        lists concatenate, user layer first. Modules may append to
        `mounts` — list definitions
        are merged by concatenation, so per-agent modules can add their
        own config files without replacing the baseline.

        Give every entry a `dest` under `/mysbx-home` unless the host
        path is meaningful inside the sandbox as well: `HOME` is
        `/mysbx-home` there (D14), so a config directory bound at its
        host path is invisible to the tools that want it.
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
              Host paths mounted into the sandbox. Each path is absolute,
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
              untrusted content (D3), so it approves nothing by itself:
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
    # The `/mysbx-home` invariant of ./docs/design/config.md D14, made
    # checkable at eval time (review-2 item 6): no in-sandbox path may
    # mirror a host home path, and a mount written `~/…` without a
    # `dest` would do exactly that — it lands at its host path, which
    # inside the sandbox is both invisible (HOME is `/mysbx-home`) and
    # indistinguishable from a real host home.
    assertions =
      let
        # Lexically resolve `.` and `..` components of an absolute
        # path — the same job the CLI's `normalize()` does, so a
        # `dest` like `/x/../home/user` is seen as `/home/user`
        # instead of slipping past a prefix check (review-3 item 4).
        # Eval-time Nix cannot canonicalize against the host tree;
        # lexical is the strongest available, and a `dest` with a
        # symlink in it is the runtime layer's problem (D8: the
        # runtime canonicalizes mount sources, and dest rules are
        # guarded in the argv builder).
        normalizePath =
          path:
          let
            parts = lib.splitString "/" path;
            step =
              acc: part:
              if part == "" || part == "." then
                acc
              else if part == ".." then
                # `lib.init []` throws; "/.." and friends would abort
                # the whole eval instead of failing the assertion, so
                # clamp at the root (a `..` with nothing above it
                # resolves to `/` itself).
                if acc == [ ] then acc else lib.init acc
              else
                acc ++ [ part ];
          in
          "/" + lib.concatStringsSep "/" (builtins.foldl' step [ ] parts);
        # What the mount actually lands on inside the sandbox: the
        # `dest` when given, otherwise the host path itself — in every
        # spelling D8 allows (absolute, `~/…`, or relative to the
        # generated config's own directory, `~/.config/mysbx`).
        effectiveDest = m: if m.dest != null then m.dest else m.path;
        # True when that in-sandbox path lands inside the host home,
        # however it is written: `~`/`~/…` expand there, an absolute
        # `/home/…` is one already (normalized first), and a relative
        # path resolves against `~/.config/mysbx/`, so it is one too.
        landsInHostHome =
          d:
          let
            nd = normalizePath d;
          in
          lib.hasPrefix "/home/" nd || d == "~" || lib.hasPrefix "~/" d || !(lib.hasPrefix "/" d);
        offenders = builtins.filter (m: landsInHostHome (effectiveDest m)) cfg.config.mounts;
      in
      [
        {
          assertion = offenders == [ ];
          message = ''
            myconfig.ai.mysbx.config.mounts: these entries end up at an
            in-sandbox path inside the host home, which the sandbox
            deliberately does not have — `HOME` is `/mysbx-home`
            (docs/design/config.md D14), so nothing looks for them there:
              ${lib.concatMapStringsSep "\n  " (m: m.path) offenders}
            Give each of them an explicit `dest` below `/mysbx-home`.
          '';
        }
      ];

    # Baseline mounts; further definitions (from per-agent modules or the
    # host config) are concatenated onto this list.
    myconfig.ai.mysbx.config.mounts = baselineMounts;

    # Baseline [env] (RIPGREP_CONFIG_PATH, review-3 item 6); per-agent
    # modules and the host config may extend it — attrset merge is by
    # key, so a later definition of the same key REPLACES the baseline
    # (visible in the generated file, unlike list concatenation).
    myconfig.ai.mysbx.config.env = baselineEnv;

    home-manager.sharedModules = [
      { home.packages = [ cfg.package ]; }
    ];

    # The generated user config is the host-wide layer of `mhuber`, so it
    # is written for that user only — not via `sharedModules`: an agent
    # user would expand the same `~/...` paths against its own home,
    # mounting paths that were never reviewed for it.
    home-manager.users.mhuber = {
      xdg.configFile."mysbx/config.toml".source = tomlFormat.generate "mysbx-config.toml" userConfigToml;
    };
  };
}
