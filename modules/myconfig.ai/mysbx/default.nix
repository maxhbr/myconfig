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

  # --- workmux integration (./docs/design/config.md D16) ---------------
  #
  # The pieces the sandbox needs when a workmux session is its
  # interactive payload. Which HOSTS get this is decided elsewhere —
  # ../myconfig.ai.workmux/mysbx.nix sets the options below, next to
  # the other workmux tiers (`jail.nix`, `sandbox.nix`) — so this
  # module stays independent of the workmux module's existence.
  #
  # `mysbx-workmux-entry` runs INSIDE the sandbox and is pinned into
  # the wrapper as `MYSBX_WORKMUX_ENTRY` (./nix/mysbx.nix): it boots
  # tmux on the sandbox-internal socket `/mysbx-home/.mysbx-tmux/socket`
  # and attaches. Nothing about that path is configurable (D16).
  workmuxEntry =
    if cfg.workmux.enable then
      pkgs.callPackage ./nix/workmux-entry.nix { workmux = cfg.workmux.package; }
    else
      null;

  # The workmux configuration the sandbox reads. It is deliberately NOT
  # the host's `~/.config/workmux/config.yaml`: there the named agents
  # point at the jailed launchers (`pi-workmux-launch` → `pi-bwrap`),
  # and running one inside this sandbox would start a NESTED sandbox
  # with its own tmpfs home — losing the agent's configuration exactly
  # as it does in the jail tier (../myconfig.ai.workmux/jail.nix). The
  # in-sandbox agents are therefore the plain binaries, chosen by the
  # module that fills `workmux.settings`.
  workmuxConfigFile =
    (pkgs.formats.yaml { }).generate "mysbx-workmux-config.yaml"
      cfg.workmux.settings;

  # Mounts the workmux payload needs, appended to the generated user
  # layer like every other agent module's (../programs.opencode). All
  # `ro`, all store paths (which always exist, so the eager
  # canonicalization of D8 cannot fail), with a `dest` where the tool
  # actually looks: `HOME` is `/mysbx-home` in the sandbox (D14).
  workmuxMounts = lib.optionals cfg.workmux.enable (
    [
      {
        path = "${workmuxConfigFile}";
        dest = "/mysbx-home/.config/workmux/config.yaml";
        mode = "ro";
      }
    ]
    # The host's tmux configuration (`programs.tmux` writes
    # `/etc/tmux.conf`), so the in-sandbox server has the same
    # keybindings and theme. The SOURCE is the store path, not
    # `/etc/tmux.conf`: the base binds no `/etc` beyond `localtime`,
    # and a store path cannot go missing between two runs.
    ++ lib.optional (cfg.workmux.tmuxConf != null) {
      path = "${cfg.workmux.tmuxConf}";
      dest = "/etc/tmux.conf";
      mode = "ro";
    }
  );

  userConfigToml = {
    inherit (cfg.config) network;
    mounts = map renderMount cfg.config.mounts;
    env = cfg.config.env;
  }
  // lib.optionalAttrs cfg.config.workmux { inherit (cfg.config) workmux; }
  // lib.optionalAttrs (cfg.config.backend != null) { inherit (cfg.config) backend; }
  // lib.optionalAttrs (cfg.config.gitDirs != [ ]) { git-dirs = cfg.config.gitDirs; }
  // lib.optionalAttrs (cfg.config.stateDirs != [ ]) { state-dirs = cfg.config.stateDirs; };
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
      default = pkgs.callPackage ./nix/mysbx.nix {
        inherit (cfg) extraTools;
        inherit workmuxEntry;
      };
      defaultText = literalExpression "pkgs.callPackage ./nix/mysbx.nix { inherit (cfg) extraTools; inherit workmuxEntry; }";
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

    workmux = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Make the INTERACTIVE payload of every sandbox of this user a
          workmux tmux session instead of a plain shell
          (./docs/design/config.md D16, ./docs/design/cli.md D11):
          `workmux = true` in the generated user layer, `tmux` and
          `workmux` in the sandbox tool closure, and the
          `mysbx-workmux-entry` payload pinned into the wrapper. The
          tmux socket lives inside the sandbox home tmpfs and is not
          configurable, so it can never be shared with a host tmux
          server or with another sandbox.

          `mysbx run -- CMD` is unaffected — a one-shot command starts
          no session.

          Off here by default: the wiring that turns it on where the
          host runs workmux lives with the other workmux tiers,
          ../myconfig.ai.workmux/mysbx.nix (next to `jail.nix` and
          `sandbox.nix`), which also fills `settings` and `tmuxConf`.
        '';
      };

      package = mkOption {
        type = types.nullOr types.package;
        default = null;
        example = literalExpression "config.myconfig.ai.workmux.package";
        description = ''
          The workmux package that runs *inside* the sandbox (it lands
          on the sandbox `PATH` and in the entry script's closure).
          Required when `enable` is set.
        '';
      };

      settings = mkOption {
        type = (pkgs.formats.yaml { }).type;
        default = { };
        description = ''
          The workmux configuration mounted read-only at
          `/mysbx-home/.config/workmux/config.yaml` — what the
          in-sandbox workmux reads instead of the host's
          `~/.config/workmux/config.yaml`.

          It must name the *plain* agent binaries: the sandbox is
          already the sandbox, and a jailed launcher started in a pane
          would open a nested sandbox with its own tmpfs home, losing
          the agent's configuration (the same rule the bubblewrap jail
          tier follows, ../myconfig.ai.workmux/jail.nix).
        '';
      };

      tmuxConf = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = literalExpression ''config.environment.etc."tmux.conf".source'';
        description = ''
          A tmux configuration bound read-only at `/etc/tmux.conf`, so
          the in-sandbox tmux server shares the host's keybindings and
          theme. `null` leaves the sandbox with tmux defaults.
        '';
      };
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
          workmux = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Make the interactive payload a workmux tmux session
              (./docs/design/config.md D16). Set by
              `myconfig.ai.mysbx.workmux.enable`; only written to the
              generated layer when true, so a host without the
              integration keeps a byte-identical config file.
            '';
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
          stateDirs = mkOption {
            # The eval-time mirror of the CLI's entry validator
            # (mysbx-rs/src/config.rs `state_dir_path`): the same
            # rejected spellings, so a generated layer fails at build
            # time instead of on every run of every sandbox. Kept
            # literal rather than clever — it must be readable next to
            # the Rust function it mirrors.
            type = types.listOf (
              types.addCheck types.str (
                p:
                p != ""
                && !lib.hasPrefix "/" p
                && !lib.hasPrefix "~" p
                && lib.all (c: c != "" && c != "." && c != "..") (lib.splitString "/" p)
              )
            );
            default = [ ];
            example = [ ".local/share/opencode" ];
            description = ''
              State directories (./docs/design/config.md D15): paths
              *relative to the sandbox home* whose content persists
              across runs. mysbx backs each entry with
              `<repo>.mysbx/state/<entry>` (created at run time) and
              binds it rw at `/mysbx-home/<entry>` — a sandboxed agent
              keeps its sessions and caches per repository without any
              host-home path entering the sandbox.

              The entries are NOT host paths (the D8 forms do not
              apply): the host side is derived from the sidecar, the
              entry only shapes the path below the sandbox home.
              Leading `/`, `~/`, empty and `.`/`..` components are
              rejected — here with a cheap eval-time check, and again
              by the CLI's strict parser.

              Per-agent modules append their tool's state directories
              here, exactly like they append config mounts to
              `mounts`.
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
        {
          # The entry script and the sandbox PATH both need the real
          # package; without it the sandbox would ask for a session it
          # cannot start (a run-time refusal, D16 — better caught here).
          assertion = cfg.workmux.enable -> cfg.workmux.package != null;
          message = ''
            myconfig.ai.mysbx.workmux.enable is on but
            myconfig.ai.mysbx.workmux.package is null — set it to the
            workmux package that should run inside the sandbox (on
            myconfig hosts ../myconfig.ai.workmux/mysbx.nix does that).
          '';
        }
      ];

    # Baseline mounts; further definitions (from per-agent modules or the
    # host config) are concatenated onto this list.
    myconfig.ai.mysbx.config.mounts = baselineMounts ++ workmuxMounts;

    # The workmux payload's own tooling, on the sandbox PATH: `workmux`
    # (the panes' dashboard/sidebar call it, e.g. `workmux
    # set-window-status`) and `tmux` (the entry pins its own copy, but a
    # pane running plain `tmux` must find the same binary). The agents
    # workmux launches come from the agent modules' own `extraTools`.
    myconfig.ai.mysbx.extraTools = lib.optionals cfg.workmux.enable [
      cfg.workmux.package
      pkgs.tmux
    ];

    # The generated user layer carries the switch itself (D16): both
    # layers may decide it, and this is the host-wide statement.
    # `mkDefault`, so a host that installs the integration but wants the
    # plain shell host-wide can say so without an eval conflict (a
    # single repository says it in its sidecar instead).
    myconfig.ai.mysbx.config.workmux = lib.mkDefault cfg.workmux.enable;

    # Baseline [env] (RIPGREP_CONFIG_PATH, review-3 item 6).
    #
    # Each baseline value is defined with `mkDefault` INDIVIDUALLY, not
    # the attrset as a whole (review-4 item 4). `env` is an
    # `attrsOf str`, so the module system pushes definitions down per
    # key: with a per-key `mkDefault` a host or per-agent module that
    # sets the SAME key simply wins (default priority loses to normal),
    # while a definition of a DIFFERENT key merges with the baseline.
    # Both other spellings are wrong: at normal priority two unequal
    # definitions of one key are an evaluation CONFLICT (the comment
    # here used to claim they override), and `mkDefault` on the whole
    # attrset would drop the entire baseline as soon as any other
    # module defines any key at all.
    myconfig.ai.mysbx.config.env = lib.mapAttrs (_: lib.mkDefault) baselineEnv;

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
