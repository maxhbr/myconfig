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

  # --- multiplexer integration (./docs/design/config.md D17) -----------
  #
  # One entry script per selectable multiplexer, pinned into the
  # wrapper as `MYSBX_MUX_ENTRY_<VALUE>` (./nix/mysbx.nix). Each runs
  # INSIDE the sandbox and starts its multiplexer on the
  # sandbox-internal socket directory `/mysbx-home/.mysbx-tmux`;
  # nothing about that path is configurable (D16/D17).
  #
  # An entry is built exactly when its package is available on this
  # host — that is what makes a multiplexer *selectable*: a config
  # naming one without a pin is a refused run (D17), never a silent
  # plain shell. `tmux` always is; the others are gated on their
  # package option, which the host (or the tier module next door,
  # ../myconfig.ai.workmux/mysbx.nix) sets.
  muxEntries = {
    tmux = pkgs.callPackage ./nix/tmux-entry.nix { tmux = cfg.tmux.package; };
    workmux =
      if cfg.workmux.package != null then
        pkgs.callPackage ./nix/workmux-entry.nix {
          workmux = cfg.workmux.package;
          tmux = cfg.tmux.package;
        }
      else
        null;
    herdr =
      if cfg.herdr.package != null then
        pkgs.callPackage ./nix/herdr-entry.nix {
          herdr = cfg.herdr.package;
          tmux = cfg.tmux.package;
        }
      else
        null;
    aoe =
      if cfg.aoe.package != null then
        pkgs.callPackage ./nix/aoe-entry.nix {
          aoe = cfg.aoe.package;
          tmux = cfg.tmux.package;
        }
      else
        null;
  };

  # The binaries the SELECTED multiplexer's panes need on the sandbox
  # `PATH` (`extraTools`). The entry scripts carry their own closure,
  # so this is not about starting the session — it is about a pane that
  # runs the tool itself: `workmux set-window-status` from the sidebar,
  # a plain `tmux` in a pane, `herdr pane …` from an agent. Only the
  # selected one is added: an unselected multiplexer on the PATH of
  # every sandbox would be closure (and attack surface) nobody asked
  # for.
  #
  # `filter (p: p != null)`: an unavailable selection is caught by the
  # assertion below, but the list is evaluated by the module system
  # regardless of assertion order — a `null` package in `home.packages`
  # would fail with a type error instead of the assertion's message.
  selectedMuxTools = builtins.filter (p: p != null) (
    {
      none = [ ];
      tmux = [ cfg.tmux.package ];
      # tmux too: the workmux panes' dashboard/sidebar drive the same
      # server the entry started.
      workmux = [
        cfg.workmux.package
        cfg.tmux.package
      ];
      herdr = [ cfg.herdr.package ];
      # `aoe` is a tmux front end, so a pane may reach for `tmux`.
      aoe = [
        cfg.aoe.package
        cfg.tmux.package
      ];
    }
    .${cfg.config.multiplexer}
  );

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
  # layer like every other agent module's (../programs.opencode).
  # Gated on the PACKAGE, not on `workmux.enable`: what makes the
  # in-sandbox configuration necessary is that a workmux session can
  # run at all — a host may install the payload and leave the
  # host-wide `config.multiplexer` at something else, and a repository
  # sidecar may still select `"workmux"` (D17). All
  # `ro`, all store paths (which always exist, so the eager
  # canonicalization of D8 cannot fail), with a `dest` where the tool
  # actually looks: `HOME` is `/mysbx-home` in the sandbox (D14).
  workmuxMounts = lib.optionals (cfg.workmux.package != null) (
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
    inherit (cfg.config) network multiplexer;
    mounts = map renderMount cfg.config.mounts;
    env = cfg.config.env;
  }
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
      # MYSBX_BWRAP / MYSBX_SHELL / MYSBX_TOOLS_PATH / MYSBX_TERMINAL
      # pinned to store paths.
      # The unwrapped crate build stays reachable as
      # `<package>.passthru.crate` (used by nix/checks.nix).
      default = pkgs.callPackage ./nix/mysbx.nix {
        inherit (cfg) extraTools;
        inherit muxEntries;
        alacritty = cfg.terminal.package;
      };
      defaultText = literalExpression "pkgs.callPackage ./nix/mysbx.nix { inherit (cfg) extraTools; inherit muxEntries; alacritty = cfg.terminal.package; }";
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

    tmux.package = mkOption {
      type = types.package;
      default = pkgs.tmux;
      defaultText = literalExpression "pkgs.tmux";
      description = ''
        The tmux that runs *inside* the sandbox: the server of the
        `tmux`, `workmux` and `aoe` multiplexers
        (./docs/design/config.md D17), and the `tmux` on the sandbox
        `PATH` when one of them is selected.

        Unlike the other multiplexer packages this is never `null`:
        `multiplexer = "tmux"` is selectable on every host that
        installs mysbx, so its entry is always pinned.
      '';
    };

    herdr.package = mkOption {
      type = types.nullOr types.package;
      default = pkgs.herdr;
      defaultText = literalExpression "pkgs.herdr";
      description = ''
        The herdr package that runs *inside* the sandbox when
        `config.multiplexer = "herdr"` (./docs/design/config.md D17) —
        the mysbx tier of ../programs.herdr.nix, which owns the host
        installation and the bubblewrap-jail tier of the same tool.

        `null` means "this host does not carry herdr": no entry is
        pinned, and a configuration selecting it is a refused run
        instead of a silent plain shell.
      '';
    };

    aoe.package = mkOption {
      type = types.nullOr types.package;
      # Gated on the agent-of-empires module being ENABLED, not merely
      # present: `aoe` is a heavy from-source Rust build from a flake
      # input (../programs.agent-of-empires/), so pulling it into every
      # mysbx host's closure to make a selection possible would be the
      # wrong default. `or null` keeps this module independent of that
      # module's existence, exactly like the workmux tier wiring.
      default =
        if (config.myconfig.ai.agent-of-empires.enable or false) then
          config.myconfig.ai.agent-of-empires.package
        else
          null;
      defaultText = literalExpression "config.myconfig.ai.agent-of-empires.package (when that module is enabled, else null)";
      description = ''
        The Agent of Empires (`aoe`) package that runs *inside* the
        sandbox when `config.multiplexer = "aoe"`
        (./docs/design/config.md D17).

        `null` means "this host does not carry aoe": no entry is
        pinned, and a configuration selecting it is a refused run
        instead of a silent plain shell.
      '';
    };

    terminal = {
      package = mkOption {
        type = types.nullOr types.package;
        # Gated on the desktop the same way the aoe package is gated on
        # its module: `mysbx gui` opens a terminal window
        # (./docs/design/cli.md D15), which needs a graphical session —
        # a headless host carries no alacritty in its closure, and its
        # `mysbx gui` should fail at runtime over the PATH fallback
        # rather than pull a GUI stack into every headless rebuild.
        default = if (config.myconfig.desktop.enable or false) then pkgs.alacritty else null;
        defaultText = literalExpression "pkgs.alacritty (when myconfig.desktop.enable, else null)";
        description = ''
          The terminal emulator `mysbx gui` starts on the HOST
          (./docs/design/cli.md D15) — the window that runs the inner
          `mysbx`, pinned into the wrapper as `MYSBX_TERMINAL`.

          Unlike the multiplexer packages this runs OUTSIDE the
          sandbox, in the graphical session the command was typed in,
          so it is deliberately absent from the dev-tool closure and
          the sandbox argv.

          `null` (the default on hosts without `myconfig.desktop`)
          pins nothing: the unwrapped crate's plain `alacritty` PATH
          lookup applies instead.
        '';
      };
    };

    workmux = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Wire the workmux integration into every sandbox of this user
          (./docs/design/config.md D16/D17, ./docs/design/cli.md D11):
          it fills the `workmux.package` used by the pinned
          `mysbx-workmux-entry` payload, mounts the in-sandbox workmux
          configuration, and makes `"workmux"` the default of
          `config.multiplexer` — so the INTERACTIVE payload of a
          sandbox is a workmux tmux session instead of a plain shell.
          The tmux socket lives inside the sandbox home tmpfs and is
          not configurable, so it can never be shared with a host tmux
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
          multiplexer = mkOption {
            type = types.enum [
              "tmux"
              "workmux"
              "herdr"
              "aoe"
              "none"
            ];
            # `"workmux"` where the integration is wired, the plain
            # shell everywhere else (./docs/design/config.md D17): the
            # behaviour-preserving default — a host that never had a
            # session keeps getting none, and one that had the workmux
            # session keeps it under the new spelling. A host wanting
            # something else simply sets this option; the *sidecar* of
            # a single repository overrides it either way (D17).
            default = if cfg.workmux.enable then "workmux" else "none";
            defaultText = literalExpression ''if config.myconfig.ai.mysbx.workmux.enable then "workmux" else "none"'';
            description = ''
              Which terminal multiplexer the INTERACTIVE payload of
              every sandbox of this user is
              (./docs/design/config.md D17, ./docs/design/cli.md D11):
              `tmux`, `workmux`, `herdr`, `aoe`, or `none` for a plain
              interactive shell. `mysbx run -- CMD` is unaffected — a
              one-shot command starts no session.

              This is the host-wide DEFAULT: a repository's sidecar
              config may name another value (or `none`), and it wins
              — both layers are trusted and the key grants no host
              access (D7/D17).

              The selected multiplexer must be available on this host,
              i.e. its package option must be set
              (`workmux.package`, `herdr.package`, `aoe.package`;
              `tmux` always is). An unavailable selection is an
              evaluation error here, and a refused run for a sidecar
              that names one — never a silently started plain shell.

              Whichever is selected, its socket and state live INSIDE
              the sandbox (`/mysbx-home/.mysbx-tmux`, exported as
              `TMUX_TMPDIR` after `[env]`, so no layer can repoint
              it), never shared with the host or another sandbox.
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
        {
          # Availability of the SELECTED multiplexer, checked at eval
          # time (D17). The runtime refuses an unpinned selection too
          # — that is the guard for a *sidecar* naming one — but a
          # host-wide default nobody can start is a build error: it
          # would break the interactive form of every sandbox of this
          # user, discovered on the first `mysbx`.
          assertion = muxEntries.${cfg.config.multiplexer} or null != null;
          message = ''
            myconfig.ai.mysbx.config.multiplexer is
            "${cfg.config.multiplexer}", but this host carries no
            ${cfg.config.multiplexer} for the sandbox — set
            myconfig.ai.mysbx.${cfg.config.multiplexer}.package (for
            workmux: myconfig.ai.mysbx.workmux.enable, which
            ../myconfig.ai.workmux/mysbx.nix does), or select another
            multiplexer (docs/design/config.md D17).
          '';
        }
      ];

    # Baseline mounts; further definitions (from per-agent modules or the
    # host config) are concatenated onto this list.
    myconfig.ai.mysbx.config.mounts = baselineMounts ++ workmuxMounts;

    # The selected multiplexer's own tooling, on the sandbox PATH: the
    # entries pin their own copies, but a PANE that runs the tool (the
    # workmux sidebar's `workmux set-window-status`, a plain `tmux`, a
    # `herdr pane …` from an agent) resolves it from `PATH`. The agents
    # a multiplexer launches come from the agent modules' own
    # `extraTools`. See `selectedMuxTools` for why only the selected
    # one is added.
    myconfig.ai.mysbx.extraTools = selectedMuxTools;

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
