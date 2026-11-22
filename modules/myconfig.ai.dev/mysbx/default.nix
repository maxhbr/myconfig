# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.mysbx — the `mysbx` sandboxing CLI (see ./README.md).
#
# `mysbx` is the successor experiment to the other sandboxing tiers in this
# repo (`myconfig.ai.dev.jail`, `myconfig.ai.dev.nono-agent-sandbox`,
# `myconfig.ai.dev.microvm`): a single CLI
# that owns the sidecar directory next to a repository and drives the
# underlying backend (bubblewrap by default, podman-gvisor and nono
# alongside; qemu/microvm later).
#
# Like the other sandbox tiers, this module is OFF by default and enabled
# explicitly per host — it is never switched on implicitly by the broad
# `myconfig.ai.llmops.enable`.
#
# The module also generates the *user* configuration layer
# (`~/.config/mysbx/config.toml`, see ./docs/design/config.md D6) from
# `myconfig.ai.dev.mysbx.config`: the host-wide layer, mounted into every
# sandbox of this user. Per-agent modules (e.g.
# ../programs/programs.pi-coding-agent/) are expected to extend
# `myconfig.ai.dev.mysbx.config.mounts` with their own agent config files.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.ai.dev.mysbx;

  # Nix has no `builtins.toToml` (only `builtins.fromTOML`), so the
  # nixpkgs TOML generator is the equivalent: it renders `[[mounts]]`
  # arrays of tables and an `[env]` table, which is exactly the subset
  # ./mysbx-rs/src/toml.rs parses.
  tomlFormat = pkgs.formats.toml { };

  # The words an agent KIND may not reuse (bd myconfig-fv4h): every
  # kind is also the `--<kind>` shorthand of `--agent`
  # (mysbx-rs/src/lib.rs `split_global_flags`), so a kind named after a
  # multiplexer value, a backend value or an existing flag spelling
  # would shadow it — the multiplexer and backend shorthand arms sit
  # around the agent arm, and a bare flag word would never reach the
  # kind at all. The list mirrors the parser's own tokens; the Rust
  # side needs no twin because it never sees a colliding name (this
  # assertion is the guard).
  RESERVED_AGENT_KINDS = [
    # multiplexer values (config::Multiplexer::NAMES)
    "tmux"
    "workmux"
    "herdr"
    "aoe"
    "orca"
    "none"
    # backend values (backends::NAMES)
    "bubblewrap"
    "podman-gvisor"
    "nono"
    "podman-krun"
    "krun"
    "microvm-qemu"
    "microvm-ch"
    # the CLI's own flag words (cli.md D9/D14/D16/D30/D31/D32)
    "agent"
    "no-agent"
    "multiplexer"
    "backend"
    "session"
    "sidecar"
    "config"
    "ro"
    "rw"
    "result"
    "timeout"
    "detach"
    "detachable"
    "dry-run"
    "verbose"
    "help"
    "version"
  ];

  # Baseline mounts: common agent-tooling host config, read-only.
  #
  # The three subtrees are rendered through the shared dereferenced-copy
  # helper (./nix/sandbox-config.nix — bd myconfig-dqq0): home-manager
  # deploys these config files as SYMLINKS into its generation tree
  # (`…-home-manager-files/…`), and a baked-store guest run has no
  # host-store share to resolve the link targets through — the raw `~/…`
  # mounts handed the sandbox dangling symlinks (observed live:
  # RIPGREP_CONFIG_PATH pointing at a `ripgreprc` that does not exist
  # there, a hard `rg` failure on every call). The helper builds real-file
  # copies from home-manager's merged `home.file` — the single source of
  # truth — so a host config change lands in the sandbox on the next
  # build, and every mount path is an absolute store path that exists.
  #
  # Each entry carries a `dest` under the sandbox home: `HOME` is
  # `/mysbx-home` in the sandbox (./docs/design/config.md D14), so `git`
  # finds its config where it already looks. `homeDest` keeps the `~/…`
  # spelling usable for the OTHER baseline paths (none today —
  # `shellConfigMounts` renders store paths — but the expansion is the
  # module's documented idiom, ./docs/design/config.md D8).
  homeDest =
    p:
    if lib.hasPrefix "~/" p then
      "/mysbx-home/" + lib.removePrefix "~/" p
    else
      throw "myconfig.ai.dev.mysbx: homeDest expects a `~/…` path, got `${p}`";

  # The shared helper, consumed like the tool modules consume it
  # (../../programs/programs.rtk/default.nix): a plain function file, no
  # option machinery. Read from the SAME user the config layer is
  # generated for (`hmUser`), referenced only under `mkIf cfg.enable`
  # below (Nix is lazy); not circular — no subtree matches
  # `.config/mysbx`.
  baselineConfigLib = import ./nix/sandbox-config.nix {
    inherit lib;
    runCommand = pkgs.runCommand;
  };
  baselineConfigDirs = [
    ".config/git"
    ".config/ripgrep"
    ".config/bat"
  ];
  baselineConfig = baselineConfigLib.mkSandboxConfig {
    homeFile = hmUser.home.file;
    homeDirectory = hmUser.home.homeDirectory;
    subtrees = baselineConfigDirs;
    name = "mysbx-baseline-config";
  };

  baselineMounts =
    map (sub: {
      path = "${baselineConfig.tree}/${sub}";
      dest = "/mysbx-home/${sub}";
      mode = "ro";
    }) baselineConfigDirs
    ++ shellConfigMounts;

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

  # The home-manager user whose shell/config is rendered into the
  # sandbox (bd myconfig-l44); `myconfig.user` is the only home-manager
  # user this repo configures.
  hmUser = config.home-manager.users."${myconfig.user}";
  hmFish = hmUser.programs.fish;

  # The shared host-side LiteLLM forwarder (../myconfig.ai.dev.litellm-forwarder.nix):
  # the endpoint a podman-gvisor container reaches the host proxy at.
  litellmForwarder = config.myconfig.ai.dev.litellm-forwarder;

  # Home Manager's difftastic integration (bd myconfig-kvo): when it
  # activates `diff.external` (the `external`/`both` git modes), every
  # `git diff` on the host runs through the structural diff renderer —
  # and the mounted `~/.config/git` carries that setting into the
  # sandbox, where its output is useless to agent payloads that parse
  # `git diff` (see ./nix/git-default-diff.nix for the full rationale
  # and the call contract).
  hmDifftastic = config.home-manager.users.mhuber.programs.difftastic;
  hmDifftasticExternal =
    hmDifftastic.enable
    && hmDifftastic.git.enable
    && builtins.elem hmDifftastic.git.mode [
      "external"
      "both"
    ];

  # The interactive payload shell (bd myconfig-l44) and its
  # SANDBOX-SAFE configuration: the host's shell binary with a rendered
  # `~/.config/fish` tree (./nix/shell-config.nix) mounted FILE BY FILE
  # below the sandbox home.
  #
  # Why not the host's `~/.config/fish` as one read-only directory
  # mount (what `baselineMounts` did before): three separate defects.
  #
  #   1. The mounted `config.fish` sources the host's
  #      `hm-session-vars.fish`, which carries HOST XDG paths and model
  #      credentials — a directory mount hands both to the payload, the
  #      opposite of what `--clearenv` is for. It also `source`s
  #      absolute host-home function files that resolve nowhere inside
  #      the sandbox.
  #   2. fish must WRITE `fish_variables` in `$__fish_config_dir`
  #      (universal variables, key bindings). A read-only directory
  #      mount makes every start print "Unable to create temporary
  #      file …/fish_variables: Read-only file system".
  #   3. A directory mount would hide any file a layer mounts below it,
  #      and the sandbox home is a tmpfs whose `.config` must stay
  #      writable for the tools' own state.
  #
  # So the tree is rendered from the user's OWN home-manager `home.file`
  # entries (`mkShellConfig`), dereferenced into real files, and the
  # three paths are mounted INDIVIDUALLY: the configuration is
  # read-only, while fish's state lives on the writable tmpfs home.
  shellConfig =
    if cfg.shell.path != null && hmFish.enable then
      (import ./nix/shell-config.nix {
        inherit (pkgs) lib pkgs runCommand;
      })
        {
          homeFile = hmUser.home.file;
          homeDirectory = hmUser.home.homeDirectory;
          fish = hmFish;
        }
    else
      null;

  # The store paths the rendered fish configuration NAMES at startup
  # (`nix/shell-config.nix` `runtimePackages`): fish itself (hydro
  # re-execs it on every prompt tick), the plugin probes (`grc`) and the
  # tools its aliases call (`eza`, `bat`). Without them on the sandbox
  # `PATH` the rendered config is half-broken: `You need to install
  # grc!` on every prompt, and `ls`/`ll`/`cat` naming an eza/bat that is
  # not there. They come OUT of the rendering so the two cannot drift.
  shellRuntimeTools = if shellConfig != null then shellConfig.runtimePackages else [ ];

  shellConfigMounts = lib.optionals (shellConfig != null) (
    map (sub: {
      path = "${shellConfig.tree}/${sub}";
      dest = "/mysbx-home/.config/fish/${sub}";
      mode = "ro";
    }) shellConfig.subtrees
  );

  # The wrapper the baseline `[env]` below pins: renders the DEFAULT
  # unified diff (git's own machinery minus `diff.external`), self-
  # contained in its closure — its store path is reachable inside the
  # sandbox through the ro `/nix/store` bind.
  gitDefaultDiff = pkgs.callPackage ./nix/git-default-diff.nix { };

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
  #
  # `GIT_EXTERNAL_DIFF` (bd myconfig-kvo): `GIT_EXTERNAL_DIFF` overrides
  # the mounted `diff.external`, and pointing it at `gitDefaultDiff`
  # makes `git diff` render the default unified diff again. Gated on
  # `hmDifftasticExternal`, so a host whose git already uses the default
  # diff gets no entry — the override is the REPAIR for the difftastic
  # activation, not a blanket rewrite of every host's git behavior.
  # The configured browser with `--no-sandbox` baked in: a chrome-family
  # browser cannot build its own namespace sandbox inside this one, and
  # every caller is a machine that passes its own argv (see ./nix/browser.nix).
  browserPackage = pkgs.callPackage ./nix/browser.nix { browser = cfg.browser.package; };

  # The sandbox has its own editor (`neovim` in the dev-tool closure), so
  # `$EDITOR` names a program that exists in here. The forwarded host
  # value is overridden on purpose: it routinely names something the
  # sandbox cannot run at all (`code --wait`, an `emacsclient` talking to
  # a host socket) or a bare name that is not on the sandbox PATH.
  editorEnv = {
    EDITOR = lib.getExe cfg.package.editor;
    VISUAL = lib.getExe cfg.package.editor;
  };

  baselineEnv =
    editorEnv
    // (lib.optionalAttrs (hmRipgrep.enable && hmRipgrep.arguments != [ ]) {
      RIPGREP_CONFIG_PATH = homeDest "~/.config/ripgrep/ripgreprc";
    })
    // (lib.optionalAttrs hmDifftasticExternal {
      GIT_EXTERNAL_DIFF = lib.getExe gitDefaultDiff;
    })
    // (lib.optionalAttrs cfg.browser.enable {
      AGENT_BROWSER_EXECUTABLE_PATH = lib.getExe browserPackage;
      # The same build for `playwright-cli` (../../skills/playwright-cli.nix),
      # whose host value names a path the podman backends cannot see. Both keys
      # carry one executable, so an agent switching between the two CLIs lands
      # on the same browser.
      PLAYWRIGHT_MCP_EXECUTABLE_PATH = lib.getExe browserPackage;
    });

  # The backends this host carries (the wrapper pins the set as
  # MYSBX_ENABLED_BACKENDS and the CLI refuses everything else with a
  # message naming the option): `bubblewrap` and `nono` are the always-on
  # base, the three heavy backends ride their opt-in flag. `podman.enable`
  # covers BOTH podman backends — they share the image and the host setup.
  enabledBackends = [
    "bubblewrap"
    "nono"
  ]
  ++ lib.optionals cfg.podman.enable [
    "podman-gvisor"
    "podman-krun"
  ]
  ++ lib.optional cfg.krun.enable "krun"
  ++ lib.optional cfg.microvm-qemu.enable "microvm-qemu"
  ++ lib.optional cfg.microvm-ch.enable "microvm-ch";

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

  # The shell the multiplexer panes get (`pinShell` of
  # ./nix/mux-entry-lib.nix): the same payload shell as a plain
  # `multiplexer = "none"` session, so a pane does not silently fall back
  # to bash on a fish host (bd myconfig-l44).
  payloadShell = cfg.shell.path;

  # The entry scripts are AGENT-AGNOSTIC (bd myconfig-fv4h): none
  # receives a `defaultAgent` — the resolved agent reaches them at
  # RUN time through the `MYSBX_AGENT_*` environment (every backend's
  # argv builder emits it, resolved by the CLI from the config layers
  # or the `--agent` flags), so one entry script serves every choice
  # and no rebuild of the sandbox tier is needed to change it.
  muxEntries = {
    tmux = pkgs.callPackage ./nix/tmux-entry.nix {
      tmux = cfg.tmux.package;
      inherit payloadShell;
    };
    workmux =
      if cfg.workmux.package != null then
        pkgs.callPackage ./nix/workmux-entry.nix {
          workmux = cfg.workmux.package;
          tmux = cfg.tmux.package;
          inherit payloadShell;
        }
      else
        null;
    herdr =
      if cfg.herdr.package != null then
        pkgs.callPackage ./nix/herdr-entry.nix {
          herdr = cfg.herdr.package;
          tmux = cfg.tmux.package;
          inherit payloadShell;
        }
      else
        null;
    aoe =
      if cfg.aoe.package != null then
        pkgs.callPackage ./nix/aoe-entry.nix {
          aoe = cfg.aoe.package;
          tmux = cfg.tmux.package;
          inherit payloadShell;
        }
      else
        null;
    # `orca` (bd myconfig-1os): the payload is the Orca runtime
    # server (`orca serve`), not the desktop window — see
    # ./nix/orca-entry.nix for the display/network/state decisions.
    # No tmux: orca is not a tmux front end.
    orca =
      if cfg.orca.package != null then
        pkgs.callPackage ./nix/orca-entry.nix {
          inherit (pkgs) appimage-run xvfb;
          orca = cfg.orca.package;
          inherit payloadShell;
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
      # Nothing: the Orca AppImage is not directly PATH-usable
      # (it is an AppImage named `orca-<version>.AppImage`, and no
      # `appimage-run` sits on the pane PATH to run it) — the runtime
      # the panes talk to is the server the entry of
      # ./nix/orca-entry.nix already started. The key must exist or the
      # `.${cfg.config.multiplexer}` lookup below would throw.
      orca = [ ];
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

  # The agent registry of this host, rendered for the CLI's
  # `MYSBX_AGENTS_FILE` pin (bd myconfig-fv4h, config.md D32):
  # `[<kind>] command = "…"` (+ `args` when non-empty) — the exact
  # shape `resolve_agent` in the crate parses. `args` is omitted for
  # an empty list so a registry entry and its Rust twin agree on what
  # "no arguments" looks like, and the whole table stays under the
  # crate's own grammar (a command with a `\n` cannot be spelled: the
  # formats renderer quotes/escapes it like any TOML string).
  agentsFile = tomlFormat.generate "mysbx-agents.toml" (
    lib.mapAttrs (
      _: agent:
      {
        inherit (agent) command;
      }
      // lib.optionalAttrs (agent.args != [ ]) { inherit (agent) args; }
    ) cfg.agents
  );
  # The `MYSBX_AGENT_NAMES` pin: sorted, because `attrNames` order is
  # unspecified and the pin text flows into the wrapper derivation —
  # an unsorted list would churn the drv hash on every registry edit.
  agentNames = lib.sort builtins.lessThan (lib.attrNames cfg.agents);

  userConfigToml = {
    inherit (cfg.config) network multiplexer;
    display = cfg.config.display;
    mounts = map renderMount cfg.config.mounts;
    env = cfg.config.env;
    "forward-env" = cfg.forwardedEnvVars;
  }
  # [nix] only when declared: a host that configures nothing nixy
  # leaves the sandbox on nix's own defaults (plus the wrapper's
  # /etc/nix/nix.conf pin).
  // lib.optionalAttrs (cfg.config.nix != { }) { nix = cfg.config.nix; }
  // lib.optionalAttrs (cfg.config.backend != null) { inherit (cfg.config) backend; }
  // lib.optionalAttrs (cfg.config.profile != null) { inherit (cfg.config) profile; }
  // lib.optionalAttrs (cfg.config.gitDirs != [ ]) { git-dirs = cfg.config.gitDirs; }
  // lib.optionalAttrs (cfg.config.stateDirs != [ ]) { state-dirs = cfg.config.stateDirs; }
  # The D21 allowlist keys (bd myconfig-xob): seeded host-wide, enforced
  # per backend by the CLI (enforced on `nono`, refused on the others —
  # never accepted-and-ignored, D21). Like the layers' own lists these
  # concatenate: the user layer's entries land BEFORE the sidecar's, so
  # a sidecar adds domains without restating the host-wide ones.
  // lib.optionalAttrs (cfg.config.allowDomains != [ ]) { allow-domains = cfg.config.allowDomains; }
  // lib.optionalAttrs (cfg.config.connectPorts != [ ]) { connect-ports = cfg.config.connectPorts; }
  // lib.optionalAttrs (cfg.config.listenPorts != [ ]) { listen-ports = cfg.config.listenPorts; }
  # The D34 host-service rows (bd myconfig-db56): seeded host-wide as
  # name->port, rendered in name order so the generated layer (and
  # its argv) is byte-stable across evaluations.
  // lib.optionalAttrs (cfg.config.hostServices != { }) {
    host-services = map (name: {
      inherit name;
      port = cfg.config.hostServices.${name};
    }) (lib.sort builtins.lessThan (builtins.attrNames cfg.config.hostServices));
  }
  // lib.optionalAttrs (cfg.config.nestedVirt != null) { nested-virt = cfg.config.nestedVirt; }
  // lib.optionalAttrs (cfg.config.memory != null) { inherit (cfg.config) memory; }
  // lib.optionalAttrs (cfg.config.scratchSize != null) { "scratch-size" = cfg.config.scratchSize; }
  // lib.optionalAttrs (cfg.config.cpus != null) { inherit (cfg.config) cpus; }
  // lib.optionalAttrs (cfg.config.pidsLimit != null) { pids-limit = cfg.config.pidsLimit; }
  # The seeded `default-agent` (config.md D32, bd myconfig-fv4h): the
  # option is DEMOTED to this seed — the user config layer it lands in
  # is what a sidecar (`mysbx init` records the value, D32's
  # recording rule) or the `--agent` flags override at RUN time.
  # `null` seeds nothing: a host without a default keeps the
  # pre-D32 behaviour (no agent starts, no key in the file).
  // lib.optionalAttrs (cfg.defaultAgent != null) { "default-agent" = cfg.defaultAgent; };
  # The mysbx crate — the SAME derivation the wrapper builds around
  # (./nix/crate.nix): one definition, two consumers, so the in-image
  # relay and the `MYSBX_PODMAN_EGRESS_RELAY` pin can never drift
  # apart (bd myconfig-eov2).
  egressRelayCrate = pkgs.callPackage ./nix/crate.nix { };
  # C7b (n8kv idea 13, bd myconfig-f23e): the credential-free
  # `.npmrc` the wrapper pins as `MYSBX_NPMRC`. GENERATED, never
  # copied from the host — `.npmrc` is on every tier's credential
  # denylist precisely because the host file may carry
  # `_authToken` lines, and a read-only bind protects nothing
  # there (the same reasoning as the generated `nix.conf`,
  # review-2 item 3). What stays is the minimum a proxy-unaware
  # npm needs: the public registry. With the E8 proxy variables
  # injected on a broker-lane run, `npm install` reaches the
  # registry through the lane without any credential file.
  # `sandboxNpmrc` is a PACKAGE (not a bare writeText) so every
  # backend pins the same FILE path (`${sandboxNpmrc}/etc/mysbx-npmrc`):
  # the bwrap/VM wrappers bind it from the host store, and the podman
  # backends resolve the SAME path inside the image because
  # `podman.imagePackages` bakes the package's store path into the
  # image layers (the egress-relay precedent).
  sandboxNpmrc = pkgs.runCommand "mysbx-npmrc" { } ''
    mkdir -p $out/etc
    cat > $out/etc/mysbx-npmrc <<'EOF'
    # Generated by myconfig for the mysbx sandbox — do not edit.
    # The host's ~/.npmrc is deliberately NOT mounted (credential
    # denylist); the sandbox carries no registry tokens.
    registry=https://registry.npmjs.org/
    EOF
  '';
  # The wrapper factory (`mkWrapper <extraTools> <backends> <binName>
  # <vmStoreBaked>`): ONE call site of ./nix/mysbx.nix for the base
  # `package` AND every flavor of `profiles.<name>` below — a flavor
  # shares every backend pin with the base by construction and can
  # never drift from it (enabled backends, CA bundle,
  # vmStoreExtraRoots, waypipe, mux entries). Five arguments vary:
  # `extraTools` (the sandbox `PATH` closure), `backends` (the
  # MYSBX_ENABLED_BACKENDS pin — a flavor pins the host's set WITHOUT
  # the podman backends), `binName` (a flavor's binary is
  # `mysbx-<profile>`, so it does not collide in home.packages),
  # `vmStoreBaked` — the base follows `vm.store.enable`, a FLAVOR is
  # always `false`: a per-flavor baked store disk would multiply the
  # ~7 GiB erofs image per profile, so a VM run under a flavor keeps
  # the host-store share (`store` resolves to "host") and an explicit
  # `store = "baked"` is refused at launch (no MYSBX_VM_STORE_DISK pin
  # to resolve it through) — and `profile` (config.md D30, bd
  # myconfig-011r): `null` for the base, a flavor's own name, pinned
  # as `MYSBX_PROFILE` for the start-time launcher gate.
  mkWrapper =
    extraTools: backends: binName: vmStoreBaked: profile:
    pkgs.callPackage ./nix/mysbx.nix {
      inherit
        extraTools
        binName
        vmStoreBaked
        profile
        ;
      enabledBackends = backends;
      inherit muxEntries;
      # The agent registry pins (bd myconfig-fv4h, config.md D32):
      # the SORTED kind names and the rendered registry the CLI
      # resolves `default-agent`/`--agent` against.
      agentNames = lib.concatStringsSep " " agentNames;
      agentsFile = "${agentsFile}";
      alacritty = cfg.terminal.package;
      shell = cfg.shell.path;
      podmanImage = if cfg.podman.enable then cfg.podman.image else null;
      podmanShell = if cfg.podman.enable then cfg.podman.shell else null;
      podmanPastaSpec = if cfg.podman.enable then cfg.podman.pastaSpec else null;
      podmanEnv = if cfg.podman.enable then cfg.podman.env else { };
      # The `network = "proxy-only"` channel pins (../docs/design/
      # config.md D20, bd myconfig-mo3.2.4, myconfig-ps3i): the egress
      # socket file name (the per-run broker binds it inside
      # `$XDG_RUNTIME_DIR/mysbx-egress/<pid>/`) and the port the
      # in-sandbox relay listens on. Pinned whenever the host runs
      # LiteLLM, independent of the podman backends (the profile runs
      # on plain bubblewrap).
      egressSocketName = if config.services.litellm.enable then litellmForwarder.unixSocketName else null;
      egressPort = if config.services.litellm.enable then litellmForwarder.port else null;
      # The podman-krun runtime (backends.md D2): the backend is
      # `podman-gvisor` with the runtime swapped, so it rides the
      # podman flag — the shared image is what makes it runnable.
      krunRuntime = if cfg.podman.enable then cfg.krun.runtime else null;
      # The scratch size flows through the generated user config
      # (`config.scratchSize`, rendered as the TOML `scratch-size`)
      # since D23 made it a config key: the crate falls back to it
      # when the env pin is absent. `MYSBX_KRUN_SCRATCH_SIZE` stays
      # the per-invocation override (`--set-default` semantics in
      # the shell), never baked here anymore.

      # The direct-libkrun backend (bd myconfig-dak.4): the
      # launcher and the guest rootfs, pinned as
      # MYSBX_KRUN_LAUNCHER/MYSBX_KRUN_ROOTFS.
      krunLauncher = if cfg.krun.enable then cfg.krun.direct.launcher else null;
      krunRootfs = if cfg.krun.enable then cfg.krun.direct.rootfs else null;
      # `gitDefaultDiff` (bd myconfig-w4zc): the wrapper pins it as
      # GIT_EXTERNAL_DIFF whenever Home Manager's difftastic runs in
      # external/both git mode, so the baked disk must carry its
      # store path too — a run with the guest-own store has no
      # host-store share to resolve it through.
      vmStoreExtraRoots =
        lib.optionals cfg.krun.nix.enable cfg.krun.nix.packages
        ++ lib.optionals hmDifftasticExternal [ gitDefaultDiff ];
      # The native-qemu backend (backends.md D9, bd myconfig-2l9b):
      # the launcher, the SHARED guest rootfs and the kernel
      # libkrun bundles, pinned as MYSBX_MICROVM_QEMU_*.
      microvmQemuLauncher = if cfg.microvm-qemu.enable then cfg.microvm-qemu.launcher else null;
      microvmQemuRootfs = if cfg.microvm-qemu.enable then cfg.microvm-qemu.rootfs else null;
      # The IMAGE FILE, not the kernel package: qemu's -kernel
      # reads a file (a directory is EISDIR), and doctor's
      # is-file rule matches only the image (bd myconfig-q3se).
      microvmQemuKernel = if cfg.microvm-qemu.enable then "${cfg.microvm-qemu.kernel}/bzImage" else null;
      # Backend-specific env pins (bd myconfig-mzys): the SLiRP
      # guest's model endpoint, the qemu twin of `podmanEnv`.
      microvmQemuEnv = if cfg.microvm-qemu.enable then cfg.microvm-qemu.env else { };
      # The cloud-hypervisor backend (bd myconfig-9wkp.8): the
      # same three pins over the CH spelling; the kernel is the
      # SAME image the qemu arm pins (one kernel, both VM
      # backends, bd myconfig-9wkp.3).
      microvmChLauncher = if cfg.microvm-ch.enable then cfg.microvm-ch.launcher else null;
      microvmChRootfs = if cfg.microvm-ch.enable then cfg.microvm-ch.rootfs else null;
      microvmChKernel = if cfg.microvm-ch.enable then "${cfg.microvm-ch.kernel}/bzImage" else null;
      microvmChEnv = if cfg.microvm-ch.enable then cfg.microvm-ch.env else { };
      waypipe = cfg.display.package;
      # The dtach master of detached runs (docs/design/detach.md
      # B10): pinned so `--detach` needs no host PATH lookup.
      dtach = pkgs.dtach;
      podmanWaypipe = if cfg.podman.enable then cfg.podman.waypipe else null;
      # The in-image egress relay of the podman-gvisor proxy-only
      # channel (bd myconfig-eov2 — n8kv.2 C5): the crate's
      # `bin/mysbx-egress-relay` at its store path — the SAME
      # derivation `podman.imagePackages` bakes into the image (both
      # come from ./nix/crate.nix), so the pin always names a binary
      # the loaded image carries. Pinned only when the host runs
      # LiteLLM (the profile's upstream): without it the image bake
      # is skipped too, and a `proxy-only` run is refused naming the
      # pin — fail closed, the same gate as the bwrap pins above.
      podmanEgressRelay =
        if cfg.podman.enable && config.services.litellm.enable then
          "${egressRelayCrate}/bin/mysbx-egress-relay"
        else
          null;
      podmanMuxEntries = if cfg.podman.enable then cfg.podman.muxEntries else { };
      nono = cfg.nono.package;
      ssh-keygen = pkgs.openssh;
      # C7b (n8kv idea 13, bd myconfig-f23e): the declared nodejs
      # major of the toolchain closure (the module option — a host
      # fact, never a probe) and the generated credential-free
      # `.npmrc` the wrapper pins as `MYSBX_NPMRC`.
      nodeMajor = cfg.nodeMajor;
      npmrc = "${sandboxNpmrc}/etc/mysbx-npmrc";
      # The podman spelling of the same pin (the egress-relay
      # precedent): the host store path, which resolves INSIDE the
      # image because `podman.imagePackages` bakes the package and
      # the image layers carry its store path verbatim — the
      # container mounts nothing from the host, but the image IS
      # the store slice.
      podmanNpmrc = "${sandboxNpmrc}/etc/mysbx-npmrc";
    };

  # The flavor wrappers of `profiles.<name>`: each carries the SAME
  # backend pins as the base (`mkWrapper`), with the profile's
  # `extraTools` on the sandbox PATH and the podman backends
  # REFUSED — see the `profiles` option for why the podman backends
  # cannot have flavors, and why a flavor never bakes a store disk
  # (`vmStoreBaked = false`: the ~7 GiB erofs image per profile would
  # be the one order-of-magnitude multiplier of a flavor's closure;
  # a VM run under a flavor keeps the host-store share and an explicit
  # `store = "baked"` is refused instead). Built lazily (only when
  # `profiles != {}`), like every other option consumer.
  # A profile name becomes a binary name interpolated into the
  # wrapper's postBuild shell script (binName), so anything outside
  # `[a-z][a-z0-9-]*` is an eval-time error, not a build-time injection
  # — `$`, backticks and quotes inside a `"…${binName}…"` would
  # otherwise expand in the build sandbox. The hyphen cannot start a
  # name, so `mysbx-<profile>` keeps its two-part shape.
  validProfileName =
    name:
    # `once` would collide with the base once entry's argv[0]
    # spelling (the crate's profile fallback parses `mysbx-once`;
    # the schema refuses the value too, so the two spellings stay
    # in sync).
    name != "once" && builtins.match "[a-z][a-z0-9-]*" name != null;

  profileWrappers = lib.mapAttrs (
    name: profile:
    if !(validProfileName name) then
      throw "myconfig.ai.dev.mysbx.profiles: invalid profile name `${name}` — use a lowercase alphanumeric name with internal hyphens (it becomes the bin/mysbx-<profile> binary name)"
    else
      mkWrapper (cfg.extraTools ++ profile.extraTools) (lib.subtractLists [
        "podman-gvisor"
        "podman-krun"
      ] enabledBackends) "mysbx-${name}" false name
  ) cfg.profiles;
in
{
  imports = [
    ./mcp.nix
    ./podman.nix
  ];

  options.myconfig.ai.dev.mysbx = with lib; {
    enable = mkEnableOption "myconfig.ai.dev.mysbx";

    package = mkOption {
      type = types.package;
      # The wrapped package from ./nix/mysbx.nix: the crate's binary with
      # MYSBX_BWRAP / MYSBX_SHELL / MYSBX_TOOLS_PATH / MYSBX_TERMINAL
      # / MYSBX_CA_BUNDLE
      # pinned to store paths.
      # The unwrapped crate build stays reachable as
      # `<package>.passthru.crate` (used by nix/checks.nix).
      # Backend availability is an opt-in (podman.enable / krun.enable /
      # microvm-qemu.enable, default off): every backend pin below is
      # `null` unless its flag is on, so a disabled backend leaves the
      # wrapper's closure AND the CLI refuses the run (MYSBX_ENABLED_BACKENDS,
      # passed as `enabledBackends`). `null` pins nothing — the contract
      # nix/mysbx.nix already documents for a hand-overridden image or a
      # host that runs `bubblewrap`/`nono` only.
      default = mkWrapper cfg.extraTools enabledBackends "mysbx" cfg.vm.store.enable null;
      defaultText = literalExpression "mkWrapper cfg.extraTools enabledBackends \"mysbx\" cfg.vm.store.enable null — the shared ./nix/mysbx.nix pin block of `mkWrapper` in this module's let";
      description = ''
        The `mysbx` package to install (built from ./mysbx-rs in this repo).
      '';
    };

    extraTools = mkOption {
      type = types.listOf types.package;
      default = [ ];
      example = literalExpression "[ pkgs.pi-coding-agent ]";
      description = ''
        mysbx-specific packages appended on top of the dev-tool closure
        that is baked into the sandbox `PATH` (`toolsEnv` in
        ./nix/mysbx.nix) — the extension point for tooling that belongs
        in mysbx ALONE and in no other tier: the selected
        multiplexer's payload (set by this module itself) and
        per-agent CLIs whose tier wiring is not the shared hook.

        Shared tooling that EVERY sandbox tier should carry does NOT
        belong here: it goes through
        `myconfig.ai.dev.sandboxTools.extraPackages`
        (../../sandboxes/myconfig.ai.sandboxTools.nix), which this
        module consumes as well — the two lists concatenate in the
        dev-tool closure.

        Anything listed here is on the PATH of every mysbx payload, so
        the same "security-relevant list, not packaging detail" rule as
        for the baseline closure applies.
      '';
    };

    nodeMajor = mkOption {
      type = types.nullOr types.ints.unsigned;
      default = null;
      example = 24;
      description = ''
        The major version of the nodejs the toolchain closure carries
        (C7b, n8kv idea 13, bd myconfig-f23e). A declared host fact —
        the wrapper pins it as `MYSBX_NODE_MAJOR`, and the argv
        builders inject `NODE_USE_ENV_PROXY=1` on a table-carrying
        egress channel exactly when this value is at least npm's
        env-proxy support (24).

        `null` (the default) means the closure carries no nodejs the
        host declares, and no injection happens. Declare it when a
        node-based agent (whose wrapper may exec `node` by store
        path, so no PATH probe could find it) ships in `extraTools`.
      '';
    };

    profiles = mkOption {
      type = types.attrsOf (
        types.submodule {
          options.extraTools = mkOption {
            type = types.listOf types.package;
            default = [ ];
            example = literalExpression "[ pkgs.himalaya ]";
            description = ''
              Packages appended ON TOP of the base `extraTools` on the
              sandbox `PATH` of this profile's launcher only — the same
              security rule as `extraTools` applies: whatever lands here
              is on the `PATH` of every payload started through
              `mysbx-<profile>`.
            '';
          };
        }
      );
      default = { };
      example = literalExpression ''
        {
          assistant.extraTools = [ pkgs.himalaya ];
        }
      '';
      description = ''
        Named tool-closure PROFILES: each attribute becomes a second
        mysbx launcher `mysbx-<profile>` in `home.packages`, built by
        the SAME wrapper factory as the base `package` (`mkWrapper` in
        this module's `let`) with the profile's `extraTools` appended
        to the sandbox `PATH` (`MYSBX_TOOLS_PATH`). The base `mysbx`
        and its closure stay unchanged.

        There is deliberately no per-run selection mechanism for this
        (no `tools` config key, no `--profile` flag): `PATH` is
        infrastructure — `MYSBX_TOOLS_PATH` is pinned per wrapper at
        build time (D14, ./nix/mysbx.nix) and the sidecar schema has
        no key for it (D4 forbids evaluated config). A profile IS the
        launcher: the operator (or the agent harness) picks
        `mysbx-assistant` over `mysbx`, and the sidecar
        (`<repo>.mysbx/config.toml`) is shared by all launchers, so
        per-repo mounts and `[env]` stay per-repo.

        A configuration MAY however demand a launcher: the `profile`
        config key (config.md D30, bd myconfig-011r) — set host-wide
        via `config.profile`, per repo in the sidecar — refuses a
        run (exit 70) that starts through any OTHER launcher, so a
        repo pinned to a profile's closure cannot silently run with
        the base one. Each flavor wrapper pins its own name as
        `MYSBX_PROFILE`, the identity the gate compares against.
        The podman backends (`podman-gvisor`, `podman-krun`) are NOT
        available under a profile launcher and a selection of one is
        refused at launch (`--backend podman-gvisor` names a backend
        the profile wrapper does not enable): they run a SHARED OCI
        image baked from `podman.imagePackages` for every launcher — a
        per-profile image would either bake the profile tools into
        the shared image for everyone (defeating the point) or fork
        per-profile images (duplicated pinning, image loading and
        drift). For the same cost reason a flavor never builds a
        guest-own store disk (`vm.store.enable` is honored by the
        BASE wrapper only): the ~7 GiB erofs image per profile would
        be the one order-of-magnitude multiplier of a flavor's
        closure. `bubblewrap`, `nono`, `krun`, `microvm-qemu` and
        `microvm-ch` keep working: they resolve the sandbox `PATH`
        from the launcher's own pins; a VM run under a profile
        always resolves `store` to `"host"`, and an explicit
        `store = "baked"` is refused at launch (the wrapper pins no
        `MYSBX_VM_STORE_DISK`).
      '';
    };

    browser = {
      enable = mkEnableOption ''
        a chrome-family browser on the mysbx sandbox PATH, for
        `agent-browser` and other CDP clients. Off by default: the
        closure is large and a browser is a wide attack surface, so a
        host opts in per deployment.

        What lands on the PATH is `package` wrapped by
        ./nix/browser.nix, which prepends `--no-sandbox` (the browser
        cannot build its own namespace sandbox inside this one) and
        sets `FONTCONFIG_FILE` to a generated fonts.conf (the rootfs
        has no `/etc/fonts`; without it chromium aborts on the first
        font enumeration), passing every other argument through: every
        caller is a machine with its own fixed argv. The wrapper is pinned as
        `AGENT_BROWSER_EXECUTABLE_PATH` in the generated `[env]` too,
        so `agent-browser` uses exactly this build instead of probing
        the PATH or downloading its own Chrome into the sandbox home
      '';

      package = mkPackageOption pkgs "chromium" { };
    };

    forwardedEnvVars = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = literalExpression ''
        [ "SOME_TOKEN" ]
      '';
      description = ''
        Host environment variables forwarded into every sandbox IN
        ADDITION to the crate's built-in default allowlist
        (`FORWARDED_ENV_VARS`, mysbx-rs/src/lib.rs: the technical
        terminal/locale/editor block), each only when actually set at
        `mysbx` launch. Rendered into the host-wide
        `~/.config/mysbx/config.toml` as the `forward-env` key, which
        CONCATENATES onto that built-in default (the crate merges both
        layers' `forward-env` lists the same way), so a repo sidecar may
        add more per repository.

        The built-in default carries NO secrets. A credential lives only
        in the host environment — never in a store path — so an `[env]`
        entry cannot forward it; naming it here is the only way a
        sandboxed agent reaches its model endpoint (bd myconfig-20j).

        This repository sets NO credential names: the model API key,
        token and base-URL variables are named by the private flake,
        which assigns them here. Keep it that way — which endpoints a
        deployment talks to is not public configuration.

        ADDITIVE by design, like `myconfig.ai.dev.jail.fwdEnvs`: a
        deployment appends its variables with a plain assignment
        `forwardedEnvVars = [ "SOME_TOKEN" ];` — no `mkForce`, no
        self-reference — and the built-in block stays.
      '';
    };

    tmux.package = mkOption {
      type = types.package;
      default = pkgs.tmux;
      defaultText = literalExpression "pkgs.tmux";
      description = ''
        The tmux that runs *inside* the sandbox: the server of the
        `tmux`, `workmux` and `aoe` multiplexers — `orca` is not one
        of them, its payload is the Orca runtime server (see
        ./nix/orca-entry.nix) — and the `tmux` on the sandbox
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

    agents = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            command = mkOption {
              type = types.str;
              example = "pi";
              description = ''
                The executable to start. Must be on the sandbox PATH —
                the sandbox never sees host binaries, so the module
                registering the agent must also add its package to
                `extraTools`.
              '';
            };
            args = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Appended literally after `command`.";
            };
          };
        }
      );
      default = { };
      description = ''
        The agent REGISTRY of this host (bd myconfig-fv4h,
        ./docs/design/config.md D32): every kind a mysbx run may
        select through `default-agent` or `--agent`. The wrapper
        pins the (sorted) kind names as `MYSBX_AGENT_NAMES` and the
        rendered registry as `MYSBX_AGENTS_FILE`; the CLI resolves
        the selected kind against them and hands the command to the
        entry scripts as `MYSBX_AGENT_COMMAND`/`ARG1..N` — a
        command/args never enters a TOML layer (D4: configuration
        that can execute is configuration that can escape).

        The agent PROGRAM modules register here
        (../programs.pi-coding-agent sets `pi`,
        ../programs.opencode sets `opencode`); `defaultAgent` names
        one of these keys as the default.
      '';
    };

    defaultAgent = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The KIND of the agent every INTERACTIVE session of this user
        starts (bd myconfig-fv4h, ./docs/design/config.md D32): a
        key of `agents`, a string — never a command. The value seeds
        the generated user config layer's `default-agent` line (a
        sidecar or the `--agent`/`--<kind>`/`--no-agent` flags
        override it per repository / per run), and `null` seeds
        nothing (pre-existing behaviour: no agent starts).
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
        if (config.myconfig.ai.dev.agent-of-empires.enable or false) then
          config.myconfig.ai.dev.agent-of-empires.package
        else
          null;
      defaultText = literalExpression "config.myconfig.ai.dev.agent-of-empires.package (when that module is enabled, else null)";
      description = ''
        The Agent of Empires (`aoe`) package that runs *inside* the
        sandbox when `config.multiplexer = "aoe"`
        (./docs/design/config.md D17).

        `null` means "this host does not carry aoe": no entry is
        pinned, and a configuration selecting it is a refused run
        instead of a silent plain shell.
      '';
    };

    orca.package = mkOption {
      type = types.nullOr types.package;
      # Gated on the orca module being ENABLED, not merely present:
      # the AppImage is a heavy download, so pulling it into every
      # mysbx host's closure to make a selection possible would be
      # the wrong default — the same reasoning as `aoe.package`.
      default =
        if (config.myconfig.ai.dev.orca.enable or false) then config.myconfig.ai.dev.orca.package else null;
      defaultText = literalExpression "config.myconfig.ai.dev.orca.package (when that module is enabled, else null)";
      description = ''
        The Orca AppImage package (../../services.orca.nix) that runs
        *inside* the sandbox when `config.multiplexer = "orca"`
        (./docs/design/config.md D17): the payload is the Orca
        runtime server (`orca serve`), reached from the Orca
        desktop/mobile client over the pairing endpoint — see
        ./nix/orca-entry.nix for the display/network/state decisions.

        `null` means "this host does not carry orca": no entry is
        pinned, and a configuration selecting it is a refused run
        instead of a silent plain shell.
      '';
    };

    # The waypipe binary of the display channel (./docs/design/config.md
    # D18, `display = "waypipe"`): the wrapper pins it as
    # `MYSBX_WAYPIPE` (the HOST-side `waypipe client` mysbx
    # starts per run) and the bwrap backend ships it into the sandbox
    # as the guest-side server (`waypipe server`). A `null` (the
    # default on hosts that do not opt in) pins nothing: `display =
    # "waypipe"` is then an evaluation error host-wide and a refused
    # run for a sidecar — never a silently headless sandbox. Setting
    # the package is the opt-in that makes the selection available,
    # the same shape as `terminal.package`.
    display = {
      package = mkOption {
        type = types.nullOr types.package;
        default = null;
        example = literalExpression "pkgs.waypipe";
        description = ''
          The waypipe package of the display channel, pinned into the
          mysbx wrapper as `MYSBX_WAYPIPE`. `null` disables the
          `"waypipe"` value of `config.display` on this host.
        '';
      };
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

    # The interactive payload shell of the sandbox (bd myconfig-l44):
    # the same shell the user logs into on the host, instead of the bare
    # `bash` of mysbx's own closure — which reads NO configuration at all
    # (there is no host `/etc/bashrc` and no `~/.bashrc` in the tmpfs
    # home), so a sandbox session lost every host alias, abbreviation and
    # function.
    shell = {
      # `null` keeps the crate default: `bashInteractive` from mysbx's
      # own closure (`MYSBX_SHELL` in ./nix/mysbx.nix), the payload every
      # host had before.
      path = mkOption {
        type = types.nullOr types.str;
        default =
          let
            hm = config.home-manager.users."${myconfig.user}";
            fish = hm.programs.fish;
          in
          if config.programs.fish.enable && fish.enable then "${fish.package}/bin/fish" else null;
        defaultText = literalExpression ''
          the fish binary path of the home-manager user's
          `programs.fish.package` when fish is the user's shell
          (`programs.fish.enable` and the home-manager user's
          `programs.fish.enable`), else null'';
        description = ''
          The shell the INTERACTIVE payload of an `multiplexer = "none"`
          sandbox execs, pinned into the wrapper as `MYSBX_SHELL`.

          The default is the fish binary of the home-manager user's
          `programs.fish.package` when fish is that user's shell — the
          same shell a host login lands in, so a sandbox session has the
          host's prompt, abbreviations and aliases. Its configuration is
          rendered SANDBOX-SAFE by ./nix/shell-config.nix (the host's
          generated `config.fish` is never mounted wholesale: it sources
          `hm-session-vars.fish`, which carries host XDG paths and model
          credentials, and absolute host-home function files).

          A path from the wrapper's own closure, like every other pin:
          the unmodified binary is reachable inside the sandbox through
          the read-only `/nix/store` bind.

          `null` keeps the crate default (`bash` from the same closure,
          with no configuration; also the fallback on a host whose user
          does not run fish).
        '';
      };
    };

    podman = {
      enable = mkEnableOption ''
        the podman backends (`podman-gvisor` and `podman-krun`): the
        agent OCI image (`podman.image`), the host setup of ./podman.nix
        (rootless podman, gvisor's runsc registration, subuid ranges) and
        every MYSBX_PODMAN_* pin of the wrapper. Off by default: the image
        and gvisor are heavy closures (and heavy builds), so a host that
        only runs `bubblewrap`/`nono` sandboxes carries none of it.

        Off means REFUSED, never absent-pin-by-accident: the wrapper
        pins `MYSBX_ENABLED_BACKENDS` without the two values and the
        CLI refuses a `--podman-gvisor`/`--podman-krun` run with a
        message naming this option. That explicit gate is load-bearing
        for `podman-krun`, whose crate has a PATH fallback
        (`MYSBX_KRUN_RUNTIME` defaults to `crun`) — without it a
        disabled run would silently land in an ordinary container.
      '';

      # The interactive payload shell of the podman-gvisor backend (bd
      # myconfig-cew): the same shell the user logs into on the host,
      # baked into the image by `podman.extraImagePackages` (which
      # carries the fish world of the home-manager user whenever
      # `programs.fish.enable`, ./podman.nix). The image contains the
      # binary at its store path, and the read-only
      # `~/.config/fish` paths of `shellConfigMounts` (config.md D25)
      # carry the configuration, aliases and plugins — the image is
      # provisioned so the store paths the rendered config names
      # resolve (every one of them is in the image closure).
      #
      # `null` keeps the crate default: the image's own `Cmd`
      # (`/bin/bash`), i.e. the pre-cew behaviour of a bare bash.
      shell = mkOption {
        type = types.nullOr types.str;
        default =
          let
            hm = config.home-manager.users."${myconfig.user}".programs.fish;
          in
          if config.programs.fish.enable && hm.enable then "${hm.package}/bin/fish" else null;
        defaultText = literalExpression ''
          the fish binary path of the home-manager user's
          `programs.fish.package` when fish is the user's shell
          (`programs.fish.enable` and the home-manager user's
          `programs.fish.enable`), else null'';
        description = ''
          The shell the podman-gvisor backend's INTERACTIVE payload
          execs — a path INSIDE the container image (bd myconfig-cew:
          the container must be provisioned with the same shell as
          the host). Pinned into the wrapper as `MYSBX_PODMAN_SHELL`.

          The default is the fish binary of the home-manager user's
          `programs.fish.package` — `podman.image` bakes exactly
          that package, so the store path resolves inside the
          container and the mounted `~/.config/fish` paths give it the
          same aliases and configuration as on the host.

          `null` keeps the image's own `Cmd` (`/bin/bash`).
        '';
      };

      # The waypipe binary INSIDE the podman-gvisor image (D18 on the
      # container backend): the store path of the waypipe binary as it
      # exists inside `podmanImage`, pinned as `MYSBX_PODMAN_WAYPIPE` for
      # the guest (server) end of the display channel. `null` pins
      # nothing — `display = "waypipe"` with `backend = "podman-gvisor"`
      # is a refused run.
      #
      # The default threads the SAME package as the host side: when
      # `display.package` is set, waypipe is baked into the image (via
      # `podman.imagePackages`, which `podman.extraImagePackages`
      # folds in) and the store path —
      # `/nix/store/…-waypipe/bin/waypipe` — resolves inside the
      # container exactly like `podman.shell` does.
      waypipe = mkOption {
        type = types.nullOr types.str;
        default = if cfg.display.package != null then "${cfg.display.package}/bin/waypipe" else null;
        defaultText = literalExpression ''
          "''${config.myconfig.ai.dev.mysbx.display.package}/bin/waypipe"
          when that package is set, else null'';
        description = ''
          The waypipe binary the podman-gvisor backend's payload is
          wrapped in when `display = "waypipe"` — a path INSIDE the
          container image, pinned into the wrapper as
          `MYSBX_PODMAN_WAYPIPE`. The default is the same package as
          the host side, baked into the image via
          `podman.imagePackages`. `null` pins nothing: the selection
          is refused on this backend.
        '';
      };

      # The multiplexer entry scripts baked into the podman image
      # (via `podman.imagePackages`) and pinned as
      # `MYSBX_PODMAN_MUX_ENTRY_<VALUE>` for both podman backends. The
      # store path is the same inside the image as on the host.
      muxEntries = mkOption {
        type = types.attrsOf types.package;
        default = lib.filterAttrs (name: entry: entry != null && name != "orca") muxEntries;
        defaultText = literalExpression ''
          every available multiplexer entry except orca (its
          appimage-run FHS env and loopback pairing endpoint are not
          verified inside a container)'';
        description = ''
          The multiplexer entry scripts the podman backends can start,
          keyed by the `multiplexer` value. Each is baked into
          `podman.image` (through `podman.imagePackages`) and pinned
          into the wrapper as `MYSBX_PODMAN_MUX_ENTRY_<VALUE>`. A
          multiplexer without an entry here is a refused run on both
          podman backends.
        '';
      };

      pastaSpec = mkOption {
        type = types.nullOr types.str;
        # The host's LiteLLM proxy binds 127.0.0.1 only, and inside a
        # container that address is the CONTAINER's loopback, so the
        # sandbox needs the shared forwarder of
        # ../myconfig.ai.dev.litellm-forwarder.nix plus pasta's
        # `--map-guest-addr` translation of its advertised address.
        default =
          let
            fwd = config.myconfig.ai.dev.litellm-forwarder;
          in
          if fwd.enable then "pasta:--map-guest-addr,${fwd.address}" else null;
        defaultText = literalExpression ''
          "pasta:--map-guest-addr,''${config.myconfig.ai.dev.litellm-forwarder.address}"
          when that forwarder runs, else null'';
        description = ''
          The podman `--network` spec of the podman-gvisor backend,
          pinned into the wrapper as `MYSBX_PODMAN_PASTA_SPEC`
          (`--set-default`, so an invocation can still override it).

          `null` leaves podman's default (shared) network, which
          reaches everything the host reaches EXCEPT services bound to
          the host loopback. `network = false` still forces
          `--network none` regardless of this option.
        '';
      };

      env = mkOption {
        type = types.attrsOf types.str;
        default = { };
        example = literalExpression ''{ OPENAI_BASE_URL = "http://192.168.84.1:14000/v1"; }'';
        description = ''
          Environment variables handed to the payload of the
          podman-gvisor backend ALONE, pinned into the wrapper as
          `MYSBX_PODMAN_ENV` and emitted as `--env` after the config
          layers' `[env]` (so they win over a configured value) and
          before the sandbox's own `HOME`/`PATH`/XDG variables (which
          no pin can repoint).

          This is for environment that is only CORRECT under this
          backend: the container has its own network stack and its own
          loopback, so the host endpoints named by `config.env` — which
          the other backends share, and which the bubblewrap backend reaches
          through the host network namespace — are wrong here. Values
          must not contain whitespace (the variable is a
          space-separated list).

          Per-agent modules append their own entries, like they append
          to `config.mounts`.
        '';
      };

      imagePackages = mkOption {
        type = types.listOf types.package;
        default =
          selectedMuxTools
          ++ builtins.attrValues cfg.podman.muxEntries
          ++ lib.optionals (cfg.display.package != null) [ cfg.display.package ]
          # The mysbx crate when the host runs LiteLLM (bd
          # myconfig-eov2 — n8kv.2 C5): the `network = "proxy-only"`
          # channel on podman-gvisor wraps the payload in the IN-IMAGE
          # `mysbx-egress-relay` — the same derivation
          # `egressRelayCrate` pins as MYSBX_PODMAN_EGRESS_RELAY, so the
          # image always carries the binary the wrapper names. Only
          # under LiteLLM: a host without the upstream has no profile
          # to serve, and the crate stays out of the image.
          ++ lib.optionals config.services.litellm.enable [ egressRelayCrate ]
          # C7b (n8kv idea 13, bd myconfig-f23e): the credential-free
          # `.npmrc` — the SAME generated file `sandboxNpmrc` the
          # wrapper pins for every backend, baked into the image
          # layers by its store path (the egress-relay precedent:
          # the image IS the store slice, so the in-container path
          # equals the host store path).
          ++ [ sandboxNpmrc ]
          # The nested-podman userspace of the krun backend (bd
          # myconfig-6di.5.8): the image is SHARED between both podman
          # backends, so provisioning rides this seam — appended only
          # when the host opts in, a gvisor-only host pays no podman
          # closure.
          ++ lib.optionals cfg.krun.nestedPodman.enable cfg.krun.nestedPodman.packages
          ++ lib.optionals cfg.krun.nix.enable cfg.krun.nix.packages
          # Image processing on the container `PATH` — the same payload
          # the bwrap backend gets via `extraTools`.
          ++ [ pkgs.imagemagick ];
        defaultText = literalExpression "the selected multiplexer's tools, the `podman.muxEntries` scripts, plus waypipe, see `display.package`; plus the mysbx crate when the host runs LiteLLM (the in-image egress relay, bd myconfig-eov2); plus the generated credential-free npmrc (C7b, bd myconfig-f23e); plus `krun.nestedPodman.packages` / `krun.nix.packages` when `krun.nestedPodman.enable` / `krun.nix.enable`; plus imagemagick";
        description = ''
          Packages baked into the container image (`podman.image`, see
          ./podman.nix) next to the enabled agents, the fish world and
          `sandboxTools` (bd myconfig-cew: the container must be
          provisioned with the tools). Defaults to the selected
          multiplexer's own tooling, so a pane inside a container
          session finds it on `PATH` — the same payload the bwrap
          backend gets via `extraTools` — the `podman.muxEntries`
          entry scripts, plus the nested-podman
          userspace of the podman-krun backend when
          `krun.nestedPodman.enable` (bd myconfig-6di.5.8: the podman
          storage wrapper and its guest configuration tree, baked into
          the shared image), and the guest nix wrappers when
          `krun.nix.enable` (bd myconfig-pz6).
        '';
      };
    };

    # The direct-libkrun backend group. The PODMAN-krun variant lives
    # under `podman.enable` (it is the podman-gvisor argv with the runtime
    # swapped): only `krun.direct.*` below is this flag's backend.
    krun = {
      enable = mkEnableOption ''
        the direct-libkrun backend (`backend = "krun"`): the mysbx-krun
        launcher and the guest rootfs (bd myconfig-dak) — each run a
        rootless KVM microVM with its own kernel, the host /nix/store
        shared read-only over virtiofs. Off by default: libkrun, the
        rootfs build (static busybox/e2fs/sqlite/store-scan) and the
        launcher are a heavy closure a host that never runs the backend
        should not carry.

        Off means REFUSED, never absent-pin-by-accident: the wrapper
        pins `MYSBX_ENABLED_BACKENDS` without `krun` and the CLI
        refuses the run with a message naming this option.
      '';

      # The podman-krun backend (mysbx-rs/src/podman_gvisor.rs under a
      # swapped runtime, docs/design/backends.md D2, bd myconfig-6di.5):
      # `backend = "podman-krun"` runs the podman-gvisor argv with the
      # OCI runtime swapped to crun built against libkrun — each run a
      # rootless KVM microVM with its own kernel, mounts over
      # virtio-fs. Requires the user to have rw access to `/dev/kvm`
      # (the `kvm` group or the seat udev ACL); a run on a host without
      # it is refused, never a silent fallback. Available with
      # `podman.enable` (the shared image and host setup), not this
      # flag.
      runtime = mkOption {
        type = types.nullOr types.package;
        # nixpkgs' crun already defaults `withLibkrun` to
        # `lib.meta.availableOn stdenv.hostPlatform libkrun` (true on
        # x86_64-linux), so the plain `crun` package carries the krun
        # handler and no override is needed — pinned verbatim so a
        # future nixpkgs default change cannot silently strip the krun
        # build out of the wrapper's closure.
        default = pkgs.crun.override { withLibkrun = true; };
        defaultText = literalExpression "pkgs.crun.override { withLibkrun = true; }";
        description = ''
          The OCI runtime of the podman-krun backend — crun built
          against libkrun (nixpkgs `crun`, `withLibkrun = true`),
          pinned into the wrapper as `MYSBX_KRUN_RUNTIME`
          (`--set-default`, so an invocation can still override it).

          `null` pins nothing: `backend = "podman-krun"` then runs
          against the crate's bare `crun` PATH fallback — a host
          whose PATH `crun` lacks libkrun gets crun's own refusal,
          never a silently unsandboxed run.
        '';
      };

      # The DIRECT-libkrun backend (mysbx-rs/src/krun.rs,
      # docs/design/backends.md D3, bd myconfig-dak): `backend =
      # "krun"` execs bwrap → mysbx-krun → VM with no podman, no
      # crun and no OCI image — the launcher drives libkrun directly,
      # the guest rootfs is a Nix-built plain directory, and the host
      # /nix/store rides in as one read-only virtiofs share. Requires
      # rw /dev/kvm like the podman-krun variant.
      direct = {
        launcher = mkOption {
          type = types.package;
          default = pkgs.callPackage ./nix/krun-launcher.nix {
            # The libkrun the runs dlopen. The DEFAULT nixpkgs build
            # has no blk symbols (bd myconfig-dak.1 finding 1); the
            # scratch disk needs krun_add_disk2, so the pin builds
            # with the blk feature (backends.md D7, bd
            # myconfig-dak.7). The launcher refuses a run whose lib
            # lacks the symbol.
            libkrun = pkgs.callPackage ./nix/libkrun.nix { };
          };
          defaultText = literalExpression "pkgs.callPackage ./nix/krun-launcher.nix { libkrun = pkgs.callPackage ./nix/libkrun.nix { }; }";
          description = ''
            The mysbx-krun launcher of the direct-libkrun backend —
            the zero-dependency Rust binary (../krun-rs) that
            dlopens the pinned libkrun at runtime and
            krun_start_enter()s the VM. Pinned into the wrapper as
            MYSBX_KRUN_LAUNCHER.
          '';
        };

        rootfs = mkOption {
          type = types.package;
          default = pkgs.callPackage ./nix/krun-rootfs.nix {
            # The STATIC busybox of the guest entry — pkgsStatic, not
            # the dynamic busybox of the package set.
            busyboxStatic = pkgs.pkgsStatic.busybox;
            # The scratch formatter (bd myconfig-dak.7, backends.md
            # D7): the static mkfs.ext4 the init runs before any
            # store share is visible.
            e2fsprogsStatic = pkgs.pkgsStatic.e2fsprogs;
            # The db reconcile's static sqlite3 (bd myconfig-mxu):
            # the init prunes copied-db rows the staged share cannot
            # keep, before any store path is visible.
            sqliteStatic = pkgs.pkgsStatic.sqlite.bin;
            # The db reconcile's static bulk store scanner (bd
            # myconfig-kid): the scan replaces the per-path stat loop
            # of a host-sized db (~90s of boot on f13).
            storeScanStatic = pkgs.callPackage ./nix/krun-store-scan.nix { };
            # The restricted mode's static guest relay (bd
            # myconfig-ild7 — n8kv.2 C3): the bridge the init starts
            # under MYSBX_KRUN_NET=restricted, before any store path
            # is visible.
            vsockBridgeStatic = pkgs.callPackage ./nix/krun-vsock-bridge.nix { };
            # The `env` of the /usr/bin/env shebangs (the podman
            # image's usrBinEnv twin).
            coreutils = pkgs.coreutils;
          };
          defaultText = literalExpression "pkgs.callPackage ./nix/krun-rootfs.nix { busyboxStatic = pkgs.pkgsStatic.busybox; e2fsprogsStatic = pkgs.pkgsStatic.e2fsprogs; sqliteStatic = pkgs.pkgsStatic.sqlite.bin; storeScanStatic = pkgs.callPackage ./nix/krun-store-scan.nix { }; vsockBridgeStatic = pkgs.callPackage ./nix/krun-vsock-bridge.nix { }; coreutils = pkgs.coreutils; }";
          description = ''
            The guest rootfs of the direct-libkrun backend —
            the Nix-built plain directory (./nix/krun-rootfs.nix)
            with the baked guest init, the static busybox entry
            path and the shell symlinks (backends.md D3). Pinned
            into the wrapper as MYSBX_KRUN_ROOTFS.
          '';
        };

        userNofileLimit = mkOption {
          type = types.nullOr types.ints.positive;
          default = 8388608;
          description = ''
            The hard RLIMIT_NOFILE of systemd user managers and their
            units (soft stays 1024). The mysbx-krun launcher raises its
            soft limit to this ceiling, and its virtiofs server holds
            one host fd per guest-visited inode, so the ceiling is the
            headroom a fast guest walk has before the sandbox wedges
            on EMFILE between two inode-watchdog checks (bd
            myconfig-jjfy). `null` keeps systemd's default (524288).
            Sessions not started under the user manager (a plain tty
            or ssh login) keep the system default.
          '';
        };
      };

      nestedPodman = {
        # Nested rootless podman inside the krun guest (bd
        # myconfig-6di.5.8, backends.md D2's scope decision). The
        # "rootless" of the nested podman is the VM boundary itself:
        # the payload runs as GUEST ROOT (verified, bd
        # myconfig-6di.5.4 — the libkrun guest init never setuids),
        # so no setuid/fcap newuidmap can or must exist — nixpkgs
        # cannot ship one (shadow builds 0755, dockerTools chowns
        # layers 0:0) and virtiofs would not carry the bit anyway.
        # What the nested pod needs is userspace + config, baked
        # into the (shared) agent image.
        enable = mkEnableOption "nested rootless podman inside the podman-krun guest";

        packages = mkOption {
          type = types.listOf types.package;
          # One tree: the guest configuration plus `bin/podman`, the
          # storage wrapper around pkgs.podman (whose closure carries
          # conmon, crun, netavark, passt, fuse-overlayfs). pkgs.podman
          # itself is NOT listed: the image's buildEnv ignores
          # collisions, so a second `bin/podman` could silently shadow
          # the wrapper.
          default = [ (pkgs.callPackage ./nix/krun-guest-conf.nix { }) ];
          defaultText = literalExpression "[ (pkgs.callPackage ./nix/krun-guest-conf.nix { }) ]";
          description = ''
            The nested-podman userspace baked into the agent image
            when `krun.nestedPodman.enable` — consumed through
            `podman.imagePackages` (the image is shared between both
            podman backends, so provisioning rides the same seam):
            by default the guest tree of ./nix/krun-guest-conf.nix —
            `bin/podman`, a wrapper that puts podman's state on
            guest-native tmpfs and then execs `pkgs.podman`, plus
            containers.conf, storage.conf, policy.json, /etc/subuid
            and /etc/subgid. A different podman goes in as that
            file's `podman` argument, never as a second list entry.

            Live validation of the nested run is bd
            myconfig-6di.5.7's runbook — the agent sandbox has no
            /dev/kvm, so nothing here is executed at check time.
          '';
        };
      };
      nix = {
        # Nix inside the krun guest (bd myconfig-pz6, backends.md D2):
        # guest-root wrappers self-overlay the image's own /nix/store
        # with an upper layer on guest tmpfs, next to a tmpfs copy of
        # the registered image database. See ./nix/krun-guest-nix.nix.
        enable = mkEnableOption ''
          nix inside the podman-krun guest. Guest-root `bin/nix*` wrappers
          overlay the image's own /nix/store with a per-run upper layer on
          guest tmpfs, so new paths cost VM RAM (set `MYSBX_PODMAN_MEMORY`).
          Also builds `podman.image` with a registered nix database
          (`includeNixDB`)'';

        package = mkOption {
          type = types.package;
          default = config.nix.package;
          defaultText = literalExpression "config.nix.package";
          description = ''
            The nix the guest wrappers exec. Defaults to the host's
            `nix.package`.
          '';
        };

        settings = mkOption {
          type = types.lines;
          default = lib.concatStringsSep "\n" (
            lib.optional (config.nix.settings.substituters or [ ] != [ ]) (
              "substituters = " + lib.concatStringsSep " " config.nix.settings.substituters
            )
            ++ lib.optional (config.nix.settings.trusted-public-keys or [ ] != [ ]) (
              "trusted-public-keys = " + lib.concatStringsSep " " config.nix.settings.trusted-public-keys
            )
          );
          defaultText = literalExpression "the host's `nix.settings.substituters` and `trusted-public-keys`";
          description = ''
            `nix.conf` lines the guest wrappers append to their fixed
            defaults (`build-users-group =`, `sandbox = false`, the
            experimental features) in `NIX_CONFIG`. Mirrors the host's
            caches by default: the guest store is per-run, so every run
            substitutes again.
          '';
        };

        scratchSize = mkOption {
          type = types.str;
          default = "32g";
          description = ''
            The size cap of the per-run scratch disk, rendered into
            the generated user config as the `scratch-size` key
            (the default of `config.scratchSize`). The crate
            truncates a per-run SPARSE file under
            `<repo>.mysbx/scratch/` to the merged value before the
            exec. The guest nix wrapper loop-mounts that file as a
            disk-backed ext4 — the nix scratch (overlay upper/work,
            state, logs, cache, TMPDIR) stops costing VM RAM. The
            file is sparse: creation costs no disk space, only the
            guest's writes fill it. A podman `--memory`-shaped value
            (bytes or K/M/G suffix, 64 MiB floor).

            The disk-backed scratch needs the guest's loop + ext4
            support (libkrunfw kernel) — validated by the live probes
            of bd myconfig-0pi's runbook. When the kernel cannot
            attach the loop device, the guest wrapper refuses the nix
            call with the diagnosis instead of silently falling back.
            The tmpfs fallback stays for runs WITHOUT the key and
            announces itself.
          '';
        };

        packages = mkOption {
          type = types.listOf types.package;
          default = [
            (pkgs.callPackage ./nix/krun-guest-nix.nix {
              nix = cfg.krun.nix.package;
              sqlite = pkgs.sqlite;
              storeScan = pkgs.callPackage ./nix/krun-store-scan.nix { };
              storeWatch = pkgs.callPackage ./nix/krun-store-watch.nix { };
              nixConfig = cfg.krun.nix.settings;
            })
          ];
          defaultText = literalExpression "[ (pkgs.callPackage ./nix/krun-guest-nix.nix { nix = cfg.krun.nix.package; sqlite = pkgs.sqlite; storeScan = pkgs.callPackage ./nix/krun-store-scan.nix { }; storeWatch = pkgs.callPackage ./nix/krun-store-watch.nix { }; nixConfig = cfg.krun.nix.settings; }) ]";
          description = ''
            The guest nix userspace baked into the agent image when
            `krun.nix.enable`, consumed through `podman.imagePackages`:
            by default the wrapper tree of ./nix/krun-guest-nix.nix
            (high priority, so it wins the image's buildEnv collision
            against a plain `nix`).
          '';
        };
      };
    };

    # The VM backends' SHARED guest side (backends.md D9): the knobs
    # of machinery both `krun` and `microvm-qemu` boot — the same
    # rootfs, the same init, the same guest env contract. It sits
    # beside `krun` and `microvm-qemu`, not under either backend,
    # because neither owns it: a knob here changes both backends'
    # runs identically.
    vm = {
      # The guest-own nix store of the VM backends
      # (docs/design/guest-store.md): an erofs image of THIS
      # wrapper's own tool closure — the shell, the mux-entry
      # closures, every `extraTools` package, and with
      # `krun.nix.enable` the guest nix wrappers — built
      # read-only and pinned as MYSBX_VM_STORE_DISK, ONE pin for
      # both VM backends (the krun launcher's krun_add_disk2 and
      # the qemu drive both negotiate the write protection into
      # the guest, the init tells the disk from the writable
      # scratch by the /sys/block ro flag and mounts it at /nix).
      # A run opts in per config layer (`store = "baked"`); a run
      # that does not keeps the host-store share (D3), byte for
      # byte.
      store = {
        enable = mkEnableOption ''
          the guest-own nix store disk of the VM backends
          (`backend = "krun"` and `backend = "microvm-qemu"`):
          the erofs image of this wrapper's own tool closure, built
          once per wrapper (not per run) and mounted read-only at
          /nix by a run that opts in via the `store = "baked"`
          config key — a baked run sees its own tools only, no
          host store paths behind them. With the flag on, `baked`
          is also the built-in default of the two VM backends' runs
          (a silent config resolves it; an explicit `store =
          "host"` opts a run back onto the shared store), and off
          (the default) every run keeps the host-store share — a
          `store = "baked"` run is refused with a message naming
          this option, never a silent host-store run.

          A heavy addition: the image is a full copy of the tool
          closure (hundreds of MiB for a plain tools set, GiB-scale
          for a kitchen-sink one). Needs a VM backend enabled
          (`krun.enable` or `microvm-qemu.enable`) — the disk is
          attached by their launchers only.
        '';
      };
    };

    # The native-qemu backend (mysbx-rs/src/microvm_qemu.rs,
    # docs/design/backends.md D9, bd myconfig-6di.6): the krun
    # contract on a stock VMM — `backend = "microvm-qemu"` execs
    # bwrap → mysbx-microvm-qemu → rootless virtiofsd ×N + qemu, the
    # SAME shared guest rootfs as the direct krun backend plus a
    # pinned kernel (libkrun bundles one; qemu needs it handed
    # over). Requires rw /dev/kvm for the supported (accelerated)
    # mode — the launcher's kvm:tcg fallback is a diagnosis, not a
    # supported mode (D9).
    microvm-qemu = {
      enable = mkEnableOption ''
        the native-qemu backend (`backend = "microvm-qemu"`): the
        mysbx-microvm-qemu launcher, the guest rootfs and the
        microvm kernel (bd myconfig-2l9b). Off by default: qemu,
        virtiofsd and a rebuilt kernel are a heavy closure a host that
        never runs the backend should not carry.

        Off means REFUSED, never absent-pin-by-accident: the wrapper
        pins `MYSBX_ENABLED_BACKENDS` without `microvm-qemu` and the
        CLI refuses the run with a message naming this option.
      '';

      launcher = mkOption {
        type = types.package;
        default = pkgs.callPackage ./nix/microvm-qemu-launcher.nix {
          # The qemu and virtiofsd the runs invoke — pinned into the
          # launcher wrapper, so a host swapping either rebuilds the
          # backend (no PATH lookup under bwrap).
          qemu = pkgs.qemu_test;
          virtiofsd = pkgs.virtiofsd;
          # The guestfwd relay of the restricted channel (bd
          # myconfig-krab): the SAME crate derivation the bwrap
          # channel's relay pin uses, pinned into the launcher's
          # OWN wrapper — under bwrap's --clearenv the mysbx
          # wrapper's env pins never reach the launcher, so the
          # pin rides the launcher wrapper (the QEMU/VIRTIOFSD
          # idiom). Null (the standalone default) keeps the
          # wrapper pinless: a --network restricted run then
          # fails closed naming the pin.
          egressRelay = egressRelayCrate;
        };
        defaultText = literalExpression "pkgs.callPackage ./nix/microvm-qemu-launcher.nix { qemu = pkgs.qemu_test; virtiofsd = pkgs.virtiofsd; egressRelay = null; }";
        description = ''
          The mysbx-microvm-qemu launcher of the native-qemu
          backend — the zero-dependency Rust binary
          (../microvm-qemu-rs) that spawns one rootless virtiofsd per
          device and one qemu microVM per run. Pinned into the
          wrapper as MYSBX_MICROVM_QEMU_LAUNCHER.
        '';
      };

      rootfs = mkOption {
        type = types.package;
        # D9: the guest side is SHARED, not forked — the krun rootfs
        # serves both backends (its init mounts virtiofs devices by
        # TAG, which qemu carries namespaced exactly like libkrun).
        default = cfg.krun.direct.rootfs;
        defaultText = literalExpression "cfg.krun.direct.rootfs";
        description = ''
          The guest rootfs of the native-qemu backend — the
          direct-krun rootfs consumed AS-IS (backends.md D9: the
          same baked init, static busybox entry path and shell
          symlinks; only the VMM differs). Pinned into the wrapper
          as MYSBX_MICROVM_QEMU_ROOTFS.
        '';
      };

      kernel = mkOption {
        type = types.package;
        default = pkgs.callPackage ./nix/microvm-qemu-kernel.nix {
          # The kernel version the runs boot — the one input libkrun
          # bundles (libkrunfw) and a stock qemu needs as a bzImage.
          # Built with the virtio device stack compiled IN (the
          # static busybox guest init loads no modules).
          kernelPackages = pkgs.linuxKernel.packages.linux_6_12;
        };
        defaultText = literalExpression "pkgs.callPackage ./nix/microvm-qemu-kernel.nix { kernelPackages = pkgs.linuxKernel.packages.linux_6_12; }";
        description = ''
          The guest kernel bzImage of the native-qemu backend —
          rebuilt with the microvm device set built in
          (virtio_pci/virtio_fs/virtio_blk/ext4/overlayfs: the
          static-busybox guest init loads no modules, so nothing
          may stay a module). Pinned into the wrapper as
          MYSBX_MICROVM_QEMU_KERNEL; the launcher forwards it as
          the qemu -kernel.
        '';
      };

      env = mkOption {
        type = types.attrsOf types.str;
        default = { };
        example = literalExpression ''{ OPENAI_BASE_URL = "http://10.0.2.2:4000/v1"; }'';
        description = ''
          Environment variables handed to the payload of the
          microvm-qemu backend ALONE, pinned into the wrapper as
          `MYSBX_MICROVM_QEMU_ENV` and emitted as `--env` after the
          config layers' `[env]` (so they win over a configured value)
          and before the sandbox's own `HOME`/`PATH`/XDG variables
          (which no pin can repoint).

          This is for environment that is only CORRECT under this
          backend: the SLiRP guest's loopback is its OWN (nothing
          translates `127.0.0.1` to the host, unlike the krun
          backend's TSI), so the host loopback URLs the other
          backends share are dead here. The SLiRP gateway
          `10.0.2.2` maps to the host loopback, so the keyless
          host LiteLLM proxy is reachable there directly — no
          host-side forwarder, unlike the podman-gvisor backend.
          Values must not contain whitespace (the variable is a
          space-separated list).

          Per-agent modules append their own entries, like they
          append to `config.mounts` and `podman.env`.
        '';
      };

      userNofileLimit = mkOption {
        type = types.nullOr types.ints.positive;
        default = 8388608;
        description = ''
          The hard RLIMIT_NOFILE of systemd user managers and their
          units (soft stays 1024), raised when this backend is
          enabled — the same ceiling the krun backend's
          `krun.direct.userNofileLimit` sets. The launcher raises its
          own soft limit to this ceiling before spawning virtiofsd,
          and virtiofsd keeps one host fd per guest-visited inode,
          so the ceiling is the headroom a closure-heavy guest build
          has before its store reads wedge on ENFILE (bd
          myconfig-qq3i). `null` keeps systemd's default (524288).
          Sessions not started under the user manager (a plain tty
          or ssh login) keep the system default.
        '';
      };
    };

    # The cloud-hypervisor backend (bd myconfig-9wkp): `backend =
    # "microvm-ch"` execs bwrap → mysbx-microvm-ch → rootless
    # virtiofsd ×N + passt --vhost-user + cloud-hypervisor — the
    # SAME shared guest rootfs and kernel as the microvm-qemu
    # backend (only the VMM differs: a Rust VMM with per-thread
    # seccomp instead of qemu's larger surface). Requires rw
    # /dev/kvm. The 16550 carries the exit channel; passt mimics
    # SLiRP's address plan so the guest config carries over.
    microvm-ch = {
      enable = mkEnableOption ''
        the cloud-hypervisor backend (`backend = "microvm-ch"`):
        the mysbx-microvm-ch launcher, the SHARED guest rootfs and
        the SAME microvm kernel the qemu arm pins (bd
        myconfig-9wkp). Off by default: cloud-hypervisor, passt,
        virtiofsd and the rebuilt kernel are a heavy closure a host
        that never runs the backend should not carry.

        Off means REFUSED, never absent-pin-by-accident: the
        wrapper pins `MYSBX_ENABLED_BACKENDS` without
        `microvm-ch` and the CLI refuses the run with a message
        naming this option.
      '';

      launcher = mkOption {
        type = types.package;
        default = pkgs.callPackage ./nix/microvm-ch-launcher.nix {
          # The cloud-hypervisor, virtiofsd and passt the runs
          # invoke — pinned into the launcher wrapper, so a host
          # swapping either rebuilds the backend (no PATH lookup
          # under bwrap).
          cloud-hypervisor = pkgs.cloud-hypervisor;
          inherit (pkgs) virtiofsd passt;
          # The vsock-listener relay of the restricted channel (bd
          # myconfig-fo1r): the SAME crate derivation the bwrap
          # channel's relay pin uses, pinned into the launcher's
          # OWN wrapper — under bwrap's --clearenv the mysbx
          # wrapper's env pins never reach the launcher, so the
          # pin rides the launcher wrapper (the qemu twin's
          # MYSBX_MICROVM_QEMU_EGRESS_RELAY idiom). Null (the
          # standalone default) keeps the wrapper pinless: a
          # --network restricted run then fails closed naming
          # the pin.
          egressRelay = egressRelayCrate;
        };
        defaultText = literalExpression "pkgs.callPackage ./nix/microvm-ch-launcher.nix { cloud-hypervisor = pkgs.cloud-hypervisor; virtiofsd = pkgs.virtiofsd; passt = pkgs.passt; egressRelay = null; }";
        description = ''
          The mysbx-microvm-ch launcher of the cloud-hypervisor
          backend — the zero-dependency Rust binary
          (../microvm-ch-rs) that spawns one rootless virtiofsd per
          device, one passt child per shared-network run and one
          cloud-hypervisor microVM per run. Pinned into the wrapper
          as MYSBX_MICROVM_CH_LAUNCHER.
        '';
      };

      rootfs = mkOption {
        type = types.package;
        # The guest side is SHARED, not forked — the SAME krun
        # rootfs serves krun, microvm-qemu and microvm-ch.
        default = cfg.krun.direct.rootfs;
        defaultText = literalExpression "cfg.krun.direct.rootfs";
        description = ''
          The guest rootfs of the cloud-hypervisor backend — the
          shared direct-krun rootfs consumed AS-IS. Pinned into
          the wrapper as MYSBX_MICROVM_CH_ROOTFS.
        '';
      };

      kernel = mkOption {
        type = types.package;
        # One kernel serves both VM backends (bd myconfig-9wkp.3):
        # the SAME expression the qemu arm pins — the spike proved
        # the bzImage boots CH as-is, and PVPANIC=y is inert under
        # qemu (which presents no pvpanic device).
        default = cfg.microvm-qemu.kernel;
        defaultText = literalExpression "cfg.microvm-qemu.kernel";
        description = ''
          The guest kernel bzImage of the cloud-hypervisor
          backend — the SAME kernel the microvm-qemu backend pins
          (one kernel serves both VM backends; the microvm device
          set is built in because the static-busybox guest init
          loads no modules). Pinned into the wrapper as
          MYSBX_MICROVM_CH_KERNEL; the launcher forwards it as the
          CH --kernel.
        '';
      };

      env = mkOption {
        type = types.attrsOf types.str;
        default = { };
        example = literalExpression ''{ OPENAI_BASE_URL = "http://10.0.2.2:4000/v1"; }'';
        description = ''
          Environment variables handed to the payload of the
          microvm-ch backend ALONE, pinned into the wrapper as
          `MYSBX_MICROVM_CH_ENV` (the qemu arm's MYSBX_MICROVM_QEMU_ENV
          rule). This is for environment that is only CORRECT
          under this backend: the passt guest's loopback is its
          OWN (nothing translates `127.0.0.1` to the host), so the
          host loopback URLs the other backends share are dead
          here. The SLiRP-plan gateway `10.0.2.2` NATs to the host
          loopback, so the keyless host LiteLLM proxy is reachable
          there directly. Values must not contain whitespace
          (the variable is a space-separated list).

          Per-agent modules append their own entries, like they
          append to `microvm-qemu.env`.
        '';
      };

      userNofileLimit = mkOption {
        type = types.nullOr types.ints.positive;
        default = 8388608;
        description = ''
          The hard RLIMIT_NOFILE of systemd user managers and their
          units (soft stays 1024), raised when this backend is
          enabled — the same ceiling as
          `microvm-qemu.userNofileLimit`, for the same reason: the
          launcher raises its soft limit to this ceiling, and every
          virtiofsd it spawns keeps one host fd per guest-visited
          inode. `null` keeps systemd's default (524288).
        '';
      };
    };

    nono = {
      # The nono backend (mysbx-rs/src/nono.rs): `nono run` under
      # Landlock + seccomp — the same sandbox the tier-2 standalone
      # wrappers of
      # ../../sandboxes/myconfig.ai.nono-agent-sandbox.nix use. The
      # package below is what the wrapper pins as `MYSBX_NONO`
      # (./nix/mysbx.nix); `MYSBX_NONO_PROFILE` keeps its `"default"`
      # inside the package file.
      package = mkOption {
        type = types.nullOr types.package;
        # A plain nixpkgs package, so it defaults to `pkgs.nono`
        # directly, the same shape as the display/waypipe option defaulting to a
        # package.
        default = pkgs.nono;
        defaultText = literalExpression "pkgs.nono";
        description = ''
          The nono sandbox binary of the nono backend (Landlock +
          seccomp; upstream nolabs-ai/nono, `pkgs.nono` — the same
          tool the tier-2 wrappers of
          ../../sandboxes/myconfig.ai.nono-agent-sandbox.nix use),
          pinned into the wrapper as `MYSBX_NONO`.

          `null` pins nothing: `backend = "nono"` is a refused run,
          never a silently-unsandboxed one.
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
        example = literalExpression "config.myconfig.ai.dev.workmux.package";
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
            type = types.nullOr (
              types.enum [
                "bubblewrap"
                "podman-gvisor"
                "nono"
                "podman-krun"
                "krun"
                "microvm-qemu"
                "microvm-ch"
              ]
            );
            default = "bubblewrap";
            description = "Sandbox backend (`bubblewrap`, `podman-gvisor`, `nono`, `podman-krun`, `krun`, `microvm-qemu` or `microvm-ch`); `null` leaves the choice to the sidecar.";
          };
          profile = mkOption {
            type = types.nullOr (types.strMatching "[a-z][a-z0-9-]*");
            default = null;
            example = "assistant";
            description = ''
              The launcher profile every sandbox of this user
              REQUIRES (./docs/design/config.md D30, bd
              myconfig-011r): rendered into the generated user
              config as the `profile` key, which the crate enforces
              at start — a run is refused (exit 70) unless it was
              started through that profile's launcher
              (`mysbx-<profile>`, `profiles.<name>`, bd
              myconfig-xl93). `null` (the default) demands nothing:
              every launcher is admitted. This is how a host pins
              a profile host-wide; a repo sidecar may still set its
              own `profile` (the sidecar wins on conflict, like
              `backend`).

              The named profile must exist in `profiles` — an eval
              assertion throws otherwise, because demanding a
              launcher nothing provides would refuse every run.
            '';
          };
          network = mkOption {
            type = types.either types.bool (
              types.enum [
                "none"
                "proxy-only"
                "shared"
              ]
            );
            default = true;
            description = ''
              The sandbox's network policy (`./docs/design/config.md`
              D20): `"none"` (the `false` alias) denies the network,
              `"proxy-only"` grants only the host model endpoint, and
              `"shared"` (the `true` alias, the default) shares it.
              The bool aliases stay accepted forever, so a host that
              keeps the default renders `network = true` and its
              generated TOML is unchanged. A sidecar may tighten the
              value, never widen it.'';
          };
          nestedVirt = mkOption {
            type = types.nullOr types.bool;
            default = null;
            description = ''
              Hand a `microvm-qemu` or `microvm-ch` sandbox hardware
              virtualization
              (./docs/design/config.md D27): the guest gets its own
              `/dev/kvm` and can run nested VMs. `null` (the default)
              leaves the decision to the sidecar; `false` is a
              host-wide deny the sidecar cannot lift; only `true`
              with one of those two backends runs — every other
              backend warns and drops the key. Off by default:
              nested VMX widens the host KVM attack surface.
            '';
          };
          multiplexer = mkOption {
            type = types.enum [
              "tmux"
              "workmux"
              "herdr"
              "aoe"
              "orca"
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
            defaultText = literalExpression ''if config.myconfig.ai.dev.mysbx.workmux.enable then "workmux" else "none"'';
            description = ''
              Which terminal multiplexer the INTERACTIVE payload of
              every sandbox of this user is
              (./docs/design/config.md D17, ./docs/design/cli.md D11):
              `tmux`, `workmux`, `herdr`, `aoe`, `orca`, or `none` for a plain
              interactive shell. `mysbx run -- CMD` is unaffected — a
              one-shot command starts no session.

              This is the host-wide DEFAULT: a repository's sidecar
              config may name another value (or `none`), and it wins
              — both layers are trusted and the key grants no host
              access (D7/D17).

              The selected multiplexer must be available on this host,
              i.e. its package option must be set
              (`workmux.package`, `herdr.package`, `aoe.package`,
              `orca.package`; `tmux` always is). An unavailable
              selection is an evaluation error here, and a refused run
              for a sidecar that names one — never a silently started
              plain shell.

              Whichever is selected, its socket and state live INSIDE
              the sandbox (`/mysbx-home/.mysbx-tmux`, exported as
              `TMUX_TMPDIR` after `[env]`, so no layer can repoint
              it), never shared with the host or another sandbox.
            '';
          };
          display = mkOption {
            type = types.enum [
              "off"
              "waypipe"
            ];
            default = "off";
            description = ''
              How a Wayland compositor reaches the sandbox
              (./docs/design/config.md D18): `off` (the default) for a
              headless run, `waypipe` for the per-run waypipe channel —
              the host side (`waypipe client`) is started by
              mysbx before the payload, the guest display socket always
              lives at `/mysbx-home/wayland-0` inside the sandbox home
              tmpfs, and neither the host compositor socket nor
              `$XDG_RUNTIME_DIR` is ever bound.

              This is the host-wide DEFAULT: a repository's sidecar
              config may name another value (or `off`), and it wins —
              both layers are trusted and the key grants no host
              access (D7/D18).

              `waypipe` needs the wrapper pin, i.e. a non-null
              `display.package` (and, for the podman-gvisor backend, a
              waypipe inside the image, `podman.waypipe`). An
              unpinned selection is an evaluation error here, and a
              refused run for a sidecar that names one — never a
              silently headless sandbox.
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
          nix = mkOption {
            type = types.attrsOf types.str;
            default = { };
            example = {
              experimental-features = "nix-command flakes";
            };
            description = ''
              Nix settings forwarded into the sandbox (the `[nix]`
              table, ./docs/design/config.md): setting name → value,
              rendered by mysbx as the sandbox's `NIX_CONFIG` — which
              nix reads in addition to its conf files, so this composes
              with the wrapper's own `/etc/nix/nix.conf` pin.
            '';
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

          allowDomains = mkOption {
            type = types.listOf types.str;
            default = [ ];
            example = [
              "api.openai.com"
              "github.com"
            ];
            description = ''
              Host-wide `allow-domains` (./docs/design/config.md D21):
              domains the sandbox may connect to — the network
              allowlist, seeded into the generated user layer.

              Enforcement is per backend and NEVER accepted-and-ignored
              (D21): enforced on `backend = "nono"` (nono's egress
              proxy, plain host names only — a URL/path form is refused,
              bd myconfig-6di.4.4); every other backend refuses the
              run when any allowlist key is set. These options just
              seed the layer: the backend choice stays
              per repository (sidecar) or per run (`--backend`).
            '';
          };
          connectPorts = mkOption {
            type = types.listOf types.port;
            default = [ ];
            example = [
              443
              22
            ];
            description = ''
              Host-wide `connect-ports` (./docs/design/config.md D21):
              outbound TCP ports the sandbox may target. Same per-backend
              enforcement as `allowDomains` (enforced on `nono`, refused
              on the other backends).
            '';
          };
          listenPorts = mkOption {
            type = types.listOf types.port;
            default = [ ];
            example = [ 8080 ];
            description = ''
              Host-wide `listen-ports` (./docs/design/config.md D21):
              TCP ports the sandbox may listen on. Same per-backend
              enforcement as `allowDomains`; on `nono`, listen ports
              WITHOUT a domain/connect restriction are refused (with
              only listen ports nono reports "outbound allowed", bd
              myconfig-a14) — seed a real restriction, not port noise.
            '';
          };

          hostServices = mkOption {
            type = types.attrsOf types.port;
            default = { };
            example = {
              my-mcp = 8765;
            };
            description = ''
              Host-wide `[[host-services]]` rows (./docs/design/config.md
              D34, bd myconfig-db56): named HOST loopback services a
              `network = "proxy-only"` run may reach through the
              per-run egress broker, keyed by service name. The
              address is not spellable — a row always forwards to the
              host's own loopback.

              Legal only next to the broker channel: a run with rows
              but without `backend = "bubblewrap"` +
              `network = "proxy-only"` is refused (never
              accepted-and-ignored, D34's pairing rule). These seed
              the user layer; a sidecar adds rows, it cannot override
              one (the merge dedupes by name, first occurrence wins,
              and a same-name/different-port conflict across trusted
              layers is a merge refusal).
            '';
          };

          memory = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "8g";
            description = ''
              Host-wide `memory` limit (./docs/design/config.md D23),
              podman `--memory` grammar. Enforced by the podman backends
              (a krun VM size, a cgroup limit under podman-gvisor when
              cgroups are active); the other backends warn. A sidecar
              value wins; `MYSBX_PODMAN_MEMORY` overrides both.
            '';
          };

          scratchSize = mkOption {
            type = types.nullOr types.str;
            default = if cfg.krun.nix.enable then cfg.krun.nix.scratchSize else null;
            defaultText = literalExpression "cfg.krun.nix.scratchSize when krun.nix.enable";
            example = "100g";
            description = ''
              Host-wide `scratch-size` (./docs/design/config.md D23):
              the per-run nix scratch disk cap, podman `--memory`
              grammar. Rendered into the generated user config —
              the layer a sidecar may override. Defaults to
              `krun.nix.scratchSize` when the host opts into guest
              nix (the scratch exists only there).
            '';
          };

          cpus = mkOption {
            type = types.nullOr (types.either types.ints.positive types.float);
            default = null;
            example = 4;
            description = ''
              Host-wide `cpus` limit (./docs/design/config.md D23). A krun
              VM needs a whole number. Same layering as `memory`
              (`MYSBX_PODMAN_CPUS`).
            '';
          };

          pidsLimit = mkOption {
            type = types.nullOr types.ints.positive;
            default = null;
            example = 1024;
            description = ''
              Host-wide `pids-limit` (./docs/design/config.md D23). Only
              podman-gvisor enforces it (with cgroups); the other
              backends warn. Same layering as `memory`
              (`MYSBX_PODMAN_PIDS_LIMIT`).
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
            myconfig.ai.dev.mysbx.config.mounts: these entries end up at an
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
            myconfig.ai.dev.mysbx.workmux.enable is on but
            myconfig.ai.dev.mysbx.workmux.package is null — set it to the
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
            myconfig.ai.dev.mysbx.config.multiplexer is
            "${cfg.config.multiplexer}", but this host carries no
            ${cfg.config.multiplexer} for the sandbox — set
            myconfig.ai.dev.mysbx.${cfg.config.multiplexer}.package (for
            workmux: myconfig.ai.dev.mysbx.workmux.enable, which
            ../myconfig.ai.workmux/mysbx.nix does), or select another
            multiplexer (docs/design/config.md D17).
          '';
        }
        {
          # The kind grammar (config.md D32): the TOML key and the CLI
          # shorthand both accept `[a-z][a-z0-9_-]{0,31}` — the same
          # grammar the crate's `valid_agent_kind` checks at RUN
          # time; catching a bad key here names the registry entry
          # at eval time instead of a refused run later.
          assertion = lib.all (kind: builtins.match "[a-z][a-z0-9_-]{0,31}" kind != null) (
            lib.attrNames cfg.agents
          );
          message = ''
            myconfig.ai.dev.mysbx.agents has a key that is not a valid
            agent kind (the grammar is "[a-z][a-z0-9_-]{0,31}",
            lowercase): ${
              toString (
                lib.filter (kind: builtins.match "[a-z][a-z0-9_-]{0,31}" kind == null) (lib.attrNames cfg.agents)
              )
            }.
          '';
        }
        {
          # config.md D32 (bd myconfig-fv4h): the kind must exist in
          # the registry — `defaultAgent` names one of `agents`, so a
          # typo is caught at EVAL time, not as a refused run later.
          assertion = cfg.defaultAgent == null || cfg.agents ? ${cfg.defaultAgent};
          message = ''
            myconfig.ai.dev.mysbx.defaultAgent is "${cfg.defaultAgent}",
            but myconfig.ai.dev.mysbx.agents has no such key
            (available: ${toString (lib.sort builtins.lessThan (lib.attrNames cfg.agents))}).
          '';
        }
        {
          # The `--<kind>` shorthands (bd myconfig-fv4h): an agent kind
          # must not collide with a flag word the CLI parses earlier
          # or a value another shorthand family claims
          # (mysbx-rs/src/lib.rs `split_global_flags`: the multiplexer
          # arm first, then the agent arm, then the backend arm) —
          # a kind named e.g. `krun` would shadow the backend flag
          # and every user of it. One reserved list, asserted per
          # registry key and per `defaultAgent`.
          assertion = lib.all (kind: !builtins.elem kind RESERVED_AGENT_KINDS) (lib.attrNames cfg.agents);
          message = ''
            myconfig.ai.dev.mysbx.agents has a kind that collides with
            a reserved CLI word (a multiplexer name, a backend name, or
            a mysbx flag spelling): ${
              toString (lib.filter (kind: builtins.elem kind RESERVED_AGENT_KINDS) (lib.attrNames cfg.agents))
            }. The kind is also the `--<kind>` shorthand of
            `--agent`, so a reserved word would shadow an existing
            flag (docs/design/cli.md).
          '';
        }
        {
          # Availability of the podman-gvisor image pin (D18): a
          # `podman.waypipe` set by hand implies a waypipe package to
          # thread — the default derives the pin FROM the package, so
          # a non-null pin with a null package can only be a hand
          # override that would push nothing into the image and leave
          # the pinned path nonexistent inside it.
          assertion = cfg.podman.waypipe == null || cfg.display.package != null;
          message = ''
            myconfig.ai.dev.mysbx.podman.waypipe is set, but
            myconfig.ai.dev.mysbx.display.package is null — the image
            needs the waypipe package baked in
            (podman.imagePackages) for the pin to resolve inside it;
            set display.package (pkgs.waypipe) too
            (docs/design/config.md D18).
          '';
        }
        {
          # Availability of the SELECTED display channel, checked at
          # eval time (D18) — the same shape as the multiplexer
          # assertion above. The runtime refuses an unpinned selection
          # too (that is the guard for a *sidecar* naming one), but a
          # host-wide default nobody can serve is a build error.
          assertion = cfg.config.display != "waypipe" || cfg.display.package != null;
          message = ''
            myconfig.ai.dev.mysbx.config.display is "waypipe", but
            myconfig.ai.dev.mysbx.display.package is null — set it to the
            waypipe package (pkgs.waypipe), or select "off"
            (docs/design/config.md D18).
          '';
        }
        {
          # Availability of the SELECTED backend, checked at eval time —
          # the same shape as the multiplexer assertion above: a host-wide
          # default nobody can run is a build error. The runtime refuses a
          # disabled backend too (that is the guard for a *sidecar* naming
          # one); the explicit CLI gate is load-bearing for podman-krun,
          # whose bare-`crun` PATH fallback would otherwise silently run
          # an ordinary container instead of a microVM.
          assertion = cfg.config.backend == null || builtins.elem cfg.config.backend enabledBackends;
          message = ''
            myconfig.ai.dev.mysbx.config.backend is
            "${cfg.config.backend}", but that backend is not enabled on
            this host — set myconfig.ai.dev.mysbx.podman.enable
            (podman-gvisor, podman-krun), .krun.enable (krun),
            .microvm-qemu.enable (microvm-qemu) or .microvm-ch.enable
            (microvm-ch), or select another backend.
          '';
        }
        {
          # The host-wide `config.profile` (config.md D30, bd
          # myconfig-011r) must name a launcher this host actually
          # provides: demanding a profile nothing provides would
          # refuse every run of this user at start time — a build
          # error, caught here at eval time instead.
          assertion =
            cfg.config.profile == null || builtins.elem cfg.config.profile (builtins.attrNames cfg.profiles);
          message = ''
            myconfig.ai.dev.mysbx.config.profile is
            "${toString cfg.config.profile}", but no such profile
            exists — set
            myconfig.ai.dev.mysbx.profiles."${toString cfg.config.profile}".extraTools
            (the profile becomes the mysbx-${toString cfg.config.profile}
            launcher), or set config.profile to null.
          '';
        }
        {
          # The opt-in must be satisfiable: `podman.enable` with a
          # hand-nulled image is a refused run nobody asked for.
          assertion = !cfg.podman.enable || cfg.podman.image != null;
          message = ''
            myconfig.ai.dev.mysbx.podman.enable is on, but
            myconfig.ai.dev.mysbx.podman.image is null — the podman
            backends need the agent image pinned. Restore the default
            image or turn the flag off.
          '';
        }
        {
          # The guest-own store disk is attached by the VM backends'
          # launchers only — baked with no VM backend enabled would
          # be a heavy image nothing can consume. Refused at eval,
          # not a silently useless closure.
          assertion =
            !cfg.vm.store.enable || cfg.krun.enable || cfg.microvm-qemu.enable || cfg.microvm-ch.enable;
          message = ''
            myconfig.ai.dev.mysbx.vm.store.enable is on, but no VM
            backend is — the store disk is attached by the krun,
            microvm-qemu and microvm-ch launchers only. Set
            myconfig.ai.dev.mysbx.krun.enable,
            myconfig.ai.dev.mysbx.microvm-qemu.enable or
            myconfig.ai.dev.mysbx.microvm-ch.enable, or turn the
            flag off.
          '';
        }
      ];

    # Baseline mounts; further definitions (from per-agent modules or the
    # host config) are concatenated onto this list.
    myconfig.ai.dev.mysbx.config.mounts = baselineMounts ++ workmuxMounts;

    # The VM launchers' fd ceiling (`krun.direct.userNofileLimit`,
    # `microvm-qemu.userNofileLimit` and `microvm-ch.userNofileLimit`):
    # user@.service bounds the user manager itself, user.conf the
    # units (and so the sessions) it starts. Gated on the enabled
    # backends: they are the only consumers of the raised ceiling
    # (the krun launcher hosts libkrun's virtiofs server in-process;
    # the microvm-qemu and microvm-ch launchers spawn one virtiofsd
    # per device, and all pin one host fd per guest-visited inode,
    # bd myconfig-jjfy / myconfig-qq3i).
    systemd =
      let
        # A host that enables several VM backends gets the max of
        # their ceilings.
        enabledLimit = enable: limit: if enable && limit != null then limit else 0;
        nofileLimit = lib.foldl' lib.max 0 [
          (enabledLimit cfg.krun.enable cfg.krun.direct.userNofileLimit)
          (enabledLimit cfg.microvm-qemu.enable cfg.microvm-qemu.userNofileLimit)
          (enabledLimit cfg.microvm-ch.enable cfg.microvm-ch.userNofileLimit)
        ];
        raisesCeiling = nofileLimit != 0;
        limit = "1024:${toString nofileLimit}";
      in
      {
        services."user@".serviceConfig.LimitNOFILE = lib.mkIf raisesCeiling limit;
        user.settings.Manager.DefaultLimitNOFILE = lib.mkIf raisesCeiling limit;
      };

    # The host side of model access for the podman-gvisor backend: the
    # shared port-scoped forwarder of
    # ../myconfig.ai.dev.litellm-forwarder.nix, whose advertised address
    # `podman.pastaSpec` maps into the container. Gated on an image
    # being available — that is what makes `backend = "podman-gvisor"`
    # runnable on this host at all — and on the host running LiteLLM.
    # The forwarder is socket-activated, so an enabled-but-unused one
    # runs nothing.
    #
    # The bubblewrap backend needs none of this: it shares the host
    # network namespace, where `127.0.0.1:<litellm port>` is the proxy
    # itself.
    myconfig.ai.dev.litellm-forwarder.enable = lib.mkIf (
      cfg.podman.enable && config.services.litellm.enable
    ) true;

    # The endpoint as the CONTAINER sees it, for tools that read
    # `OPENAI_BASE_URL`. The agent CLIs whose model
    # configuration is a generated file add their own entries from their
    # own modules (../programs/programs.pi-coding-agent,
    # ../programs/programs.opencode).
    myconfig.ai.dev.mysbx.podman.env = lib.mkIf (cfg.podman.enable && litellmForwarder.enable) {
      OPENAI_BASE_URL = litellmForwarder.endpoint;
    };

    # The endpoint as the SLiRP GUEST of the microvm-qemu backend sees
    # it (bd myconfig-mzys): its loopback is its own — nothing maps
    # `127.0.0.1` to the host, unlike the krun backend's TSI — but
    # the SLiRP gateway `10.0.2.2` maps to the host loopback, where
    # the keyless LiteLLM proxy listens. No host-side forwarder is
    # needed (the port-scoped forwarder of
    # ../myconfig.ai.dev.litellm-forwarder.nix exists for pasta's
    # `--map-guest-addr`, which maps an address, not a loopback). The
    # same per-agent modules that fill `podman.env` add their own
    # `microvm-qemu.env` entries.
    myconfig.ai.dev.mysbx.microvm-qemu.env =
      lib.mkIf (cfg.microvm-qemu.enable && config.services.litellm.enable)
        {
          OPENAI_BASE_URL = "http://10.0.2.2:${toString config.services.litellm.port}/v1";
        };

    # The same endpoint for the passt guest of the microvm-ch
    # backend: the launcher runs passt on SLiRP's address plan, and
    # passt maps the gateway `10.0.2.2` to the host loopback.
    myconfig.ai.dev.mysbx.microvm-ch.env =
      lib.mkIf (cfg.microvm-ch.enable && config.services.litellm.enable)
        {
          OPENAI_BASE_URL = "http://10.0.2.2:${toString config.services.litellm.port}/v1";
        };

    # The selected multiplexer's own tooling, on the sandbox PATH: the
    # entries pin their own copies, but a PANE that runs the tool (the
    # workmux sidebar's `workmux set-window-status`, a plain `tmux`, a
    # `herdr pane …` from an agent) resolves it from `PATH`. The agents
    # a multiplexer launches come from the agent modules' own
    # `extraTools`. See `selectedMuxTools` for why only the selected
    # one is added.
    #
    # `shellRuntimeTools` is the same kind of obligation for the PAYLOAD
    # shell (`shellRuntimeTools` above): the rendered fish configuration
    # names fish, grc, eza and bat by bare name, so a sandbox that mounts
    # the config but not the binaries prints `You need to install grc!`
    # on every prompt.
    #
    # Shared sandbox tooling (../../sandboxes/myconfig.ai.sandboxTools.nix),
    # phase 2d: mysbx honours the cross-tier hook like the other
    # tiers. The hook's packages land on the sandbox PATH here, via
    # the same `extraTools` the agent modules append to — one `listOf`
    # definition, so all of them simply concatenate in `toolsEnv`.
    #
    # The phase-2d decision: `extraTools` STAYS as the mysbx-specific
    # extension ON TOP of this hook (for the selected multiplexer's
    # payload and per-tier agent CLIs), never as a parallel copy of
    # it — a tool wanted in every tier goes through the hook, exactly
    # once.
    myconfig.ai.dev.mysbx.extraTools =
      selectedMuxTools
      ++ shellRuntimeTools
      ++ config.myconfig.ai.dev.sandboxTools.extraPackages
      # Image processing for payloads (screenshot inspection, resizing
      # before handing images to vision-capable models).
      ++ [ pkgs.imagemagick ]
      ++ lib.optional cfg.browser.enable browserPackage
      # The waypipe binary of the display channel (D18): the argv
      # already pins it for the guest server wrap, but putting it on
      # the sandbox PATH too lets a payload inspect the channel
      # (`waypipe --version`) without reaching for the store path.
      ++ lib.optionals (cfg.config.display == "waypipe" && cfg.display.package != null) [
        cfg.display.package
      ];

    # Baseline [env] (RIPGREP_CONFIG_PATH — review-3 item 6 — and
    # GIT_EXTERNAL_DIFF — bd myconfig-kvo), merged with the shared
    # sandbox-tool env from the hook. The `//` keeps
    # that merge INSIDE one definition — on a key clash the tier's own
    # baseline wins over the hook, the same precedence the bubblewrap
    # tier gives its wrapper-specific `extraRuntimeEnv` over
    # `sharedEnv` — and the per-key `mkDefault` below then lets a host
    # or per-agent module win over both.
    #
    # Each value is defined with `mkDefault` INDIVIDUALLY, not the
    # attrset as a whole (review-4 item 4). `env` is an `attrsOf
    # str`, so the module system pushes definitions down per key:
    # with a per-key `mkDefault` a host or per-agent module that sets
    # the SAME key simply wins (default priority loses to normal),
    # while a definition of a DIFFERENT key merges with the baseline.
    # Both other spellings are wrong: at normal priority two unequal
    # definitions of one key are an evaluation CONFLICT (the comment
    # here used to claim they override), and `mkDefault` on the whole
    # attrset would drop the entire baseline as soon as any other
    # module defines any key at all.
    myconfig.ai.dev.mysbx.config.env = lib.mapAttrs (_: lib.mkDefault) (
      config.myconfig.ai.dev.sandboxTools.extraEnv // baselineEnv
    );

    home-manager.sharedModules = [
      {
        # The base wrapper plus every flavor of `profiles.<name>`
        # (bd myconfig-xl93): a flavor's derivation name is the crate's
        # (`mysbx-0.1.0`) for both — build-pkg-for-host.sh matches on
        # it — while the BINARIES differ (`mysbx` vs `mysbx-<profile>`),
        # so the two symlinkJoins never collide in `bin/`.
        home.packages = [ cfg.package ] ++ builtins.attrValues profileWrappers;
      }
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
