# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The Rust `mysbx` CLI (../mysbx-rs) and the package that installs it.
#
# The crate is zero-dependency by design, so the lockfile is trivial and no
# `outputHashes` can ever be needed.
#
# The final package keeps the crate's `pname`/`version` (name
# `mysbx-0.1.0`) so `./build-pkg-for-host.sh mysbx-0.1.0 <host>` keeps
# finding it in `home.packages`. It symlinks the crate's `bin/` mirror and
# re-wraps the binary (`makeBinaryWrapper`) with the three
# `MYSBX_*` pin variables the Rust CLI reads (src/lib.rs `env_or` calls):
#
#   MYSBX_BWRAP       the bubblewrap backend binary (plan.md: "The base")
#   MYSBX_SHELL       the payload shell — bash from *this wrapper's*
#                     closure, never the host `$SHELL` (plan.md:
#                     "Payload shell")
#   MYSBX_BINSH       a shell for the sandbox's `/bin/sh` — bash's own
#                     `bin/sh` from the same closure. tmux runs EVERY
#                     `run-shell`/`if-shell`/`#()` job through
#                     `execl("/bin/sh", …)` (tmux ≥ 3.5a hardcodes
#                     `_PATH_BSHELL` for jobs; `default-shell` covers
#                     panes and popups only), and the minimal sandbox
#                     root has no `/bin` at all — without this pin those
#                     jobs die with `execl failed` and tmux surfaces
#                     `'<hook command>' returned 1` popups (the workmux
#                     sidebar hooks hit exactly that). Unwrapped builds
#                     get no `/bin/sh`, like they get no pinned nix.conf.
#   MYSBX_TOOLS_PATH  the dev-tool closure on PATH (plan.md: "The base",
#                     row "dev-tool closure on PATH")
#   MYSBX_MUX_ENTRY_TMUX / _WORKMUX / _HERDR / _AOE / _ORCA
#                     the INTERACTIVE payload of a sandbox that selects
#                     that multiplexer (`multiplexer = "…"`,
#                     ../docs/design/config.md D17): the entry scripts of
#                     ./tmux-entry.nix, ./workmux-entry.nix,
#                     ./herdr-entry.nix, ./aoe-entry.nix and
#                     ./orca-entry.nix, each of
#                     which starts its multiplexer on the
#                     sandbox-internal socket. One pin per multiplexer,
#                     absent for the ones a host does not carry — a
#                     config selecting an absent one fails loudly
#                     instead of silently starting a plain shell.
#   MYSBX_WAYPIPE     the host-side `waypipe client` binary of the
#                     display channel (../docs/design/config.md D18,
#                     `display = "waypipe"`): started per run before
#                     the payload. Absent for hosts that carry no
#                     waypipe — a config selecting it is a refused
#                     run, never a silently headless one.
#   MYSBX_PODMAN_WAYPIPE
#                     the waypipe binary INSIDE the podman-gvisor
#                     image, the server end of the same channel on
#                     the container backend. Absent = refused.
#   MYSBX_KRUN_RUNTIME
#                     the podman-krun backend's OCI runtime (docs/
#                     design/backends.md D2): crun built against
#                     libkrun, the runtime a `backend =
#                     "podman-krun"` run passes to `podman
#                     --runtime`. Pinned with `--set-default`, an
#                     operator knob like MYSBX_PODMAN_PASTA_SPEC;
#                     absent = the crate's bare `crun` PATH fallback.
#   MYSBX_WAYPIPE_SECCTX
#                     NOT a wrapper pin — a deliberate operator
#                     override: the security-context application ID
#                     the host client passes to the compositor when
#                     it supports the security-context protocol
#                     (waypipe's `--secctx`). Unset (the default)
#                     means no `--secctx`.
#   MYSBX_NIX_CONF    a SANITIZED nix client configuration bound at
#                     /etc/nix/nix.conf inside the sandbox (review-2
#                     item 3). The host's own /etc/nix/nix.conf is
#                     never bound: it may hold `access-tokens` and
#                     other credentials, which a read-only bind hands
#                     to the payload just the same. The pin is bound
#                     at /etc/nix/nix.conf exactly when the daemon
#                     socket is (with the shared network).
#   MYSBX_CA_BUNDLE   a CA bundle (`ca-bundle.crt` of the `cacert`
#                     package — nss-cacert — from THIS wrapper's
#                     closure) whose store path the argv sets as
#                     `SSL_CERT_FILE` / `GIT_SSL_CAINFO` /
#                     `NIX_SSL_CERT_FILE` inside the sandbox, after
#                     `[env]` like `HOME`/`PATH` (bd myconfig-938).
#                     Belt and suspenders on top of the resolver binds
#                     of `/etc/ssl` + `/etc/static`: the pinned bundle
#                     works whatever the host's /etc layout is, and
#                     is reproducible with the rest of the closure.
#                     The same mechanism the gvisor agent image uses
#                     (agent-image.nix sets the three variables at its
#                     pinned bundle).
#   MYSBX_NONO       the nono backend binary (Landlock + seccomp
#                     sandbox, upstream nolabs-ai/nono, `pkgs.nono`;
#                     mysbx-rs/src/nono.rs). The wrapped package pins
#                     it from its own closure; an unwrapped build
#                     gets no pin and the crate's PATH fallback
#                     (`nono` via env_or) applies.
#   MYSBX_NONO_PROFILE
#                     NOT a closure path — an operator knob like
#                     MYSBX_PODMAN_PASTA_SPEC: the nono profile of
#                     every `nono run` (the crate's `--profile`
#                     flag). Pinned with `--set-default`, so an
#                     invocation can still override it. Default
#                     `"default"`, nono's built-in conservative base
#                     profile.
#   MYSBX_NONO_PROFILE
#                     the mysbx nono profile — a JSON store file
#                     generated below (`nonoProfileJson`), the empty
#                     policy that makes nono 0.74.0 a pure second
#                     layer inside the bwrap view (docs/design/
#                     backends.md D1 "nono's own inputs", bd
#                     myconfig-6di.4.3): no profile grants, no `$HOME`
#                     credential denies (with a grant covering an
#                     ancestor those abort Landlock with
#                     "deny-overlap is not enforceable"), `/tmp`
#                     and `$TMPDIR` read+write for the private
#                     tmpfs, signal isolation. Pinned with
#                     `--set-default`:
#                     an invocation can still override it with a
#                     nono profile NAME (resolved in the sandbox
#                     `$XDG_CONFIG_HOME`) or another store path.
#   MYSBX_ENV        the coreutils `env` of the layered nono backend
#                     (mysbx-rs/src/bwrap.rs, docs/design/
#                     backends.md D1 "Two environments"): it runs as
#                     nono's child inside the sandbox and applies
#                     the payload environment — `-u` for every
#                     nono-infra name, then the forwarded/[env]/pin
#                     values — so nono's environment filter never
#                     sees the payload environment. Pinned from this
#                     wrapper's own closure (`coreutils`); an
#                     unwrapped build gets no pin and the crate's
#                     PATH fallback (`env` via env_or) applies.
#
# All these pins are absolute store paths — nothing is left to host lookup.
# (MYSBX_NONO_PROFILE is an operator knob among them, like
# MYSBX_PODMAN_PASTA_SPEC.)
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  symlinkJoin,
  buildEnv,
  writeText,
  runCommand,
  linkFarm,
  bubblewrap,
  bash,
  # The CA bundle pinned as `MYSBX_CA_BUNDLE` (bd myconfig-938): the
  # `cacert` package (nss-cacert), carrying
  # `etc/ssl/certs/ca-bundle.crt` at the path the argv sets the TLS
  # env variables to. Injected by `callPackage` like every other
  # closure input, so callers never spell it out and checks pin the
  # same derivation the host closure uses. A parameter (not a hard
  # `pkgs.` reference) keeps this file evaluable against any nixpkgs
  # revision the caller brings.
  cacert,
  # The terminal emulator of `mysbx gui` (docs/design/cli.md D15): the
  # window that runs the inner mysbx, on the HOST (in the graphical
  # session the command was typed in) — deliberately NOT part of the
  # dev-tool closure or the sandbox argv. Optional: a headless host
  # passes nothing, no `MYSBX_TERMINAL` pin is set, and `mysbx gui`
  # falls back to the plain `alacritty` PATH lookup of the unwrapped
  # crate.
  alacritty ? null,
  # the dev-tool closure baked into the sandbox PATH (see the comment at
  # `toolsEnv` below for why these entries and no others)
  coreutils,
  findutils,
  gnugrep,
  gnused,
  gawk,
  which,
  less,
  procps,
  hostname,
  ripgrep,
  fd,
  jq,
  git,
  tig,
  neovim,
  nix,
  python3,
  curl,
  # `ssh` for git remotes over SSH, authenticated with the per-repo
  # sandbox key (`mysbx ssh-pubkey`).
  openssh,
  # The basic archive/patch dev tools added back to the closure (bd
  # myconfig-en7): `diff`/`cmp` (diffutils), `tar` and `gzip`/`unzip` are
  # coreutils-adjacent basics every coding-agent payload reaches for
  # (`diff` for reviewing changes, `tar`/`gzip` for handoffs and
  # tarballs); their absence broke sandbox sessions on the first `diff`.
  diffutils,
  gnutar,
  gzip,
  unzip,
  # Extra packages appended to the dev-tool closure by feature modules
  # (`myconfig.ai.dev.mysbx.extraTools`), e.g. the `pi` coding agent from
  # ../../programs/programs.pi-coding-agent. Same security note as the hardcoded
  # list below: whatever lands here is on the sandbox PATH.
  extraTools ? [ ],
  # The multiplexer entry scripts, keyed by the `multiplexer` value
  # they are the payload of (../docs/design/config.md D17): an attrset
  # like `{ tmux = <drv>; workmux = <drv>; orca = <drv>; }`, built by ../default.nix
  # for the multiplexers whose package the host has. Each entry becomes
  # the `MYSBX_MUX_ENTRY_<VALUE>` pin.
  #
  # The empty default — what an unwrapped `nix-build` of this file gets
  # — pins nothing, so every `multiplexer = "…"` is a refused run
  # rather than a silent bare shell. A `null` value is treated like an
  # absent one, so callers may pass a gated attrset unfiltered.
  muxEntries ? { },
  # The gVisor agent OCI image of the podman-gvisor backend
  # (../docs/podman-load-image.md, bd myconfig-6di.1): `null` pins
  # nothing — `backend = "podman-gvisor"` is a refused run and
  # `mysbx podman-load-image` a usage error, never an invented
  # `localhost/…` reference pulled from a registry that does not
  # exist (bd myconfig-xrt). The module layer defaults this to
  # `myconfig.ai.dev.mysbx.podman.image` (../podman.nix).
  podmanImage ? null,
  # The shell of the podman-gvisor backend's INTERACTIVE payload (bd
  # myconfig-cew): the store path of the fish binary as it exists INSIDE
  # `podmanImage`. The wrapper pins it as `MYSBX_PODMAN_SHELL` so a
  # container session lands in the same shell as the host — the image
  # carries fish and the plugin/alias closure of the host's rendered
  # `~/.config/fish`, which mysbx mounts read-only, so the binary path
  # resolves inside the container. `null` keeps the image's own
  # `Cmd` (`/bin/bash`, agent-image.nix).
  #
  # A store path, like every other pin: the caller passes the fish
  # binary path of the SAME package the image bakes (the module layer
  # threads it), so the path always matches a build of the image
  # actually loaded.
  podmanShell ? null,
  # The podman network spec of the podman-gvisor backend
  # (`MYSBX_PODMAN_PASTA_SPEC`, ../docs/podman-load-image.md): a
  # `pasta:--map-guest-addr,<address>` spec that makes the host's
  # LiteLLM forwarder reachable from inside the container. `null`
  # leaves podman's default (shared) network.
  #
  # Pinned with `--set-default`, not `--set`: the variable is also an
  # operator override (the docs document it as one), and a network
  # spec is a per-invocation debugging knob rather than a closure
  # path that must match the build.
  podmanPastaSpec ? null,
  # Backend-specific environment pins of the podman-gvisor backend
  # (`MYSBX_PODMAN_ENV`): an attrset rendered as a space-separated
  # `KEY=VALUE` list. The container gets them as `--env` after the
  # config layers and before the sandbox's own infrastructure
  # variables (src/podman_gvisor.rs section 7). What belongs here is
  # environment that is only correct under THIS backend, e.g. the
  # container-side URL of the host's LiteLLM forwarder, which differs
  # from the host loopback URL the bwrap backend uses.
  #
  # Values must not contain whitespace — the crate splits the list on
  # it, so a value with a space would be silently truncated.
  podmanEnv ? { },
  # The podman-krun backend's OCI runtime (backends.md D2, bd
  # myconfig-6di.5.2): crun built against libkrun — nixpkgs' `crun`
  # already defaults `withLibkrun` to `lib.meta.availableOn
  # stdenv.hostPlatform libkrun` (true on x86_64-linux), so the
  # plain `crun` package carries the krun handler and no override is
  # needed. `null` pins nothing — `backend = "podman-krun"` then runs
  # against the crate's bare `crun` PATH fallback (an unwrapped
  # build's spelling, the same contract as MYSBX_BWRAP).
  #
  # Pinned with `--set-default`, not `--set`: the variable stays an
  # operator knob (a runtime swap for debugging), the same reasoning
  # as MYSBX_PODMAN_PASTA_SPEC.
  krunRuntime ? null,
  # The size cap of the per-run scratch disk of a krun + guest-nix run
  # (bd myconfig-0pi): the wrapper pins it as
  # MYSBX_KRUN_SCRATCH_SIZE, and the crate truncates a per-run
  # SPARSE file under `<sidecar>/scratch/` to it before the exec —
  # disk-backed, never tmpfs. `null` pins nothing: the guest nix
  # wrapper then announces its tmpfs fallback. Set by default.nix
  # when `krun.nix.enable` (the same host that bakes the loop-mounting
  # wrapper into the image).
  krunScratchSize ? null,
  # The waypipe binary of the HOST side of the display channel
  # (../docs/design/config.md D18, `display = "waypipe"`): the wrapper
  # pins it as `MYSBX_WAYPIPE`, and a run that selects waypipe without
  # the pin is refused — a host that carries no waypipe never starts
  # a silently headless sandbox. `null` pins nothing.
  waypipe ? null,
  # The waypipe binary INSIDE the podman-gvisor image (D18 on the
  # container backend): the store path of the waypipe binary as it
  # exists inside `podmanImage`, pinned as `MYSBX_PODMAN_WAYPIPE` for
  # the same refusal semantics as the bwrap-side `waypipe`. `null`
  # pins nothing — `display = "waypipe"` with `backend =
  # "podman-gvisor"` is refused.
  podmanWaypipe ? null,
  # The nono backend binary (mysbx-rs/src/nono.rs, upstream
  # nolabs-ai/nono): `null` pins nothing and the crate's PATH
  # fallback (`nono` via env_or) applies — the same contract as the
  # bubblewrap pin above. Injected by callPackage; a parameter, not a
  # `pkgs.` reference, keeps this file evaluable against any nixpkgs
  # revision the caller brings.
  nono ? null,
  # The `ssh-keygen` generating the per-repo sandbox keypair
  # (mysbx-rs/src/lib.rs `ensure_ssh_key`, docs/design/config.md
  # D22): `null` pins nothing and the crate's PATH fallback applies —
  # the same contract as the bubblewrap pin above. Injected by
  # callPackage (the module layer passes `openssh`); a parameter, not
  # a `pkgs.` reference, keeps this file evaluable against any nixpkgs
  # revision the caller brings.
  ssh-keygen ? null,
}:

let
  # The dev-tool `PATH` value the argv builder uses (../docs/TODOs/
  # mvp-4-bwrap-argv.md section 6): a single store tree whose /bin is the
  # union of the tools' bins, so `toolsPath` stays one absolute path.
  #
  # This is the MVP's hardcoded dev-tool closure, mirroring
  # ../fns/bubblewrap-app.nix `devTools` minus the package-management
  # and linting extras (`wget`, `shfmt`, `shellcheck` — `curl` already
  # covers fetching, and linting tools are per-project taste agents
  # fetch via nix/flake, not from the sandbox base), plus `hostname`
  # and `tig` (the git TUI: the payload is always a git worktree, and
  # reviewing it is the one interactive job a sandboxed agent session
  # hands back to the human — a tiny closure next to the `git` that is
  # already shipped). `neovim` is the sandbox's `$EDITOR`: without an
  # editor `git commit` aborts, `mysbx edit` has nothing to open
  # (`../docs/design/cli.md` D12) and a payload that spawns `$EDITOR`
  # fails — forwarding the host's value does not help, because it names
  # a program that need not exist in here. `diffutils`, `gnutar`,
  # `gzip` and `unzip` are back in (bd myconfig-en7): they are not
  # package management but basic dev tools, and their absence broke
  # sandbox payloads on the first `diff`/`tar`. Per plan.md phase 2d, `mysbx` consumes the
  # shared `myconfig.ai.dev.sandboxTools` hook like every other tier
  # — the hook's packages arrive in `extraTools` via ../default.nix.
  # The baseline list lives HERE, next to the code that consumes it,
  # because (per mvp-6) "it is a security-relevant list, not packaging
  # detail".
  #
  # `extraTools` is the ONE extension point on top of that list — the
  # shared sandbox-tool packages of the hook plus mysbx-specific
  # additions (the selected multiplexer's payload, per-agent CLIs like
  # the `pi` of ../../programs/programs.pi-coding-agent) — instead of
  # editing this list.
  toolsEnv = buildEnv {
    name = "mysbx-tools";
    paths = [
      coreutils
      findutils
      gnugrep
      gnused
      gawk
      which
      less
      procps
      hostname
      ripgrep
      fd
      jq
      git
      tig
      neovim
      nix
      python3
      curl
      openssh
      diffutils
      gnutar
      gzip
      unzip
      bash
    ]
    ++ extraTools;
  };

  # The bare Rust crate, without the wrapper. Exposed as
  # `mysbx.passthru.crate` so the `nix/checks.nix` check (mvp-6) can run
  # the cargo test suite against the unwrapped binary — the tests set
  # their own `MYSBX_*` variables and must never see the wrapper's pins.
  crate = rustPlatform.buildRustPackage {
    pname = "mysbx";
    version = "0.1.0";

    src = ../mysbx-rs;
    cargoLock.lockFile = ../mysbx-rs/Cargo.lock;

    # The test suite runs in `nix/checks.nix`; keep it out of every
    # production host rebuild.
    doCheck = false;

    meta = {
      description = "My sandboxing tool (unwrapped crate build — for tests)";
      mainProgram = "mysbx";
      platforms = lib.platforms.linux;
    };
  };
  # The sandbox's nix.conf (review-2 item 3): GENERATED, never the
  # host's. The host file is what carries `access-tokens` (GitHub and
  # GitLab credentials), `netrc-file` pointers and other per-machine
  # secrets — none of which a sandboxed agent should read, and a
  # read-only bind protects nothing there.
  #
  # What stays is the minimum that makes the shipped `nix` usable at
  # all: the flake CLI (agents run `nix build`/`nix develop` on flake
  # repos) and the public cache. Nothing here is a credential, and
  # nothing is copied from the host, so this file is safe to show in
  # `--dry-run` output.
  #
  # It only takes effect when the network is shared: the daemon socket
  # under /nix/var/nix is bound with `--share-net` and nowhere else.
  #
  # The substituter lines are near-inert for an untrusted client (the
  # daemon decides what it fetches), so they document intent rather
  # than grant anything: the host's own substituters — which may carry
  # credentials in their URLs — are deliberately NOT carried over. If a
  # host ever needs more here, this becomes a module option; it is a
  # string today because there is exactly one consumer.
  sandboxNixConfText = writeText "mysbx-nix.conf" ''
    # Generated by myconfig for the mysbx sandbox — do not edit.
    # The host's /etc/nix/nix.conf is deliberately NOT mounted.
    experimental-features = nix-command flakes
    substituters = https://cache.nixos.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
  '';
  # The directory the pin's `NIX_CONF_DIR` arithmetic resolves to:
  # nono's exec env sets `NIX_CONF_DIR` to the parent of the pinned
  # file, and nix reads that directory's `nix.conf` — so the pin must
  # BE a file named `nix.conf` inside a directory (bd myconfig-bf2; a
  # bare writeText file made the parent `/nix/store`, and the
  # configuration silently missed). The same linkFarm idiom the
  # flake's own dev shell uses for its NIX_CONF_DIR.
  sandboxNixConfDir = linkFarm "mysbx-nix-conf-dir" [
    {
      name = "nix.conf";
      path = sandboxNixConfText;
    }
  ];
  # The pin itself: `<dir>/nix.conf`, the file bwrap binds and nono's
  # NIX_CONF_DIR derives its containing directory from. Concatenated
  # (`+`, not `"${...}"`) so the string keeps the derivation-output
  # context of the directory.
  sandboxNixConf = sandboxNixConfDir + "/nix.conf";

  # The mysbx nono profile (docs/design/backends.md D1 "nono's own
  # inputs", bd myconfig-6di.4.3): a JSON store file that turns nono
  # 0.74.0 into a pure second layer inside the bwrap view. nono's
  # built-in profiles (the `default` group set) carry `$HOME`
  # credential denies — browser configs, shell rc files — and a deny
  # inside an ancestor the argv grants (`--allow /mysbx-home`) aborts
  # Landlock on Linux with "deny-overlap is not enforceable" BEFORE
  # the payload runs. The profile here carries:
  #
  # - `filesystem.write = ["/tmp", "$TMPDIR"]` (with the `read`
  #   counterpart above): /tmp is bwrap's
  #   private tmpfs and TMPDIR is /mysbx-nono/tmp (the `--setenv
  #   TMPDIR` infra env), so both exist in the view; Landlock has no
  #   default access, without this the payload cannot drop temp
  #   files at all.
  # - `filesystem.read = ["/tmp", "$TMPDIR"]` next to the write
  #   grant: nono's `write` axis is Landlock WRITE-ONLY (nono 0.74.0
  #   `AccessMode::Write` adds no `ReadFile`) — without the read
  #   counterpart every temp file the payload drops is one it can
  #   never open(O_RDONLY) again (EACCES on read-back; `cargo`
  #   round-tripping cc objects through $TMPDIR was the live
  #   casualty, bd myconfig-2pe). Both paths still exist only in
  #   the bwrap view (D1 below), so the read grant widens nothing
  #   into the host.
  # - NO other read/allow grants and NO deny: every filesystem
  #   grant beyond /tmp + $TMPDIR is derived from the RESOLVED
  #   bwrap layout by nono.rs (D1 "grants follow the resolved
  #   layout") — a static profile grant could only fight it, and
  #   the `$HOME` credential denies of the built-in `default`
  #   profile are exactly what cannot be expressed under a broad
  #   grant.
  # - `network.block = false`: outbound network is the `nono run`
  #   argv's decision (block/allowlist/proxy), the profile stays out
  #   of it.
  # - `security.signal_mode = "isolated"`: the payload runs in the
  #   bwrap pid namespace — a signal from inside must not escape to
  #   nono's own process.
  # - `groups.include = []`: no nono group expansion — pack/group
  #   contents are a moving target between nono releases, and the
  #   extended upstream `default` groups are the $HOME denies above.
  # - `workdir.access = "none"`: the CWD grant comes from the argv
  #   (`--allow-cwd` + the repo grant), the profile adds nothing.
  #
  # The build VALIDATES the file with the pinned nono
  # (`nono profile validate` — nono 0.74.0 parses profiles as
  # JSON/JSONC, not TOML), so a schema change upstream fails the
  # packaging, never a sandbox run. When the caller pins no nono the
  # profile is not referenced by any pin either, so `nativeBuild`
  # skips the validation step.
  nonoProfileJson =
    runCommand "mysbx-nono-profile.json"
      {
        passAsFile = [ "mysbxNonoProfileText" ];
        mysbxNonoProfileText = builtins.toJSON {
          meta = {
            name = "mysbx";
            description = "mysbx layered nono backend profile (docs/design/backends.md D1, bd myconfig-6di.4.3)";
          };
          groups = {
            include = [ ];
          };
          security = {
            signal_mode = "isolated";
          };
          linux = {
            # bd myconfig-6di.4.5 — pathname AF_UNIX seccomp mediation.
            # Without it, nono 0.74.0's default is "off": ANY filesystem
            # grant (and mysbx's grants are broad!) makes pathname
            # sockets reachable on Landlock V4+ — so any file
            # `/mysbx-home/.mysbx-tmux/socket` could be connected by
            # the payload once the directory grant covers the path.
            # With "pathname" the seccomp filter requires an explicit
            # unix_socket* grant instead, which mysbx's argv emits
            # exactly for the mux socket dir when a session starts
            # (bwrap.rs MUX_SOCKET_DIR — D16/D17-isolated from the
            # host), keeping tmux's socket LANDLOCK-ONLY-REACHABLE,
            # never host-visible. `--allow-unix-socket-*` CLI flags
            # funnel through the same gate.
            af_unix_mediation = "pathname";
          };
          network = {
            block = false;
          };
          workdir = {
            access = "none";
          };
          # bwrap's private tmpfs; bwrap `--clearenv` drops the host
          # TMPDIR and the infra env re-seeds it below the nono state
          # tmpfs (`/mysbx-nono/tmp`), so the profile's own `$TMPDIR`
          # resolves to the nono-private tmpfs, never to a host path
          # (bd myconfig-7hh — nono's `validated_tmpdir()` defaults to
          # /tmp when TMPDIR is unset, which would grant the HOST /tmp
          # through Landlock). /tmp here covers bwrap's private tmpfs
          # for payloads that hardcode it instead of TMPDIR.
          #
          # `read` mirrors `write` path for path (bd myconfig-2pe):
          # nono 0.74.0's `AccessMode::Write` is Landlock WRITE-ONLY
          # (no `ReadFile` in its rule), so a temp file the payload
          # created could never be opened for reading again — the
          # read axis is the read-back half of the SAME tmpfs grant,
          # not a new path into the host.
          filesystem.read = [
            "/tmp"
            "$TMPDIR"
          ];
          filesystem.write = [
            "/tmp"
            "$TMPDIR"
          ];
        };
        nativeBuildInputs = lib.optionals (nono != null) [ nono ];
      }
      (
        # nono parses the generated profile: a schema change upstream
        # fails HERE, not at a sandbox run (`profile validate` is the
        # same gate `nono profile promote` runs before writing a
        # draft).
        lib.optionalString (nono != null) ''
          ${lib.getExe nono} profile validate "$mysbxNonoProfileTextPath"
        ''
        + ''
          cp "$mysbxNonoProfileTextPath" "$out"
        ''
      );
  # One `--set MYSBX_MUX_ENTRY_<VALUE>` per available entry. The
  # variable names are the ones `config.rs::Multiplexer::entry_var`
  # reads — the mapping is `"MYSBX_MUX_ENTRY_" + uppercase(value)`, and
  # the crate's own test asserts that shape, so a new multiplexer needs
  # no change here beyond `muxEntries` gaining a key.
  muxEntryPins = lib.concatStringsSep " \\\n      " (
    lib.mapAttrsToList (
      name: entry: "--set MYSBX_MUX_ENTRY_${lib.toUpper name} '${lib.getExe entry}'"
    ) (lib.filterAttrs (_: entry: entry != null) muxEntries)
  );
  # The `mysbx gui` terminal pin (docs/design/cli.md D15): an absolute
  # store path, the same wrapper idiom as MYSBX_BWRAP. Empty when the
  # caller passes no alacritty — the fallback PATH lookup of the
  # unwrapped crate applies then.
  terminalPin = if alacritty != null then "--set MYSBX_TERMINAL '${lib.getExe alacritty}'" else "";
  # The pinned CA bundle (bd myconfig-938): the `cacert` package's
  # (nss-cacert's) own `ca-bundle.crt`, at the in-package path the argv
  # sets the three TLS env variables to. An absolute store path from
  # THIS closure, like every other pin — the sandbox's TLS trust
  # anchors are therefore reproducible and independent of the host's
  # /etc layout.
  caBundle = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  # The fish tab completion shipped in the package
  # (../mysbx-rs/completions) — a nix store path, not a $src reference:
  # `symlinkJoin` has no source directory to install from.
  completions = ../mysbx-rs/completions/mysbx.fish;
  # The podman-gvisor pins: the image tarball, the reference runs use,
  # and the expected image ID (the config-blob digest, extracted ONCE
  # at build time).
  # `podman` runs the reference; `podman-load-image` compares IDs to
  # detect a stale build under the same tag. All three are LAZY — a
  # `null` podmanImage must not force `imageName` on null.
  #
  # The shell pin (bd myconfig-cew) sits in its own optionalString:
  # it is meaningful even for a caller that builds its own image
  # reference (MYSBX_PODMAN_IMAGE by hand), but a `null` pins nothing
  # and the crate's image-OCI default (`/bin/bash`) applies. The
  # container `PATH` needs NO pin: the image's buildEnv links every
  # baked package's `bin` into `/bin`, so the OCI `PATH=/bin:/usr/bin`
  # already covers the provisioned tools.
  # The rendered `MYSBX_PODMAN_ENV` value (see the `podmanEnv`
  # argument): `KEY=VALUE` entries, space-separated, in attribute
  # order. A value carrying whitespace cannot survive that encoding,
  # so refuse it here instead of shipping a truncated variable.
  podmanEnvValue = lib.concatStringsSep " " (
    lib.mapAttrsToList (
      name: value:
      if builtins.match ".*[[:space:]].*" value != null then
        throw "mysbx: podmanEnv.${name} must not contain whitespace (MYSBX_PODMAN_ENV is a space-separated list), got `${value}`"
      else
        "${name}=${value}"
    ) podmanEnv
  );

  podmanPins =
    lib.optionalString (podmanImage != null) (
      "--set MYSBX_PODMAN_TARBALL '${podmanImage}' "
      + "--set MYSBX_PODMAN_IMAGE '${podmanImage.imageName}:${podmanImage.imageTag}' "
      + "--set MYSBX_PODMAN_IMAGE_ID \"$(cat ${
        runCommand "mysbx-gvisor-image-id"
          {
            nativeBuildInputs = [
              gnutar
              gzip
              gnused
            ];
          }
          ''
            # The config entry is `<sha256hex>.json`, with or without the
            # `sha256:` prefix depending on the archive writer —
            # dockerTools' buildLayeredImage omits it.
            tar --extract --to-stdout --file ${podmanImage} manifest.json \
              | tr -d '"' | sed -n 's/.*Config[[:space:]]*:[[:space:]]*\(sha256:\)\{0,1\}\([0-9a-f]\{64\}\)\.json.*/\2/p' > $out
          ''
      })\" "
    )
    + lib.optionalString (podmanShell != null) "--set MYSBX_PODMAN_SHELL '${podmanShell}' "
    + lib.optionalString (
      podmanPastaSpec != null
    ) "--set-default MYSBX_PODMAN_PASTA_SPEC '${podmanPastaSpec}' "
    + lib.optionalString (podmanEnv != { }) "--set MYSBX_PODMAN_ENV '${podmanEnvValue}' ";
  # The podman-krun runtime pin (backends.md D2): the crun+libkrun
  # store path `--runtime` gets on the krun variant. `--set-default`:
  # an invocation can still point MYSBX_KRUN_RUNTIME at any runtime.
  krunPins =
    lib.optionalString (
      krunRuntime != null
    ) "--set-default MYSBX_KRUN_RUNTIME '${krunRuntime}/bin/crun' "
    # The per-run scratch disk of the krun + guest-nix run (bd
    # myconfig-0pi): the size cap the crate truncates its sparse
    # file to. `--set-default`: an invocation can shrink or drop it
    # (an empty value falls back to the announced tmpfs scratch),
    # the same operator-knob reasoning as the runtime pin.
    + lib.optionalString (
      krunScratchSize != null
    ) "--set-default MYSBX_KRUN_SCRATCH_SIZE '${krunScratchSize}'";
  # The display-channel pins (D18): the host-side client binary and
  # the in-image server binary. Both optional, both absolute store
  # paths — the wrapper idiom of every other pin.
  waypipePins =
    lib.optionalString (waypipe != null) "--set MYSBX_WAYPIPE '${lib.getExe waypipe}' "
    + lib.optionalString (podmanWaypipe != null) "--set MYSBX_PODMAN_WAYPIPE '${podmanWaypipe}'";
  # The nono backend pins of the layered backend (bd
  # myconfig-6di.4.3): the binary (`--set`, an absolute store path
  # like every other closure pin) and the mysbx profile (the
  # generated store file `nonoProfileJson` above, `--set-default` —
  # an invocation can still point MYSBX_NONO_PROFILE at any nono
  # profile name or path). Both are gated on `nono != null`
  # together: without the binary neither pin has a consumer in the
  # sandbox, so a null-pinning caller gets neither.
  nonoPins = lib.optionalString (
    nono != null
  ) "--set MYSBX_NONO '${lib.getExe nono}' --set-default MYSBX_NONO_PROFILE '${nonoProfileJson}'";
  # The pinned `env` of the layered nono backend (backends.md D1,
  # "Two environments"): the coreutils `env` from THIS closure — an
  # absolute store path visible through the read-only `/nix/store`
  # bind, never a host PATH lookup. Gated with the nono pins: without
  # the backend the env pin is meaningless noise in the wrapper, the
  # same reasoning as the profile pin above.
  nonoEnvPins = lib.optionalString (nono != null) "--set MYSBX_ENV '${coreutils}/bin/env'";
  # The `ssh-keygen` of the sandbox keypair generation (docs/design/
  # config.md D22): an absolute store path from mysbx's own closure —
  # the same wrapper idiom as MYSBX_BWRAP, so host-side key generation
  # never depends on the host PATH. Empty when the caller pins
  # nothing — the fallback PATH lookup of the unwrapped crate applies.
  sshKeygenPins =
    if ssh-keygen != null then "--set MYSBX_SSH_KEYGEN '${ssh-keygen}/bin/ssh-keygen'" else "";
in
symlinkJoin {
  # keep the crate's derivation name: build-pkg-for-host.sh matches on
  # `(p.name or p.pname)` == "mysbx-0.1.0" in home.packages
  inherit (crate) pname version;

  paths = [ crate ];

  nativeBuildInputs = [ makeBinaryWrapper ];

  postBuild = ''
    wrapProgram "$out/bin/mysbx" \
      --set MYSBX_BWRAP '${bubblewrap}/bin/bwrap' \
      --set MYSBX_SHELL '${bash}/bin/bash' \
      --set MYSBX_BINSH '${bash}/bin/sh' \
      --set MYSBX_TOOLS_PATH '${toolsEnv}/bin' \
      --set MYSBX_NIX_CONF '${sandboxNixConf}' \
      --set MYSBX_CA_BUNDLE '${caBundle}' \
      ${muxEntryPins} \
      ${terminalPin} \
      ${podmanPins} \
      ${krunPins} \
      ${waypipePins} \
      ${nonoPins} \
      ${nonoEnvPins} \
      ${sshKeygenPins}

    # Hand-written fish tab completion (../mysbx-rs/completions, kept in
    # sync with the CLI surface by the `mysbx-completions` check in
    # checks.nix). The crate stays
    # zero-dependency: this is a plain fish script, not clap-generated.
    # The vendor path is the one `installShellFiles --fish` uses and
    # fish's NixOS integration collects.
    install -Dm 0644 ${completions} \
      $out/share/fish/vendor_completions.d/mysbx.fish
  '';

  meta = {
    description = "My sandboxing tool — a bubblewrap sandbox CLI for coding agents";
    mainProgram = "mysbx";
    platforms = lib.platforms.linux;
  };

  passthru = {
    inherit crate toolsEnv sandboxNixConfDir;
    # The pin the wrapper sets (`<dir>/nix.conf`), for the checks.nix
    # shape assertion of bd myconfig-bf2.
    sandboxNixConfPin = sandboxNixConf;
    # The mysbx nono profile the wrapper sets (bd myconfig-6di.4.3),
    # for the checks.nix gate that re-validates the DELIVERED file
    # with the nono the wrapper pins (mysbx-nono-profile-test).
    nonoProfilePin = nonoProfileJson;
    # The editor the generated `[env]` points `EDITOR`/`VISUAL` at, so
    # the variables and the closure can never name different builds.
    editor = neovim;
    caBundle = caBundle;
    completions = completions;
  };
}
