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
#   MYSBX_SHELL       the interactive payload shell: the `shell` argument
#                     when the caller pins one (the host's own shell, bd
#                     myconfig-l44), else bash from *this wrapper's*
#                     closure — never the host `$SHELL` (plan.md:
#                     "Payload shell")
#   MYSBX_GUI_SHELL   a POSIX shell for the `gui` closing-message
#                     wrapper (src/lib.rs GUI_FINAL_MESSAGE_SCRIPT).
#                     Deliberately NOT MYSBX_SHELL: a payload shell may
#                     be fish, which cannot run that script at all
#                     (`status=$?`)
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
#   MYSBX_DTACH      the dtach master of detached runs (../docs/
#                     design/detach.md B10, `--detach`): the host-side
#                     process that holds the detached run's pty.
#                     Absent for hosts that carry no dtach — a
#                     `--detach` is a refused run naming the
#                     variable, never a silently undetached one;
#                     other runs are unaffected.
#   MYSBX_PODMAN_WAYPIPE
#                     the waypipe binary INSIDE the podman-gvisor
#                     image, the server end of the same channel on
#                     the container backend. Absent = refused.
#   MYSBX_PODMAN_MUX_ENTRY_TMUX / _WORKMUX / _HERDR / _AOE / _ORCA
#                     the entry scripts INSIDE the podman image, the
#                     multiplexer payload of both podman backends
#                     (same scripts as MYSBX_MUX_ENTRY_*, baked into
#                     the image). Absent = refused.
#   MYSBX_KRUN_RUNTIME
#                     the podman-krun backend's OCI runtime (docs/
#                     design/backends.md D2): crun built against
#                     libkrun, the runtime a `backend =
#                     "podman-krun"` run passes to `podman
#                     --runtime`. Pinned with `--set-default`, an
#                     operator knob like MYSBX_PODMAN_PASTA_SPEC;
#                     absent = the crate's bare `crun` PATH fallback.
#   MYSBX_VM_STORE_DISK
#                     the guest-own nix store disk of the VM
#                     backends (docs/design/guest-store.md): an
#                     erofs image carrying the tools closure plus
#                     a registered nix db, attached as a second
#                     virtio-blk device and mounted read-only at
#                     /nix — the host store is then NOT shared at
#                     all. Pinned by this wrapper when
#                     `vmStoreBaked` (../default.nix wires it to
#                     `vm.store.enable`); absent = the host-store
#                     share of backends.md D3, unchanged.
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
#   MYSBX_ENABLED_BACKENDS
#                     NOT a closure path — the availability gate: the
#                     space-separated list of backends this host carries
#                     (the module layer's `enabledBackends`, from
#                     myconfig.ai.dev.mysbx.podman.enable / krun.enable /
#                     microvm-qemu.enable; `bubblewrap` and `nono`
#                     always). The CLI refuses every backend NOT in the
#                     list with a message naming the module option —
#                     load-bearing for `podman-krun`, whose bare-`crun`
#                     PATH fallback would otherwise silently run an
#                     ordinary container instead of a microVM.
#                     UNSET (an unwrapped build, or a caller that
#                     passes `null`) means no gate: every backend the
#                     crate knows is selectable, the historical
#                     contract of a hand-run build.
#   MYSBX_PROFILE    the launcher identity of this wrapper (config.md
#                     D30, bd myconfig-011r): a flavor wrapper pins
#                     its profile name so the crate's start-time gate
#                     can tell a `mysbx-<profile>` run from a base
#                     one when the merged config sets `profile`.
#                     The base wrapper pins nothing — unset means
#                     the base launcher, exactly the identity the
#                     gate compares against. NOT to be confused with
#                     MYSBX_NONO_PROFILE (a nono backend operator
#                     knob naming the backend's profile file): this
#                     variable names the INVOCATION, not a backend
#                     setting, and overriding it changes only which
#                     launcher the gate admits — never the tool
#                     closure, which is embedded in this binary.
#
# All these pins are absolute store paths — nothing is left to host lookup.
# (MYSBX_NONO_PROFILE is an operator knob among them, like
# MYSBX_PODMAN_PASTA_SPEC.)
{
  lib,
  callPackage,
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
  # The interactive payload shell of a `multiplexer = "none"` run (bd
  # myconfig-l44): the host's own shell binary, so a sandbox session has
  # the host's prompt, abbreviations and aliases instead of a bare bash
  # that reads no configuration at all. `null` keeps this wrapper's
  # `bash` (and the crate's `/bin/sh` PATH fallback for an unwrapped
  # build) — the pre-l44 payload.
  #
  # The path must be a store path of the caller's closure: it is
  # reachable inside the sandbox through the read-only `/nix/store` bind,
  # and it is the SAME binary the host user logs into. Its configuration
  # is mounted separately (../default.nix `shellConfigMounts`), rendered
  # sandbox-safe by ./shell-config.nix.
  shell ? null,
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
  # The guest-own store disk's build inputs (./krun-store-image.nix,
  # the `vmStoreBaked` pin): the closure dumper, the erofs packer
  # and the db writer — injected by `callPackage`, consumed only when
  # the disk is built.
  closureInfo,
  erofs-utils,
  sqlite,
  # Extra packages appended to the dev-tool closure by feature modules
  # (`myconfig.ai.dev.mysbx.extraTools`), e.g. the `pi` coding agent from
  # ../../programs/programs.pi-coding-agent. Same security note as the hardcoded
  # list below: whatever lands here is on the sandbox PATH.
  extraTools ? [ ],
  # C7b (n8kv idea 13, bd myconfig-f23e): the declared major version
  # of the nodejs the toolchain closure carries, or null when the
  # host declares none. Pinned as `MYSBX_NODE_MAJOR`; the argv
  # builders gate the `NODE_USE_ENV_PROXY` injection on it (a
  # WRAPPER-SIDE fact — the builders are pure and `node` is not on
  # the sandbox PATH).
  nodeMajor ? null,
  # The generated credential-free `.npmrc` (the module builds it —
  # never the host's): pinned as `MYSBX_NPMRC`, bound read-only at
  # `$HOME/.npmrc` inside the sandbox. Null pins nothing.
  npmrc ? null,
  # The podman spelling of the npmrc pin: the same host store
  # path, which resolves inside the image because
  # `podman.imagePackages` bakes the package into the image
  # layers (the egress-relay pin's rule). Null pins nothing.
  podmanNpmrc ? null,
  # The binary name under $out/bin: "mysbx" for the base wrapper,
  # "mysbx-<profile>" for a flavor wrapper of `profiles.<name>` in
  # ../default.nix. The crate builds ONE `bin/mysbx`; this symlinkJoin
  # mirrors it under the given name so several wrappers — same crate,
  # different `extraTools` — can sit next to each other in
  # `home.packages` without a file collision. Everything else (the
  # pins, the closure, the second `mysbx-once` entry) is identical
  # shape; `mainProgram` and the fish completions follow the name.
  #
  # The once-mode dispatch (src/once.rs `ONCE_ARGV0`) matches the
  # literal `mysbx-once` basename, so a flavor keeps that name for its
  # once entry: the MYSBX_ONCE=1 marker the second wrapped program
  # sets carries the mode without argv[0] (which a makeBinaryWrapper
  # chain cannot — each `exec` resets it).
  binName ? "mysbx",
  # The launcher profile this wrapper IS (config.md D30, bd
  # myconfig-011r): `null` (the base wrapper) pins nothing; a flavor
  # passes its own name and the wrapper sets `MYSBX_PROFILE`, the pin
  # the crate's profile gate (mysbx-rs/src/profile.rs) reads to tell
  # its launcher apart from the base. Unrelated to MYSBX_NONO_PROFILE
  # (a nono operator knob): this pin is the INVOCATION identity, not
  # a backend setting — an invocation may override it, which only
  # affects the consistency gate, never what runs inside the sandbox
  # (the tool closure is this wrapper's own MYSBX_TOOLS_PATH either
  # way).
  profile ? null,
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
  # The entry scripts baked into `podmanImage` (the module layer's
  # `podman.muxEntries`), same shape as `muxEntries`. Each becomes the
  # `MYSBX_PODMAN_MUX_ENTRY_<VALUE>` pin of both podman backends; the
  # store path is the same inside the image as on the host.
  podmanMuxEntries ? { },
  # The agent registry pins (bd myconfig-fv4h, ../docs/design/
  # config.md D32): `agentNames` is the whitespace-separated, SORTED
  # kind list (`MYSBX_AGENT_NAMES` — the `--<kind>` shorthand arms
  # match it), `agentsFile` the path of the rendered registry
  # (`MYSBX_AGENTS_FILE` — `[<kind>] command = "…"`, the CLI's
  # `resolve_agent` parses it). Empty/null pins nothing: every agent
  # selection is then a refused run, like an unpinned multiplexer.
  agentNames ? "",
  agentsFile ? null,
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
  # The socket file NAME of the `network = "proxy-only"` channel
  # (`MYSBX_EGRESS_SOCKET_NAME`, docs/design/egress.md E4, bd
  # myconfig-ps3i): the file the per-run egress broker binds inside
  # `$XDG_RUNTIME_DIR/mysbx-egress/<pid>/`. One plain file-name
  # component — the Rust side refuses anything else. `null` pins
  # nothing, and a `proxy-only` run without a pinned name is REFUSED
  # (it fails closed). Pinned with `--set-default`: a debugging run
  # may point it at another name.
  egressSocketName ? null,
  # The host LiteLLM port the in-sandbox relay listens on
  # (`MYSBX_EGRESS_PORT`, docs/design/config.md D20): the relay binds
  # `127.0.0.1:<port>` inside the sandbox and forwards to the egress
  # socket, so an agent keeps its baked `127.0.0.1:<litellm port>`
  # URLs. `null` pins nothing.
  egressPort ? null,
  # The per-run egress broker (`MYSBX_EGRESS_BROKER`,
  # docs/design/egress.md E1, bd myconfig-n8kv.1.2) is REQUIRED for
  # `network = "proxy-only"` (bd myconfig-ps3i): the static
  # `agent-litellm-forward-unix` unit is gone, so the broker is the
  # only channel. A closure path, so `--set`, not `--set-default`: it
  # must match this build.
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
  # Backend-specific environment pins of the microvm-qemu backend
  # (`MYSBX_MICROVM_QEMU_ENV`, bd myconfig-mzys): an attrset rendered
  # as a space-separated `KEY=VALUE` list, the qemu twin of
  # `podmanEnv`. The SLiRP guest's loopback is its own, so the model
  # endpoint URLs the other backends share (the host loopback the
  # bwrap backend reaches, the TSI-translated loopback of the krun
  # backend) are dead here — the pins repoint them at the SLiRP
  # gateway `10.0.2.2`, which libslirp maps to the HOST loopback.
  #
  # Values must not contain whitespace — the crate splits the list on
  # it, so a value with a space would be silently truncated.
  microvmQemuEnv ? { },
  # The availability gate (MYSBX_ENABLED_BACKENDS): the backends this
  # host carries, space-separated in the wrapper pin — `null` (the
  # callPackage default, and what the unwrapped stub builds of
  # nix/checks.nix pass) pins NOTHING and leaves every backend the
  # crate knows selectable. Non-null values are validated against the
  # crate's universe at build time, so a typo in the module layer fails
  # the build instead of a sandbox run.
  enabledBackends ? null,
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
  # The size cap of the per-run scratch disk is a config key now
  # (D23, `scratch-size` in the generated user config, fed by
  # `krun.nix.scratchSize`): the crate truncates a per-run SPARSE
  # file under `<sidecar>/scratch/` to the merged value before the
  # exec — disk-backed, never tmpfs. Without the key the guest nix
  # wrapper announces its tmpfs fallback. The env var
  # MYSBX_KRUN_SCRATCH_SIZE stays a per-invocation override the
  # operator may set in the shell; the wrapper pins nothing.
  # The guest-own nix store disk of the VM backends (docs/design/
  # guest-store.md): when true, this wrapper builds the erofs
  # store image (./krun-store-image.nix) from the SAME tool closure
  # the sandbox `PATH` carries (`toolsEnv` below — the shell, the
  # mux-entry closures and every `extraTools` package included) and
  # pins it as MYSBX_VM_STORE_DISK. A run then attaches it as a
  # second virtio-blk device and mounts it read-only at /nix instead
  # of sharing the host store (backends.md D3's whole-store share) —
  # the first tier NARROWER than the store it replaces. Absent (the
  # default, and what an unwrapped `nix-build` of this file gets)
  # pins nothing and every run keeps the host-store share, byte for
  # byte. ../default.nix wires this to `vm.store.enable`.
  vmStoreBaked ? false,
  # Extra store roots for the guest-own store disk (the module
  # layer's guest-side packages the disk must carry beyond the
  # wrapper's own closure — `krun.nix.packages`, the in-guest nix
  # wrappers of the direct backend, and the git-default-diff wrapper
  # the GIT_EXTERNAL_DIFF pin names, whose store paths a baked run
  # has no host-store share to resolve through).
  vmStoreExtraRoots ? [ ],
  # The waypipe binary of the HOST side of the display channel
  # (../docs/design/config.md D18, `display = "waypipe"`): the wrapper
  # pins it as `MYSBX_WAYPIPE`, and a run that selects waypipe without
  # the pin is refused — a host that carries no waypipe never starts
  # a silently headless sandbox. `null` pins nothing.
  waypipe ? null,
  # The dtach master of detached runs (docs/design/detach.md B10,
  # `--detach`): the wrapper pins it as `MYSBX_DTACH`, the
  # `MYSBX_BWRAP` idiom — an unpinned, missing or non-executable
  # dtach makes `--detach` (and, later, `--detachable`/`attach`)
  # fail with `70` naming the variable; other runs are unaffected.
  # `null` pins nothing and the crate's bare `dtach` PATH fallback
  # of an unwrapped build applies.
  dtach ? null,
  # The waypipe binary INSIDE the podman-gvisor image (D18 on the
  # container backend): the store path of the waypipe binary as it
  # exists inside `podmanImage`, pinned as `MYSBX_PODMAN_WAYPIPE` for
  # the same refusal semantics as the bwrap-side `waypipe`. `null`
  # pins nothing — `display = "waypipe"` with `backend =
  # "podman-gvisor"` is refused.
  podmanWaypipe ? null,
  # The IN-IMAGE egress relay of the podman-gvisor proxy-only channel
  # (bd myconfig-eov2 — n8kv.2 C5): the store path of
  # `bin/mysbx-egress-relay` as it exists INSIDE `podmanImage` — the
  # same crate the wrapper builds, baked into the image by
  # `podman.imagePackages`. Pinned as `MYSBX_PODMAN_EGRESS_RELAY` (the
  # `MYSBX_PODMAN_WAYPIPE` idiom: a path inside the container, because
  # this backend mounts nothing from the host `/nix/store`). `null`
  # pins nothing — a `proxy-only` podman-gvisor run is then REFUSED
  # (it fails closed, naming the pin).
  podmanEgressRelay ? null,
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
  # The direct-libkrun backend (docs/design/backends.md D3, bd
  # myconfig-dak.4): the launcher binary (../krun-rs, packaged by
  # ./krun-launcher.nix — zero-dependency Rust, dlopen of the pinned
  # libkrun at runtime) and the guest rootfs (./krun-rootfs.nix — the
  # plain directory with the baked guest init). `null` pins nothing
  # and `backend = "krun"` runs are refused with a naming message
  # (the same contract as the podman image pin). Parameters, not
  # `pkgs.` references, so a host pins exactly the libkrun build its
  # runs need (the direct launcher's default pin builds withBlk for
  # the scratch disk, backends.md D7; a host overrides the
  # krun-launcher.nix `libkrun` argument to change it).
  krunLauncher ? null,
  krunRootfs ? null,
  # The native-qemu backend (docs/design/backends.md D9, bd
  # myconfig-2l9b): the launcher binary (../microvm-qemu-rs, packaged
  # by ./microvm-qemu-launcher.nix — zero-dependency Rust, rootless
  # virtiofsd per device + one qemu microVM per run), the SHARED guest
  # rootfs (./krun-rootfs.nix consumed as-is — the guest side is not
  # forked) and the kernel bzImage (./microvm-qemu-kernel.nix — the one
  # input libkrun bundles and qemu needs handed over; built with the
  # virtio stack compiled in, the static busybox init loads no
  # modules). The kernel pin is the bzImage FILE inside the package
  # (`${pkg}/bzImage`), never the package itself: qemu's -kernel
  # reads a file, and doctor's is-file rule rejects a directory (bd
  # myconfig-q3se). `null` pins nothing and `backend =
  # "microvm-qemu"` runs are refused with a naming message (the same
  # contract as the krun pins). Parameters, not `pkgs.` references, so
  # a host pins exactly the qemu/virtiofsd/kernel builds its runs need.
  microvmQemuLauncher ? null,
  microvmQemuRootfs ? null,
  microvmQemuKernel ? null,
  # The cloud-hypervisor backend (bd myconfig-9wkp.8): the same
  # three pins over the CH spelling — the mysbx-microvm-ch launcher
  # (../microvm-ch-rs, packaged by ./microvm-ch-launcher.nix — it
  # spawns one virtiofsd per device, one passt child and one
  # cloud-hypervisor VM per run), the SHARED guest rootfs and the
  # SAME kernel bzImage the qemu arm pins (one kernel serves both
  # VM backends, bd myconfig-9wkp.3 — the file inside the package,
  # never the package itself). `null` pins nothing and `backend =
  # "microvm-ch"` runs are refused with a naming message.
  microvmChLauncher ? null,
  microvmChRootfs ? null,
  microvmChKernel ? null,
  # Backend-specific env pins of the microvm-ch backend
  # (`MYSBX_MICROVM_CH_ENV`): the CH twin of `microvmQemuEnv` — the
  # passt guest's loopback is its own (the same reasoning as the
  # qemu arm's SLiRP gap), and the gateway `10.0.2.2` NATs to the
  # host loopback (bd myconfig-9wkp.5's plan). Values must not
  # contain whitespace.
  microvmChEnv ? { },
}:

let
  # `binName` is interpolated into the postBuild shell script inside
  # double quotes, so a `$`/backtick/quote in it would expand at build
  # time. The module layer validates profile names (default.nix
  # `validProfileName`), but this file is callPackage-able from anywhere
  # — validate here too, at the same place that interpolates, by routing
  # every interpolation through this binding (laziness keeps the check
  # off the unwrapped crate builds of checks.nix that never build the
  # wrapper).
  checkedBinName =
    if builtins.match "[a-z][a-z0-9-]*" binName != null then
      binName
    else
      throw "mysbx.nix: binName `${binName}` must match [a-z][a-z0-9-]* — it is interpolated into the build script";

  # The crate's backend universe, the nix-side spelling of
  # mysbx-rs/src/backends.rs's REGISTRY (bd myconfig-9wkp.2):
  # `mysbx-backends-sync-test` (nix/checks.nix) runs the BUILT
  # binary and refuses a drift between the two — a backend added to
  # the table without its nix twin (or vice versa) fails the check,
  # never a silent mismatch.
  backendUniverse = [
    "bubblewrap"
    "podman-gvisor"
    "nono"
    "podman-krun"
    "krun"
    "microvm-qemu"
    "microvm-ch"
  ];
  # The dev-tool `PATH` value the argv builder uses (bwrap.rs):
  # a single store tree whose /bin is the union of the tools' bins,
  # so `toolsPath` stays one absolute path.
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

  # The guest-own nix store disk (docs/design/guest-store.md), built
  # when `vmStoreBaked` from THIS wrapper's own closure — the
  # sandbox `PATH` (`toolsEnv`), the host shell a session's payload
  # runs (`shell`), every multiplexer entry script and the waypipe
  # of the display channel: everything a run resolves INSIDE the
  # guest, so the baked store and the pins can never name different
  # builds. Host-side binaries (bwrap, nono, the launchers, dtach,
  # the terminal) stay OUT — they never exec in the guest. `null`
  # when disabled, so the wrapper's closure pulls the image in only
  # on a host that opted in.
  storeRootPaths = [
    toolsEnv
    # The CA bundle the wrapper pins as MYSBX_CA_BUNDLE and every
    # guest run sets SSL_CERT_FILE/GIT_SSL_CAINFO/NIX_SSL_CERT_FILE
    # to (krun.rs): its path must resolve through the guest's own
    # store, or every TLS fetch inside a baked run fails at the
    # trust anchors (bd myconfig-qq3i's fd wedges aside, this is the
    # w4zc lesson applied to the wrapper's own pin).
    cacert
  ]
  ++ lib.optionals (shell != null) [ shell ]
  ++ lib.optionals (muxEntries != null) (
    # A `null` entry is "absent" by contract (the comment at the
    # `muxEntries` parameter); closureInfo's exportReferencesGraph
    # would reject it as a root path, so filter like muxEntryPins.
    lib.filter (entry: entry != null) (lib.attrValues muxEntries)
  )
  ++ lib.optionals (waypipe != null) [ waypipe ]
  ++ vmStoreExtraRoots;
  storeDisk =
    if vmStoreBaked then
      # The recipe's inputs are all callPackage defaults; rootPaths is
      # the one override.
      callPackage ./krun-store-image.nix { } { rootPaths = storeRootPaths; }
    else
      null;

  # The bare Rust crate, without the wrapper. One expression with
  # the wrapper's own closure (./crate.nix, so the podman image and
  # the wrapper pin the SAME derivation — see that file); exposed as
  # `mysbx.passthru.crate` so the `nix/checks.nix` check (mvp-6) can run
  # the cargo test suite against the unwrapped binary — the tests set
  # their own `MYSBX_*` variables and must never see the wrapper's pins.
  crate = callPackage ./crate.nix { };
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
  # The in-image counterpart (`config.rs::Multiplexer::podman_entry_var`).
  podmanMuxEntryPins = lib.concatStringsSep " \\\n      " (
    lib.mapAttrsToList (
      name: entry: "--set MYSBX_PODMAN_MUX_ENTRY_${lib.toUpper name} '${lib.getExe entry}'"
    ) (lib.filterAttrs (_: entry: entry != null) podmanMuxEntries)
  );
  # The agent registry pins (bd myconfig-fv4h, config.md D32): the
  # kind list feeds the `--<kind>` shorthand arms, the file the
  # kind→command resolution. `lib.escapeShellArg` keeps a hostile kind
  # (a shell metacharacter would be caught by the module assertion's
  # grammar, this is the wrapper's own defense in depth) from
  # repointing the pin; `MYSBX_AGENT_NAMES` empty-means-unset in the
  # CLI (the `env_opt` rule), so an unset kind list pins "no agents
  # installable" honestly.
  agentNamesPin =
    if agentNames != "" then "--set MYSBX_AGENT_NAMES ${lib.escapeShellArg agentNames}" else "";
  agentsFilePin =
    if agentsFile != null then "--set MYSBX_AGENTS_FILE ${lib.escapeShellArg agentsFile}" else "";
  # The `mysbx gui` terminal pin (docs/design/cli.md D15): an absolute
  # store path, the same wrapper idiom as MYSBX_BWRAP. Empty when the
  # caller passes no alacritty — the fallback PATH lookup of the
  # unwrapped crate applies then.
  terminalPin = if alacritty != null then "--set MYSBX_TERMINAL '${lib.getExe alacritty}'" else "";
  # The profile pin (config.md D30, bd myconfig-011r): the launcher
  # identity the crate's profile gate reads. Empty for the base
  # wrapper (a blank slot on the continued wrapProgram line, the
  # `nodeMajorPin` idiom); a flavor pins its own name. `--set`, not
  # `--set-default`: the pin is a fact about the binary, not an
  # operator knob — an invocation that overrides it only re-labels
  # the launcher the gate compares against, and the actual closure is
  # embedded in the binary anyway.
  profilePin = if profile != null then "--set MYSBX_PROFILE '${profile}'" else "";
  # C7b (n8kv idea 13, bd myconfig-f23e): the declared nodejs major
  # of the tools closure and the generated credential-free `.npmrc`.
  # The null arm is the empty string like `terminalPin`, so a blank
  # slot on the continued wrapProgram line stays a blank argument —
  # never a stray `--set` command.
  nodeMajorPin = if nodeMajor != null then "--set MYSBX_NODE_MAJOR '${toString nodeMajor}'" else "";
  npmrcPin = if npmrc != null then "--set MYSBX_NPMRC '${npmrc}'" else "";
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

  # The rendered `MYSBX_MICROVM_QEMU_ENV` value (see the
  # `microvmQemuEnv` argument): `KEY=VALUE` entries,
  # space-separated, in attribute order — the qemu twin of
  # `podmanEnvValue`. A value carrying whitespace cannot survive
  # that encoding, so refuse it here instead of shipping a
  # truncated variable.
  microvmQemuEnvValue = lib.concatStringsSep " " (
    lib.mapAttrsToList (
      name: value:
      if builtins.match ".*[[:space:]].*" value != null then
        throw "mysbx: microvmQemuEnv.${name} must not contain whitespace (MYSBX_MICROVM_QEMU_ENV is a space-separated list), got `${value}`"
      else
        "${name}=${value}"
    ) microvmQemuEnv
  );

  # The CH twin of `microvmQemuEnvValue` (the `microvmChEnv`
  # argument), same encoding rule.
  microvmChEnvValue = lib.concatStringsSep " " (
    lib.mapAttrsToList (
      name: value:
      if builtins.match ".*[[:space:]].*" value != null then
        throw "mysbx: microvmChEnv.${name} must not contain whitespace (MYSBX_MICROVM_CH_ENV is a space-separated list), got `${value}`"
      else
        "${name}=${value}"
    ) microvmChEnv
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
  krunPins = lib.optionalString (
    krunRuntime != null
  ) "--set-default MYSBX_KRUN_RUNTIME '${krunRuntime}/bin/crun' ";
  # The scratch size is a config key now (D23, `scratch-size` in
  # the generated user config): the crate truncates its sparse file
  # to the merged value. `MYSBX_KRUN_SCRATCH_SIZE` stays a
  # per-invocation override the operator may set in the shell —
  # never baked here.
  # The display-channel pins (D18): the host-side client binary and
  # the in-image server binary. Both optional, both absolute store
  # paths — the wrapper idiom of every other pin.
  # The in-image relay of the podman-gvisor channel (bd
  # myconfig-eov2) and C7b's npmrc in-image path (bd
  # myconfig-f23e, baked via podman.imagePackages) join the same
  # expression: the podman binds name in-image paths, never host
  # store paths.
  waypipePins = lib.concatStrings (
    lib.optional (waypipe != null) "--set MYSBX_WAYPIPE '${lib.getExe waypipe}' "
    ++ lib.optional (podmanWaypipe != null) "--set MYSBX_PODMAN_WAYPIPE '${podmanWaypipe}' "
    ++ lib.optional (
      podmanEgressRelay != null
    ) "--set MYSBX_PODMAN_EGRESS_RELAY '${podmanEgressRelay}' "
    ++ lib.optional (podmanNpmrc != null) "--set MYSBX_PODMAN_NPMRC '${podmanNpmrc}'"
  );
  # The dtach master pin of detached runs (detach.md B10): an
  # absolute store path like every other closure pin. `--set`, not
  # `--set-default`: the pin is infrastructure, not an operator
  # knob — an invocation overriding it could name a dtach that
  # differs from the one this build validated.
  dtachPins = lib.optionalString (dtach != null) "--set MYSBX_DTACH '${lib.getExe dtach}'";
  # The `network = "proxy-only"` channel pins (docs/design/config.md
  # D20, bd myconfig-mo3.2.4): the host forwarder's Unix socket and the
  # LiteLLM port the in-sandbox relay listens on. `--set-default`, the
  # operator-knob reasoning of the pasta spec: a debugging run may
  # point them elsewhere, and a `proxy-only` run without a pinned
  # socket is refused (it fails closed).
  # The name lookup of a `network = "proxy-only"` sandbox: an empty
  # netns has no resolver, and without a hosts file glibc falls back to
  # DNS on 127.0.0.1:53 and even `localhost` fails (EAI_AGAIN). The
  # builder binds these two files over /etc/hosts and /etc/nsswitch.conf,
  # so `localhost` is the relay's 127.0.0.1 (IPv4 only: the relay does
  # not listen on ::1) and every other name fails fast as not found.
  egressEtc = runCommand "mysbx-egress-etc" { } ''
    mkdir "$out"
    printf '127.0.0.1 localhost\n' > "$out/hosts"
    printf 'hosts: files\n' > "$out/nsswitch.conf"
  '';
  egressPins =
    lib.optionalString (
      egressSocketName != null
    ) "--set-default MYSBX_EGRESS_SOCKET_NAME '${egressSocketName}' "
    + lib.optionalString (
      egressPort != null
    ) "--set-default MYSBX_EGRESS_PORT '${toString egressPort}' "
    # The in-sandbox relay binary (bd myconfig-mo3.2.5): a closure path,
    # so `--set`, not `--set-default` — it must match this build.
    + "--set MYSBX_EGRESS_RELAY '${crate}/bin/mysbx-egress-relay' "
    # The host-side broker (docs/design/egress.md E1, bd
    # myconfig-n8kv.1.2): a closure path, so `--set`. Always pinned —
    # the profile has no other channel (bd myconfig-ps3i).
    + "--set MYSBX_EGRESS_BROKER '${crate}/bin/mysbx-egress-broker' "
    + "--set MYSBX_EGRESS_ETC '${egressEtc}'";
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
  # The availability gate pin (MYSBX_ENABLED_BACKENDS): validated
  # against the crate's universe BEFORE the wrapper exists, so a bad
  # name in the module layer is a build error. `null` pins nothing and
  # the crate treats an unset variable as "everything selectable" —
  # the unwrapped-build contract the checks' stub builds rely on.
  enabledBackendsPin =
    if enabledBackends == null then
      ""
    else
      let
        # The single nix-side spelling of the registry (bd
        # myconfig-9wkp.2), asserted against the BUILT crate by
        # mysbx-backends-sync-test (nix/checks.nix).
        universe = backendUniverse;
        unknown = builtins.filter (b: !builtins.elem b universe) enabledBackends;
      in
      if unknown != [ ] then
        throw "mysbx: enabledBackends must name only ${toString universe}, got: ${toString unknown}"
      else
        "--set MYSBX_ENABLED_BACKENDS '${lib.concatStringsSep " " enabledBackends}'";
  # The direct-libkrun backend's pins (backends.md D3, bd
  # myconfig-dak.4): the launcher binary and the guest rootfs, both
  # absolute store paths (`--set`, the closure-pin idiom of every
  # other backend pin — no PATH fallback exists for a launcher a
  # host did not build, and an invented path would run nothing). A
  # `krun` run without them is refused by the crate.
  krunDirectPins =
    lib.optionalString (
      krunLauncher != null
    ) "--set MYSBX_KRUN_LAUNCHER '${krunLauncher}/bin/mysbx-krun' "
    + lib.optionalString (krunRootfs != null) "--set MYSBX_KRUN_ROOTFS '${krunRootfs}' "
    # The guest-own store disk pin: ONE pin for both VM backends —
    # the guest side is shared (backends.md D9), so the krun builder
    # and the microvm-qemu renderer consume the same MYSBX_VM_*
    # variable, like every guest-side switch (MYSBX_VM_SCRATCH,
    # the manifest).
    + lib.optionalString vmStoreBaked "--set MYSBX_VM_STORE_DISK '${storeDisk}'";
  # The native-qemu backend's pins (backends.md D9, bd myconfig-2l9b):
  # the same closure-pin idiom — absolute store paths, no PATH
  # fallback. A `microvm-qemu` run without them is refused by the
  # crate (the launcher, rootfs and kernel each name their pin in the
  # refusal).
  microvmQemuPins =
    lib.optionalString (
      microvmQemuLauncher != null
    ) "--set MYSBX_MICROVM_QEMU_LAUNCHER '${microvmQemuLauncher}/bin/mysbx-microvm-qemu' "
    + lib.optionalString (
      microvmQemuRootfs != null
    ) "--set MYSBX_MICROVM_QEMU_ROOTFS '${microvmQemuRootfs}' "
    + lib.optionalString (
      microvmQemuKernel != null
    ) "--set MYSBX_MICROVM_QEMU_KERNEL '${microvmQemuKernel}' "
    # The backend's env pins (bd myconfig-mzys): `--set-default`, the
    # same operator-knob reasoning as the pasta spec — an invocation
    # can override a pinned endpoint for a debugging run.
    + lib.optionalString (
      microvmQemuEnv != { }
    ) "--set-default MYSBX_MICROVM_QEMU_ENV '${microvmQemuEnvValue}'";
  # The cloud-hypervisor backend's pins (bd myconfig-9wkp.8): the
  # same closure-pin idiom over the CH spelling.
  microvmChPins =
    lib.optionalString (
      microvmChLauncher != null
    ) "--set MYSBX_MICROVM_CH_LAUNCHER '${microvmChLauncher}/bin/mysbx-microvm-ch' "
    + lib.optionalString (microvmChRootfs != null) "--set MYSBX_MICROVM_CH_ROOTFS '${microvmChRootfs}' "
    + lib.optionalString (microvmChKernel != null) "--set MYSBX_MICROVM_CH_KERNEL '${microvmChKernel}' "
    + lib.optionalString (
      microvmChEnv != { }
    ) "--set-default MYSBX_MICROVM_CH_ENV '${microvmChEnvValue}'";
in
symlinkJoin {
  # keep the crate's derivation name: build-pkg-for-host.sh matches on
  # `(p.name or p.pname)` == "mysbx-0.1.0" in home.packages
  inherit (crate) pname version;

  paths = [ crate ];

  nativeBuildInputs = [ makeBinaryWrapper ];

  postBuild = ''
    ${lib.optionalString (
      checkedBinName != "mysbx"
    ) "mv \"$out/bin/mysbx\" \"$out/bin/${checkedBinName}\""}
    wrapProgram "$out/bin/${checkedBinName}" \
      --set MYSBX_BWRAP '${bubblewrap}/bin/bwrap' \
      --set MYSBX_SHELL '${if shell != null then shell else "${bash}/bin/bash"}' \
      --set MYSBX_GUI_SHELL '${bash}/bin/sh' \
      --set MYSBX_BINSH '${bash}/bin/sh' \
      --set MYSBX_TOOLS_PATH '${toolsEnv}/bin' \
      --set MYSBX_NIX_CONF '${sandboxNixConf}' \
      --set MYSBX_CA_BUNDLE '${caBundle}' \
      ${nodeMajorPin} \
      ${npmrcPin} \
      ${muxEntryPins} \
      ${podmanMuxEntryPins} \
      ${agentNamesPin} \
      ${agentsFilePin} \
      ${terminalPin} \
      ${profilePin} \
      ${podmanPins} \
      ${krunPins} \
      ${waypipePins} \
      ${dtachPins} \
      ${egressPins} \
      ${nonoPins} \
      ${nonoEnvPins} \
      ${sshKeygenPins} \
      ${krunDirectPins} \
      ${microvmQemuPins} \
      ${microvmChPins} \
      ${enabledBackendsPin}

    # `mysbx-once` (bd myconfig-7zwt, cli.md D22): the SAME wrapped
    # binary under a second name, with the once marker the Rust CLI
    # reads (`src/once.rs::active`) — argv[0] does not survive a
    # makeBinaryWrapper chain (each `exec` resets it to the literal
    # program path), so the marker is the dispatch. Everything else
    # is inherited from the wrapped `mysbx` beside it: the pins, the
    # closure, the environment. A flavor's once entry is
    # `mysbx-<profile>-once` — the marker, not argv[0], is the
    # dispatch, so any name works and the base wrapper's
    # `mysbx-once` is not shadowed in home.packages.
    makeBinaryWrapper "$out/bin/${checkedBinName}" "$out/bin/${checkedBinName}-once" \
      --set MYSBX_ONCE 1

    # Hand-written fish tab completion (../mysbx-rs/completions, kept in
    # sync with the CLI surface by the `mysbx-completions` check in
    # checks.nix). The crate stays
    # zero-dependency: this is a plain fish script, not clap-generated.
    # The vendor path is the one `installShellFiles --fish` uses and
    # fish's NixOS integration collects. A flavor completes under its
    # OWN command name (`complete -c <name>`): the base wrapper
    # installs the maintained sources byte for byte (the checks.nix
    # sync contract stays on the base spelling), a flavor installs
    # copies with the command name rewritten (`-c mysbx` →
    # `-c ${binName}`, the same for the once file).
    ${
      if checkedBinName == "mysbx" then
        ''
          install -Dm 0644 ${completions} \
            $out/share/fish/vendor_completions.d/mysbx.fish
          install -Dm 0644 ${./../mysbx-rs/completions/mysbx-once.fish} \
            $out/share/fish/vendor_completions.d/mysbx-once.fish
        ''
      else
        ''
          mkdir -p $out/share/fish/vendor_completions.d
          sed -e "s/complete -c mysbx\\b/complete -c ${checkedBinName}/" \
            ${completions} \
            > $out/share/fish/vendor_completions.d/${checkedBinName}.fish
          sed -e "s/complete -c mysbx-once/complete -c ${checkedBinName}-once/" \
            ${./../mysbx-rs/completions/mysbx-once.fish} \
            > $out/share/fish/vendor_completions.d/${checkedBinName}-once.fish
        ''
    }
  '';

  meta = {
    description = "My sandboxing tool — a bubblewrap sandbox CLI for coding agents";
    mainProgram = checkedBinName;
    platforms = lib.platforms.linux;
  };

  passthru = {
    inherit
      crate
      toolsEnv
      sandboxNixConfDir
      binName
      profile
      ;
    # The backends the wrapper pins in MYSBX_ENABLED_BACKENDS (null
    # for the callPackage default, which pins nothing): the module
    # layer's `profiles.<name>` gate asserts a flavor wrapper strips
    # the podman backends, and this is the eval-visible spelling of
    # that pin (the wrapper binary itself is an ELF — makeBinaryWrapper
    # embeds the assignments in it — so no check can readFile it).
    inherit enabledBackends;
    # The nix-side backend universe (the let binding above), exposed
    # for checks.nix's sync test (bd myconfig-9wkp.2).
    inherit backendUniverse;
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
    # The guest-own store disk the wrapper pins when `vmStoreBaked`
    # (null otherwise), for the checks.nix build-and-inspect gate of
    # the image.
    vmStoreDisk = storeDisk;
    # Whether the wrapper pins the disk at all — the module's
    # `vm.store.enable` feeds it; the checks.nix wiring gate reads
    # it at eval time.
    vmStoreBaked = vmStoreBaked;
    # The `network = "proxy-only"` channel pins (bd myconfig-mo3.2.4,
    # myconfig-ps3i), for the generated-config gate: the egress socket
    # file name and the port the in-sandbox relay listens on. `null`
    # when the caller pinned nothing.
    inherit egressSocketName egressPort;
    # The in-sandbox relay binary the wrapper pins (bd myconfig-mo3.2.5),
    # for the checks.nix gate that runs it live.
    egressRelay = "${crate}/bin/mysbx-egress-relay";
    # The host-side broker the wrapper pins (docs/design/egress.md E1, bd
    # myconfig-n8kv.1.2), for the gates that drive it directly.
    egressBroker = "${crate}/bin/mysbx-egress-broker";
    # The proxy-only sandbox's `hosts` and `nsswitch.conf`.
    egressEtc = "${egressEtc}";
    # The in-image relay the wrapper pins for the podman-gvisor channel
    # (bd myconfig-eov2), for the same gate: the pin and the image bake
    # must both follow the host's LiteLLM.
    inherit podmanEgressRelay;
    # The disk's closure roots as store paths, for the same gate:
    # `vmStoreExtraRoots` (the guest nix wrappers, the
    # git-default-diff wrapper) must appear here when the module
    # enables them.
    vmStoreRoots = map toString storeRootPaths;
  };
}
