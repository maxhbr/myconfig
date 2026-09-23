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
# re-wraps the binary (same `makeBinaryWrapper` idiom as
# ../../sandboxes/myconfig.ai.gvisor-agent-sandbox/nix/agent-gvisor.nix) with the three
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
#   MYSBX_GVISOR_WAYPIPE
#                     the waypipe binary INSIDE the podman-gvisor
#                     image, the server end of the same channel on
#                     the container backend. Absent = refused.
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
#                     to the payload just the same.
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
#
# All these pins are absolute store paths — nothing is left to host lookup.
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  symlinkJoin,
  buildEnv,
  writeText,
  runCommand,
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
  # (../docs/gvisor-load-image.md, bd myconfig-6di.1): `null` pins
  # nothing — `backend = "podman-gvisor"` is a refused run and
  # `mysbx gvisor-load-image` a usage error, never an invented
  # `localhost/…` reference pulled from a registry that does not
  # exist (bd myconfig-xrt). The module layer defaults this to the
  # gvisor tier's effective image (same build the `agent-gvisor`
  # sessions run).
  gvisorImage ? null,
  # The shell of the podman-gvisor backend's INTERACTIVE payload (bd
  # myconfig-cew): the store path of the fish binary as it exists INSIDE
  # `gvisorImage`. The wrapper pins it as `MYSBX_GVISOR_SHELL` so a
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
  gvisorShell ? null,
  # The podman network spec of the podman-gvisor backend
  # (`MYSBX_GVISOR_PASTA_SPEC`, ../docs/gvisor-load-image.md): a
  # `pasta:--map-guest-addr,<address>` spec that makes the host's
  # LiteLLM forwarder reachable from inside the container, the same
  # spec the gvisor tier bakes as `AGENT_GVISOR_NETWORK`. `null`
  # leaves podman's default (shared) network.
  #
  # Pinned with `--set-default`, not `--set`: the variable is also an
  # operator override (the docs document it as one), and a network
  # spec is a per-invocation debugging knob rather than a closure
  # path that must match the build.
  gvisorPastaSpec ? null,
  # Backend-specific environment pins of the podman-gvisor backend
  # (`MYSBX_GVISOR_ENV`): an attrset rendered as a space-separated
  # `KEY=VALUE` list. The container gets them as `--env` after the
  # config layers and before the sandbox's own infrastructure
  # variables (src/podman_gvisor.rs section 7). What belongs here is
  # environment that is only correct under THIS backend, e.g. the
  # container-side URL of the host's LiteLLM forwarder, which differs
  # from the host loopback URL the bwrap backend uses.
  #
  # Values must not contain whitespace — the crate splits the list on
  # it, so a value with a space would be silently truncated.
  gvisorEnv ? { },
  # The waypipe binary of the HOST side of the display channel
  # (../docs/design/config.md D18, `display = "waypipe"`): the wrapper
  # pins it as `MYSBX_WAYPIPE`, and a run that selects waypipe without
  # the pin is refused — a host that carries no waypipe never starts
  # a silently headless sandbox. `null` pins nothing.
  waypipe ? null,
  # The waypipe binary INSIDE the podman-gvisor image (D18 on the
  # container backend): the store path of the waypipe binary as it
  # exists inside `gvisorImage`, pinned as `MYSBX_GVISOR_WAYPIPE` for
  # the same refusal semantics as the bwrap-side `waypipe`. `null`
  # pins nothing — `display = "waypipe"` with `backend =
  # "podman-gvisor"` is refused.
  gvisorWaypipe ? null,
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
    # production host rebuild, like agent-gvisor does.
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
  sandboxNixConf = writeText "mysbx-nix.conf" ''
    # Generated by myconfig for the mysbx sandbox — do not edit.
    # The host's /etc/nix/nix.conf is deliberately NOT mounted.
    experimental-features = nix-command flakes
    substituters = https://cache.nixos.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
  '';
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
  # at build time — the same mechanism as the gvisor tier's
  # `agent-gvisor-image-id` derivation in
  # ../../sandboxes/myconfig.ai.gvisor-agent-sandbox/nix/load-image.nix).
  # `podman` runs the reference; `gvisor-load-image` compares IDs to
  # detect a stale build under the same tag. All three are LAZY — a
  # `null` gvisorImage must not force `imageName` on null.
  #
  # The shell pin (bd myconfig-cew) sits in its own optionalString:
  # it is meaningful even for a caller that builds its own image
  # reference (MYSBX_GVISOR_IMAGE by hand), but a `null` pins nothing
  # and the crate's image-OCI default (`/bin/bash`) applies. The
  # container `PATH` needs NO pin: the image's buildEnv links every
  # baked package's `bin` into `/bin`, so the OCI `PATH=/bin:/usr/bin`
  # already covers the provisioned tools.
  # The rendered `MYSBX_GVISOR_ENV` value (see the `gvisorEnv`
  # argument): `KEY=VALUE` entries, space-separated, in attribute
  # order. A value carrying whitespace cannot survive that encoding,
  # so refuse it here instead of shipping a truncated variable.
  gvisorEnvValue = lib.concatStringsSep " " (
    lib.mapAttrsToList (
      name: value:
      if builtins.match ".*[[:space:]].*" value != null then
        throw "mysbx: gvisorEnv.${name} must not contain whitespace (MYSBX_GVISOR_ENV is a space-separated list), got `${value}`"
      else
        "${name}=${value}"
    ) gvisorEnv
  );

  gvisorPins =
    lib.optionalString (gvisorImage != null) (
      "--set MYSBX_GVISOR_TARBALL '${gvisorImage}' "
      + "--set MYSBX_GVISOR_IMAGE '${gvisorImage.imageName}:${gvisorImage.imageTag}' "
      + "--set MYSBX_GVISOR_IMAGE_ID \"$(cat ${
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
            tar --extract --to-stdout --file ${gvisorImage} manifest.json \
              | tr -d '"' | sed -n 's/.*Config[[:space:]]*:[[:space:]]*\(sha256:\)\{0,1\}\([0-9a-f]\{64\}\)\.json.*/\2/p' > $out
          ''
      })\" "
    )
    + lib.optionalString (gvisorShell != null) "--set MYSBX_GVISOR_SHELL '${gvisorShell}' "
    + lib.optionalString (
      gvisorPastaSpec != null
    ) "--set-default MYSBX_GVISOR_PASTA_SPEC '${gvisorPastaSpec}' "
    + lib.optionalString (gvisorEnv != { }) "--set MYSBX_GVISOR_ENV '${gvisorEnvValue}' ";
  # The display-channel pins (D18): the host-side client binary and
  # the in-image server binary. Both optional, both absolute store
  # paths — the wrapper idiom of every other pin.
  waypipePins =
    lib.optionalString (waypipe != null) "--set MYSBX_WAYPIPE '${lib.getExe waypipe}' "
    + lib.optionalString (gvisorWaypipe != null) "--set MYSBX_GVISOR_WAYPIPE '${gvisorWaypipe}'";
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
      ${gvisorPins} \
      ${waypipePins}

    # Hand-written fish tab completion (../mysbx-rs/completions, kept in
    # sync with the CLI surface by the `mysbx-completions` check in
    # checks.nix), the same idiom as agent-gvisor.nix. The crate stays
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
    inherit crate toolsEnv sandboxNixConf;
    # The editor the generated `[env]` points `EDITOR`/`VISUAL` at, so
    # the variables and the closure can never name different builds.
    editor = neovim;
    caBundle = caBundle;
    completions = completions;
  };
}
