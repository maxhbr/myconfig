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
# ../myconfig.ai.gvisor-agent-sandbox/nix/agent-gvisor.nix) with the three
# `MYSBX_*` pin variables the Rust CLI reads (src/lib.rs `env_or` calls):
#
#   MYSBX_BWRAP       the bubblewrap backend binary (plan.md: "The base")
#   MYSBX_SHELL       the payload shell — bash from *this wrapper's*
#                     closure, never the host `$SHELL` (plan.md: "Payload
#                     shell")
#   MYSBX_TOOLS_PATH  the dev-tool closure on PATH (plan.md: "The base",
#                     row "dev-tool closure on PATH")
#   MYSBX_WORKMUX_ENTRY
#                     the INTERACTIVE payload of a `workmux = true`
#                     sandbox (../docs/design/config.md D16): the
#                     `mysbx-workmux-entry` script of
#                     ./workmux-entry.nix, which boots the workmux tmux
#                     session on the sandbox-internal socket. Absent on
#                     hosts without the integration — a config that then
#                     says `workmux = true` fails loudly instead of
#                     silently starting a plain shell.
#   MYSBX_NIX_CONF    a SANITIZED nix client configuration bound at
#                     /etc/nix/nix.conf inside the sandbox (review-2
#                     item 3). The host's own /etc/nix/nix.conf is
#                     never bound: it may hold `access-tokens` and
#                     other credentials, which a read-only bind hands
#                     to the payload just the same.
#
# All four are absolute store paths — nothing is left to host lookup.
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  symlinkJoin,
  buildEnv,
  writeText,
  bubblewrap,
  bash,
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
  nix,
  python3,
  curl,
  # Extra packages appended to the dev-tool closure by feature modules
  # (`myconfig.ai.mysbx.extraTools`), e.g. the `pi` coding agent from
  # ../../programs.pi-coding-agent. Same security note as the hardcoded
  # list below: whatever lands here is on the sandbox PATH.
  extraTools ? [ ],
  # The workmux entry script pinned as `MYSBX_WORKMUX_ENTRY`
  # (./workmux-entry.nix, built by ../default.nix when
  # `myconfig.ai.mysbx.workmux.enable` is on). `null` — the default, and
  # what an unwrapped `nix-build` of this file gets — pins nothing, so
  # `workmux = true` is a refused run rather than a silent bare shell
  # (../docs/design/config.md D16).
  workmuxEntry ? null,
}:

let
  # The dev-tool `PATH` value the argv builder uses (../docs/TODOs/
  # mvp-4-bwrap-argv.md section 6): a single store tree whose /bin is the
  # union of the tools' bins, so `toolsPath` stays one absolute path.
  #
  # This is the MVP's hardcoded dev-tool closure, mirroring
  # ../../fns/bubblewrap-app.nix `devTools` minus the package-management
  # and linting extras (`wget`, `unzip`, `diffutils`, `tar`/`gzip`,
  # `shfmt`, `shellcheck`), plus `hostname`: agents fetch those extras
  # per project via nix/flake, not from the sandbox base. Per plan.md
  # phase 2d, `mysbx` should
  # eventually join the shared `myconfig.ai.sandboxTools` option instead
  # of growing this parallel list — until then the list lives HERE, next
  # to the code that consumes it, because (per mvp-6) "it is a
  # security-relevant list, not packaging detail".
  #
  # `extraTools` is the ONE extension point on top of that list: the
  # agent modules that integrate with mysbx (today only
  # ../../programs.pi-coding-agent) add their own binary there instead
  # of editing this list.
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
      nix
      python3
      curl
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
      --set MYSBX_TOOLS_PATH '${toolsEnv}/bin' \
      --set MYSBX_NIX_CONF '${sandboxNixConf}' \
      ${lib.optionalString (
        workmuxEntry != null
      ) "--set MYSBX_WORKMUX_ENTRY '${lib.getExe workmuxEntry}'"}
  '';

  meta = {
    description = "My sandboxing tool — a bubblewrap sandbox CLI for coding agents";
    mainProgram = "mysbx";
    platforms = lib.platforms.linux;
  };

  passthru = {
    inherit crate toolsEnv sandboxNixConf;
  };
}
