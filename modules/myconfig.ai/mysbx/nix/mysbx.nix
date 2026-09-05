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
#
# All three are absolute store paths — nothing is left to host lookup.
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  symlinkJoin,
  buildEnv,
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
    ];
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
      --set MYSBX_TOOLS_PATH '${toolsEnv}/bin'
  '';

  meta = {
    description = "My sandboxing tool — a bubblewrap sandbox CLI for coding agents";
    mainProgram = "mysbx";
    platforms = lib.platforms.linux;
  };

  passthru = {
    inherit crate toolsEnv;
  };
}
