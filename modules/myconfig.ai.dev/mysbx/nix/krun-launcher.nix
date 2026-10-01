# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Direct libkrun launcher: a zero-dependency Rust binary that runs under
# bwrap with a read-only virtiofs root, explicit environment, and
# per-tag read-only/read-write shares.
#
# libkrun is loaded at runtime from the pinned library path set by the
# wrapper, so the package has no build-time libkrun dependency.
#
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  # The libkrun the RUN should dlopen. A parameter, not a `pkgs.`
  # reference (the same convention as every input of ./mysbx.nix), so
  # a host pins exactly the libkrun build its runs need.
  libkrun,
}:
rustPlatform.buildRustPackage {
  pname = "mysbx-krun";
  version = "0.1.0";

  src = ../krun-rs;
  cargoLock.lockFile = ../krun-rs/Cargo.lock;

  # Zero dependencies: no vendoring, no outputHashes, like the main
  # mysbx crate. The libkrun linkage is RUNTIME (dlopen), so there is
  # nothing to build against.

  nativeBuildInputs = [ makeBinaryWrapper ];

  # The wrapper pins the exact .so of the libkrun input; under bwrap
  # (no host LD_LIBRARY_PATH) this is the only reliable lookup.
  postInstall = ''
    wrapProgram $out/bin/mysbx-krun \
      --set MYSBX_KRUN_LIB '${libkrun}/lib64/libkrun.so'
  '';

  meta = {
    description = "mysbx krun backend: direct libkrun launcher (bd myconfig-dak)";
    mainProgram = "mysbx-krun";
    platforms = libkrun.meta.platforms;
  };
}
