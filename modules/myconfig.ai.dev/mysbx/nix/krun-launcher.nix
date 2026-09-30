# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The mysbx-krun launcher (../krun-rs, bd myconfig-dak.1): a small
# zero-dependency Rust binary that drives libkrun DIRECTLY — one
# context, a read-only virtiofs root (KRUN_FS_ROOT_TAG), per-tag
# ro/rw shares, an explicit envp, krun_start_enter. The future
# `backend = "krun"` (bd myconfig-dak) will exec this under bwrap:
#   bwrap -> mysbx-krun -> VM
# so the host-side virtiofs server can only open what the bwrap argv
# left visible.
#
# The binary dlopen()s libkrun and resolves every symbol itself, so
# it needs NO libkrun at build time — the launcher runs against the
# libkrun its run makes visible. It honors one pin, MYSBX_KRUN_LIB
# (the .so path, set by the wrapper below), and falls back to a bare
# dlopen("libkrun.so") for an unwrapped build.
#
# nixpkgs' default `libkrun` is built WITHOUT `withNet`/`withBlk`:
# krun_set_passt_fd / krun_add_disk2 do not exist in it (verified:
# nm -D on the 1.19.0 store path carries no krun_add_net_* nor
# krun_set_passt_fd). The launcher degrades honestly — the optional
# symbols resolve to None and no-network stays no-network; a net or
# scratch-disk story (bd myconfig-dak.6/.7) passes an overridden
# `libkrun` here.
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
      --set MYSBX_KRUN_LIB '${libkrun}/lib/libkrun.so'
  '';

  meta = {
    description = "mysbx krun backend: direct libkrun launcher (bd myconfig-dak)";
    mainProgram = "mysbx-krun";
    platforms = libkrun.meta.platforms;
  };
}
