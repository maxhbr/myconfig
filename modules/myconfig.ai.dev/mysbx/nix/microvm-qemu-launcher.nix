# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The microvm-qemu backend's launcher (backends.md D9, bd
# myconfig-kvq8): the zero-dependency Rust twin of mysbx-krun that
# spawns one rootless virtiofsd per device and one qemu microVM per
# run — no libkrun to dlopen, so the wrapper pins the TWO binaries
# the launcher invokes instead of a library.
#
# The binaries are RUNTIME paths (MYSBX_MICROVM_QEMU_VIRTIOFSD /
# MYSBX_MICROVM_QEMU_QEMU — the launcher refuses a missing pin,
# never a PATH guess), pinned to the exact qemu and virtiofsd
# builds of the inputs: under bwrap (no host PATH) this is the only
# reliable lookup, and a host swapping its qemu must rebuild the
# wrapper for a run to see it.
#
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  # The qemu-system the RUN should invoke (nixpkgs' qemu minimal is
  # enough: no display, no SPICE — the launcher runs headless). A
  # parameter, not a `pkgs.` reference (the same convention as every
  # input of ./mysbx.nix and ./krun-launcher.nix), so a host pins
  # exactly the qemu build its runs need.
  qemu,
  # The virtiofsd the RUN should invoke (rust-virtiofsd of nixpkgs).
  virtiofsd,
}:
rustPlatform.buildRustPackage {
  pname = "mysbx-microvm-qemu";
  version = "0.1.0";

  src = ../microvm-qemu-rs;
  cargoLock.lockFile = ../microvm-qemu-rs/Cargo.lock;

  # Zero dependencies: no vendoring, no outputHashes, like the main
  # mysbx crate and the krun launcher. qemu and virtiofsd are
  # separate PROCESSES invoked by absolute paths — nothing to link
  # against.

  nativeBuildInputs = [ makeBinaryWrapper ];

  # The wrapper pins the exact binaries of the inputs; under bwrap
  # (no host PATH) this is the only reliable lookup.
  postInstall = ''
    wrapProgram $out/bin/mysbx-microvm-qemu \
      --set MYSBX_MICROVM_QEMU_QEMU '${lib.getExe qemu}' \
      --set MYSBX_MICROVM_QEMU_VIRTIOFSD '${lib.getExe virtiofsd}'
  '';

  meta = {
    description = "mysbx microvm-qemu backend: rootless virtiofsd + qemu launcher (bd myconfig-6di.6)";
    mainProgram = "mysbx-microvm-qemu";
    platforms = lib.platforms.linux;
  };
}
