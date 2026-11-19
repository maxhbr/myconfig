# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The bare Rust crate of mysbx, without the wrapper: the `mysbx` CLI
# plus the two egress channel binaries (`bin/mysbx-egress-broker`,
# `bin/mysbx-egress-relay`). One expression with two consumers:
#
# - `./mysbx.nix` wraps it and exposes it as `mysbx.passthru.crate`
#   (used by `nix/checks.nix` to run the cargo test suite against the
#   unwrapped binary — the tests set their own `MYSBX_*` variables and
#   must never see the wrapper's pins);
# - `../default.nix` bakes it into the podman container image and pins
#   the in-image relay path for the `network = "proxy-only"` channel
#   (bd myconfig-eov2 — n8kv.2 C5): the image carries
#   `bin/mysbx-egress-relay`, and both consumers must name the SAME
#   derivation, so the image's binary is always the one the wrapper
#   pins.
{
  lib,
  rustPlatform,
}:

rustPlatform.buildRustPackage {
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
}
