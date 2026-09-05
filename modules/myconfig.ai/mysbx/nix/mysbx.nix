# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The Rust `mysbx` CLI (../mysbx-rs).
# The crate is zero-dependency by design, so the lockfile is trivial and no
# `outputHashes` can ever be needed.
{
  lib,
  rustPlatform,
}:

rustPlatform.buildRustPackage {
  pname = "mysbx";
  version = "0.1.0";

  src = ../mysbx-rs;
  cargoLock.lockFile = ../mysbx-rs/Cargo.lock;

  meta = {
    description = "My sandboxing tool";
    mainProgram = "mysbx";
    platforms = lib.platforms.linux;
  };
}
