# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# CI checks for the Rust `mysbx` CLI (../docs/TODOs/mvp-6-packaging.md):
#
#   mysbx-tests   cargo test — the full behavioural suite in
#                 ../mysbx-rs/tests/ (golden argv tests, layer merge,
#                 repo discovery, CLI subprocess flows).
#
#   mysbx-generated-config-test
#                 module-EVALUATION test of the user configuration layer
#                 ../default.nix generates (review-4 item 4) — the cargo
#                 suite hand-writes its `config.toml` and cannot see a
#                 regression in the generator. See ./config-eval-test.nix.
#
# Wired into `nix flake check` for `x86_64-linux` in `flake.nix`, following
# ../../myconfig.ai.gvisor-agent-sandbox/nix/checks.nix.
#
# Deliberately NOT a check here (mvp-6, "Explicitly not in this item"):
# bubblewrap is not on the test PATH. The two real-execution tests in
# tests/cli.rs are written to *skip* when no runnable bwrap is found, and
# executing bwrap inside `nix flake check` would need nested user
# namespaces — environment-dependent, so the argv golden tests are the CI
# gate and real execution stays the operator's manual acceptance step.
{
  self,
  inputs,
  system,
}:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};

  # The full package (./mysbx.nix), whose `passthru.crate` is the bare
  # rustPlatform.buildRustPackage — the tests set their own `MYSBX_*`
  # variables and must not see the wrapper's pins.
  pkg = pkgs.callPackage ../nix/mysbx.nix { };
  crate = pkg.passthru.crate;
  # Known and accepted (same property as the gvisor tier's check): CI
  # tests this crate from the locked `inputs.nixpkgs`, which can differ
  # slightly from the host-eval nixpkgs the wrapped binary on a host
  # was built with. The crate is dependency-free, so the drift surface
  # is the toolchain, not the library set.
in
{
  # The crate itself, with `doCheck = true`: `cargo test` in the build
  # sandbox. Same pattern as the gvisor tier's `agent-gvisor-tests`.
  # The generator, evaluated: what a host actually gets in
  # `~/.config/mysbx/config.toml` (review-4 item 4).
  mysbx-generated-config-test = import ./config-eval-test.nix { inherit inputs system; };

  mysbx-tests = crate.overrideAttrs (old: {
    doCheck = true;
    # The CLI tests drive the built binary as a subprocess with a
    # hand-rolled fixed environment (tests/cli.rs::spawn); `TMPDIR` and a
    # writable HOME suffice. `cargo`/`rustc` come from the stdenv set up
    # by buildRustPackage.
  });
}
