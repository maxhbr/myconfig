# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# CI checks for the Rust `agent-gvisor` rewrite (../docs/spec.md):
#
#   agent-gvisor-tests        cargo test — the full parity suite in
#                             ../rust/tests/ (podman argv, state layout,
#                             exact error messages, shell quoting, home
#                             seeding), against recording git/podman stubs
#                             generated at test time.
#   agent-gvisor-cli-harness end-to-end CLI flows (doctor, a full session
#                             cycle, list) through the same stubs, driven
#                             by ../tests/agent-gvisor-cli-harness.sh.
#
# Both build the crate WITHOUT the production wrapper: the wrapper prepends
# the real git/podman to PATH, which would shadow the stubs.
{
  self,
  inputs,
  system,
}:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};

  crate = pkgs.rustPlatform.buildRustPackage {
    pname = "agent-gvisor";
    version = "0.1.0";
    src = ../rust;
    # Zero dependencies by design, so the lockfile is trivial and no
    # `outputHashes` can ever be needed.
    cargoLock.lockFile = ../rust/Cargo.lock;
    doCheck = true;
  };
in
{
  agent-gvisor-tests = crate;

  agent-gvisor-cli-harness =
    pkgs.runCommand "agent-gvisor-cli-harness"
      {
        nativeBuildInputs = with pkgs; [
          bash
          coreutils
          gnugrep
        ];
      }
      ''
        BIN=${crate}/bin/agent-gvisor \
        STUBS=${../rust/tests/stubs} \
          bash ${../tests/agent-gvisor-cli-harness.sh}
        touch "$out"
      '';
}
