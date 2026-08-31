# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The Rust `agent-gvisor` CLI (../rust, ../docs/spec.md).
# The crate is zero-dependency by design, so the lockfile is trivial and no
# `outputHashes` can ever be needed.
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  coreutils,
  git,
  podman,
  gvisor,
  agent-gvisor-image,
  # Default image reference used by `agent-gvisor start --image`.
  defaultImage ? "${agent-gvisor-image.imageName}:${agent-gvisor-image.imageTag}",
  # Default Podman `--runtime` value. An absolute path works without
  # registering a named runtime in containers.conf.
  defaultRuntime ? "${gvisor}/bin/runsc",
}:

rustPlatform.buildRustPackage {
  pname = "agent-gvisor";
  version = "0.1.0";

  src = ../rust;
  cargoLock.lockFile = ../rust/Cargo.lock;

  # The test suite (cargo test + the CLI harness) runs in `nix/checks.nix`
  # against the recording git/podman stubs; keep it out of every production
  # host rebuild.
  doCheck = false;

  nativeBuildInputs = [ makeBinaryWrapper ];

  # The binary execs only `git`, `podman` and `sha256sum` from PATH
  # (docs/spec.md §14.5); everything else is internal
  # (realpath/grep/sed/tr).
  postInstall = ''
    wrapProgram $out/bin/agent-gvisor \
      --prefix PATH : ${
        lib.makeBinPath [
          coreutils
          git
          podman
        ]
      } \
      --set-default AGENT_GVISOR_DEFAULT_IMAGE "${defaultImage}" \
      --set-default AGENT_GVISOR_DEFAULT_RUNTIME "${defaultRuntime}"

    # Hand-written fish tab completion (../rust/completions, kept in sync
    # with src/usage.txt by the `agent-gvisor-completions` check in
    # checks.nix). The crate stays zero-dependency: this is a plain fish
    # script, not clap-generated. The vendor path is the one
    # `installShellFiles --fish` uses and fish's NixOS integration
    # collects.
    install -Dm 0644 $src/completions/agent-gvisor.fish \
      $out/share/fish/vendor_completions.d/agent-gvisor.fish
  '';

  meta = {
    description = "Manage rootless Podman + gVisor coding-agent worktree sessions";
    mainProgram = "agent-gvisor";
    platforms = lib.platforms.linux;
  };
}
