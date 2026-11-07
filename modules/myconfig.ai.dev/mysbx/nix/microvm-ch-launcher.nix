# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The microvm-ch backend's launcher (bd myconfig-9wkp.6, the
# microvm-qemu-launcher.nix twin): the zero-dependency Rust twin of
# mysbx-microvm-qemu that spawns one rootless virtiofsd per device,
# one passt child per shared-network run, and one cloud-hypervisor
# VM per run.
#
# The binaries are RUNTIME paths
# (MYSBX_MICROVM_CH_VIRTIOFSD / MYSBX_MICROVM_CH_PASST /
# MYSBX_MICROVM_CH_CH — the launcher refuses a missing pin, never a
# PATH guess), pinned to the exact builds of the inputs: under
# bwrap (no host PATH) this is the only reliable lookup, and a host
# swapping its cloud-hypervisor must rebuild the wrapper for a run
# to see it.
#
{
  lib,
  rustPlatform,
  makeBinaryWrapper,
  # The cloud-hypervisor the RUN should invoke. A parameter, not a
  # `pkgs.` reference (the same convention as every input of
  # ./mysbx.nix), so a host pins exactly the build its runs need.
  cloud-hypervisor,
  # The virtiofsd the RUN should invoke (rust-virtiofsd of nixpkgs).
  virtiofsd,
  # The passt the RUN should invoke (the vhost-user networking of
  # --network shared, bd myconfig-9wkp.5).
  passt,
}:
rustPlatform.buildRustPackage {
  pname = "mysbx-microvm-ch";
  version = "0.1.0";

  src = ../microvm-ch-rs;
  cargoLock.lockFile = ../microvm-ch-rs/Cargo.lock;

  # Zero dependencies: no vendoring, no outputHashes, like the qemu
  # twin. cloud-hypervisor, virtiofsd and passt are separate
  # PROCESSES invoked by absolute paths — nothing to link against.

  nativeBuildInputs = [ makeBinaryWrapper ];

  # The wrapper pins the exact binaries of the inputs; under bwrap
  # (no host PATH) this is the only reliable lookup.
  postInstall = ''
    wrapProgram $out/bin/mysbx-microvm-ch \
      --set MYSBX_MICROVM_CH_CH '${lib.getExe cloud-hypervisor}' \
      --set MYSBX_MICROVM_CH_VIRTIOFSD '${lib.getExe virtiofsd}' \
      --set MYSBX_MICROVM_CH_PASST '${lib.getExe passt}'
  '';

  meta = {
    description = "mysbx microvm-ch backend: rootless virtiofsd + passt + cloud-hypervisor launcher (bd myconfig-9wkp.6)";
    mainProgram = "mysbx-microvm-ch";
    platforms = lib.platforms.linux;
  };
}
