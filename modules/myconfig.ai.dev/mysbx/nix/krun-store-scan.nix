# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The static build of nix/krun-store-scan.c: the bulk directory
# scanner of the krun guests' db reconcile. STATIC (musl), like the
# rootfs's busybox/mkfs.ext4/sqlite3 — the direct backend's init runs
# it before any store path is visible; the podman-krun twin bakes it
# into the image's setup path.

{
  lib,
  pkgsStatic,
}:

pkgsStatic.stdenv.mkDerivation {
  pname = "mysbx-krun-store-scan";
  version = "0.1.0";

  src = ./krun-store-scan.c;

  dontConfigure = true;
  dontUnpack = true;

  buildPhase = ''
    runHook preBuild
    cc=$(command -v x86_64-unknown-linux-musl-gcc) \
      || cc=$(command -v musl-gcc) \
      || cc=$(command -v cc) \
      || { echo "store-scan: no static cc on PATH" >&2; exit 1; }
    "$cc" -static -Os -o store-scan "$src"
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm555 store-scan "$out/bin/store-scan"
    runHook postInstall
  '';

  meta = {
    description = "mysbx krun guests: bulk getdents scanner of the db reconcile";
    platforms = lib.platforms.linux;
    mainProgram = "store-scan";
  };
}
