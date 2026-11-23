# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The static build of nix/krun-vsock-bridge.c: the krun restricted
# mode's guest relay (bd myconfig-ild7 — n8kv.2 C3). The proxy-only
# krun guest has NO net device — the payload's
# http://127.0.0.1:<port> endpoint URLs reach this bridge, which
# pumps every accepted connection to the explicit vsock port the
# launcher mapped onto the per-run egress broker's socket. STATIC
# (musl), like the rootfs's busybox/mkfs.ext4/sqlite3/store-scan —
# the guest init starts it before any store path is visible.

{
  lib,
  pkgsStatic,
}:

pkgsStatic.stdenv.mkDerivation {
  pname = "mysbx-krun-vsock-bridge";
  version = "0.1.0";

  src = ./krun-vsock-bridge.c;

  dontConfigure = true;
  dontUnpack = true;

  buildPhase = ''
    runHook preBuild
    cc=$(command -v x86_64-unknown-linux-musl-gcc) \
      || cc=$(command -v musl-gcc) \
      || cc=$(command -v cc) \
      || { echo "vsock-bridge: no static cc on PATH" >&2; exit 1; }
    "$cc" -static -Os -o vsock-bridge "$src"
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm555 vsock-bridge "$out/bin/vsock-bridge"
    runHook postInstall
  '';

  passthru.guestPath = "/bin/mysbx-vsock-bridge";

  meta = {
    description = "mysbx krun guests: the restricted mode's vsock relay";
    platforms = lib.platforms.linux;
    mainProgram = "vsock-bridge";
  };
}
