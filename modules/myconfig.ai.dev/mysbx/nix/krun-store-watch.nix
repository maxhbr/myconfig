# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The static build of nix/krun-store-watch.c: the inotify store
# watcher of the krun guests' flake-source poisoning instrumentation
# (doc/TODOs/krun-guest-flake-source-symlink.md, bd myconfig-yiw).
# STATIC (musl), like the rootfs's busybox/mkfs.ext4/sqlite3 — the
# guest nix wrapper starts it alongside the first nix call, and it
# must run without any store path.

{
  lib,
  pkgsStatic,
}:

pkgsStatic.stdenv.mkDerivation {
  pname = "mysbx-krun-store-watch";
  version = "0.1.0";

  src = ./krun-store-watch.c;

  dontConfigure = true;
  dontUnpack = true;

  buildPhase = ''
    runHook preBuild
    cc=$(command -v x86_64-unknown-linux-musl-gcc) \
      || cc=$(command -v musl-gcc) \
      || cc=$(command -v cc) \
      || { echo "store-watch: no static cc on PATH" >&2; exit 1; }
    "$cc" -static -Os -o store-watch "$src"
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm555 store-watch "$out/bin/store-watch"
    runHook postInstall
  '';

  meta = {
    description = "mysbx krun guests: inotify watcher of new *-source store entries";
    platforms = lib.platforms.linux;
    mainProgram = "store-watch";
  };
}
