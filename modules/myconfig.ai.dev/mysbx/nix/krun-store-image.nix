# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The guest-own nix store DISK of the VM backends (docs/design/
# guest-store.md, the option-B realization): one READ-ONLY erofs
# image carrying a whole `/nix` tree — `store/<name>` for every
# path of the tools closure plus `var/nix/` with a REGISTERED nix
# database — attached as a second virtio-blk device (the scratch of
# backends.md D7 is the first) and mounted read-only at `/nix` by
# the guest init. The host store is not shared at all in this mode:
# the erofs IS the guest's store, and the per-run scratch overlay
# (krun-guest-nix.nix) writes new paths on top of it exactly as on
# the podman tier's image store (backends.md D2's scope decision —
# `includeNixDB`'s db contract, same `nix-store --load-db` recipe).
#
# erofs, not squashfs or ext4: the format needs no kernel the
# backends do not already run — libkrunfw x86_64 carries
# CONFIG_EROFS_FS=y (xattr, posix acl, zip/lz4 — checked against the
# pinned libkrunfw 5.5.0 config), and the qemu kernel pin
# (./microvm-qemu-kernel.nix) adds the same line to its structured
# config. ext4 would work too (the scratch proves it) but costs the
# full uncompressed closure per rebuild; erofs packs the closure
# directly from the store paths (the `--tar` mode — no populated
# tree, no copies), compressed, content-defined and therefore built
# once per wrapper rebuild.
#
# The db is a REAL one: `nix-store --load-db < registration` with
# NIX_REMOTE=local?root=$PWD (the dockerTools recipe, pkgs/
# build-support/docker/default.nix's mkDbExtraCommand) — so the
# guest's single-user nix accepts every baked path as valid without
# the host-db copy-and-reconcile of bd myconfig-j23/mxu. The
# gcroots mirror dockerTools too: one symlink per top-level
# rootPath, so an in-guest GC keeps the baked closure.
#
# Determinism: the closure's own mtimes are stable, `-T 0` fixes the
# fs timestamps, the UUID is a fixed literal, uid/gid are forced to
# 0, mkfs sorts by path, and the db's registrationTime is reset via
# sqlite (the dockerTools reproducibility step) — the image hash is
# a function of the closure alone. The implicit `store/` directory
# tar creates lands mode 0777 (no pseudo-file mechanism exists for
# erofs): harmless — the image mounts read-only, and the overlay's
# upper layer owns the merged view whenever the guest nix runs.
{
  lib,
  runCommand,
  closureInfo,
  erofs-utils,
  gnutar,
  nix,
  sqlite,
}:
{
  # The store paths whose CLOSURES fill the disk: what the sandbox
  # `PATH` carries (`toolsEnv` of ./mysbx.nix), the payload-relevant
  # pins (the shell, the mux entries) — everything a run may exec
  # when the host store is not visible (docs/design/guest-store.md
  # "what a narrowed store breaks").
  rootPaths,
}:
let
  info = closureInfo { inherit rootPaths; };
in
runCommand "mysbx-krun-store-disk.img"
  {
    nativeBuildInputs = [
      erofs-utils
      gnutar
      nix
      sqlite
    ];
    # The fixed image metadata (the determinism block above): a
    # literal UUID and label, every timestamp at the epoch.
    env = {
      SOURCE_DATE_EPOCH = "1";
      EROFS_UUID = "5f6d7962-7374-6f72-6564-69736b2d0001";
    };
  }
  ''
    set -euo pipefail

    # The nix database (the dockerTools recipe): a local store rooted
    # at $PWD, so the db lands at nix/var/nix/db/ — the layout the
    # image carries as var/nix/.
    export NIX_REMOTE="local?root=$PWD"
    export USER=nobody

    nix-store --load-db < ${info}/registration
    ${lib.getExe sqlite} nix/var/nix/db/db.sqlite \
      "UPDATE ValidPaths SET registrationTime = $SOURCE_DATE_EPOCH"

    # The gcroots of dockerTools: one symlink per top-level rootPath,
    # so an in-guest GC keeps the baked closure alive.
    mkdir -p nix/var/nix/gcroots/mysbx
    for p in ${lib.escapeShellArgs (map toString rootPaths)}; do
      ln -s "$p" "nix/var/nix/gcroots/mysbx/$(basename "$p")"
    done

    # The whole `/nix` tree into one image, no populated tree, no
    # copies: the closure's store paths become store/<name>, the db
    # tree becomes var/nix/ — the tar transforms of nixos/lib/
    # erofs-store-image.nix (the nixpkgs precedent) plus the var/
    # rewrite and the case-hack strip.
    ${gnutar}/bin/tar \
      --create --absolute-names --verbatim-files-from \
      --transform 'flags=rSh;s|/nix/store/|store/|' \
      --transform 'flags=rSh;s|~nix~case~hack~[[:digit:]]\+||g' \
      --transform 'flags=rSh;s|^nix/var|var|' \
      --files-from ${info}/store-paths \
      nix/var \
      | ${erofs-utils}/bin/mkfs.erofs \
        --quiet \
        --force-uid=0 --force-gid=0 \
        -L mysbx-store \
        -U "$EROFS_UUID" \
        -T 0 \
        -zlz4 \
        --hard-dereference \
        --tar=f \
        "$out"
  ''
