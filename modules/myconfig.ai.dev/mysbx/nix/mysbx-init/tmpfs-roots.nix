# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the writable tmpfs roots: /tmp, the sandbox home tree, and the
# tmpfs_first_components/mounted_devices/manifest_chdir accumulators.
{ basePrefix, reconcileSelect }:
''
  # The guest's writable scratch: the root is a READ-ONLY virtiofs
  # share, so nothing below / may change — /tmp is where every
  # writable thing lives, the share mounts included (finding 9:
  # a second virtiofs device cannot nest below the root share,
  # EBUSY).
  step "mounting the guest tmpfs on /tmp"
  "$BB" mount -t tmpfs tmpfs /tmp \
      || fail "cannot mount the guest tmpfs on /tmp"
  "$BB" mkdir -p /tmp/mysbx-shares /tmp/mysbx-home \
      || fail "cannot create the tmpfs roots"
  # The sandbox home's tree (config.md D14): the baked /mysbx-home
  # symlink resolves here. The XDG parents exist for tools that
  # write before reading $XDG_* (the payload env names these paths);
  # the mux socket dir is the same MUX_SOCKET_DIR the env's
  # TMUX_TMPDIR names (config.md D16/D17) — tmpfs, so guest-internal
  # sockets work and never touch a host-shared surface.
  "$BB" mkdir -p /tmp/mysbx-home/.local/share /tmp/mysbx-home/.local/state \
      /tmp/mysbx-home/.config /tmp/mysbx-home/.cache \
      /tmp/mysbx-home/.mysbx-tmux \
      || fail "cannot create the sandbox home tree"

  # A tmpfs at the FIRST component of a sandbox path outside /tmp
  # and the home: the ro root can hold no new entries, but mounting
  # a tmpfs OVER a root-share subdirectory is fine (libkrun's own
  # implicit init mounts devtmpfs/proc/sysfs exactly that way; the
  # EBUSY of finding 9 is virtiofs-on-virtiofs only). The first
  # component of a share's sandbox path carries nothing but the
  # rootfs skeleton below it, so covering it costs no content —
  # and the share's mountpoint then lives on writable tmpfs. Mounted
  # once per component, shared by every share under it.
  tmpfs_first_components=""
  mounted_devices=""
  manifest_chdir=""

''
