# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — pick_vd(): the ONE /dev/vd* whose ro flag matches — the shared
# device identification of the store disk and the scratch.
{ basePrefix, reconcileSelect }:
''
  # The shared vd* pick (docs/design/guest-store.md): the ONE
  # /dev/vd* whose /sys/block ro flag matches WANT — 1 for the
  # read-only store disk, 0 for the writable scratch — never a
  # name (the launcher's block_id names an MMIO slot host-side,
  # no serial the guest reads). Exactly one match may exist: two
  # read-only or two writable vd* devices are an ambiguous attach
  # the init refuses rather than guess at — a silently wrong mount
  # is the worst outcome. Prints the single match, empty when
  # none attached; the CALLER refuses the empty case with its own
  # announcement-specific message.
  pick_vd() {
      # ash-local: a caller must not collide with the scan's
      # working names.
      local want_ro role mode_word dev count d
      want_ro=$1
      role=$2
      if [ "$want_ro" = "1" ]; then
          mode_word="read-only"
      else
          mode_word="writable"
      fi
      dev=
      count=0
      for d in /sys/block/vd*; do
          [ -d "$d" ] || continue
          if [ "$("$BB" cat "$d/ro")" = "$want_ro" ]; then
              dev="/dev/''${d##*/}"
              count=$((count + 1))
          fi
      done
      if [ "$count" -gt 1 ]; then
          fail "$count $mode_word /dev/vd* devices — $role is ambiguous, refusing to guess"
      fi
      printf '%s' "$dev"
  }

''
