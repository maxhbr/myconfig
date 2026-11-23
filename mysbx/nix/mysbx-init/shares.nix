# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the share placement: the nested-virt refuse-or-announce and the
# per-share device mounts and sandbox-path bind loop.
{ basePrefix, reconcileSelect }:
''
  # Nested virtualization (docs/design/config.md D27): the run
  # asked for `/dev/kvm` in the guest (the microvm-qemu builder rode
  # the marker on the manifest env; the launcher booted -cpu host,
  # accel=kvm). Refuse-or-announce, the store-disk rule's twin: a
  # marked run WITHOUT /dev/kvm means the VM came up without
  # VMX/SVM after all (a TCG boot, a missing host module) — better
  # to fail HERE, naming the marker, than at the payload's first
  # nested VM launch. An UNMARKED run never checks anything: the
  # krun guest (no marker, no KVM in libkrunfw) is untouched.
  if [ "''${MYSBX_MICROVM_NESTED_VIRT:-}" = "1" ]; then
      [ -e /dev/kvm ] \
          || fail "MYSBX_MICROVM_NESTED_VIRT=1 but /dev/kvm does not exist — the VM has no hardware virtualization (a TCG boot or a host without nesting: docs/design/config.md D27)"
      step "nested-virt: /dev/kvm present"
  fi

  # Each share: one "DEVICE SLOT SANDBOX_PATH ro|rw" record — the
  # manifest's share lines above, or the legacy
  # MYSBX_VM_SHARES env (';'-separated, cmdline-sized debug
  # runs only — the seventh live finding killed that route for
  # real configs). Each device is mounted ONCE under
  # /tmp/mysbx-shares keyed by its tag; the SLOT is the share's
  # dir inside it, and the SANDBOX path is a bind mount of the
  # device mount's slot — the payload's contract is the sandbox
  # layout, the tmpfs placement is this init's.
  share_records="''${share_records:-}"
  if [ -z "$manifest" ]; then
      shares="''${MYSBX_VM_SHARES:-}"
      while [ -n "$shares" ]; do
          entry=''${shares%%';'*}
          [ "$shares" = "$entry" ] && shares= || shares=''${shares#*';'}
          [ -n "$entry" ] || continue
          share_records="$share_records
  $entry"
      done
  fi
  # The records as a FILE, read with input redirection (NOT a
  # pipeline: the `while` of a pipeline runs in a SUBSHELL, and a
  # mount done there must not be relied on — the live sim showed
  # the device mount vanishing with the subshell on one host).
  # Redirection keeps the loop — and every mount it performs — in
  # THIS shell.
  records_file=/tmp/mysbx-share-records
  printf '%s\n' "$share_records" > "$records_file"
  while IFS=" " read -r tag slot sandbox mode; do
      [ -n "$tag" ] || continue
      # The LINKLESS shares (bd myconfig-j23): consumed directly at
      # their backing path, never linked into the sandbox layout.
      [ "$sandbox" = "-" ] && continue
      mountpoint=/tmp/mysbx-shares/$tag
      step "placing share $sandbox (device $tag, slot $slot, $mode)"
      case " $mounted_devices " in
          *" $tag "*) ;;
          *)
              step "mounting device $tag ($mode) at $mountpoint"
              "$BB" mkdir -p "$mountpoint" \
                  || fail "cannot create the mountpoint $mountpoint"
              if [ "$mode" = ro ]; then
                  "$BB" mount -t virtiofs -o ro "$tag" "$mountpoint" \
                      || fail "cannot mount the virtiofs tag $tag at $mountpoint (ro)"
              else
                  "$BB" mount -t virtiofs "$tag" "$mountpoint" \
                      || fail "cannot mount the virtiofs tag $tag at $mountpoint (rw)"
              fi
              mounted_devices=" $mounted_devices $tag "
              ;;
      esac
      sharetarget=$mountpoint/$slot
      # The sandbox path already links at the mount (the rootfs
      # baked /nix/store and /mysbx-home): nothing to place.
      [ "$(  "$BB" readlink "$sandbox" || true)" = "$sharetarget" ] && continue
      # Where does the mountpoint live? /tmp/... for tmpfs-rooted
      # paths, /mysbx-home/... for the home's state shares (through
      # the baked /mysbx-home link), a fresh tmpfs at the FIRST
      # component for everything else (the ro root can hold no new
      # entries; mounting a tmpfs OVER a root-share subdirectory is
      # fine — libkrun's own implicit init mounts devtmpfs/proc/
      # sysfs exactly that way, the EBUSY of the spike's finding 9
      # is virtiofs-on-virtiofs only).
      case "$sandbox" in
          /tmp/*|/mysbx-home/*)
              # The parents on the tmpfs (the ninth live finding:
              # the home tree creates only the XDG roots — a share
              # below a DEEPER path, .config/workmux/config.yaml,
              # needs its parents created).
              "$BB" mkdir -p "$(  "$BB" dirname "$sandbox")" \
                  || fail "cannot create the parents of $sandbox"
              ;;
          /*/*)
              first=''${sandbox#/}
              first=/''${first%%/*}
              case " $tmpfs_first_components " in
                  *" $first "*) ;;
                  *)
                      "$BB" mount -t tmpfs tmpfs "$first" \
                          || fail "cannot mount the tmpfs for $sandbox"
                      tmpfs_first_components=" $tmpfs_first_components $first "
                      ;;
              esac
              # The parents live ON the tmpfs just mounted at
              # $first — creating the real parent path after the
              # mount writes into the tmpfs.
              "$BB" mkdir -p "$(  "$BB" dirname "$sandbox")" \
                  || fail "cannot create the parents of $sandbox"
              ;;
          *)
              fail "the share path $sandbox cannot be placed (a krun mount dest needs a parent)"
              ;;
      esac
      # The sandbox path is a BIND MOUNT of the device slot, not a
      # symlink: getcwd() and realpath() then report the sandbox
      # path, as on the bwrap/podman backends, so tools that
      # canonicalize (git worktree metadata, aoe, herdr) never see
      # the /tmp/mysbx-shares spelling. A bind is no new virtiofs
      # device, so finding 9 does not apply. The mountpoint takes
      # the slot's type; a state share mounts OVER an XDG dir the
      # home tree created (.cache and friends), like the bwrap
      # backend's later bind over the tmpfs dir. Nothing is ever
      # removed recursively: only a symlink or a type-mismatched
      # empty entry is replaced.
      if [ -d "$sharetarget" ]; then
          if [ -L "$sandbox" ] || { [ -e "$sandbox" ] && [ ! -d "$sandbox" ]; }; then
              "$BB" rm -f "$sandbox" \
                  || fail "cannot replace $sandbox with a directory mountpoint"
          fi
          "$BB" mkdir -p "$sandbox" \
              || fail "cannot create the mountpoint $sandbox"
      else
          if [ -L "$sandbox" ]; then
              "$BB" rm -f "$sandbox" \
                  || fail "cannot replace $sandbox with a file mountpoint"
          elif [ -d "$sandbox" ]; then
              "$BB" rmdir "$sandbox" \
                  || fail "cannot replace the directory $sandbox with a file mountpoint"
          fi
          [ -e "$sandbox" ] || : > "$sandbox" \
              || fail "cannot create the mountpoint $sandbox"
      fi
      # The canonical mountpoint: a parent may be a symlink (the
      # baked /mysbx-home), and busybox's remount looks the target
      # up in /proc/mounts by its literal, resolved spelling.
      mnt=$("$BB" readlink -f "$sandbox") \
          || fail "cannot resolve the mountpoint $sandbox"
      step "binding $sandbox ($mnt) to $sharetarget"
      "$BB" mount -o bind "$sharetarget" "$mnt" \
          || fail "cannot bind $sharetarget at $sandbox"
      if [ "$mode" = ro ]; then
          "$BB" mount -o remount,bind,ro "$mnt" \
              || fail "cannot make the bind of $sandbox read-only"
      fi
  done < "$records_file"
  "$BB" rm -f "$records_file"

''
