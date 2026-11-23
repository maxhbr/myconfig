# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the guest store: the erofs store disk, the scratch disk, the db
# reconcile, the store overlay and the single-user nix env.
{ basePrefix, reconcileSelect }:
''
  # The guest-own nix store disk (docs/design/guest-store.md): the
  # erofs image the launcher attached as a SECOND virtio-blk
  # device, READ-ONLY at the device level (VIRTIO_BLK_F_RO — the
  # launcher passed read_only=true, so the guest kernel refuses
  # writes before any mount flag even applies). Mounted over the
  # rootfs's baked /nix dir: its store/ is a REAL directory (the
  # baked /nix/store SYMLINK is hidden under the mount and the
  # host-store share it targets is absent — the manifest carries
  # no store record in this mode), its var/nix/ carries the
  # REGISTERED db the image baked (the includeNixDB recipe), so
  # the scratch half below needs NO reconcile: db and store match
  # by construction, both baked in one image. Device
  # identification is the shared pick_vd scan above: the store
  # disk is the read-only vd*, the scratch the writable one.
  if [ "''${MYSBX_VM_STORE_DISK:-}" = "1" ]; then
      store_dev=$(pick_vd 1 "the store disk")
      [ -n "$store_dev" ] \
          || fail "MYSBX_VM_STORE_DISK=1 but no read-only /dev/vd* device — the store disk did not attach"
      step "mounting the guest-own nix store ($store_dev, erofs, read-only) at /nix"
      "$BB" mount -t erofs -o ro "$store_dev" /nix \
          || fail "cannot mount the store disk $store_dev as erofs at /nix"
  else
      # Refuse-or-announce (the house rule): a read-only vd* the
      # env never announced means a disk attached that the
      # manifest forgot — seeing it without a mount the init
      # controls would be exactly the silent switch the contract
      # forbids.
      for d in /sys/block/vd*; do
          [ -d "$d" ] || continue
          if [ "$("$BB" cat "$d/ro")" = "1" ]; then
              fail "a read-only /dev/vd*''${d##*/} exists but MYSBX_VM_STORE_DISK is unset — the disk attached without its announcement"
          fi
      done
  fi

  # The scratch disk (bd myconfig-dak.7, backends.md D7): a REAL
  # virtio-blk device the launcher attached (krun_add_disk2), the
  # per-run raw file — no stale fs ever survives to be probed.
  # Identified by its READ-ONLY FLAG, never by name: the block_id
  # names the MMIO slot host-side, it is no serial the guest reads;
  # the scratch is the WRITABLE /dev/vd* (the store disk above is
  # the read-only one). The manifest env announces it
  # (MYSBX_VM_SCRATCH=1); a run without it attaches no scratch —
  # no silent RAM fallback.
  #
  # The disk becomes the nix scratch: the ext4 carries the overlay
  # upper/work over the ro store (the baked erofs above or the ro
  # host share), nix state, logs, cache and
  # TMPDIR — the guest kernel's OWN filesystem (chown and overlay
  # xattrs work natively, nothing over the virtiofs xattr
  # surface). The overlay mounts at /nix/store itself.
  if [ "''${MYSBX_VM_SCRATCH:-}" = "1" ]; then
      # The device: the WRITABLE /dev/vd* through the shared
      # pick_vd scan above (the root is virtiofs, not blk; the
      # store disk above is the read-only one).
      scratch_dev=$(pick_vd 0 "the scratch")
      [ -n "$scratch_dev" ] \
          || fail "MYSBX_VM_SCRATCH=1 but no writable /dev/vd* device — the disk did not attach"
      dev=$scratch_dev
      step "formatting $dev as the nix scratch (ext4)"
      /bin/mkfs.ext4 -q -F "$dev" \
          || fail "cannot mkfs.ext4 the scratch device $dev"
      # The copied database and the overlay's upper/work live on
      # the freshly formatted disk.
      "$BB" mkdir -p /mysbx-nix
      "$BB" mount -t ext4 "$dev" /mysbx-nix \
          || fail "cannot mount the scratch $dev at /mysbx-nix"

      # The guest nix state (bd myconfig-j23): the db copy the
      # podman image bakes via includeNixDB, done here because the
      # direct rootfs carries none. The SOURCE follows the store
      # mode: the baked disk's own REGISTERED db (/nix/var/nix of
      # the erofs above — it matches its store by construction,
      # the image baked both, so the RECONCILE below never runs in
      # that mode) or the LINKLESS nix-var share (stage-ro, the
      # network gate's twin of the bwrap tier's /nix/var/nix bind)
      # holding the HOST's db — copying it registers every shared
      # store path as valid, so the guest's single-user nix never
      # re-adds (a re-add deletes a lower-layer entry through the
      # overlay and crashes in fchmodat2). In both
      # modes ONLY the db: the source tree may carry more, but a
      # HOST's live var also carries builds/ (0700 per-build log
      # dirs owned by root/nixbld, unreadable through the share)
      # and profiles/gcroots/temproots session state the guest
      # never needs — the single-user nix creates its own.
      # big-lock/reserved are 0600 root-owned and carry no data (a
      # lock and reserved space); nix recreates both.
      if [ "''${MYSBX_VM_STORE_DISK:-}" = "1" ]; then
          db_src=/nix/var/nix/db
      else
          db_src=/tmp/mysbx-shares/stage-ro/nix-var/db
      fi
      if [ -d "$db_src" ]; then
          step "copying the nix db ($db_src) to the scratch"
          "$BB" mkdir -p /mysbx-nix/state/db \
              || fail "cannot create the nix state dir on the scratch"
          # The SOURCE is $db_src in BOTH modes — the host-share
          # spelling only in the share mode, the erofs's own
          # /nix/var/nix/db in the baked mode.
          for f in "$db_src"/*; do
              [ -e "$f" ] || continue
              case "''${f##*/}" in
                  big-lock | reserved) continue ;;
              esac
              "$BB" cp -R "$f" /mysbx-nix/state/db/ \
                  || fail "cannot copy the nix db entry ''${f##*/}"
          done
      else
          step "no nix-var db — the guest nix db starts empty"
      fi
      # The db reconcile (bd myconfig-mxu, the direct twin of the
      # podman setup wrapper's step) — HOST-SHARE MODE ONLY: the
      # copied db is a SNAPSHOT of host state, the staged store
      # share is LIVE — a host rebuild restaging the share leaves
      # rows for paths the share no longer carries, and an
      # in-session fetch once registered a flake source as a store
      # SYMLINK (doc/TODOs/krun-guest-flake-source-symlink.md).
      # Both divergences break every later eval that trusts the
      # row ("is a symlink" / "Stale file handle") and nix's re-add
      # of the lower-layer entry crashes (fchmodat2). Prune the
      # rows the share cannot keep against the bare mounted ro
      # share, before overlay dentries can retain its inodes.
      # The freshly formatted scratch has no upper entries, so
      # the baked /nix/store link sees the same paths. The baked
      # disk's db needs NONE of this: store and db are one image,
      # they cannot diverge. POSIX sh +
      # the baked static sqlite3 (a real copy like busybox):
      # [ -e ] is FALSE for a dangling symlink, so both poison
      # shapes (dangling and live-target) prune, as does a plain
      # missing path. `nix store delete` cannot do this job
      # (liveness semantics, plus its store-tree unlink is
      # exactly the lower-layer crash).
      if [ "''${MYSBX_VM_STORE_DISK:-}" != "1" ] && [ -f /mysbx-nix/state/db/db.sqlite ]; then
          registered_file=/mysbx-nix/registered-paths.txt
          scan_file=/mysbx-nix/store-scan.txt
          recheck_file=/mysbx-nix/recheck-paths.txt
          doomed_file=/mysbx-nix/doomed-paths.txt
          step "enumerating the registered store paths from the copied nix db"
          /bin/sqlite3 /mysbx-nix/state/db/db.sqlite \
              ${reconcileSelect} \
              > "$registered_file" \
              || fail "cannot enumerate registered store paths in the copied nix database"
          n_registered=$("$BB" wc -l < "$registered_file")
          : > "$doomed_file"
          # One bulk getdents pass replaces the per-path stat: every
          # [ -e ]/[ -L ] of the old loop was one virtiofs FUSE round
          # trip, and ~175k registered paths made the reconcile the
          # minute of the boot (bd myconfig-kid). The scan carries
          # each entry's d_type straight from the readdir stream
          # (libkrun passes the host's through), the awk join
          # classifies all rows in bulk, and only the rows it
          # cannot classify — DT_UNKNOWN (d_type 0) or a path
          # deeper than the scanned root — fall back to the old
          # per-path stat loop, exact semantics kept: [ -e ] false
          # for missing AND dangling symlinks, [ -L ] true for
          # every symlink (live-target poison included). A
          # single-component row whose root entry carries a KNOWN
          # d_type other than symlink is decidable in bulk — [ -e ]
          # is true and [ -L ] false for it, whatever the type — so
          # DT_DIR rows (the ~100% case: a registered store path
          # IS a directory) stay in the join and never stat.
          step "scanning the read-only store share (bulk getdents)"
          /bin/store-scan /nix/store > "$scan_file" \
              || fail "cannot scan the read-only store share"
          step "checking $n_registered registered store path(s) against the read-only store share"
          "$BB" awk -v store_prefix="/nix/store/" -v doomed_file="$doomed_file" -v recheck_file="$recheck_file" '
              NR == FNR { entry[$2] = $1; next }
              {
                  if (index($0, store_prefix) != 1) next
                  p = substr($0, length(store_prefix) + 1)
                  deep = index(p, "/")
                  name = deep > 0 ? substr(p, 1, deep - 1) : p
                  if (name == "") next
                  t = entry[name]
                  if (t == "" || (t == 10 && deep == 0))
                      print $0 >> doomed_file
                  else if (deep > 0 || t == 0)
                      print $0 >> recheck_file
              }
          ' "$scan_file" "$registered_file" \
              || fail "cannot join the registered paths against the store scan"
          "$BB" rm -f "$scan_file" "$registered_file"
          # The fallback stat loop: only the rows the scan could not
          # classify (at most the DT_UNKNOWN and deep ones), the
          # same [ -e ]/[ -L ] semantics the old full loop had.
          if [ -s "$recheck_file" ]; then
              while IFS= read -r p; do
                  [ -n "$p" ] || continue
                  if [ ! -e "$p" ] || [ -L "$p" ]; then
                      printf '%s\n' "$p" >> "$doomed_file"
                  fi
              done < "$recheck_file"
          fi
          "$BB" rm -f "$recheck_file"
          n_doomed=$("$BB" wc -l < "$doomed_file")
          if [ "$n_doomed" -gt 0 ]; then
              step "reconciling the db: $n_doomed of $n_registered registered store path(s) missing or symlinked, dropping their rows"
              {
                  printf 'PRAGMA foreign_keys=ON;\nBEGIN;\n'
                  while IFS= read -r p; do
                      [ -n "$p" ] || continue
                      printf "DELETE FROM Refs WHERE reference = (SELECT id FROM ValidPaths WHERE path = '%s');\n" "$p"
                      printf "DELETE FROM ValidPaths WHERE path = '%s';\n" "$p"
                  done < "$doomed_file"
                  printf 'COMMIT;\n'
              } | /bin/sqlite3 -bail /mysbx-nix/state/db/db.sqlite \
                  || fail "cannot reconcile dependent rows in the copied nix database"
          fi
          "$BB" rm -f "$doomed_file"
          fk_violations=$(/bin/sqlite3 /mysbx-nix/state/db/db.sqlite 'PRAGMA foreign_key_check;') \
              || fail "cannot check foreign keys in the copied nix database"
          [ -z "$fk_violations" ] \
              || fail "foreign-key violations remain in the copied nix database"
          step "copied nix db validation complete"
      fi

      "$BB" mkdir -p /mysbx-nix/store-upper /mysbx-nix/store-work \
          || fail "cannot create the overlay dirs on the scratch"
      if [ "''${MYSBX_VM_STORE_DISK:-}" = "1" ]; then
          # The baked disk's /nix/store is a REAL directory of the
          # erofs mounted at /nix (the baked rootfs SYMLINK is
          # hidden under that mount) — the overlay mounts directly
          # on it, no tmpfs mountpoint trick needed (that exists
          # only for the share mode, where /nix/store is a baked
          # SYMLINK into the stage tree and a mount THROUGH a
          # symlink lands at the link's TARGET).
          step "mounting the store overlay (erofs lower, scratch upper)"
          "$BB" mount -t overlay overlay \
              -o lowerdir=/nix/store,upperdir=/mysbx-nix/store-upper,workdir=/mysbx-nix/store-work \
              /nix/store \
              || fail "cannot mount the store overlay (erofs lower, scratch upper)"
      else
          # /nix/store is a baked SYMLINK into the stage tree, and a
          # mount THROUGH a symlink lands at the link's TARGET — on
          # the share mount that shadows the whole stage device's
          # tree. The overlay needs a REAL mountpoint at the link's
          # PATH. The tmpfs at /nix SHADOWS the baked link — nothing
          # of the ro root is visible under it, no removal needed;
          # the real dir lives on the tmpfs, then the overlay mounts
          # at the link's own path. The lowerdir names the SHARE's
          # backing path directly.
          "$BB" mount -t tmpfs tmpfs /nix \
              || fail "cannot mount the tmpfs for /nix (the overlay's mountpoint root)"
          "$BB" mkdir -p /nix/store \
              || fail "cannot create the real /nix/store mountpoint"
          step "mounting the store overlay (fresh scratch upper)"
          "$BB" mount -t overlay overlay \
              -o lowerdir=/tmp/mysbx-shares/stage-ro/store,upperdir=/mysbx-nix/store-upper,workdir=/mysbx-nix/store-work \
              /nix/store \
              || fail "cannot mount the store overlay (scratch upper)"
      fi

      "$BB" mkdir -p /mysbx-nix/log /mysbx-nix/cache /mysbx-nix/tmp \
          || fail "cannot create the nix log/cache/tmp dirs on the scratch"
      # The single-user nix environment (the podman image's
      # wrapper env, the direct twin): NIX_REMOTE=local (no daemon
      # in the VM), state/log/cache/TMPDIR on the scratch. NIX_CONFIG
      # composes defaults FIRST so a config [nix] layer (the
      # manifest env) overrides them.
      export NIX_REMOTE=local
      export NIX_STATE_DIR=/mysbx-nix/state
      export NIX_LOG_DIR=/mysbx-nix/log
      export NIX_CACHE_HOME=/mysbx-nix/cache
      export TMPDIR=/mysbx-nix/tmp
      # One directive per line, built by append so no literal
      # newline ever sits at a column the indented-string
      # stripping would mangle (the shebang guard's own rule).
      nix_config_defaults="build-users-group ="
      nix_config_defaults="$nix_config_defaults
  sandbox = false"
      nix_config_defaults="$nix_config_defaults
  experimental-features = nix-command flakes"
      if [ -n "''${NIX_CONFIG:-}" ]; then
          nix_config_defaults="$nix_config_defaults
  $NIX_CONFIG"
      fi
      export NIX_CONFIG="$nix_config_defaults"
      step "the nix scratch is $dev at /mysbx-nix; the store overlay is up"
  fi

''
