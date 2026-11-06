# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the /etc and network surface: the uid files, the resolver, and
# the SLiRP NIC configuration.
{ basePrefix, reconcileSelect }:
''
  # The uid files (bd myconfig-v3fk, the live run's finding): the
  # baked /etc/passwd + /etc/group name the uid the payload runs
  # as — ssh-keygen (the sandbox keypair step) and every
  # uid-resolving tool refuses an unnamed uid. The /etc tmpfs the
  # share loop or the resolver step may mount hides the baked
  # files; this helper re-creates the minimal pair wherever a
  # file is missing, on whatever /etc is mounted by the time it
  # runs. Idempotent: present files are never touched (an
  # operator-provided passwd of a future feature stays).
  ensure_uid_files() {
      if [ ! -f /etc/passwd ]; then
          printf 'root:x:0:0:root:/root:/bin/sh\nnobody:x:65534:65534:nobody:/nonexistent:/bin/false\n' \
              > /etc/passwd \
              || fail "cannot write /etc/passwd (the uid files)"
      fi
      if [ ! -f /etc/group ]; then
          printf 'root:x:0:\nnobody:x:65534:\n' > /etc/group \
              || fail "cannot write /etc/group (the uid files)"
      fi
  }

  # The guest resolver (bd myconfig-dak.6, backends.md D6): TSI
  # dials from the launcher's netns — the HOST netns — so the
  # host's resolver addresses are exactly what the proxy can
  # reach. Nothing else writes /etc/resolv.conf inside the guest
  # (TSI has no net device, no DHCP); the manifest's env is the
  # transport. The share loop's tmpfs-at-/etc (when it ran) is
  # already in place; on a rootfs without /etc shares this mounts
  # the component first. After the resolver exists, only the
  # SLiRP step below may write it (the DNS rewrite, bd
  # myconfig-m0to).
  if [ -n "''${MYSBX_VM_RESOLV:-}" ]; then
      step "writing /etc/resolv.conf from the host resolver"
      # /etc is the ro root's share-root unless a share below it
      # already tmpfs'd it: probe writability, mount when ro.
      if ! "$BB" touch /etc/.mysbx-resolv-probe 2>/dev/null; then
          # Copy the baked /etc/hosts (and the uid files, bd
          # myconfig-v3fk) before the tmpfs hides the ro root's
          # /etc: the guest has no other source of the localhost
          # → 127.0.0.1 mapping, and ssh-keygen (the sandbox
          # keypair step) refuses an unnamed uid. Without the
          # mapping Node.js fetch() fails with getaddrinfo
          # ENOTFOUND localhost (curl resolves localhost itself,
          # so it masked the gap).
          etc_backup=/tmp/.mysbx-etc
          "$BB" mkdir -p "$etc_backup"
          for _f in hosts passwd group; do
              "$BB" cp /etc/$_f "$etc_backup/$_f" 2>/dev/null || "$BB" rm -f "$etc_backup/$_f"
          done
          "$BB" mount -t tmpfs tmpfs /etc \
              || fail "cannot mount the tmpfs for /etc (the resolver)"
          for _f in hosts passwd group; do
              [ -f "$etc_backup/$_f" ] && "$BB" cp "$etc_backup/$_f" /etc/$_f || true
          done
          "$BB" rm -rf "$etc_backup"
      else
          "$BB" rm -f /etc/.mysbx-resolv-probe
      fi
      # Ensure /etc/hosts exists on the (possibly tmpfs-backed)
      # /etc: the share loop may have mounted a tmpfs over /etc
      # before this code ran, hiding the baked file. The guest
      # has no other source of the localhost → 127.0.0.1 mapping.
      if [ ! -f /etc/hosts ]; then
          printf '127.0.0.1 localhost\n::1 localhost\n' > /etc/hosts \
              || fail "cannot write /etc/hosts"
      fi
      # The uid files (bd myconfig-v3fk), the same ensure rule as
      # hosts: a tmpfs the share loop mounted BEFORE the resolver
      # hides the baked passwd/group too — re-create the minimal
      # pair when either is missing (the payload's ssh-keygen
      # refuses an unnamed uid).
      ensure_uid_files
      # The env record was ALREADY decoded by the loader (every
      # manifest env line exports plain text) — write verbatim.
      printf '%s\n' "$MYSBX_VM_RESOLV" > /etc/resolv.conf \
          || fail "cannot write /etc/resolv.conf"
  else
      # No resolver to write, but /etc/hosts may still be missing
      # if the share loop mounted a tmpfs over /etc.
      if [ ! -f /etc/hosts ]; then
          printf '127.0.0.1 localhost\n::1 localhost\n' > /etc/hosts 2>/dev/null || true
      fi
      # And the uid files share the hosts' fate — same tmpfs,
      # same ensure rule (bd myconfig-v3fk).
      ensure_uid_files
  fi

  # The SLiRP NIC of the qemu backend (bd myconfig-k106, the qemu
  # bridge): libkrun's network is TSI (the guest has NO ethernet
  # device — the resolver step above is the whole story there),
  # but qemu's `-netdev user` attaches a REAL virtio-net-pci NIC
  # that nothing configures guest-side: without an address and a
  # route, resolv.conf alone resolves nothing. The marker env is
  # set ONLY by the microvm-qemu builder when the network is
  # shared (the krun backend sets none — no step runs, no eth0
  # exists to configure). qemu's user netdev carries FIXED SLiRP
  # defaults — guest 10.0.2.15/24, gateway 10.0.2.2, DNS proxy
  # 10.0.2.3 — so the config is static, no DHCP. The interface
  # is found by DRIVER (the first virtio_net device), never by a
  # name the kernel's probe order could change. The host's own
  # resolver addresses are host-local (e.g. systemd-resolved's
  # 127.0.0.53): behind the SLiRP NAT they point into the GUEST,
  # where nothing listens — DNS would be dead (bd
  # myconfig-m0to). SLiRP's DNS proxy is the .3 address, NOT the
  # .2 gateway; the resolver written above is therefore REWRITTEN
  # to the proxy (the krun arm never runs this — TSI dials from
  # the HOST netns, where the host's addresses are exactly right).
  if [ "''${MYSBX_MICROVM_NET:-}" = "1" ]; then
      _nic=""
      for _if in /sys/class/net/*; do
          # -e, never -f: a sysfs driver bind is a symlink to a
          # DIRECTORY — [ -f ] follows the link and is false for
          # it (the live-shaped fixture caught this).
          [ -e "$_if/device/driver" ] || continue
          case "$(  "$BB" readlink "$_if/device/driver")" in
              *virtio_net) _nic="$(  "$BB" basename "$_if")" ;;
          esac
          [ -n "$_nic" ] && break
      done
      if [ -n "$_nic" ]; then
          step "configuring the SLiRP NIC $_nic (10.0.2.15/24 via 10.0.2.2)"
          "$BB" ip addr add 10.0.2.15/24 dev "$_nic" \
              || fail "cannot address the NIC $_nic"
          "$BB" ip link set "$_nic" up \
              || fail "cannot bring the NIC $_nic up"
          "$BB" ip route add default via 10.0.2.2 \
              || fail "cannot add the default route via 10.0.2.2"
          # The SLiRP DNS proxy — the host's resolver addresses
          # are unreachable from behind the NAT (bd
          # myconfig-m0to). A fresh tmpfs (no /etc shares) keeps
          # the baked /etc/hosts; a tmpfs'd /etc carries it —
          # the localhost mapping must survive the rewrite. The
          # uid files ride the same carry (bd myconfig-v3fk): the
          # resolver step's ensure already placed them, its
          # tmpfs hid nothing new — this re-ensure is for the
          # share-loop tmpfs the earlier step could not see.
          if ! "$BB" touch /etc/.mysbx-resolv-probe 2>/dev/null; then
              etc_backup=/tmp/.mysbx-etc-slrp
              "$BB" mkdir -p "$etc_backup"
              for _f in hosts passwd group; do
                  "$BB" cp /etc/$_f "$etc_backup/$_f" 2>/dev/null || "$BB" rm -f "$etc_backup/$_f"
              done
              "$BB" mount -t tmpfs tmpfs /etc \
                  || fail "cannot mount the tmpfs for /etc (the SLiRP resolver)"
              for _f in hosts passwd group; do
                  [ -f "$etc_backup/$_f" ] && "$BB" cp "$etc_backup/$_f" /etc/$_f || true
              done
              "$BB" rm -rf "$etc_backup"
          else
              "$BB" rm -f /etc/.mysbx-resolv-probe
          fi
          ensure_uid_files
          [ -f /etc/hosts ] || "$BB" printf '%s\n' \
              '127.0.0.1 localhost' > /etc/hosts
          "$BB" printf 'nameserver 10.0.2.3\n' > /etc/resolv.conf \
              || fail "cannot write the SLiRP resolver"
      else
          fail "MYSBX_MICROVM_NET=1 but no virtio_net interface exists — the NIC did not attach"
      fi
  fi

''
