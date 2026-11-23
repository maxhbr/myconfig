# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the PID-1 mount bootstrap: shebang, set -eu, $BB, fail(),
# the proc/sys/dev/devpts guard and the console printk quieting.
{ basePrefix, reconcileSelect }:
''
  #!/bin/busybox sh
  set -eu

  # No PATH in the guest env (the launcher passes an explicit envp
  # only) and nothing else is mounted — every applet is invoked
  # through the static busybox at $BB, never by bare name.
  BB=/bin/busybox

  fail() {
      echo "mysbx-init: $*" >&2
      exit 125
  }

  # PID-1 boot (the qemu bridge, backends.md D9): the krun guest
  # runs after libkrun's implicit init mounted proc/sys/dev +
  # devpts + /dev/shm + the /dev/fd link; the qemu guest runs this
  # script AS PID 1 (the kernel's init=), so the mounts the init
  # needs are not up yet — except /dev: the pinned nixpkgs kernel
  # has CONFIG_DEVTMPFS_MOUNT=y and automounts devtmpfs on /dev
  # right after mounting root, so the init's own devtmpfs mount
  # would hit EBUSY. A mounted devtmpfs IS the goal — the
  # /proc/mounts probe accepts it and moves on. The /proc/uptime
  # probe keeps the krun path unchanged:
  # mounted pseudo-filesystems skip every mount. The devpts mount
  # is what makes PTYs exist at all (tmux/mux sessions allocate
  # them in the guest — the supported session path); /dev/shm is
  # POSIX shared memory; /dev/fd is the standard fd redirection
  # link; lo up makes 127.0.0.1 bindable (the baked /etc/hosts
  # localhost mapping is dead weight without it) (bd
  # myconfig-af0j).
  if [ ! -f /proc/uptime ]; then
      "$BB" mount -t proc proc /proc \
          || fail "cannot mount proc as pid 1"
      "$BB" mount -t sysfs sysfs /sys \
          || fail "cannot mount sysfs as pid 1"
      "$BB" grep -q ' /dev devtmpfs ' /proc/mounts \
          || "$BB" mount -t devtmpfs devtmpfs /dev \
          || fail "cannot mount devtmpfs as pid 1"
      "$BB" mkdir -p /dev/pts /dev/shm
      "$BB" mount -t devpts devpts /dev/pts \
          || fail "cannot mount devpts as pid 1"
      "$BB" mount -t tmpfs tmpfs /dev/shm \
          || fail "cannot mount /dev/shm as pid 1"
      "$BB" ln -s /proc/self/fd /dev/fd \
          || fail "cannot link /dev/fd as pid 1"
      "$BB" ip link set lo up \
          || fail "cannot bring up lo as pid 1"
  fi

  # Quiet the serial console below KERN_WARNING: under a loaded
  # 1-2 vCPU guest the clocksource watchdog's timer softirq misses
  # its window ("Long readout interval, skipping watchdog check")
  # and the pr_warn lands on the caller's stdio over console=ttyS0.
  # Errors (loglevel <=3) still print; dmesg keeps everything.
  # Best-effort: /proc/sys exists on both backends, but a read-only
  # or absent procfs must not fail the boot.
  echo 3 > /proc/sys/kernel/printk 2> /dev/null || true

''
