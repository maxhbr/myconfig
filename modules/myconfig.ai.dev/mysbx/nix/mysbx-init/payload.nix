# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the payload handoff: the argv guard, the chdir, the fd ceiling,
# the degradation probe, the inode watchdog, the console size and
# the exec (the exit-channel run first).
{ basePrefix, reconcileSelect }:
''
  # The payload is our own argv: the manifest's argv records
  # REPLACED $* when one exists (bd myconfig-hq7k); else the
  # kernel's `--` split handed the krun_set_exec vector over as
  # $1.. (the krun guest's own route — and the qemu debug run
  # without a manifest).
  [ "$#" -gt 0 ] || fail "no payload — neither the manifest's argv records nor the cmdline reached this init"

  # The workdir, applied AFTER the shares exist (the manifest's
  # chdir record — the cmdline route had /init.krun chdir BEFORE
  # the workspace share was mounted, silently landing at /).
  # PWD follows (bd myconfig-mr4): the workspace share is a bind
  # mount at its sandbox path, so getcwd() already reports it;
  # the export additionally pins the record's exact spelling when
  # a PARENT of the chdir is a symlink (the baked /mysbx-home).
  if [ -n "$manifest_chdir" ]; then
      step "chdir to the workspace: $manifest_chdir"
      cd "$manifest_chdir" \
          || fail "cannot chdir to the workdir $manifest_chdir"
      export PWD="$manifest_chdir"
  fi

  # The fd ceiling (bd myconfig-mnr): the payload execs as PID 1
  # of the VM with the kernel's default RLIMIT_NOFILE (soft 1024)
  # — on the bwrap backends it would inherit the host session's
  # limits, here nothing raises them, and a big-repo libgit2 walk
  # (nix flake check on the workspace share) dies with EMFILE at
  # 1024. Raise the hard ceiling first (the soft one cannot exceed
  # it), then the soft, best-effort: the payload must still run on
  # a kernel with a smaller nr_open, so a refusal is a WARNING plus
  # whatever limit stands, never a dead guest.
  # ulimit is an ash BUILTIN (not a busybox applet), so the
  # calls go to the shell itself — no $BB prefix here. As the VM's
  # root the hard raise reaches nr_open (1048576); a constrained
  # kernel keeps its own hard ceiling, and the soft then goes to
  # THAT — still a raise, never the 1024 default.
  nofile_target=1048576
  ulimit -Hn "$nofile_target" 2>/dev/null \
      || "$BB" echo "mysbx-init: WARNING: cannot raise the hard fd limit to $nofile_target (keeping $(ulimit -Hn))"
  if ulimit -n "$nofile_target" 2>/dev/null; then
      step "fd ceiling raised to $(ulimit -n)"
  elif ulimit -n "$(ulimit -Hn)" 2>/dev/null; then
      "$BB" echo "mysbx-init: WARNING: fd ceiling capped at the hard limit $(ulimit -n)"
  else
      "$BB" echo "mysbx-init: WARNING: cannot raise the soft fd limit (keeping $(ulimit -n))"
  fi

  # The store-share degradation probe (bd myconfig-mxu): the
  # payload's every shell spawn goes through bin/bash and bin/sh,
  # store symlinks valid only while the share carries their
  # targets — the live mxu failure had exactly these die mid-
  # session (spawn /bin/bash ENOENT, every command after it).
  # The init cannot fix a degraded share (the fix is a VM
  # refresh), but it can refuse to exec a payload whose shell
  # already dangles, naming the cause — a launch-time ENOENT with
  # a diagnosis beats a session that dies on its first command
  # with none. This runs AFTER the store overlay (a scratch-
  # carried bash would resolve through the upper layer too).
  # The shell's test builtin follows symlinks: -x on a link
  # whose target is gone is false (the db reconcile's own
  # property — "[ -e ] is FALSE for a dangling symlink"), so
  # this needs no readlink.
  for probe in /bin/bash /bin/sh /usr/bin/env; do
      [ -x "$probe" ] \
          || fail "the store share lost $probe — the baked symlink's target does not resolve; refresh the sandbox (the mxu degradation)"
  done

  # The fd-pin watchdog (bd myconfig-dj4): every guest inode
  # reached through a virtiofs share pins one host O_PATH fd in
  # the launcher (the FUSE lookup ref), released only on the
  # guest's FORGET — and this VM is never under memory pressure,
  # so its inode cache grows monotonically until the HOST hits
  # EMFILE (three live wedges at the raised 524288 ceiling). The
  # eviction lever is here, guest-side: past a threshold, drop
  # reclaimable dentries+inodes (drop_caches=2 keeps the page-
  # cache); the kernel then sends the FORGETs and the host fds
  # fall with them. Measured live: find /nix/store -maxdepth 2
  # pins 269k inodes, one echo 2 reclaims them all.
  # The loop rides a plain subshell fork of THIS init: it survives
  # the exec below, dies with the VM (PID 1 exit) and needs no
  # daemon, timer or extra store path. Both knobs ride the manifest
  # env route:
  # - MYSBX_VM_INODE_WATCHDOG (default 20000, 0 disables): the
  #   allowed growth above the post-drop baseline. The baseline is
  #   the inode count right after the last drop (0 before the
  #   first), so the in-use set that a drop cannot reclaim does not
  #   retrigger a drop every tick.
  # - MYSBX_VM_INODE_WATCHDOG_INTERVAL (default 1 s): the check
  #   interval. A walk pins fds between two checks, so the interval
  #   bounds the overshoot (bd myconfig-jjfy).
  # Every drop appends a line to /tmp/mysbx-inode-watchdog.log
  # (rotated to .1 past 256 KiB).
  if [ "''${MYSBX_VM_INODE_WATCHDOG:-20000}" != "0" ]; then
      step "arming the inode watchdog (growth ''${MYSBX_VM_INODE_WATCHDOG:-20000} over the post-drop baseline, interval ''${MYSBX_VM_INODE_WATCHDOG_INTERVAL:-1}s)"
      (
          _iw_limit="''${MYSBX_VM_INODE_WATCHDOG:-20000}"
          case "$_iw_limit" in
              "" | *[!0-9]*) _iw_limit=20000 ;;
          esac
          _iw_interval="''${MYSBX_VM_INODE_WATCHDOG_INTERVAL:-1}"
          case "$_iw_interval" in
              "" | *[!0-9]*) _iw_interval=1 ;;
          esac
          [ "$_iw_interval" -ge 1 ] 2>/dev/null || _iw_interval=1
          _iw_log=/tmp/mysbx-inode-watchdog.log
          _iw_base=0
          echo "armed: growth $_iw_limit, interval ''${_iw_interval}s" 2>/dev/null >> "$_iw_log" || :
          while :; do
              # inode-state is TAB-separated; read's default IFS
              # splits on any whitespace, so the first field alone
              # lands in _iw_inodes.
              read _iw_inodes _ < /proc/sys/fs/inode-state 2>/dev/null || _iw_inodes=0
              case "$_iw_inodes" in
                  "" | *[!0-9]*) _iw_inodes=0 ;;
              esac
              if [ "$_iw_inodes" -gt "$((_iw_base + _iw_limit))" ]; then
                  # `|| :` throughout: the init runs under set -e,
                  # and a failing command would end this loop.
                  "$BB" sync || :
                  echo 2 > /proc/sys/vm/drop_caches 2>/dev/null || :
                  read _iw_base _ < /proc/sys/fs/inode-state 2>/dev/null || _iw_base=0
                  case "$_iw_base" in
                      "" | *[!0-9]*) _iw_base=0 ;;
                  esac
                  read _iw_up _ < /proc/uptime 2>/dev/null || _iw_up=?
                  _iw_size=$("$BB" wc -c 2>/dev/null < "$_iw_log" || :)
                  _iw_size=''${_iw_size##* }
                  if [ "''${_iw_size:-0}" -gt 262144 ] 2>/dev/null; then
                      "$BB" mv -f "$_iw_log" "$_iw_log.1" 2>/dev/null || :
                  fi
                  echo "[''${_iw_up}s] drop: inodes $_iw_inodes -> $_iw_base, next drop above $((_iw_base + _iw_limit))" 2>/dev/null >> "$_iw_log" || :
              fi
              "$BB" sleep "$_iw_interval"
          done
      ) &
  fi

  # The console size (bd myconfig-a2uq): the qemu guest's serial
  # console is a 16550 — it carries NO winsize, so the guest ctty
  # reports a 0x0 grid and herdr refuses to attach (fish renders
  # blind). The pipeline read the CALLER's terminal size and rode
  # it through the manifest env; this stty applies it to the
  # guest console before the payload runs, so interactive payloads
  # see a real grid. Gated on the marker: the krun guest (libkrun's
  # console proxy carries a real size) and piped runs never set it.
  # The 16550 has no resize signaling either — the size is the
  # boot-time snapshot; a host resize mid-session does not
  # propagate (recorded in backends.md D9).
  _csize="''${MYSBX_MICROVM_CONSOLE_SIZE:-}"
  case "$_csize" in
      x*|*x*x|*x) ;;
      [0-9]*x[0-9]*)
          _rows=''${_csize%%x*}
          _cols=''${_csize#*x}
          case "$_rows$_cols" in
              *[!0-9]*) ;;
              *)
                  if [ "$_rows" -ge 1 ] && [ "$_cols" -ge 1 ]; then
                      "$BB" stty rows "$_rows" cols "$_cols" 2>/dev/null || :
                  fi
                  ;;
          esac
          ;;
  esac

  step "exec-ing the payload: $1"
  if [ "$exit_channel" = serial16550 ]; then
      # The cloud-hypervisor bridge (bd myconfig-9wkp.4): CH has no
      # multiport virtio-serial, so the exit code travels on the
      # 16550 the launcher wired to --serial file= (the spike's Q4:
      # one "N\n" line reaches the file). The node is fixed
      # (/dev/ttyS0, no sysfs walk, no udev), and the teardown is
      # `poweroff -f` — CH has no -no-reboot, so a PID-1 exit's
      # kernel panic would HANG the VM (the spike's Q5), and
      # panic=-1 would REBOOT and re-run the payload. The ACPI
      # poweroff makes CH exit; the launcher maps the serial line
      # to the run's exit code.
      set +e
      "$BB" setsid -c "$@"
      _exit_code=$?
      set -e
      step "the payload exited with $_exit_code"
      printf 'mysbx-exit:%s\n' "$_exit_code" > /dev/ttyS0 2>/dev/null \
          || echo "mysbx-init: cannot write the exit code to the 16550" >&2
      "$BB" poweroff -f
      # poweroff -f never returns; if it ever does, the code is
      # already on the serial and the panic that follows can only
      # hang — the launcher's teardown timeout owns that case.
      exit 0
  fi
  if [ -n "$exit_channel" ]; then
      # The exit channel (the qemu bridge): the payload runs as a
      # CHILD so this PID 1 can observe its exit code, hand it to
      # the launcher's reader (one "N\n" line on the virtio-serial
      # port), and exit — the kernel panics on PID-1 exit and
      # qemu's -no-reboot turns that panic into a clean exit.
      #
      # The port's DEVICE NODE is devtmpfs's own (the driver's
      # device_create names it vport<pdev>p<id>), but the FRIENDLY
      # /dev/virtio-ports/<name> spelling is a UDEV symlink — the
      # driver generates the udev event itself (virtio_console.c:
      # "Generate a udev event so that appropriate symlinks can be
      # created based on udev rules"), and this static-busybox
      # guest runs no udev. Resolve the port by NAME through sysfs
      # instead (bd myconfig-dnmk): walk /sys/class/virtio-ports/*,
      # match the name file, and write to the /dev/vport* node the
      # class dir names (devtmpfs already created it — a fallback
      # mknod covers a guest whose /dev is not devtmpfs). A failed
      # resolution or write falls through to the same teardown: the
      # launcher reports no code rather than hanging the VM.
      _exit_port=/dev/virtio-ports/$exit_channel
      for _port_dir in /sys/class/virtio-ports/*; do
          [ "$("$BB" cat "$_port_dir/name" 2>/dev/null)" = "$exit_channel" ] || continue
          _dev="$("$BB" cat "$_port_dir/dev" 2>/dev/null)" || continue
          _node=/dev/"$("$BB" basename "$_port_dir")"
          if [ -e "$_node" ]; then
              _exit_port=$_node
          elif "$BB" mknod "$_node" c "''${_dev%%:*}" "''${_dev#*:}" 2>/dev/null; then
              _exit_port=$_node
          fi
          break
      done
      # SYNCHRONOUS, never `cmd &`: a NON-INTERACTIVE ash gives an
      # async child /dev/null as stdin (the POSIX async-list rule —
      # live probe with this very busybox 1.37.0: the &-child read
      # EOF, the synchronous form read the piped line, bd
      # myconfig-8j3t), so a shell session or a piped-stdin payload
      # would see EOF the moment it starts. wait already blocks
      # this init until the payload exits — the synchronous form
      # reaps it just the same, keeps the console's stdin for the
      # payload, and $? carries the code.
      #
      # The CONTROLLING TERMINAL (bd myconfig-w8jd): PID 1 owns the
      # session; a plain child inherits the fds but never a ctty —
      # /dev/tty is ENXIO, job control is off, and ^C reaches no
      # foreground group. `setsid -c` execs the payload as a NEW
      # session leader WITH the console as its ctty (live-probed:
      # from ash the child is not a group leader, so setsid execs
      # in place and $? still carries the payload's code; a
      # non-tty stdin makes TIOCSCTTY fail silently and the
      # payload simply runs on, so piped one-shot runs are
      # unaffected).
      set +e
      "$BB" setsid -c "$@"
      _exit_code=$?
      set -e
      step "the payload exited with $_exit_code"
      printf '%s\n' "$_exit_code" > "$_exit_port" \
          || echo "mysbx-init: cannot write the exit code to the $exit_channel port ($_exit_port)" >&2
      exit 0
  fi
  exec "$@"
''
