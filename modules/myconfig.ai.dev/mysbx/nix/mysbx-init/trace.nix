# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the trace of last resort: the mysbx.* cmdline parser and the
# step() timer (MYSBX_VM_TRACE / mysbx.trace=1).
{ basePrefix, reconcileSelect }:
''
  # The mysbx.* cmdline params (the qemu bridge): libkrun hands the
  # guest its settings through envp; qemu has no envp — the kernel
  # cmdline carries them instead (mysbx.trace=, mysbx.manifest=,
  # mysbx.exit-channel=). Env stays first-class: every consumer
  # below falls back to _cval ONLY when its variable is unset, and
  # the krun cmdline (libkrun's own) carries no mysbx.* params.
  _cval=""
  _cmdline_mysbx() {
      # $1: the param name; sets _cval to the LAST value before
      # the `--` split (empty when absent). The payload argv
      # follows `--` and must not be parsed as a param.
      _cval=""
      for _cw in $("$BB" cat /proc/cmdline 2>/dev/null); do
          case "$_cw" in
              --) break ;;
              "mysbx.$1="*) _cval="''${_cw#*=}" ;;
          esac
      done
  }

  # The trace of last resort (live debugging, bd myconfig-2n8):
  # the guest console carries every step when the wrapper or the
  # user sets MYSBX_VM_TRACE=1 — a silent hang otherwise leaves
  # nothing to diagnose. Defined BEFORE the first step call: sh
  # runs top to bottom, a call above the definition would try an
  # external `step` and die under set -eu before the first mount
  # (the fourth live finding). Cost when off: one [ -n ] test.
  # The launcher's --verbose arms the trace too, so a verbose
  # launch narrates the boot gap: the launcher's own phase lines
  # stop at "vm setup" and resume only after guest exit — the
  # steps between are exactly what the init spends the gap on.
  # Each traced step carries the kernel uptime in [brackets] and
  # its own duration since the previous step (the first one
  # therefore measures from kernel boot — it lands late when the
  # kernel spent long booting before the init started).
  # busybox date has no %N (no sub-second clock), but
  # /proc/uptime carries the fraction; the 10# prefixes keep a
  # leading-zero fraction like ".08" from tripping ash's octal
  # parsing. A read failure (no /proc yet) reports 0 — timing is
  # best-effort, the step names are the point.
  trace="''${MYSBX_VM_TRACE:-}"
  if [ -z "$trace" ]; then
      _cmdline_mysbx trace
      if [ "$_cval" = "1" ]; then trace=1; fi
  fi
  _now_ms() {
      if read _up _ < /proc/uptime 2>/dev/null; then
          _ms=$(( (10${basePrefix}''${_up%%.*} * 100 + 10${basePrefix}''${_up#*.}) * 10 ))
      else
          _ms=0
      fi
  }
  step() {
      [ -n "$trace" ] || return 0
      _now_ms
      "$BB" echo "mysbx-init: [''${_ms}ms] $* (+$((_ms - _step_t0)) ms)"
      _step_t0=$_ms
  }
  _step_t0=0

''
