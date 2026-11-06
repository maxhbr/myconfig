# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# mysbx-init part — the manifest route: the cmdline pointer, the stage-ro mount, the
# env/share/argv/chdir record loader, the exit-channel pick.
{ basePrefix, reconcileSelect }:
''
  # The MANIFEST route (the cmdline budget finding, bd
  # myconfig-2n8's seventh): the launcher wrote env/share/chdir
  # records into the ro stage device; the cmdline carries only the
  # pointer. Mount stage-ro NOW — every share's link target
  # resolves through it — read the file, and apply the env lines
  # at once (the init's OWN steps use none of them; the payload
  # inherits everything through exec).
  manifest="''${MYSBX_VM_MANIFEST:-}"
  if [ -z "$manifest" ]; then
      _cmdline_mysbx manifest
      manifest=$_cval
  fi
  # The exit channel (the qemu bridge, backends.md D9): the
  # virtio-serial port the payload's exit code travels back on.
  # Cmdline-only — the krun guest has no channel and never sets
  # one.
  exit_channel=""
  _cmdline_mysbx exit-channel
  exit_channel=$_cval
  if [ -n "$manifest" ]; then
      mtag=''${manifest%%':'*}
      mslot=''${manifest#*':'}
      step "reading the manifest (device $mtag, slot $mslot)"
      mpoint=/tmp/mysbx-shares/$mtag
      "$BB" mkdir -p "$mpoint" \
          || fail "cannot create the manifest mountpoint $mpoint"
      "$BB" mount -t virtiofs -o ro "$mtag" "$mpoint" \
          || fail "cannot mount the manifest device $mtag"
      mounted_devices=" $mounted_devices $mtag "
      mpath=$mpoint/$mslot
      [ -f "$mpath" ] || fail "the manifest $mpath does not exist"
      # The payload argv records (bd myconfig-hq7k, the qemu
      # bridge): the stock kernel's `--` cmdline split is LOSSY
      # (whitespace-split, `=`-tokens to init's envp,
      # MAX_INIT_ARGS), so a manifest-bearing run carries the
      # payload here instead — one `argv <index> <base64>` record
      # per token, indices ascending. Each index is validated
      # against the running count (a truncated or reordered
      # manifest is a diagnosis, never a mangled payload); the
      # decoded tokens REPLACE this init's own $* on the first
      # record and append after (set -- keeps each token whole —
      # no re-splitting, an EMPTY token survives). The krun
      # guest rides envp, never sets a manifest, and its $* is
      # untouched.
      _argv_n=0
      # One record per line, tab-separated fields, base64 values:
      # a base64 value never contains a tab, the field split is
      # unambiguous.
      while IFS="	" read -r rkind r2 r3 r4 r5; do
          [ -n "$rkind" ] || continue
          case "$rkind" in
              env)
                  # r2 KEY, r3 base64(value). The sentinel x:
                  # $( ) strips ALL trailing newlines from the
                  # substitution (POSIX), so a value ENDING in a
                  # newline would decode short (bd
                  # myconfig-m6r1) — the sentinel makes the strip
                  # exact: only the x itself goes (the && keeps
                  # the || fail refusal semantics on a decode
                  # error).
                  val=$(printf '%s' "$r3" | "$BB" base64 -d && printf x) \
                      || fail "cannot decode the manifest env $r2"
                  val=''${val%x}
                  export "$r2=$val"
                  ;;
              share)
                  # r2 DEVICE, r3 SLOT, r4 DEST, r5 ro|rw
                  [ -n "$r5" ] || fail "manifest share record $r2:$r3 is short a mode"
                  share_records="''${share_records:-}
  $r2 $r3 $r4 $r5"
                  ;;
              argv)
                  # r2 index, r3 base64(token) — one record per
                  # payload token, the index validated against
                  # the running count (bd myconfig-hq7k). The
                  # first record DISCARDS the cmdline-passed $*
                  # (a manifest-bearing qemu run never puts the
                  # payload on the cmdline; anything the kernel
                  # split behind `--` is not the payload), the
                  # rest append (set -- keeps each token whole —
                  # no re-splitting, an EMPTY token survives
                  # losslessly).
                  [ "$r2" -eq "$_argv_n" ] \
                      || fail "manifest argv index $r2 is out of order (expected $_argv_n)"
                  # The same sentinel as the env record (bd
                  # myconfig-m6r1): a token ENDING in a newline
                  # must arrive byte-identical — $( ) alone
                  # strips it.
                  val=$(printf '%s' "$r3" | "$BB" base64 -d && printf x) \
                      || fail "cannot decode the manifest argv $_argv_n"
                  val=''${val%x}
                  if [ "$_argv_n" -eq 0 ]; then
                      set --
                  fi
                  set -- "$@" "$val"
                  _argv_n=$((_argv_n + 1))
                  ;;
              chdir)
                  # r2 base64(dir) — applied AFTER the shares exist
                  # (the same sentinel decode, bd myconfig-m6r1)
                  manifest_chdir=$(printf '%s' "$r2" | "$BB" base64 -d && printf x) \
                      || fail "cannot decode the manifest chdir"
                  manifest_chdir=''${manifest_chdir%x}
                  ;;
              *)
                  fail "unknown manifest record: $rkind"
                  ;;
          esac
      done < "$mpath"
  fi

''
