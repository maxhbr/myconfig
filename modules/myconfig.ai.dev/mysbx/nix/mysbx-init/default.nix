# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The krun guest entry script (bd myconfig-dak.5) — the assembly of the
# functional parts in this directory, one file per phase of the PID-1
# boot, concatenated IN ORDER into the ONE script the guest runs:
#
#   header      shebang, set -eu, $BB, fail(), the proc/sys/dev guard
#   trace       the mysbx.* cmdline parser and the step() timer
#   pick-vd     pick_vd(), the shared vd* device identification
#   tmpfs-roots the /tmp tmpfs, the sandbox home tree, the accumulators
#   manifest    the manifest pointer, the stage-ro mount, the record
#               loader (env/share/argv/chdir), the exit-channel pick
#   shares      the nested-virt announce and the share placement loop
#   etc-network the uid files, the resolver, the SLiRP NIC
#   store       the erofs store disk, the scratch, the db reconcile,
#               the store overlay, the single-user nix env
#   payload     the argv guard, chdir, fd ceiling, degradation probe,
#               inode watchdog, console size, the exec
#
# WHY ONE SCRIPT, NOT SEPARATE GUEST SCRIPTS THAT CALL EACH OTHER: the
# phases share SHELL state a child process cannot inherit back — the
# manifest loader rewrites this PID-1 shell's own argv (set --), the
# share loop accumulates mounted_devices/tmpfs_first_components, the
# payload must be exec'd FROM PID 1 itself (setsid -c ctty, ulimit,
# chdir, the exported env). The parts are therefore SOURCE modules,
# not guest executables: each file is a function returning its
# fragment as a Nix indented string, and this assembly concatenates
# them — the baked bin/mysbx-init stays ONE script, byte-identical to
# the pre-split single string (the passthru.mysbxInit contract).
#
# Each part file: `{ }:` + the fragment at the original 4-space body
# indent, so the indented-string stripping of every part reproduces its
# original lines exactly. Parts stay in eval-time order — the file
# list below IS the assembly order; a part consuming another's state
# (mounted_devices, manifest_chdir, the step timer) must not be moved.
#
# The two values the fragments interpolate (reconcileSelect — the
# escapeShellArg'd reconcile SQL; basePrefix — the `#` of the step
# timer's `10#`, a bare `#` being a Nix comment) are passed through
# to the parts that use them (store, trace).
{
  basePrefix,
  reconcileSelect,
}:
let
  # One part = one { text }: the fragment is computed with the two
  # interpolation values in scope; parts that need none simply ignore
  # them.
  part = path: { text = import path { inherit basePrefix reconcileSelect; }; };
in
{
  text =
    (part ./header.nix).text
    + (part ./trace.nix).text
    + (part ./pick-vd.nix).text
    + (part ./tmpfs-roots.nix).text
    + (part ./manifest.nix).text
    + (part ./shares.nix).text
    + (part ./etc-network.nix).text
    + (part ./store.nix).text
    + (part ./payload.nix).text;
}
