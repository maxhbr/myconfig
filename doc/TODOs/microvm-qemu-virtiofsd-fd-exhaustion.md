# microvm-qemu: host virtiofsd fd ceiling wedges the guest store

Observed live on 2026-10-26, from inside a `microvm-qemu` mysbx sandbox
(the agent session doing the backend opt-in work, bd myconfig-dq2w). A
bead was staged for filing from a healthy session
(`bd create --graph /tmp/bead-staging/microvm-qemu-virtiofsd-fd-exhaustion.json`).

## Symptom

A closure-heavy `nix build` inside the sandbox (mysbx wrapper with
`podman.enable + krun.enable + microvm-qemu.enable` — pulls the gvisor,
microvm kernel, agent-image and libkrun closures) evaluated fine, then
wedged the sandbox:

- every store operation fails with `Too many open files in system`
  (ENFILE): `ls /nix/store`, `nix` itself (`error while loading shared
  libraries: libgc.so.1`), even `bd` (`libicuuc.so.78`);
- `/proc/sys/fs/file-nr` shows ~64–128 open files guest-wide with 3.27M
  free — the guest kernel is nowhere near its limits;
- process NOFILE limits are 1048576 soft/hard — not the limit;
- `/tmp/mysbx-inode-watchdog.log` reads `armed: growth 20000, interval
  1s` — armed, but cannot help (see diagnosis);
- the wedge persists >30 minutes; not disk-full (root device 96%).

## Diagnosis

The fd holder is the **host-side rootless virtiofsd** (one per device),
not anything inside the guest. virtiofsd keeps one fd per guest-visited
inode; the failed build visited ~2.7M inodes
(`/proc/sys/fs/inode-state` rose to 2754402). virtiofsd hits *its* fd
ceiling server-side and the failure surfaces guest-side as ENFILE on
store reads. The fds are not reclaimed on the >30 min timescale — the
ceiling acts as a hard run limit for closure-heavy payloads (nix builds
over the shared store).

This is the microvm-qemu twin of the krun fd accounting problem
(bd myconfig-jjfy guest-side, bd myconfig-wm28 host-side bound fds), but
the owner of the fds is the standalone virtiofsd that
`nix/microvm-qemu-launcher.nix` spawns — NOT libkrun, NOT the guest. The
krun guest watchdog (`nix/krun-rootfs.nix`) is structurally unable to
help here.

## Relevant code

- `modules/myconfig.ai.dev/mysbx/nix/microvm-qemu-launcher.nix` —
  virtiofsd spawn and its limits
- `modules/myconfig.ai.dev/mysbx/microvm-qemu-rs/src/main.rs` —
  device/daemon lifecycle
- `modules/myconfig.ai.dev/mysbx/docs/microvm-qemu-validation-log.md`
- krun twin: `nix/krun-rootfs.nix` watchdog; bd myconfig-jjfy,
  bd myconfig-wm28

## What to do

1. Reproduce on the host: closure-heavy `nix build` inside a
   microvm-qemu sandbox; watch the host virtiofsd fd count
   (`/proc/<virtiofsd-pid>/fd`).
2. Measure the launcher's RLIMIT_NOFILE for virtiofsd and the failure
   mode at exhaustion (ENFILE passthrough, hang, daemon death).
3. Mitigations to evaluate, mirroring the krun side (jjfy):
   - raise the virtiofsd NOFILE ceiling in the launcher wrapper (same
     shape as `krun.direct.userNofileLimit`);
   - check virtiofsd for an inode/announce cache eviction mode that
     closes fds for cold inodes;
   - doctor (microvm-qemu) could warn when the virtiofsd fd count is
     within N% of its ceiling.
4. Record whether the wedge ever self-recovers (the >30 min observation
   suggests pinned-open fds, no LRU reclaim).
