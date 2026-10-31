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
   (`/proc/<virtiofsd-pid>/fd`). **Live-host work, still open.**
2. Measure the launcher's RLIMIT_NOFILE for virtiofsd and the failure
   mode at exhaustion (ENFILE passthrough, hang, daemon death).
   **The measurement half is done (2026-10-31, see Triage): the
   launcher set NO limit — virtiofsd inherited the session's
   (soft 1024 under a systemd user session). The failure-mode
   half needs the live repro of (1).**
3. Mitigations to evaluate, mirroring the krun side (jjfy):
   - raise the virtiofsd NOFILE ceiling in the launcher wrapper (same
     shape as `krun.direct.userNofileLimit`);
     **DONE (2026-10-31, see Triage): launcher-side
     `raise_nofile`/`ensure_nofile` + `microvm-qemu.userNofileLimit`**;
   - check virtiofsd for an inode/announce cache eviction mode that
     closes fds for cold inodes;
     **DONE as a NEGATIVE result (2026-10-31, see Triage): no such
     mode exists in virtiofsd 1.14.0**;
   - doctor (microvm-qemu) could warn when the virtiofsd fd count is
     within N% of its ceiling. **Future work — needs live
     fd-count data from (1) to pick N and the read path.**
4. Record whether the wedge ever self-recovers (the >30 min observation
   suggests pinned-open fds, no LRU reclaim).
   **Live-host work, still open; the upstream code reading (see
   Triage) supports the no-self-recovery reading: fds drop only on
   guest FUSE FORGET.**

## Triage 2026-10-31

Reconciled with bead myconfig-qq3i (p1, open): its description is
this note verbatim — no drift. Cross-checked the krun twins
(myconfig-jjfy closed, myconfig-wm28 open).

What was verified and implemented (this worktree, commit "mysbx/
microvm-qemu: raise the virtiofsd fd ceiling (krun
userNofileLimit twin)"):

- **Item 2 (measure), by code inspection**: the launcher
  (`microvm-qemu-rs/src/main.rs`) set NO RLIMIT_NOFILE for itself
  or the virtiofsd children — they inherit the invoking
  session's ceiling (soft 1024 under a systemd user session, the
  exact wedge of the 2026-10-26 observation).
- **Mitigation bullet 1 implemented**, the krun fix's shape:
  - `raise_nofile()` + `ensure_nofile()` in the launcher (raw
    setrlimit(2), zero-dep, the `krun-rs` twin): hard to
    RLIM_INFINITY, soft to the hard, best-effort; a sub-65536
    ceiling is a diagnosable refusal (EXIT_SETUP via a plain
    `return` in `run()` — the single-exit invariant of bd
    myconfig-1ct7), because the virtiofsd children inherit the
    launcher's limits.
  - `myconfig.ai.dev.mysbx.microvm-qemu.userNofileLimit`
    (default 8388608, `null` = systemd default), and the systemd
    user-manager `LimitNOFILE` wiring now also covers this
    backend (max of the two backends' ceilings when both are
    enabled).
  - Two new launcher unit tests (the krun suite's twins); 22/22
    green. Eval-verified on test-thing/test-f13 snapshots: only
    the launcher outPath changed; a minimal microvm-qemu-only
    eval sets `DefaultLimitNOFILE = "1024:8388608"` and leaves
    it unset when the backend is off or the option is null.
- **Mitigation bullet 2 checked upstream, negative**: virtiofsd
  1.14.0 (the pinned nixpkgs build) has NO inode-fd
  eviction/LRU mode: the passthrough inode store keeps one
  `O_PATH` fd per guest-visited inode and releases it only on
  guest FUSE FORGET (`inode_store.rs` `forget_one`/
  `drop_unlocked`). `--inode-file-handles` needs
  `CAP_DAC_READ_SEARCH` — the rootless launcher does not have
  it, and `prefer` falls back to `O_PATH` fds anyway. So the
  NOFILE raise is the only available mitigation and the ceiling
  stays a hard run limit; the structural fix remains the
  wm28-style bounded-fd work, if ever brought to this backend.

Still needing a live host (f13 or thing):

- (1) the repro and the host-side fd-count measurement;
- the failure mode at exhaustion (ENFILE passthrough vs hang vs
  daemon death) under the RAISED ceiling;
- whether the wedge self-recovers (expected no, per the upstream
  reading above);
- the doctor warning (bullet 3) once fd-count data exists.

Next check: after the next live validation pass on a
microvm-qemu host (see
`modules/myconfig.ai.dev/mysbx/docs/microvm-qemu-validation-log.md`).
