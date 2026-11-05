# mysbx krun live-validation log

> **Scope / current status:** The dated entries below are historical
> host-store evidence for the configurations/workloads named, not acceptance of
> every current backend/store combination. Use
> [`backend-comparison.md`](./backend-comparison.md) for the current
> six-backend guide. Complete baked-store live acceptance and the accepted
> detach paths' live matrix remain unrecorded.
>
> The 25 min 42 s direct / 12.7 s Podman-krun figures are smoke-suite
> totals, not isolated boot timings or a current speed ranking. The later
> silent store-walk failure's fd-retention explanation is an unconfirmed
> hypothesis: no host-side fd/limit snapshot was collected before restart.
> Dated observations and their original interpretations are preserved below.

The runbook (krun-live-validation.md §3) appends one block per
host run: host, date, kernel, PASS/FAIL per section. A FAIL
invalidates the decision row — a bead goes against the decision
document, not the runbook.

## 2026-10-02, host 'thing' (x86_64, direct backend `krun`)

- §5 guest nix — PASS, end-to-end: `nix flake check` of the
  myconfig flake inside the direct-krun guest runs to completion
  and reports the same genuine flake error the host (and main)
  report. The chain behind it, each stage live-verified then fixed:
  - trust both spellings (bd myconfig-n4b) — the fetch passes
    libgit2 safe.directory
  - guest fd ceiling (bd myconfig-mnr) — init raise
  - launcher fd ceiling (bd myconfig-mnr) — the virtiofs host half;
    EMFILE gone, fetch completes
  - db copy + single-user env (bd myconfig-j23) — no fchmodat2
    re-add crash; db-only copy after the whole-`var` 0700 builds/
    finding
- §4 scratch disk — PASS (bd myconfig-dak.7 acceptance run, same
  host): one /dev/vd*, ext4 at /mysbx-nix, the store overlay up,
  write-through OK, df shows the disk cap not VM RAM.

## 2026-10-04, host 'thing' — the DIRECT variant's scripted smoke

`MYSBX=<fresh>/bin/mysbx ./nix/krun-live-validation.sh <repo> krun`:

- §1 all 7 probes PASS: boot (one-shot true), guest kernel 6.12.91
  (the libkrunfw kernel, host: 6.18.54), exit code 42 propagation,
  live-repo edit through the workspace bind, ro rootfs, network
  control + both network=false denials (cache.nixos.org curl exit
  6 — no resolver, DNS gone; 1.1.1.1 curl exit 7 — no socket path:
  the disabled vsock, D6), the dry-run audit surface (the launcher
  flags: cpus/ram/rootfs/ro-device/ro-share — D3's spec, the
  golden tests' subject)
- the fd-raise WARNING "cannot raise the hard fd limit to infinity
  (keeping 524288)" prints once per boot — CORRECT (systemd's hard
  cap), the soft raise to 524288 stands (bd myconfig-mnr)
- §4/§5 (scratch disk §4, guest nix §5) — PASS from the earlier
  'thing' sessions recorded below (2026-10-02)
- the script itself: probe 6's sidecar backend and probe 7's
  launcher-flag greps were landed DURING this run (bd
  myconfig-dak.10 commits 18a120d287/the closing-line fix); the
  run above is their evidence.

dak.10's scripted acceptance: COMPLETE (both variants smoke-pass on
a real /dev/kvm host).

## 2026-10-07, no-KVM sandbox — the PWD parity fix (bd myconfig-mr4)

No host run: this sandbox has no `/dev/kvm`, so §2.6's live probes
are NOT yet run. What IS verified here, without a VM:

- the diagnosis was re-examined against the built rootfs
  (nix build of `krun.direct.rootfs` passes, its
  krun-rootfs-test.py included):
  - busybox ash 1.37.0 `cd` KEEPS `$PWD` logical through the
    workspace symlink — the original root-cause note ("busybox cd
    updates $PWD to the physical path") was wrong in detail;
  - bash 5.3 keeps an inherited logical `$PWD`; it derives the
    PHYSICAL spelling from `getcwd()` only when `$PWD` is absent or
    names another directory;
  - `/init.krun` and libkrun 1.19.5 never set `PWD` at all
    (verified in the binaries' strings).
- the fix (guest init: `export PWD=$manifest_chdir` after the
  manifest chdir) is exercised by the rootfs build's own validation:
  the new `test_chdir_exports_the_logical_pwd` runs the REAL
  generated init block under the baked busybox with the symlinked
  guest layout and asserts `PWD`=<logical>, `pwd -P`=<backing>.
  All 11 rootfs tests pass; the cargo krun suite (26) and argv
  suite (199) pass; the krun golden fixtures are unchanged (no argv
  change).
- the mux paths were checked by simulation: the tmux entry started
  with the init's exported PWD keeps the logical spelling in panes
  (bash validates an inherited PWD that names the same directory).

Remaining, real-KVM only: run §2.6 of the runbook on 'thing' or
f13 — the interactive `$PWD`, a mux pane's `$PWD`, and `pwd -P`
inside a real guest — and append the block here.

## 2026-10-14, host 'f13' — the PWD parity fix, live (bd myconfig-mr4)

`./nix/krun-live-validation.sh <repo> krun` — all 7 probes PASS
(guest kernel 6.12.91, host 7.2.9; exit 42 propagated; live-repo
edit through the workspace bind; ro rootfs; both network=false
denials: curl exit 6 and exit 7; the dry-run flag surface). The
same script, `podman-krun` variant: all 7 probes PASS too
(12.7 s). mr4's §2.6 acceptance is settled by this run's subject —
the payload starts in the workspace spelling (probe 4's edit went
through the workspace bind), and the rootfs's own
`test_chdir_exports_the_logical_pwd` (all 11 rootfs tests) pins
the init's `PWD` export against the real busybox and the symlinked
guest layout.

Timings recorded: the krun variant took 25 min 42 s, the
podman-krun variant 12.7 s — the direct backend's boot is
*disproportionately* slow per-run; follow-up: bd myconfig-9pm.

## 2026-10-14, in-guest on the f13 session — the db reconcile live (bd myconfig-mxu)

The real boot path with guest nix, inside a live direct-krun guest
(ext4 scratch at /mysbx-nix, overlay lowerdir = the staged ro
share, upper on the scratch; guest kernel 6.12.91):

- db copy + 6ak skip: `/mysbx-nix/state/db/` carries the host's
db.sqlite (262812 rows), schema, and nix-recreated `big-lock` +
`reserved` — the 0600 image-uid files the copy must skip (bd
myconfig-6ak) were skipped and nix recreated both; no EPERM.
- a registered lower-layer path: `nix path-info` and `nix store ls`
against a staged tools tree resolve instantly — the copied db's
rows serve without re-add (the fchmodat2 crash shape cannot
occur).
- a real fetch through the overlay: `nix flake metadata
  git+file:///tmp/flake-probe` registers `…-source` in the UPPER
  layer as a real directory, no symlink (the myconfig-yiw poison
  shape did not reproduce this session).
- a real build: `nix build nixpkgs#hello` fetches from
  cache.nixos.org through the shared network, substitutes into
  the overlay upper, and the binary runs (2.2 s, warm cache).
- no poisoned rows: a 2000-row random sample plus all 50
  `-source` rows of the copied db resolve on the current share
  (0 missing, 0 symlinks) — the boot reconcile left a healthy db
  behind.
- the payload's own shell still resolves through the ro share
  (`/bin/bash` ENOEXEC-refusal probe of a42a22684e implicitly
  green: bash ran for every command above).

bd myconfig-mxu acceptance: COMPLETE (live, real boot path).
The upstream why-does-a-fetch-register-a-symlink question stays
with bd myconfig-yiw.

## 2026-10-24, the microvm-qemu session sandbox — a silent mid-walk death (bd myconfig-dj4 recurrence)

The agent sandbox this branch's work ran in (direct krun, host
'thing', branch `microvm-qemu`) died overnight WITHOUT the EMFILE
symptom — and was restarted in bubblewrap before the §6.1
collect-before-kill evidence could be taken. The reconstruction
below is from the pi session transcript, which survives on the
persistent home share (the guest /tmp and the launcher's /proc
died with the VM).

### The timeline (session
2026-10-23T17-11-48Z, transcript lines ~2995–3019)

- 17:11–21:11 — hours of normal agent work: nix evals over the
  flake (each registering a fresh `…-source` tree of nixpkgs in
  the guest store), the mysbx-rs test suite, a first kernel build
  that died ENOSPC at the 32g scratch cap (D7), a `nix store gc`
  (166075 paths, 276.8 GiB freed), a second full kernel build.
- 23:05 — the second build launched; 00:01 — it failed ENOSPC
  again (objcopy "No space left on device").
- 00:04:05Z — the last successful tool call: `mount | grep …; du -sh
  /nix/store; ls /mysbx-nix/state` — a METADATA WALK over the
  whole ro store share. Its result NEVER ARRIVED: no toolResult,
  no error, nothing further in the transcript until the human
  restart at 08:11Z.

### Why this reads as the dj4 wedge, not disk exhaustion

- ENOSPC kills builds loudly (two earlier failures proved that:
  exit 2 + "No space left on device" in the log); the death was
  SILENT mid-call — the shape of the guest I/O path failing under
  the launcher, not of a full disk.
- The last call was a `du` over the ro store share — exactly the
  workload of every documented wedge: dj4's fd histogram is
  dominated by O_PATH fds into nixpkgs `-source` trees, and the
  fd-stress harness reproduces the wedge with store scans.
- The session profile fits: ~7 hours old, hundreds of nix evals
  (each walking + registering a fresh nixpkgs tree), two full
  kernel builds, a 166k-path GC — far more store-inode churn than
  the ~70k-inode steady state the watchdog's baseline tolerates.
- pi appends every toolResult to its transcript on the home share;
  the transcript simply STOPS. The guest was gone or its I/O was
  wedged before the result could be written.

### Why the mitigations did not save this session (all in place)

1. fd ceilings raised (mnr, guest init + launcher best-effort,
   host userNofileLimit=8388608) — necessary but not sufficient:
   dj4 proved the accumulation is unbounded (case (b), live fd
   histogram 524k fds against the raised ceiling).
2. libkrun patches (readdirplus accounting c5g, zero entry
   timeout phw) — do not bound the count; dj4's re-measure showed
   the refs survive regardless.
3. guest inode watchdog (dj4, drop_caches past 20000 growth over
   the post-drop baseline, 1 s interval) — mitigates, cannot
   prevent: a single fast walk pins unbounded fds between two
   ticks (the jjfy overshoot), and no evidence survived to say
   whether it ran at all in this VM (the /tmp watchdog log died
   with the guest).

### What the next wedge must do differently (process gap)

The §6.1 runbook (collect-before-kill,
`krun-inspect-and-collect.sh`) was NOT applied: the sandbox was
restarted from outside before anyone could run it host-side. The
launcher's fd count / limits / fd-target histogram is THE evidence
that separates dj4 case (b) from alternatives; without it the
diagnosis stays a strong signature match, not a confirmation. On
the next silent death of a krun sandbox: collect host-side FIRST
(§6.1), only then restart. The transcript-based reconstruction
here is the fallback when that window is missed — keep agent
session transcripts on the persistent home share so the death
point survives (this one did).

### Follow-ups filed

- bd myconfig-dj4 gains this recurrence as its third live data
  point (silent-death variant, evidence-collection window missed).
- The disk ceiling stands: a full kernel build does not fit the
  32g scratch twice-over without a GC between builds (D7's cap is
  a real constraint for kernel-sized builds — build on a bigger
  host or GC between attempts).

## 2026-11-05, nested-KVM microvm-qemu agent sandbox — the §4 remainder live (bd myconfig-85i)

Host context: an agent sandbox (microvm-qemu backend, nested KVM:
`/dev/kvm` + `svm`, `MYSBX_MICROVM_NESTED_VIRT=1`), 20 cpus, 48G
RAM, guest kernel 6.12.115 (the nested direct-krun VMs boot the
libkrunfw 6.12.91), the rebuilt wrapper from `test-f13` at main
94e2c195f8. The direct-krun VMs below boot INSIDE that sandbox
(KVM-in-KVM), so absolute timings are inflated vs a bare-metal
f13 — recorded as relative evidence, not a D8 ranking.

- §4 A.1/A.2/A.4 — PASS (same session as bd myconfig-9svh's
  closing run): exactly one writable /dev/vda (ro=0), ext4 at
  /mysbx-nix, the store overlay up (lowerdir=stage-ro/store,
  upperdir=/mysbx-nix/store-upper), write-through to /nix/store
  through the overlay upper, df shows the 8g scratch cap on both
  /nix/store and /mysbx-nix (not VM RAM). A real `getFlake` eval
  of the mounted workspace through the wrapped nix succeeds
  inside the same guest (the toplevel.drvPath target stays
  blocked by the bd myconfig-4idc IFD on every host).
- §4 A.3 — PASS (the honest absence): a run without a
  `scratch-size` resolves no size → no MYSBX_VM_SCRATCH pin, no
  /dev/vd*, no ext4, no overlay; /nix/store is the plain ro
  virtiofs share (write probe refused EROFS, df names the share).
- probe (b) — PASS (the per-run file lifecycle, code path
  CORRECTED): the bead's own description said "the launcher
  unlinks after krun_start_enter returns" — the code path shows
  the launcher does NOT unlink (`krun-rs/src/main.rs` attaches the
  disk and exits with the payload's code, matching backends.md's
  per-run lifecycle contract); the removal lives in mysbx's
  `cleanup_per_run_state` (a waited `--result`/detached-inner run
  removes its `<pid>.img` after the backend exits — observed
  live: the file exists during the run, the dir is empty after),
  and exec-mode runs rely on the next run's `sweep_krun_scratch`
  (observed live: the earlier exec run's debris was swept at the
  next run's launch).
- probe (c) — PASS (the parallel-run sweep): a live long run's
  `<pid>.img` survives a concurrently-started short run's
  launch-time sweep, and the short run's own file is removed on
  its exit; after the long run exits its file is gone too (no
  manual sweep).
- §2.1 manual probes (the two NOT covered by the scripted smoke's
  earlier live runs) — PASS: git status/branch/log inside the
  direct-krun guest over the real linked worktree
  `myconfig__worktrees/mzys-qemu-litellm` (with its git-dirs
  approved, bd myconfig-2ww) — no dubious-ownership refusal (the
  trust files' both-spellings grant); a `state-dirs` entry is
  bound rw at /mysbx-home/<entry> with writes reaching the host
  sidecar both ways. The live-repo edit and the ro-rootfs refusal
  stay covered by the scripted probes 4/5 (2026-10-04 'thing',
  2026-10-14 f13).

Not covered here (needs the physical host): the podman-krun side
of the D8 head-to-head timing (no podman in this sandbox; the
krun one-shot true recorded at ~1.7–2.5 s warm, microvm-qemu at
~5.2–6.0 s warm, both nested-KVM-inflated), §2.2 nested podman
(no podman image), and a bare-metal f13 re-run of the timings.

## 2026-11-05, nested-KVM microvm-qemu agent sandbox — the baked-store baseline configs live (bd myconfig-dqq0)

Host context: same nested-KVM sandbox as the 85i entry above
(microvm-qemu, `/dev/kvm` + `svm`), wrapper rebuilt from
`test-f13` at main 216ef13a08, sidecar set to `backend = "krun"`,
`store = "baked"`, `network = false`, `multiplexer = "none"`,
`scratch-size = "8g"`, `git-dirs = […/myconfig/.git]`. The user
layer (`~/.config/mysbx/config.toml` — this sandbox's own
deployment does not carry it) was materialised from the same
eval (`home-manager.users.mhuber.xdg.configFile."mysbx/config.toml".source`)
so the run saw the same mounts/env a rebuilt f13 home would get;
the baked erofs store disk came from the wrapper pin
(`MYSBX_VM_STORE_DISK`), built fresh from `test-f13`'s closure.

- the bd myconfig-dqq0 acceptance (the commit 48348f1bd5 fix,
  live for the first time in a baked run) — PASS, all probes:
  - `/mysbx-home/.config/ripgrep/ripgreprc` is a REGULAR FILE
    (the raw-tree mount would have preserved the host's
    home-manager-files SYMLINK, dangling with no host-store
    share)
  - `RIPGREP_CONFIG_PATH=/mysbx-home/.config/ripgrep/ripgreprc`
    and `rg --files` (1187 files) and a pattern search both exit
    0 — the reported hard failure of every baked-run rg call is
    gone
  - `.config/git` and `.config/bat` present with ZERO symlinks in
    either tree (the same-class mounts fixed by the same commit)
  - a first probe WITHOUT the user layer mounted confirmed the
    negative control: no `.config/{ripgrep,git,bat}` in the guest
    at all, `RIPGREP_CONFIG_PATH` unset — the mounts are the
    layer's job, the baked image itself carries only what
    `storeRootPaths` names
- remaining raw-tree mounts observed in the same run (filed as
  bd myconfig-kf8j, discovered-from this one):
  `/mysbx-home/.config/beads/config.toml` and
  `/mysbx-home/.config/hunk/config.toml` are home-manager-files
  SYMLINKS inside the baked guest — the programs.beads and
  programs.hunk modules still bind `~/.config/…` raw.
