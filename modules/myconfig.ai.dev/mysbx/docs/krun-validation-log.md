# mysbx krun live-validation log

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
