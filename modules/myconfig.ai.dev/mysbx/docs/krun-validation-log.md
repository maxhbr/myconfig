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
