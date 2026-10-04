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
