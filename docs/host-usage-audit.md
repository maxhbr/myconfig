# Host Usage Audit

**Date:** 2026-07-30
**Scope:** `hosts/host.*/`, `hosts/metadata.json`, `flake.nix`, `.github/workflows/ci.yml`
**Status:** Investigation + recommendation only. **Nothing has been deleted or
modified.** The human decides what (if anything) to remove.

## Method

For every host this audit cross-checks four sources and gathers staleness
signals:

1. **Host directory** — `hosts/host.<name>/`.
2. **Metadata entry** — a key under `hosts.<name>` in `hosts/metadata.json`.
3. **Flake wiring** — a `nixosConfigurationsGen.host-<name>` generator and a
   `nixosConfigurations.test-<name>` output in `flake.nix`.
4. **CI** — inclusion in the `build-os` matrix of `.github/workflows/ci.yml`
   (all as `build --dry-run`).

Signals used: last commit touching the host dir (with subject, to distinguish
substantive edits from repo-wide mechanical sweeps), references from other
hosts, whether the config can evaluate, and TODO/skeleton markers.

Note: `myconfig.metadatalib.announceOtherHosts` iterates over **all**
`metadata.hosts`, so every metadata entry is implicitly "referenced by" every
active host (it contributes `networking.extraHosts` and WireGuard peer data).
That makes metadata entries cheap but means a stale entry is invisible until
audited — it never errors.

## Verdict table

| host | arch | last-touched (subject) | in-CI? | evaluates? | wired in flake | referenced-by | verdict |
|---|---|---|---|---|---|---|---|
| f13 | x86_64 | 2026-07-29 (active) | yes | yes | gen + test | f13 `upg.otherHosts`, self | **KEEP** |
| p14 | x86_64 | 2026-07-24 (active) | yes | yes | gen + test | f13 `upg.otherHosts`, x1extremeG2 imports `role.work` | **KEEP** |
| workstation | x86_64 | 2026-06-27 (active) | yes | yes | gen + test | f13 `upg.otherHosts`, x1extremeG2 imports `gaming/games.steam` | **KEEP** |
| vserver | x86_64 | 2026-07-27 (active) | yes | yes | gen + test | f13 `upg.otherHosts`, wg0 peer/endpoint | **KEEP** |
| nas | x86_64 | 2026-07-19 (active) | yes | yes | gen + test | f13 `upg.otherHosts` | **KEEP** |
| nuc | x86_64 | 2026-07-19 (active) | yes | yes | gen + test | f13 `upg.otherHosts`, f13 tmux session | **KEEP** |
| r6c | aarch64 | 2026-06-08 (active) | yes | yes | gen + test | f13 `upg.otherHosts` | **KEEP** |
| thing | x86_64 | 2026-07-27 (active) | yes | yes | gen + test | f13 `mr` clone of thing-priv | **KEEP** |
| iso | x86/aarch64 | 2025-08-20 | build-iso | yes (build target) | `mkISO` (not a host gen) | `build-iso-image.sh`, CI `build-iso` | **KEEP** (installer image, not a machine) |
| futro | x86_64 | 2026-07-29 (added) | no | assumed (has test-futro) | gen + test | announce only | **REVIEW** |
| odroid | aarch64 | 2026-07-29 (active) | no | assumed (has test-odroid) | gen + test | announce only | **REVIEW** |
| roc | aarch64 | 2026-07-29 (skeleton) | no | assumed (has test-roc) | gen + test | announce only | **REVIEW** |
| t6 | aarch64 | 2026-07-30 (added) | no | assumed (has test-t6) | gen + test | announce only | **REVIEW** |
| pi0 | aarch64 | 2026-07-19 (mechanical) | no | **no** (no generator) | **none** (metadata only) | announce only | **REVIEW** |
| pi3a | aarch64 | 2026-07-19 (mechanical) | no | **no** (gen commented) | gen + test **commented out** | announce only | **REVIEW** |
| pi4 | aarch64 | 2026-07-19 (mechanical) | no | **no** (gen commented) | gen + test **commented out** | announce only; `scripts/build-sd-image.sh` default | **REVIEW** |
| x1extremeG2 | x86_64 | 2026-03-26 | no | **no — broken import** | gen present, test **commented out** | nas/r6c read its pubkeys | **CANDIDATE FOR REMOVAL** |
| T470s | x86_64 | 2025-07-03 (nixfmt sweep) | no | **not wired at all** | **none** | none | **CANDIDATE FOR REMOVAL** |

Metadata-only, non-directory entry:

| entry | notes | verdict |
|---|---|---|
| Pixel9 | No `hosts/host.Pixel9/` dir; WireGuard/roaming peer only (a phone). | **KEEP** (mobile wg peer, not a NixOS host) |

## Cross-source mismatches

- **T470s** — directory exists but has **no** metadata entry and **no** flake
  wiring. `git log -S "T470s" -- flake.nix` is empty, i.e. it was never
  registered. `system.stateVersion = "20.09"` and `postgresql_10` indicate a
  long-retired laptop.
- **x1extremeG2** — has a generator but its `nixosConfigurations` line is
  commented (`# x1extremeG2 = ...`), so no `test-x1extremeG2`. Its
  `default.nix` imports `../host.p14/role.work`, which **does not exist**
  (the `role.work` tree lives under `hosts/host.f13/role.work/`). The
  generator therefore fails to evaluate (`path '.../hosts/host.p14/role.work'
  does not exist`).
- **pi0** — has a directory and metadata but **never** had a generator
  (`grep pi0 flake.nix` is empty). It cannot be built from the flake today.
- **pi3a / pi4** — directory + metadata present; generator and `test-`
  target are both **commented out** in `flake.nix`. `scripts/build-sd-image.sh`
  still defaults to `pi4`, and the `add-nixos-host` skill references `pi4` as
  an example.
- **iso** — directory exists, no metadata (expected); consumed by
  `self.lib.mkISO` and `build-iso-image.sh`, built in CI's `build-iso` job.
- **CI vs AGENTS.md** — AGENTS.md lists the CI hosts as
  `f13, workstation, nas, vserver`, but the workflow actually dry-run-builds
  eight: `f13, workstation, nas, vserver, p14, nuc, r6c, thing`. AGENTS.md is
  out of date (informational; out of scope for this task).

## Removal candidates — mechanical cleanup checklist

### 1. T470s — high confidence

Retired ThinkPad T470s; never wired into the flake, `stateVersion 20.09`,
`postgresql_10`. Removing it touches only its own directory:

- [ ] `git rm -r hosts/host.T470s/` (`default.nix`, `hardware-configuration.nix`)

No metadata entry, no flake generator, no `test-` target, no
`announceOtherHosts` reference, no secrets recipients, no other host imports
it. Cleanest possible removal.

### 2. x1extremeG2 — high confidence (already disabled + broken)

Retired Lenovo X1 Extreme Gen2; the config is already broken (dangling
`role.work` import) and its `test-` target is already commented out. Removal
steps:

- [ ] `git rm -r hosts/host.x1extremeG2/`
- [ ] Delete the `host-x1extremeG2` generator block in `flake.nix`
      (the `nixosConfigurationsGen.host-x1extremeG2 = ...` definition).
- [ ] Delete the already-commented
      `# x1extremeG2 = self.nixosConfigurationsGen.host-x1extremeG2 [ ] { };`
      line in `nixosConfigurations`.
- [ ] Remove the `x1extremeG2` entry from `hosts/metadata.json`.
- [ ] Update the two hosts that read its pubkeys **before** removing metadata,
      or they will fail to evaluate:
  - `hosts/host.nas/default.nix:151` (`"x1extremeG2"` in a list).
  - `hosts/host.r6c/default.nix:14-15`
    (`metadatalib.get.hosts.x1extremeG2.pubkeys."id_ed25519.pub"` /
    `"id_rsa.pub"`).
- [ ] Check `../priv/` for `x1extremeG2` in any agenix `secrets.nix`
      recipients / age recipient lists (not verifiable from this repo — see
      open questions).

Because `announceOtherHosts` walks all metadata, dropping the `x1extremeG2`
metadata key silently removes its `extraHosts`/wg entries from every other
host — harmless, but re-run a `test-*` dry build afterwards to confirm.

## Ambiguous (REVIEW) hosts

- **futro / odroid / roc / t6** — all recently added or actively developed
  (roc and t6 are explicitly labelled bootstrap **skeletons** with TODOs).
  They have generators and `test-` targets but are **not** in the CI matrix.
  Recommend either adding them to CI dry-runs or accepting they are
  build-on-demand ARM/thin-client targets. Not removal candidates.
- **pi0 / pi3a / pi4** — Raspberry Pi hosts. pi3a/pi4 are intentionally
  commented out; pi0 was never wired. Their last commits are repo-wide
  mechanical fixes (`nixpkgs.hostPlatform.system`, `boot.zfs.forceImportRoot`),
  not real feature work, so they are dormant. Whether to resurrect (uncomment
  + re-enable) or remove depends on whether the physical Pis are still in use —
  see open questions. If removing, the cleanup mirrors x1extremeG2 (dir +
  metadata + commented flake blocks; pi4 also needs the default in
  `scripts/build-sd-image.sh` and the `add-nixos-host` skill example updated).

## Open questions for the human

1. **T470s** — confirmed retired? (No repo signal suggests it is still used; it
   was never even wired into the flake.)
2. **x1extremeG2** — confirmed retired? Its config has been broken and disabled
   since before 2026-03-26. Safe to delete along with its metadata + pubkey
   references?
3. **futro** — is the Fujitsu Futro S740 thin client physically in use? Added
   2026-07-29 but not in CI.
4. **Raspberry Pis (pi0 / pi3a / pi4)** — are any of these physically deployed
   and expected to be re-enabled, or should they be removed? pi3a/pi4 are
   commented out; pi0 has no generator at all.
5. **roc / t6** — these are bootstrap skeletons. Is the hardware in hand and
   expected to be finished, or were they abandoned experiments?
6. **Secrets in `../priv/`** — this audit cannot see the private repo. Before
   removing any host, check `../priv/` for agenix recipients / SSH host keys /
   deployment entries referencing the removed host name.
