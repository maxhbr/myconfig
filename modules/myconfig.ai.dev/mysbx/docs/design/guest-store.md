<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the VM guests' own nix store — a baked store view instead of the host share

Status: draft. This is an EXPLORATION, not a decision record: it
gathers what today's store share costs, what a guest-own store would
change, and which probes must run before anything here can become a
decision. The decided state stays in
[backends.md](./backends.md) D3/D9; the config surface stays in
[config.md](./config.md). When the probes land, the surviving option
moves into backends.md as a new decision and this file either
records the rationale or is retired.

## The question

The `krun` and `microvm-qemu` backends share the WHOLE host
`/nix/store` into the guest, read-only, as one ro virtiofs slot of
the stage-ro device (backends.md D3, `stage_binds`'s first entry in
`../mysbx-rs/src/krun.rs`). The payload's shell, the `toolsEnv`
`PATH` tree, every per-run toolchain and every host-built path an
agent may want to exec resolve through that share.

The question this file explores: could the VM instead start with its
OWN nix store — built at Nix-wrapper build time from the tools
closure ("built in, built from tools"), with the per-run overlay
machinery writing new paths ON TOP — so the host store is never
visible to the guest at all?

## Where the design stands today

Three pieces of the current design shape the answer:

- **The whole-store share is a decision with recorded reasons**
  (backends.md D3). It makes the direct backend no more permissive
  than the bwrap tier (`--ro-bind /nix/store /nix/store`), needs no
  per-run config surface, and is spike-proven. The SAME decision
  already rejected a per-run closure view (the epic's option (b)):
  the closure would be recomputed per run (a nix call inside
  `mysbx run`), and a missing path in the view would be a runtime
  ENOENT with no diagnosis. And it kept option (c) open on purpose:
  a per-run tools list remains a FUTURE config key that can only
  NARROW — "bind specific closures when a host wants the store
  hidden". A guest-own store is option (c) taken to its conclusion.
- **The write story already exists and is store-agnostic.**
  `krun.nix.enable` (bd myconfig-pz6) bakes the guest-root nix
  wrappers into the image (the podman tier) / resolves them through
  the store share (the direct tier); the wrapper mounts the per-run
  scratch ext4, copies the nix state onto it, and overlays
  `/nix/store`: lower = whatever store the guest was given, upper =
  the scratch (`../../nix/krun-guest-nix.nix`, bd myconfig-0pi and
  backends.md D7). Swapping the lower layer touches none of this.
- **The db ride-along exists only because the lower is the HOST
  store.** The `nix-var` slot (bd myconfig-j23) copies the host
  `/nix/var/nix` into the guest, and the init RECONCILES the copied
  db against the share (bd myconfig-mxu: the static sqlite3 prune
  plus the static store-scan bulk classifier) — because a host
  rebuild restages the share, the flake-source poisoning TODO shows
  symlinked entries, and the guest nix must never re-add or delete a
  lower-layer path (the live fchmodat2 crash). A store whose db is
  BAKED TOGETHER WITH IT needs none of that machinery: db and store
  match by construction.

The key observation that answers half of the old rejection: if the
store view is built by the NIX WRAPPER at build time — like the
rootfs and `toolsEnv` already are — there is no `nix` call inside
`mysbx run` and no per-run closure computation. The view's closure is
computed once per wrapper rebuild. What survives of the rejection is
the ENOENT half: a narrowed store breaks paths the whole-store
default silently served, and that breakage must become diagnosable
(see below).

## The three ways to build and bind a guest-own store

```
today:   host /nix/store ──ro virtiofs──> guest /nix/store ──overlay──> scratch
idea:    baked store view ──ro ─────────> guest /nix/store ──overlay──> scratch
         (host store: not visible at all)
```

All three build the SAME thing at nix build time — a store view (the
closure of `toolsEnv` + `extraTools` + the payload-relevant pins: the
shell, the multiplexer entries' closure, `krun.nix.packages`, …) plus
a REGISTERED db for exactly that view (the dockerTools
`imageIncludeNixDB` precedent, applied to a plain view instead of an
OCI image). They differ in how the view reaches the guest:

| | A: bake into the rootfs | B: ro disk image via virtio-blk | C: staged view via stage-ro |
| --- | --- | --- | --- |
| Mechanism | `nix/krun-rootfs.nix` grows `nix/store/<name>` entries for every closure path; the store rides the ROOT virtiofs device | one squashfs/erofs/ext4 image built with `closureInfo`, attached with `krun_add_disk2` like the scratch; guest mounts it as the overlay lower | a standalone store-view derivation; `stage_binds()` binds IT into the `store` slot instead of `/nix/store` |
| Guest-side changes | none (the baked `/nix/store` symlink already points at the slot; the root tree grows a real store subtree) | init mounts one more device; the `--store-disk` path also needs a qemu-side launcher flag (the qemu twin supports only `--scratch` today) | NONE — init, manifest and symlink untouched |
| qemu backend | inherits | needs `--store-disk` + a virtiofsd-free blk passthrough decision | inherits identically (it renders the same staged spec) |
| Rootfs property | the rootfs stops being host-independent — it changes whenever the tools change | rootfs stays generic; the coupling moves into a separate image pin | rootfs stays generic; the coupling is a new pin (`MYSBX_KRUN_STORE_VIEW`) |
| Filesystem semantics | virtiofs lower — the known caveat class stays (no fileattr, `EOPNOTSUPP` on lower copy-up; harmless for nix, which only ever ADDS paths) | NATIVE kernel fs as the lower — the virtiofs caveats vanish entirely; xattrs, whiteouts, chown all native | same as A (virtiofs lower) |
| New kernel requirement | none | the fs driver must be built into libkrunfw (ext4 IS proven — the scratch uses it; squashfs/erofs are UNPROVEN: the rootfs is a plain dir precisely so no fs driver was ever needed) | none |
| Device/MMIO budget | unchanged (root device already exists) | ONE more virtio-blk against libkrun's IRQ budget (the scratch already spends one; the D3 budget analysis covered fs slots only — needs a probe) | unchanged (stage-ro already exists) |

Option C is the smallest-diff shape: zero guest code, both VM
backends inherit, the host-side change is one `stage_binds()` entry
plus one new wrapper pin, and the live validation harness
(`../../nix/krun-live-validation.sh`) covers it unchanged. Option B is
the cleaner ENDGAME — a native ro filesystem removes the entire
virtiofs caveat class, so the overlay-nix limitations documented in
backends.md D2's scope decision (no copy-up of image paths, no
`nix store optimise`) lift — but it is not even buildable before two
probes (kernel driver, device budget) answer its open questions.

## What gets better

- **The first backend tier that actually narrows.** Today the VM
  reads every host store path; the design itself notes the whole
  store is already readable and reserves narrowing for "a host [that]
  wants the store hidden". A baked view is that narrowing: the guest
  sees the tools closure and nothing else — no host-built configs, no
  paths of other repos, no accidentally-built artifacts.
- **The db machinery simplifies away.** No `nix-var` ride-along, no
  copied host db, no sqlite reconcile, no store-scan of a
  ~175k-path share (the scan runs at BOOT; a tools-closure view is
  orders of magnitude smaller), no fchmodat2-class hazards, and the
  flake-source poisoning TODO becomes structurally impossible (the
  view is built, never restaged from the host store).
- **Writes are unchanged.** New store paths land on the per-run
  scratch; lower entries stay immutable — which is nix's own
  contract (store paths are content-addressed, append-only), so the
  virtiofs copy-up limitation of options A/C is a non-issue.

## What a narrowed store breaks — the honest costs

- **Per-repo devshells and ad-hoc host-built paths.** The
  whole-store default is what lets a payload exec any store path the
  HOST ever built: a repo's `result` symlinks, an `.envrc` export, a
  devshell the operator entered yesterday, a toolchain outside
  `toolsEnv`. With a baked view those are ENOENT. Three mitigations,
  all needed: (a) the view must include more than the base tools —
  declared extra roots (an `extraTools`-style extension point, so a
  host/repo states what belongs in its sandbox store); (b) in-guest
  `nix` rebuilds the missing path (works, but needs
  `krun.nix.enable` plus `network` — the substitution must not be
  silently assumed); (c) the failure must be DIAGNOSED — `mysbx
  doctor` and/or the init can diff a requested path against the
  baked view and say "not in the sandbox store: add it to the tools
  or rebuild it in-guest" instead of a bare ENOENT. (c) is the
  direct answer to the recorded rejection reason.
- **Rebuild coupling.** The view pin changes on every
  `toolsEnv`/`extraTools` change — the same cadence the wrapper's
  own closure already has, but it trades away D3's "one rootfs
  serves every repo and every toolchain set" property (for A; B and
  C keep the rootfs generic and pay the pin instead).
- **A fallback must exist and be explicit.** Some repos genuinely
  need arbitrary host paths. The config wants an announced choice —
  a per-run/repo key (`store = "host" | "tools"` in the sandbox
  config's vocabulary), `host` preserving today's behavior exactly,
  refuse-or-announce, never a silent switch. Whether the DEFAULT
  flips is a separate decision, made only after the probes and at
  least one tier's worth of live runs on a real host.
- **Tier inconsistency.** The bwrap/podman tiers still bind the
  whole store. A narrowed VM tier is the first tier that is NARROWER
  than the tier it replaces — allowed (the constraint runs the other
  way: no tier may be more permissive), but worth stating: a config
  that behaves differently per backend on the same repo is a
  diagnosability cost of its own, and the `store` key should
  eventually apply to the bwrap tier too (a ro bind of the view
  instead of the whole store is mechanically trivial there).

## Open probes (the spike work, in order)

1. **View materialization (decides A vs C mechanics):** can a
   store-output tree present closure paths as real directories that
   resolve in the GUEST without the host store? Absolute
   `/nix/store/...` symlinks cannot (the guest's `/nix/store` IS the
   view — circular); a RELATIVE link farm inside the tree, or
   `cp -rl` hardlink farm (near-zero disk cost — one filesystem),
   are the candidates. One build-time verification that nix GC
   keeps hardlinked content alive through the view's link count is
   required before A/C are trustworthy.
2. **Kernel drivers (gates B):** does the pinned libkrunfw kernel
   build have squashfs or erofs in? (ext4 is proven — the scratch
   formats and mounts it.) If not, option B costs a kernel rebuild,
   which is a scope decision of its own.
3. **MMIO budget (gates B):** does a SECOND virtio-blk device
   (store image + scratch) boot, or does the IRQ budget reject it?
   Cheap to answer with the spike harness and one launcher patch.
4. **Db baking:** reproduce the `imageIncludeNixDB` result for a
   plain view — generate `/nix/var/nix/db` from `closureInfo` at
   build time — and verify the guest's single-user nix accepts it
   as-is (no reconcile, no scan; `nix path-info`, `nix build` of
   something already in the view, one substitution onto the
   scratch).
5. **End-to-end narrowed run:** option C wired behind a temporary
   flag, live on a KVM host: payload shell resolves, `toolsEnv`
   binaries run, in-guest nix overlays the view, a host-built path
   ENOENTs with the diagnosis proposed above.

## Recommendation

Spike option C first: it needs no probe except 1 and 4 (the two
build-time ones), touches no guest code, inherits to `microvm-qemu`
for free, and — behind an explicit `store = "host" | "tools"` config
key with the db baked alongside the view — can run on a real host
long enough to answer the ENOENT-diagnosis question honestly.
Option B is worth a parallel spike (probes 2 and 3 are cheap) as the
cleaner long-term shape, but it is not the first move: its payoff is
lifting caveats the overlay-nix model already tolerates, while its
open questions could invalidate it entirely.

If the C spike survives live use, the follow-up decision — flipping
the default, or extending the key to the bwrap tier — is a
backends.md/config.md change with its own bead, informed by what the
live runs broke.
