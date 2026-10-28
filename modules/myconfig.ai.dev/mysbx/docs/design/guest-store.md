<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the VM guests' own nix store — a baked store view instead of the host share

Status: implemented (option B, the EROFS disk); complete live baked-store
acceptance is not recorded. Both `host` and `baked` modes shipped behind
`store = "host" | "baked"` ([config.md](./config.md) D26, the D3 amendment
in [backends.md](./backends.md)). This file keeps the exploration and
RATIONALE: why B over A/C, what the costs are, and what still needs a
KVM host. The [current six-backend guide](../backend-comparison.md) is
the source-audited behavior reference. This documentation update ran no
live VM tests; earlier host-store logs are not baked-store verification.

## The question

In `store = "host"` mode, `krun` and `microvm-qemu` share the WHOLE
host `/nix/store` into the guest, read-only, as one ro virtiofs slot
of the stage-ro device (backends.md D3, `stage_binds` in
`../../mysbx-rs/src/krun.rs`). The payload's shell, the `toolsEnv`
`PATH` tree and arbitrary host-built toolchains resolve through it.
This was the original layout, not an unconditional current default:
with no explicit choice, a VM run uses `baked` when
`MYSBX_KRUN_STORE_DISK` is nonempty, and `host` otherwise. An explicit
layer choice wins (`Merged::resolved_store`).

The question this file explored: could the VM instead start with its
OWN nix store — built at Nix-wrapper build time from the tools
closure ("built in, built from tools"), with optional per-run overlay
machinery writing ON TOP — so the whole host store is not delivered
to the guest? This is implemented. It narrows guest-visible paths,
not the host-side view of an escaped VMM: the outer bwrap still binds
the whole host store read-only for runtime dependencies.

## Where the design stands today

The original exploration rested on three pieces of the design:

- **The whole-store share is a decision with recorded reasons**
  (backends.md D3). Its store visibility matched bwrap's
  `--ro-bind /nix/store /nix/store`, needed no per-run toolchain
  config and had spike evidence. That is a store-view rationale,
  not a whole-backend security comparison. The SAME decision
  already rejected a per-run closure view (the epic's option (b)):
  the closure would be recomputed per run (a nix call inside
  `mysbx run`), and a missing path in the view would be a runtime
  ENOENT with no diagnosis. And it kept option (c) open on purpose:
  a per-run tools list remains a FUTURE config key that can only
  NARROW — "bind specific closures when a host wants the store
  hidden". A guest-own store is option (c) taken to its conclusion.
- **The overlay model is store-agnostic, but provisioning differs.**
  Both direct VM backends call `krun::spec_of` and use the same
  `../../nix/krun-rootfs.nix` init. With scratch attached, init
  formats/mounts ext4, sets up private Nix state (copying the selected
  db when available) and overlays `/nix/store`: lower = the selected
  store, upper = private scratch.
  The direct tool environment ALWAYS includes the Nix binary;
  `krun.nix.enable` is not its availability switch. The module pins
  `krunScratchSize` only under `cfg.podman.enable && cfg.krun.nix.enable`
  (`../../default.nix`, around line 482). Direct-only or QEMU-only
  opt-ins can therefore lack scratch even with guest Nix enabled.
  Without scratch, direct init provides no managed writable-Nix
  overlay or tmpfs fallback. The separate OCI-image wrapper
  (`../../nix/krun-guest-nix.nix`, bd myconfig-0pi) DOES have that
  fallback; it is not the direct init's contract. Scratch is private,
  disposable per-run storage, not a persistent store cache.
- **The host db ride-along exists only for the HOST lower.** The
  `nix-var` slot (bd myconfig-j23) shares `/nix/var/nix` read-only
  only with networking AND scratch AND host-store mode. Init copies
  its database onto scratch and RECONCILES it against the share
  (bd myconfig-mxu: static sqlite3 prune plus store-scan classifier).
  The historical host-rebuild and flake-source-symlink findings
  explain this machinery, including the live fchmodat2 re-add crash.
  A baked store and db match by construction: with scratch, init
  copies the BAKED db to scratch and skips host reconciliation; with
  no scratch, the db remains on read-only EROFS.

The key observation that answers half of the old rejection: if the
store view is built by the NIX WRAPPER at build time — like the
rootfs and `toolsEnv` already are — there is no `nix` call inside
`mysbx run` and no per-run closure computation. The view's closure is
computed once per wrapper rebuild. What survives of the rejection is
the ENOENT half: a narrowed store breaks paths the original whole-store
default silently served, and that breakage must become diagnosable
(see below).

## The three ways to build and bind a guest-own store

```
host:    host /nix/store ──ro virtiofs──> guest /nix/store
baked:   EROFS /nix tree ──ro block────> guest /nix (store child)
with scratch: either lower ──overlay──> merged /nix/store, private upper
```

The options table below preserves the exploration before B shipped;
its then-open launcher and kernel work is resolved in the probes below.
All three proposed building the SAME thing at nix build time — a
store view (the closure of `toolsEnv` + `extraTools` + the
payload-relevant pins: the shell, the multiplexer entries' closure,
`krun.nix.packages`, …) plus
a REGISTERED db for exactly that view (the dockerTools
`imageIncludeNixDB` precedent, applied to a plain view instead of an
OCI image). They differ in how the view reaches the guest:

| | A: bake into the rootfs | B: ro disk image via virtio-blk | C: staged view via stage-ro |
| --- | --- | --- | --- |
| Mechanism | `nix/krun-rootfs.nix` grows `nix/store/<name>` entries for every closure path; the store rides the ROOT virtiofs device | one squashfs/erofs/ext4 image built with `closureInfo`, attached with `krun_add_disk2` like the scratch; guest mounts it as the overlay lower | a standalone store-view derivation; `stage_binds()` binds IT into the `store` slot instead of `/nix/store` |
| Guest-side changes | the root tree would grow a real store subtree instead of its store symlink | init mounts the EROFS device at `/nix`; krun and QEMU now both implement `--store-disk` | init, manifest and symlink would stay unchanged |
| qemu backend | would inherit | `--store-disk` and read-only virtio-blk attachment shipped | would inherit identically (it renders the same staged spec) |
| Rootfs property | the rootfs stops being host-independent — it changes whenever the tools change | rootfs stays generic; the coupling moves into a separate image pin | rootfs stays generic; the coupling is a new pin (`MYSBX_KRUN_STORE_VIEW`) |
| Filesystem semantics | virtiofs lower retains the historical fileattr/copy-up caveats | native read-only EROFS lower; overlay copy-up, whiteouts and writable metadata use the native scratch upper | same as A (virtiofs lower) |
| New kernel requirement | none | the fs driver must be built in; EROFS support was subsequently checked in libkrunfw and added to the QEMU kernel pin | none |
| Device/MMIO budget | unchanged (root device already exists) | ONE more virtio-blk against libkrun's IRQ budget (the scratch already spends one; the D3 budget analysis covered fs slots only — needs a probe) | unchanged (stage-ro already exists) |

Option C initially looked like the smallest diff: a `stage_binds()`
entry plus a wrapper pin, with both VM backends inheriting it. Option B
was the preferred endgame because a native lower avoids the store's
virtiofs-specific fileattr/copy-up caveats. That rationale is not a
live claim that every Nix operation (including `nix store optimise`)
has passed on baked mode. Kernel-driver and device-budget probes
first had to establish that B was buildable.

Probing C's mechanics killed it structurally, before any build:

- a store view the GUEST resolves cannot use absolute `/nix/store/...`
  symlinks (the guest's `/nix/store` IS the view — circular), so
  every path must be a real file, a relative symlink, or a hardlink;
- `nix build --print-out-paths ... | xargs` does not survive a real
  closure (ARG_MAX; the closure is hundreds of paths), so the view
  builder needs a files-from driver — fine, `closureInfo` gives the
  list — but then the VIEW is either a `cp -r` copy of the whole
  closure (hundreds of MiB per wrapper rebuild, the exact cost B
  avoids) or a hardlink farm (`cp -rl`, near-zero disk) —
- and the hardlink farm rides into the stage tree through a bwrap
  bind: bwrap REJECTS crosses over a bind boundary it cannot
  hardlink across, so the farm needs a bind of the whole output —
  which reintroduces the host store share for the staging window.

None of these is a tweak away from working; each is the mechanism
itself. That is what flipped the cost table to B: `mkfs.erofs
--tar` needs no populated tree at all.

## What gets better

- **A narrower guest store view.** Baked mode exposes the declared
  tool closure and extra roots, rather than every host store path.
  Workspace/configured mounts remain available; this is not a claim
  that the guest sees only tools, nor a security ranking. The outer
  bwrap still exposes the whole host store to the VMM's host process.
- **The host-db machinery is skipped.** No `nix-var` ride-along,
  copied host db, sqlite reconcile or store-scan of a ~175k-path
  host share. The baked db is still COPIED when scratch is attached.
  Baking removes the host-restaging mismatch behind those historical
  findings, not every possible Nix or filesystem failure.
- **Backing immutability is not merged-view immutability.** The
  EROFS image and host store share stay read-only. With scratch,
  new paths AND copy-up writes to existing merged paths can land in
  the private upper. Do not expect every write to an existing
  `/nix/store` path to fail just because its backing disk is ro.
  Such writes disappear with scratch when the run ends.

## What a narrowed store breaks — the honest costs

- **Per-repo devshells and ad-hoc host-built paths.** The
  whole-store mode is what lets a payload exec any store path the
  HOST ever built: a repo's `result` symlinks, an `.envrc` export, a
  devshell the operator entered yesterday, a toolchain outside
  `toolsEnv`. With a baked view those are ENOENT. Three proposed
  mitigations: (a) declared extra roots beyond the base tools (an
  `extraTools`-style extension point, so a host/repo states what
  belongs in its sandbox store); (b) in-guest `nix` rebuilds the
  missing path, requiring actual writable-Nix provisioning and
  network access for substitution/fetches — neither can be silently
  assumed; (c) the failure is DIAGNOSED — `mysbx
  doctor` and/or the init can diff a requested path against the
  baked view and say "not in the sandbox store: add it to the tools
  or rebuild it in-guest" instead of a bare ENOENT. (c) is the
  direct answer to the recorded rejection reason.

  **What shipped and what did not (the honest ledger).** (a) shipped
  HOST-side only: the Nix builder's `krunStoreExtraRoots` seam adds
  roots (`default.nix` supplies `krun.nix.packages` when
  `krun.nix.enable`); `extraTools` also extends the closure. There
  is no automatic per-repository root discovery or per-repo roots
  config key. (b) warnings/refusals are NOT wired: a baked run is
  accepted without networking or scratch. Nix is already in the
  direct tools, but `krun.nix.enable` alone does not ensure the
  scratch pin under the current module gate. (c) is NOT wired: a
  baked ENOENT is today a bare shell error; the diagnosing diff is
  future work.
  Until they land, `store = "baked"` is for runs whose tools are
  complete in the closure — and this ledger stays here so nobody
  has to rediscover which third of the mitigation shipped.
- **Rebuild coupling.** The view pin changes on every
  `toolsEnv`/`extraTools` change — the same cadence the wrapper's
  own closure already has, but it trades away D3's "one rootfs
  serves every repo and every toolchain set" property (for A; B and
  C keep the rootfs generic and pay the pin instead).
- **A fallback must exist and be explicit.** Some repos genuinely
  need arbitrary host paths. The config wants an announced choice —
  a per-run/repo key (`store = "host" | "baked"` in the sandbox
  config's vocabulary), `host` preserving the pre-key behavior
  exactly, refuse-or-announce, never a silent switch. The shipped
  default resolves `baked` only for `krun`/`microvm-qemu` with a
  NONEMPTY `MYSBX_KRUN_STORE_DISK` pin (a layer may always select
  `host`); `host` is the default otherwise. An explicit unpinned
  `baked` choice is refused, not silently replaced with `host`.
- **Backend inconsistency.** Bubblewrap/nono expose the whole host
  store; Podman backends use their image-owned store, NOT an implicit
  whole-host-store bind. Direct VMs choose host or baked. These
  different views have diagnosability costs, not a demonstrated
  ordering of backend security. Extending `store` to bwrap remains a
  separate design change, not implemented behavior.

## The probes, resolved (and the one that remains)

1. **View materialization (decided A vs C mechanics):** RESOLVED
   AGAINST A/C — not by the link farm failing, but by B needing no
   farm at all: `mkfs.erofs --tar` packs the closure straight from
   the store paths, no populated tree, no copies, and the guest
   mounts the image AT `/nix`, hiding the rootfs's store symlink.
   The image's `store` child is a real directory, so an optional
   scratch overlay mounts at `/nix/store` without host mode's
   tmpfs mountpoint trick. The circular-symlink and ARG_MAX findings
   that structurally blocked C are recorded above; the hardlink-GC
   question is moot for B.
2. **Kernel drivers (gated B):** ANSWERED, no rebuild for krun —
   the pinned libkrunfw x86_64 config already carries
   `CONFIG_EROFS_FS=y` (xattr, posix acl, zip/lz4, checked against
   the 5.5.0 config). The qemu backend's kernel pin
   (`nix/microvm-qemu-kernel.nix`) gained `EROFS_FS=y` alongside
   its ext4/overlayfs lines; the built configfile pins
   `CONFIG_EROFS_FS=y`.
3. **MMIO budget (gated B):** ANSWERED STATICALLY — the second
   virtio-blk device costs one IRQ slot; the budget's accounting
   (11 slots minus balloon, rng, console, vsock) leaves the store
   disk within it. The scratch (D7) has earlier live evidence for
   one blk device; a complete baked-plus-scratch boot acceptance
   record is still missing, not supplied by that evidence.
4. **Db baking:** ANSWERED — `nix/krun-store-image.nix` bakes
   `/nix/var/nix/db` with the dockerTools recipe
   (`nix-store --load-db` against `closureInfo`'s registration,
   registrationTime reset for determinism) plus the gcroots
   symlinks; the `mysbx-krun-store-disk-test` check extracts the
   image with `fsck.erofs` and verifies the db rows, the gcroots
   and the fixed UUID. Init copies the baked db onto scratch when
   present, but no host reconcile or `store-scan` pass runs in baked
   mode (`nix/krun-rootfs.nix`). Without scratch the baked db stays
   read-only; there is no managed writable-Nix fallback.
5. **End-to-end narrowed run:** STILL OPEN — needs a KVM host. See
   the handoff below.

## What shipped (the recorded outcome)

Option B, not the C first move the recommendation proposed: while
probing C's mechanics (probe 1) turned up the structural blockers
recorded above — the circular `/nix/store` symlink, the ARG_MAX wall
of `nix build --print-out-paths | xargs`, hardlink-refusing bwrap
binds, and a closure COPY whose size and rebuild cost dwarf the
disk's — and probe 2 answered erofs as free on the krun side, the
cost table flipped: `mkfs.erofs --tar` gives B everything C would
have needed a farm for, with no populated tree at all. The key
spelled `store = "host" | "baked"` (not `"tools"`): the disk carries
the whole tool closure, and "baked" names the mechanism, not the
contents.

Shipped pieces: the image (`nix/krun-store-image.nix`), the krun
launcher flag (`--store-disk`, `krun-rs`), the qemu twin
(`microvm-qemu-rs` + `EROFS_FS=y` in the kernel pin), the guest init
arm (`nix/krun-rootfs.nix` — erofs at `/nix`, the `/sys/block`
ro-flag discrimination, the announced-refusal of unannounced ro
devices), the spec wiring (`mysbx-rs` — the `store` key, the share
drop, the manifest gate, and refusal of unpinned baked runs or baked
runs on any backend other than `krun` and `microvm-qemu`, including
`podman-krun`), the module option
(`myconfig.ai.dev.mysbx.krun.store.enable`), the doctor line and
the check (`mysbx-krun-store-disk-test`).

If live runs survive, the follow-up decision — extending the key to
the bwrap tier (a ro bind of a view instead of the whole store) —
is a backends.md/config.md change of its own, informed by what the
live runs broke. The default flip on the VM tiers has SHIPPED with
the key itself: an omitted choice on `krun`/`microvm-qemu` with a
nonempty `MYSBX_KRUN_STORE_DISK` pin resolves `baked`
(`Merged::resolved_store`, mysbx-rs/src/merge.rs), and `host`
otherwise. An explicit layer choice still wins; the absence of a pin
is not a runtime fallback from an explicit baked request.

## Handoff: the live validation this sandbox cannot run

The rootfs derivation tests, EROFS extraction check, cargo suites and
goldens provide static/build coverage; their existence is not a live
acceptance run, and they were not rerun for this docs-only update.
`doctor` checks pins/tools and limited startup behavior; it does NOT
boot a direct VM. Normal mysbx runs of BOTH direct backends require
KVM; QEMU's launcher-level `kvm:tcg` is not a supported mysbx fallback.

The remaining acceptance procedure must record the host, revision,
backend, effective `store`, disk pins, networking and scratch presence:

1. Boot explicit `store = "baked"` runs on `krun` AND `microvm-qemu`
   with `MYSBX_KRUN_TRACE=1`: verify EROFS at `/nix`, its block
   device's read-only flag, and no guest store-share placement. Use
   the normal shares AND both disks to check the static IRQ budget.
   Also boot without scratch and confirm no managed writable-Nix
   setup appears. Do not assume `krun.nix.enable` supplied the disk:
   check the current `podman.enable && krun.nix.enable` pin gate.
2. With scratch and networking, check the db copy from the baked
   image, no host reconciliation, `nix path-info --all`, substitution
   of a path absent from the image into the upper, and verification
   of an unchanged baked path. Repeat host mode separately: the
   host db share/copy requires network + scratch + host, not baked.
3. Test BACKING immutability separately from merged-view writes.
   Without scratch, writes to the EROFS tree must fail. With scratch,
   use a disposable fixture included as an extra root to exercise
   copy-up of an existing merged path as well as new-path creation;
   those writes may succeed in the upper. Confirm the image/host
   backing is unchanged and a fresh run loses the scratch changes.
   The old proposed blanket "writes to baked paths fail" test was
   wrong for the overlay. Disk-identification refusals are separate
   tests (e.g. a missing announced ro disk), not a promised wording
   for an ordinary filesystem write error.
4. Confirm a known host-built path outside the baked closure and
   configured mounts is absent. Expect today's bare ENOENT, not
   the proposed diagnosis. Record the same path's visibility under
   explicit `store = "host"` as a control.
5. Record omitted-key defaults with and without a nonempty store
   pin, explicit `host` overriding the pinned default, and refusal
   of explicit `baked` without a pin or on any backend other than
   `krun` and `microvm-qemu`, including `podman-krun`.

The existing harnesses are `../../nix/krun-live-validation.sh` and
`../../nix/microvm-qemu-live-validation.sh`; these store-disk cases
need explicit coverage in addition to their smoke probes. The
[krun log](../krun-validation-log.md) records host-store scratch/Nix
results, and the [QEMU log](../microvm-qemu-validation-log.md) records
KVM smoke/interactive results. Neither records complete live baked
acceptance; do not relabel those earlier runs retrospectively.

The honest-costs ledger remains open: automatic per-repo roots (a),
network/writable-Nix warnings (b), and missing-path diagnosis (c) are
not implemented. The pinned-VM default has ALREADY flipped to baked;
operators must account for these gaps or explicitly choose `host`.
