<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the VM guests' own nix store — a baked store view instead of the host share

Status: implemented (option B, the erofs disk). The exploration
resolved: the probes below landed at build time, the surviving
option shipped behind the `store = "host" | "baked"` config key
([config.md](./config.md) D26, the D3 amendment in
[backends.md](./backends.md)), and this file keeps the RATIONALE:
why B over A/C, what the costs are, and the one probe that still
needs a KVM host (the live run, the handoff section at the end).

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

## The probes, resolved (and the one that remains)

1. **View materialization (decided A vs C mechanics):** RESOLVED
   AGAINST A/C — not by the link farm failing, but by B needing no
   farm at all: `mkfs.erofs --tar` packs the closure straight from
   the store paths, no populated tree, no copies, and the guest
   mounts the image AT `/nix/store` (the baked rootfs dir is a
   real directory, so the overlay mounts directly on it — no
   tmpfs trick). The circular-symlink and ARG_MAX findings that
   structurally blocked C are recorded above; the hardlink-GC
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
   disk within it, exactly like the scratch (D7) already proved
   the first blk device fits. The live boot confirms it below.
4. **Db baking:** ANSWERED — `nix/krun-store-image.nix` bakes
   `/nix/var/nix/db` with the dockerTools recipe
   (`nix-store --load-db` against `closureInfo`'s registration,
   registrationTime reset for determinism) plus the gcroots
   symlinks; the `mysbx-krun-store-disk-test` check extracts the
   image with `fsck.erofs` and verifies the db rows, the gcroots
   and the fixed UUID. No reconcile, no `store-scan` pass runs in
   a baked guest (the init's db-copy and reconcile paths are
   host-mode-only, `nix/krun-rootfs.nix`).
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
drop, the manifest gate, the refusal on unpinned baked runs and on
non-VM backends), the module option
(`myconfig.ai.dev.mysbx.krun.store.enable`), the doctor line and
the check (`mysbx-krun-store-disk-test`).

If live runs survive, the follow-up decision — flipping the
default, or extending the key to the bwrap tier (a ro bind of a
view instead of the whole store) — is a backends.md/config.md
change of its own, informed by what the live runs broke.

## Handoff: the live validation this sandbox cannot run

Everything above is verified at BUILD time (the rootfs test suite
inside the derivation, the erofs check, the cargo suites, the
goldens). What still needs a real KVM host — the probe-5 half:

1. Boot a `store = "baked"` krun run (and the microvm-qemu twin)
   with `MYSBX_KRUN_TRACE=1`: the init's trace must show the erofs
   mount at `/nix` and NO store share placement.
2. In-guest nix against the baked db: `nix path-info --all` lists
   the baked closure, one substitution onto the scratch overlay
   succeeds (the overlay's lowerdir IS the erofs mount), `nix
   verify` on a baked path passes.
3. The ro-device honesty: a write attempt to a baked store path
   fails (read-only block device), the refusal names the disk, not
   a generic EROFS.
4. A host-built path outside the tool closure ENOENTs with the
   diagnosis proposed in "What a narrowed store breaks".
5. The budget: a config with the usual share set AND the two disks
   boots (the static IRQ accounting says it fits — confirm).

The harnesses exist (`nix/krun-live-validation.sh`,
`nix/microvm-qemu-live-validation.sh`); the store-disk cases are
the addition a KVM host runs.
