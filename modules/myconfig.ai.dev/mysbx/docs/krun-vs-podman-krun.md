<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `krun` vs `podman-krun`: implementation differences and validation

Status: source-audited correction of the 2026-10 comparison; no new
live tests were run for this update. Use the
[current six-backend guide](./backend-comparison.md) for implemented
behavior across all backends. This page retains the two-backend
architecture rationale and historical evidence, not the earlier
blanket parity, performance or retirement verdict.

The design history is in [`design/backends.md`](./design/backends.md):
`podman-krun` is D2, direct `krun` is D3, and D8 recorded a successor
intent and direct-first feature-work policy. Its premise that direct
krun covers everything with verified equivalent semantics is not
supported by the current wiring and live record.

## The two backends

Both are rootless KVM microVM backends: each run boots a libkrunfw
guest kernel under `/dev/kvm`, with mounts over virtiofs. The payload
runs as **guest root**; the host VMM/file-server processes run as the
invoking host user. Guest root is not host root, but guest-kernel,
VMM and file-server escapes remain relevant. The chains below are
different boundaries, not a proven security or speed ranking.

| | `podman-krun` (D2) | `krun` direct (D3) |
| --- | --- | --- |
| layers to the VM | mysbx → podman → crun (OCI config) → libkrun → VM | mysbx → bwrap → launcher → libkrun → VM |
| filesystem contract | crun prepares an OCI rootfs host-side; libkrun shares it | staged spec shares and a plain-directory rootfs define the guest layout |
| toolchain/store | image-owned store, loaded per host (`mysbx podman-load-image`); no implicit whole-host-store bind | whole host store read-only OR a baked tool-closure disk |
| network | TSI via crun's krun handler and the container network namespace | TSI through the launcher's namespace; `network = false` disables implicit vsock AND bwrap unshares the netns |
| Nix scratch, when provisioned | sparse file loop-mounted over virtiofs (bd myconfig-0pi: crun's handler cannot attach disks) | real virtio-blk device via `krun_add_disk2` |
| git trust | per-run sidecar files bound over virtiofs, swept at startup | in-memory overlay files on a dedicated virtual device (`krun_fs_add_overlay_file`) |
| build/load coupling | OCI image per toolchain pin, loaded per host | no OCI image; optional EROFS store image built with the wrapper's closure |

The `podman-krun` argv is the `podman-gvisor` argv with the runtime
swapped to crun+libkrun plus enumerated differences: the
`run.oci.handler=krun` annotation, `--group-add=keep-groups`, omitted
`--cap-drop`/`no-new-privileges`, and VM sizing as `krun.cpus`/
`krun.ram_mib` annotations. Those omitted OCI flags do not constrain
the guest-root payload. Direct krun drives libkrun through a small
launcher (`krun-rs`, packaged by `nix/krun-launcher.nix`) under bwrap.
Its in-process file server can open the view bwrap leaves visible,
which still includes the WHOLE host store for runtime dependencies
even when only a baked closure is delivered to the guest.

### The QEMU companion

`microvm-qemu` calls the same `krun::spec_of` builder and uses the same
`nix/krun-rootfs.nix` init as direct krun. It changes the VMM to QEMU,
uses separate rootless virtiofsd processes (`--sandbox=none`, relying
on outer bwrap), and uses SLiRP user-mode NAT rather than TSI. Normal
mysbx runs of both direct backends require KVM; the QEMU launcher's
`kvm:tcg` argument is not a supported mysbx software fallback. QEMU
has its own kernel pin and console/exit-code plumbing. Shared spec
and init do not establish identical live behavior or test coverage.

## Implemented differences and gaps

### Store and writable Nix

Direct `krun` and `microvm-qemu` implement both `store = "host"` and
`store = "baked"`. An explicit configuration choice wins. Otherwise
a NONEMPTY `MYSBX_KRUN_STORE_DISK` pin selects baked for these VM
backends; host is the default without it. Explicit baked without a pin
is refused. Every other backend, including `podman-krun`, also refuses
`store = "baked"`. In host mode arbitrary host-built
store paths are visible; baked mode includes only the wrapper's tool
closure and extra roots. Automatic per-repo roots, improved missing-path
diagnosis and warnings about missing network/writable-Nix provisioning
are not implemented. See the [baked-store design](design/guest-store.md).

The EROFS image mounts at `/nix`; its `store` child is the lower store.
With scratch, the merged `/nix/store` overlay can create paths AND copy
up/write existing paths. Read-only backing does not mean blanket write
failure in the merged view. With scratch, baked mode copies its own db
and skips host reconciliation. The host db is shared/copied only with
network + scratch + host mode.

Real disk attachment removes direct krun's need for a loop mount: the
launcher owns the libkrun context and calls `krun_add_disk2`. The crun
1.30 audit (bd myconfig-0pi) found no disk annotation/attachment path,
so the OCI wrapper uses `losetup` + ext4 on the bound file instead.
Both scratches are private, per-run and disposable, not persistent Nix
caches; new paths must be obtained again in a later run.

Provisioning is not uniform. The direct tools ALWAYS include Nix, but
`default.nix` (around line 482) pins `krunScratchSize` only when
`cfg.podman.enable && cfg.krun.nix.enable`. A direct-only or QEMU-only
opt-in can lack scratch even with guest Nix enabled. Without scratch,
direct init provides no managed writable-Nix overlay or tmpfs fallback.
The separate OCI-image guest-Nix wrapper DOES have a tmpfs fallback;
its RAM cost must not be attributed to direct init. Podman uses its
image-owned store, not the whole host store.

### Other behavior to account for

| Concern | Implemented behavior / qualification |
| --- | --- |
| git trust | Direct krun registers `/etc/mysbx/gitconfig` and `/etc/gitconfig` in memory; Podman binds sidecar files and sweeps stale ones. This removes sidecar lifecycle work, not proof of feature parity. |
| `network = false` | Direct krun disables implicit vsock plus unshares the launcher's netns; Podman supplies an empty container netns, from which TSI dials. QEMU omits the NIC plus unshares the netns. These are distinct mechanisms, not a security ranking. |
| allowlists / `egress` | All three VM backends refuse domain/port allowlists. TSI and SLiRP do not implement the required policy hooks. `egress`, including `proxy-only`, is not a schema key and strict parsing rejects it on every backend. |
| waypipe | Direct krun currently accepts it IN ERROR: a host client can start, but there is no guest server/transport. `podman-krun` and QEMU explicitly refuse it. Host Unix sockets are not forwarded through virtiofs. |
| resources | CPU and RAM set VM vCPU/RAM sizes, independent of cgroups, not host-process controller limits. Direct krun/QEMU refuse PID limits. Podman-krun warns and drops configured `pids-limit`, but refuses an explicit `MYSBX_PODMAN_PIDS_LIMIT` override. |
| nested Podman | `krun.nestedPodman.enable` provisions the OCI image for `podman-krun`. It is not wired into the direct rootfs/PATH; no direct parity proof is recorded. |
| detach | Direct krun/QEMU accept bare interactive detach, but the live parallel/detach matrix is unrecorded. Podman still refuses it, although unique per-run container names already exist. |

VM CPUs must be whole vCPUs, at least one; RAM must be whole MiB and
more than 128 MiB. Resource sizing and accepted code paths are not
claims of live coverage for all combinations.

## Performance: what the record actually measures

The earlier claim that no Podman timing existed was false. The
[2026-10-14 f13 entry](krun-validation-log.md#2026-10-14-host-f13--the-pwd-parity-fix-live-bd-myconfig-mr4)
(`krun-validation-log.md`, lines 85–99) records the seven-probe suite
passing on BOTH variants: **direct krun 25 min 42 s; podman-krun
12.7 s**. These are whole smoke-suite durations, not isolated VM boot
times. They expose a problem in that direct run, not a general backend
speed ranking.

The early **282 ms warm direct-krun spike** on f13 (probe 4 of
`nix/krun-direct-spike.sh`, bd myconfig-dak.1) used a different path
and workload. The [QEMU log](microvm-qemu-validation-log.md) records
about **3.1 s** for a 2026-10-25 KVM one-shot including teardown.
Neither is directly comparable to the later smoke suites or sufficient
to rank current backends.

The later direct-krun log also records store-walk slowdowns and a
2026-10-24 silent mid-walk failure after long-running work and scratch
ENOSPC failures. Host-side evidence was not collected before restart;
file-descriptor retention is a hypothesis for that silent failure,
not a confirmed root cause. Separate QEMU virtiofsd processes move
file-server descriptor accounting out of the launcher; they do NOT
prove that server-side retention is bounded.

Fewer OCI preparation layers, direct block attachment and in-memory
trust files are useful architectural reasons to keep the direct path.
They do not establish lower end-to-end cost: store selection, database
copy/reconcile, store walks, image loading and scratch initialization
also matter. In host mode, existing host paths can avoid substitutions;
baked and image modes may need new paths fetched into disposable
scratch. This depends on contents and workload, not just the VMM.

## Validation state

The following are historical records, not tests rerun for this page:

| Surface | `podman-krun` | Direct `krun` / QEMU context |
| --- | --- | --- |
| static/golden checks | argv differences versus gVisor are pinned | launcher/spec goldens and rootfs/build checks exist; static checks do not boot a VM |
| scripted smoke | all 7 probes PASS, f13, 2026-10-14 | direct: all 7 PASS, 'thing' 2026-10-04 and f13 2026-10-14; QEMU: all 11 PASS, 'thing' 2026-10-25 |
| guest Nix | manual runbook exists; full guest-Nix acceptance not recorded here | direct host-store run on 'thing', 2026-10-02 reached the same genuine flake error as the host, establishing the Nix chain, not a clean flake check; host-db reconcile/substitution/build probes also recorded on f13, 2026-10-14 |
| scratch | manual runbook exists; full scratch acceptance not recorded here | direct host-store ext4/overlay PASS on 'thing', 2026-10-02; not baked-store verification or QEMU scratch-stress coverage |
| baked store | not this backend's store mode | complete live baked acceptance is still unrecorded for the direct VMs |
| detach / nested Podman | provisioning/refusals as above, not parity evidence | no complete live direct detach matrix or nested-Podman parity proof |

Sources: [krun log](krun-validation-log.md),
[krun runbook](krun-live-validation.md),
[QEMU log](microvm-qemu-validation-log.md), and
[detach validation gap](detach-validation-log.md). `doctor` probes tools,
pins and limited startup behavior; it does **not** boot a direct VM.
QEMU's recorded smoke/interactive success does not establish baked,
scratch-stress, nested-Podman or detach coverage.

## Conclusions and further acceptance

Direct krun removes OCI preparation and can attach native block disks;
Podman-krun retains its image provisioning path, including opt-in nested
Podman. Both remain distinct supported choices. The audited behavior
and mixed historical evidence establish neither blanket feature parity
nor a performance/security winner. D8's successor intent is history,
not grounds to retire `podman-krun` on alleged parity or speed. Recording
one more timing is not the sole outstanding acceptance item.

For a meaningful follow-up on a KVM host:

1. Run the existing `nix/krun-live-validation.sh <repo> krun` and
   `nix/krun-live-validation.sh <repo> podman-krun` with the wrapped
   `MYSBX` selected explicitly; QEMU has its own
   `nix/microvm-qemu-live-validation.sh <repo>`. Record revision, host,
   image/rootfs/kernel pins, VM sizes, network, effective store and
   actual scratch presence. Do not infer baked coverage from a pass.
2. Follow the [baked-store handoff](design/guest-store.md#handoff-the-live-validation-this-sandbox-cannot-run)
   for both direct VMs: with/without scratch, db source, absence of the
   guest host-store share, immutable backing versus writable copy-up,
   missing-path behavior and explicit/default store selection. Current
   missing-path diagnosis and network/Nix warnings are absent, not
   expected passing features.
3. Time the SAME isolated one-shot workload separately from whole
   suites, Nix initialization/substitution and image loading. Report
   warm/cold conditions and repeated runs, not a spike-versus-suite
   comparison. During long store-walk stress collect host VMM AND
   file-server fd counts/limits and failure evidence before restart
   (the krun runbook's collect-before-kill procedure).
4. Validate the features needed for any migration, including nested
   Podman provisioning and the live detach/parallel-run matrix. Do not
   count direct waypipe's erroneous acceptance as implemented display
   support or the Podman name uniqueness as lifting its detach refusal.

These are outstanding live checks, not evidence that this documentation
update executed them.
