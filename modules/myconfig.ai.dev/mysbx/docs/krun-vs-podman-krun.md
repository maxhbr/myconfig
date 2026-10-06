<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `krun` vs `podman-krun`: feature completeness and performance

Status: snapshot, checked against the tree at the time of writing
(2026-10). The design decisions live in
[`design/backends.md`](./design/backends.md): `podman-krun` is D2,
the direct `krun` backend is D3, and D8 is the closing decision that
declares `krun` the successor. This document is the comparison D8
asks for, gathered in one place.

## The two backends

Both are KVM microVM backends: each run boots a libkrunfw guest kernel
under `/dev/kvm`, rootless, with mounts over virtio-fs. They differ in
how many layers stand between mysbx and the VM, and in what the guest
sees.

| | `podman-krun` (D2) | `krun` direct (D3) |
| --- | --- | --- |
| layers to the VM | mysbx → podman → crun (OCI config) → libkrun → VM | mysbx → bwrap → launcher → libkrun → VM |
| filesystem contract | crun prepares an OCI rootfs host-side; libkrun shares it | the spec's virtiofs shares ARE the guest layout; a plain-directory rootfs |
| toolchain | baked into an OCI image, loaded per host (`mysbx podman-load-image`) | the whole host `/nix/store` shared read-only — no image, no per-toolchain build |
| network | TSI via crun's krun handler | TSI directly; `network = false` kills the vsock AND bwrap unshares the netns |
| nix scratch | loop-mount over virtio-fs (bd myconfig-0pi: crun's krun handler cannot attach disks) | real virtio-blk device via `krun_add_disk2` (no loop module, no losetup race) |
| git trust | per-run sidecar files bound over virtio-fs, swept at startup | in-memory overlay files on a virtual device (`krun_fs_add_overlay_file`) — no sidecar debris |
| image | one OCI image per toolchain pin, loaded per host | none — the ro host store share IS the visibility |

The `podman-krun` argv is the `podman-gvisor` argv with the runtime
swapped to crun+libkrun plus an enumerated set of differences (the
`run.oci.handler=krun` annotation, `--group-add=keep-groups`, dropped
`--cap-drop`/`no-new-privileges`, limits as `krun.cpus`/`krun.ram_mib`
annotations). The direct `krun` backend drives libkrun through a small
launcher binary (`krun-rs`, packaged by `nix/krun-launcher.nix`) that
runs under bwrap so the host-side virtiofs server can only open what
the bwrap argv left visible.

## Feature completeness

`krun` is the more feature-complete backend. backends.md D8 states
this directly:

> no new feature work lands on `podman-krun` — every krun-behavior
> fix lands on the direct backend first.

Everything `podman-krun` does, `krun` covers with the same verified
semantics and one fewer layer. What `krun` adds:

1. **No OCI image dependency.** `podman-krun` requires
   `mysbx podman-load-image` per rebuild and bakes the toolchain into
   the image. `krun` shares the host store read-only — the same
   visibility the bwrap tier grants with `--ro-bind /nix/store` — so
   per-run toolchains need zero config surface: whatever the host
   builds is already visible.

2. **Real disk attachment for the nix scratch.** The direct launcher
   owns the libkrun context, so the scratch disk is a genuine
   virtio-blk device (`krun_add_disk2`). `podman-krun` is stuck with
   a loop-mount workaround (`losetup` + `mkfs.ext4` inside the guest)
   because crun's krun handler parses no disk annotation and never
   calls `krun_add_disk` (verified against crun 1.30, bd
   myconfig-0pi).

3. **Cleaner git trust model.** `krun` registers the two trust
   configs (`/etc/mysbx/gitconfig` and `/etc/gitconfig`) as in-memory
   overlay files on a dedicated virtual device — no host file, no
   sidecar directory, no stage slot, no init placement. `podman-krun`
   writes per-run sidecar files into `<sidecar>/gittrust/<pid>/`,
   bound over virtio-fs, and must sweep stale ones at startup.

4. **Stronger `network = false`.** `krun` calls
   `krun_disable_implicit_vsock` (no vsock device at all — stricter
   than an empty netns, which still has loopback) AND bwrap
   `--unshare-net`s the launcher process. Defense in depth: neither
   the VMM nor the guest could dial out. `podman-krun` relies on the
   container's empty netns only.

5. **Guest nix validated end-to-end.** A real `nix flake check` of a
   large flake ran inside the direct-krun guest on host 'thing'
   (2026-10-02, all stages PASS — see
   [`krun-validation-log.md`](./krun-validation-log.md) §5). The
   podman-krun variant's guest nix was never driven that far: its
   live validation runbook (`krun-live-validation.md` §2.3) is
   designed but not recorded as passed.

### Shared limitations

Both backends inherit the same gaps from the virtio-fs + TSI model:

| limitation | why | bead |
| --- | --- | --- |
| no waypipe display | AF_UNIX sockets do not cross virtio-fs (passes inodes, not live socket objects) | bd myconfig-ef6 |
| no network allowlist | TSI is an unfiltered proxy — any AF_INET connect the guest makes, the VMM dials — no per-domain/port hook | bd myconfig-6di.5.5 |
| no `egress = "proxy-only"` | not a schema key yet; when it lands, both backends share the pasta gap | bd myconfig-mo3.2 |

## Performance

### Measured

The only measured boot number in the repo is the direct `krun` spike:
**282 ms warm boot** on f13 (probe 4 of `nix/krun-direct-spike.sh`,
bd myconfig-dak.1). No `podman-krun` timing has been recorded.

The design docs frame the remaining validation as a head-to-head
(backends.md D8):

> the variant STAYS until the head-to-head on f13 records its timing
> against the spike's numbers (`nix/krun-direct-spike.sh` probe 4 vs
> `time mysbx run -- true` under `backend = "podman-krun"`)

That head-to-head is the open acceptance item —
`krun-validation-log.md` has no podman-krun timing entry yet.

### Architectural argument

Even without the head-to-head numbers, the architecture gives `krun`
a structural advantage on every axis that matters for per-run cost:

| cost axis | `podman-krun` | `krun` |
| --- | --- | --- |
| layers to the VM | podman + crun + OCI config preparation + libkrun | bwrap + launcher + libkrun |
| image load | `mysbx podman-load-image` per rebuild (loads the full OCI image) | none — no image exists |
| scratch setup | `losetup --find --show` + `mkfs.ext4` inside the guest, every run | `krun_add_disk2` attaches a real virtio-blk device; the guest still `mkfs.ext4`s it once, but no loop detour |
| toolchain resolution | image-baked paths only; a missing tool is a rebuild | the host store is already visible — a repo runs any store path directly |
| nix substitution | per-run: the image's registered closure is there, but every genuinely new path is re-fetched, and state is rebuilt on guest tmpfs each boot | the host store share carries the full closure; the overlay-on-scratch only adds new paths, and the db copy registers existing ones as valid so nix never re-adds |

The nix scratch cost deserves emphasis. On `podman-krun`, the guest
nix state lives on guest tmpfs by default — the docs note "each run
substitutes again" and warn that "the 1024 MiB crun default is too
small for dev shells." The disk-backed scratch (bd myconfig-0pi) is
the fix, but on `podman-krun` it arrives through a loop mount over
virtio-fs. On `krun`, the scratch is a native virtio-blk device: the
guest kernel's own ext4, where chown and overlay xattrs work natively
without the virtiofs xattr surface.

## Validation state

| | `podman-krun` | `krun` |
| --- | --- | --- |
| golden tests | yes (the krun golden pins the enumerated differences vs gvisor, bd myconfig-6di.5.4) | yes (the launcher flags golden, bd myconfig-dak.3) |
| scripted smoke (`krun-live-validation.sh`) | designed (§1 of the runbook); not recorded as passed | PASS on 'thing' 2026-10-04, all 7 probes (§1 of the log) |
| guest nix end-to-end | runbook designed (§2.3); not recorded | PASS on 'thing' 2026-10-02 — `nix flake check` ran to completion (§5 of the log) |
| scratch disk | runbook designed (§2.3.1); not recorded | PASS on 'thing' 2026-10-02 (§4 of the log) |
| head-to-head timing | open | open (the 282 ms spike number stands alone) |

## Verdict

`krun` (the direct-libkrun backend) is both more feature-complete and
more performant. The design has already declared it the successor
(backends.md D8): `podman-krun` is kept only until the head-to-head
timing pass on f13 retires it, and no new feature work lands on the
podman variant. The single open acceptance item is recording that
timing comparison — the architectural evidence and the live
validation record already point one way.

### What retiring `podman-krun` looks like

Per D8, once the head-to-head timing is recorded, retirement is a
small PR, not a redesign:

- drop the crun+libkrun runtime pin
- drop the podman image build for the krun variant
- drop the `podman-krun` arm of `podman_checks`
- the direct backend's launcher, rootfs and spec builder stay
