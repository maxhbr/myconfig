<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Current backend and option comparison

This page describes implemented behavior, not a roadmap. The source audit
used commit `7846d39c36` and the pinned nono **0.80.0**. No live sandbox was
launched for that audit. Historical live results are identified separately
below; static tests and `doctor` do not establish live enforcement.

For design rationale, see [backend decisions](design/backends.md) and
[configuration decisions](design/config.md). The
[older-tier comparison](feature-comparison.md) also contains historical
snapshots; use this page for the current six-backend comparison.

## Backend families

| Backend | Execution boundary | Default store view | Network path when enabled |
| --- | --- | --- | --- |
| `bubblewrap` | Namespaces using the host kernel | Whole host `/nix/store`, read-only | Host network namespace |
| `nono` | The same bubblewrap view, then Landlock and applicable seccomp filters | Whole host store, read-only | Host namespace; optional nono port policy or hostname proxy |
| `podman-gvisor` | Rootless Podman with the gVisor userspace kernel | Image-owned store; no implicit host-store bind | Podman networking, normally pasta |
| `podman-krun` | Rootless Podman with crun's libkrun VM handler; payload is guest root | Image-owned store; no implicit host-store bind | Guest socket proxy through the container network namespace |
| `krun` | Host bubblewrap, then an in-process libkrun VM and file server; payload is guest root | Host or baked store, as selected below | libkrun TSI socket proxy through the launcher's namespace |
| `microvm-qemu` | Host bubblewrap, QEMU and separate virtiofsd file servers; payload is guest root | The same host/baked choice and guest init as direct krun | SLiRP user-mode NAT |

These are different boundaries, not a proven security ranking. VM host
processes run as the invoking user. A guest-kernel, VMM or file-server
escape remains relevant. QEMU's virtiofsd uses `--sandbox=none` and relies
on the enclosing bubblewrap view. Baked mode narrows the store delivered
to the guest; the host-side VM wrapper still exposes the whole host store
for its own runtime dependencies.

Both direct VM backends require KVM in the normal mysbx path. QEMU's
launcher argument `kvm:tcg` does not make software emulation a supported
mysbx fallback. Direct krun uses libkrun's kernel; QEMU has a separate
kernel pin.

Enabling a backend in Nix makes it available; it does not select it.
Selection is `backend` or `--backend`. The generated user configuration
normally selects `bubblewrap`. The Nix option prefix is
`myconfig.ai.dev.mysbx`.

## Supported modes and refusals

"Accepted" below is a code path, not a claim that every combination has a
recorded live test. Installed tools, image contents, runtime pins and
kernel support still matter.

| Backend | Domain/port allowlists | Waypipe display | Detached interactive run | Multiplexers |
| --- | --- | --- | --- | --- |
| `bubblewrap` | Refused | Supported with host pin | Accepted | Configured entries, including Orca |
| `nono` | Supported; see policy caveats below | Supported with socket grants | Accepted | Orca refused |
| `podman-gvisor` | Refused | Requires host client and image server | Refused | Image entries; Orca refused |
| `podman-krun` | Refused | Refused | Refused | Image entries; Orca refused |
| `krun` | Refused | **Not implemented, but currently accepted in error** | Accepted; no recorded live detach matrix | Direct guest entries; Orca refused |
| `microvm-qemu` | Refused | Refused | Accepted; no recorded live detach matrix | Direct guest entries; Orca refused |

Direct krun's waypipe acceptance is a known implementation gap: it can
start the host client without a guest server or transport. Do not select
that combination expecting graphical output.

`run -- CMD` does not start a multiplexer, even if one is configured.
`MYSBX_MULTIPLEXER` still reports the configured choice. Waypipe applies
to one-shot commands on backends that implement it. Detach is restricted
to the bare interactive form.

Podman container names already include the run ID. The detach refusal
still exists, but the old explanation that unique names are missing is
obsolete. Accepted detach paths also lack the live parallel-run matrix
required by the [detach design](design/detach.md); see the explicit
[validation gap](detach-validation-log.md).

## Network policy and endpoint access

`network = false` wins if either configuration layer sets it. It selects
an isolated network namespace or no network device, and nono also gets
`--block-net`. A nonempty nono allowlist with networking disabled is
refused.

Allowlist entries from the user and repository layers are additive and
deduplicated. A repository list does not narrow the user list. The
`egress` key, including `egress = "proxy-only"`, is **not implemented**
and is rejected during strict parsing on every backend.

| Nono configuration | Meaning in pinned 0.80.0 |
| --- | --- |
| No allowlist | Unrestricted IP networking; pathname Unix-socket mediation still installs a seccomp filter |
| `connect-ports` without domains | Destination-port grants without destination-host restrictions; DNS is not added automatically |
| `allow-domains` without connect ports | Hostname-filtering proxy; the default supervisor denies direct remote connections |
| Domains plus connect ports | Not a domain-and-port intersection. Landlock adds independent port grants, but the proxy supervisor still denies direct remote connections. Proxy bypass variables derived from the ports can cause connectivity failures. |
| `listen-ports` alone | Refused; it is not a standalone egress policy |

Do not interpret mixed domain/port policy as either "these domains on
these ports" or a demonstrated arbitrary-host port bypass. A domain
entry alone also does not constrain the proxy's upstream port. The
current combined-policy behavior needs live acceptance testing before
it is recommended as a profile.

The current payload environment builder preserves configured proxy
variables, despite the older design's reservation claim. They can
overwrite nono's injected proxy environment and break connectivity. This
does not reconfigure the Landlock/seccomp restrictions already installed
before payload execution, and is not a demonstrated policy bypass.

Host-loopback endpoints are transport-dependent: shared host networking,
Podman's gateway mapping, libkrun TSI and QEMU's SLiRP gateway are not
interchangeable. The shared Podman dispatch consumes
`MYSBX_PODMAN_ENV` and `MYSBX_PODMAN_PASTA_SPEC` for **both** Podman
backends, not just gVisor. A host TCP endpoint probe does not establish
that a sandboxed client can reach it under the selected policy.

The host Nix-daemon directory is supplied implicitly only to bubblewrap
and unrestricted-network nono runs. Nono removes it under an allowlist.
The other backends do not receive it implicitly. Socket-bearing custom
mounts are not subject to a universal host-socket ban; dedicated guards
cover the daemon path under denied or nono-allowlisted networking.
Host-to-guest Unix sockets are not forwarded by the direct VMs' virtiofs
shares.

## Resource limits

| Backend | Memory and CPU | PID limit |
| --- | --- | --- |
| `bubblewrap`, `nono` | Warned and ignored | Warned and ignored |
| `podman-gvisor` | Podman flags only when cgroups are enabled; rootless default `ignore-cgroups` warns and omits them | Same cgroup requirement |
| `podman-krun` | VM RAM/vCPU sizing, independent of cgroups | Configured value warns and is dropped; an explicit `MYSBX_PODMAN_PIDS_LIMIT` override is refused |
| `krun`, `microvm-qemu` | VM RAM/vCPU sizing, independent of cgroups | Refused; the earlier generic warning may misleadingly say ignored |

VM CPU counts must be whole vCPUs, at least one. Memory must resolve to
whole MiB and be greater than 128 MiB. The direct VM builder defaults to
2 vCPUs and 2048 MiB when no effective values are supplied; generated
host configurations can override those defaults. VM sizing is not a
host-process resource-controller limit.

Resource settings use repository-over-user precedence. The Podman
`MYSBX_PODMAN_{MEMORY,CPUS,PIDS_LIMIT}` overrides are a separate input
path, which explains the configured-versus-environment PID distinction.

## Direct VM store and writable-Nix choices

These choices apply to `krun` and `microvm-qemu`, not the Podman image
backends. Explicit `store` wins. If it is omitted, a nonempty
`MYSBX_KRUN_STORE_DISK` pin selects `baked` for these two backends;
otherwise the result is `host`.

| Store | Scratch attached | Guest result |
| --- | --- | --- |
| `host` | No | Whole host store read-only; no managed writable-Nix setup |
| `host` | Yes | Host store as read-only lower layer, private writable overlay and Nix state; with networking, copy and reconcile the host database |
| `baked` | No | Immutable EROFS tool closure and baked database; no managed writable-Nix setup |
| `baked` | Yes | Immutable EROFS lower layer, private writable overlay and copied baked database; no host database reconciliation |

The EROFS image is mounted at `/nix`; its `store` subtree provides the
lower store. With scratch, writes in the merged view can copy up into
private storage. Read-only backing storage does **not** mean every write
to an existing guest-visible store path fails.

Scratch is a per-run disposable sparse disk, not a persistent store cache.
Both direct backends use the same guest initialization. The Nix binary
is always present in their tool environment; `krun.nix.enable` concerns
writable-Nix provisioning, not binary availability.

There is a module-wiring gap: `krunScratchSize` is pinned only when both
`podman.enable` and `krun.nix.enable` are true. Direct-only or QEMU-only
configurations can therefore enable guest Nix without receiving scratch.
The direct guest init has no managed RAM fallback without that disk.
The separate Podman-image guest-Nix wrapper does have a tmpfs fallback;
do not transfer that promise to the direct init.

Baked-store contents are the wrapper's tool closure plus configured
extra roots. Arbitrary host-built executable paths may be missing.
Automatic per-repository closure roots and improved missing-path
explanations are not implemented. Podman likewise needs tools in its
image, and the image must be loaded before use.

`nestedPodman.enable` currently provisions the OCI image for
`podman-krun`. It is not wired into either direct VM's rootfs and PATH.
Direct krun cannot therefore be described as covering every provisioned
Podman-krun feature.

## Filesystem state, sessions and sockets

Live runs have the repository writable at its original path, approved
Git metadata/worktrees, configured mounts and sidecar-backed
`state-dirs`. They also receive the repository SSH key. Clone sessions
use their own clone/worktrees, force configured mounts read-only,
reject `--rw`, and omit persisted state and the repository SSH key.

Default multiplexer sockets are private per run, but trusted
configuration can weaken that separation. In particular,
`state-dirs = [".config/herdr"]` is accepted and persists herdr's API
socket directory on bubblewrap/nono. The tmux socket guard does not
protect every multiplexer API directory. This contradicts the older
blanket no-persistence claim; it is not evidence of an untrusted-repo
escape.

The tmux socket directory is normally `/mysbx-home/.mysbx-tmux`;
`podman-krun` uses `/dev/shm/mysbx-tmux`. Herdr has its separate
`/mysbx-home/.config/herdr` API directory. Direct VM sockets are guest
local, not usable host sockets forwarded through virtiofs.

## Validation and choosing a backend

The [direct-krun log](krun-validation-log.md) records guest-Nix and smoke
probes, later store-walk slowdowns, and a later silent failure. Its
file-descriptor-retention explanation is a hypothesis, not a confirmed
root cause. The [QEMU log](microvm-qemu-validation-log.md) records eleven
KVM probes and manual interactive use. It does not establish baked-store,
scratch stress, nested-Podman or detach coverage. QEMU transports initial
terminal size but not mid-session resize events.

The early 282 ms krun spike, later direct/Podman smoke-suite durations,
and QEMU one-shot durations measure different workloads. They do not
support a current speed ranking. Separate virtiofsd processes move file
descriptor accounting out of QEMU's launcher; they do not prove that
server-side descriptor retention is bounded.

The [baked-store ledger](design/guest-store.md) still lacks a complete
recorded live acceptance run. Existing host-store smoke logs cannot
validate it retrospectively. `doctor` checks tools, pins and limited
startup behavior. The direct VM probes do not boot a VM, and nono's host
endpoint probe runs outside the payload's allowlist.

Use bubblewrap for the simplest host-kernel interactive path. Use nono
when its network-policy mechanism is needed, after testing the exact
policy. Use a VM for guest-kernel compatibility, with explicit store,
scratch and image/rootfs provisioning choices. Do not retire
`podman-krun` based on the stale blanket parity or performance claims in
the earlier comparison.
