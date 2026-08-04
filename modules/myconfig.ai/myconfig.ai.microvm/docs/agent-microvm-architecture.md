<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — architecture

How the pieces fit together. Option-level reference lives in
[`agent-microvm.md`](./agent-microvm.md); procedures in the
[operator guide](./agent-microvm-operator-guide.md); the threat model in the
[security model](./agent-microvm-security-model.md).

## Module map

| File | Responsibility |
| --- | --- |
| `default.nix` | option namespace, assertions, deprecation/migration decisions; instantiates the shared module args (`agentRegistry`, `agentNetwork`, `agentResourceClasses`) |
| `agents.nix` | **the** supported-agent registry (packages, guest env, interactive + batch invocation, declared state) |
| `slots.nix` | the deterministic slot table derived from the resource classes |
| `network-profiles.nix` | the network capability table (`offline`/`proxy-only`/`package-access`/`internet`) |
| `guest.nix` | microvm.nix host integration, one prebuilt VM per slot, the guest NixOS config, `agent-run`, host-side hypervisor limits |
| `guest-home.nix` | allowlisted copy of the host operator's dotfiles into the guest (home-manager inside the guest) |
| `network.nix` | private bridge, TAP enslavement + L2 isolation, firewall chains, NAT, bridge-only LiteLLM forwarder |
| `hostkeys.nix` | per-slot SSH host identities + the host `known_hosts` |
| `job.nix` | versioned batch job format, per-slot job dirs, guest `agent-job` runner + hardened unit |
| `state.nix` | opt-in, task-scoped agent state and the guest-side linker |
| `launcher.nix` | the host `agent-microvm` CLI: allocation, clones, mounts, lifecycle, events |
| `workmux.nix` | registers `microvm-<agent>` agents with workmux |
| `secrets.nix` | agenix stub for the dedicated SSH private key |

## Prebuilt slot pool

`resourceClasses` (`{ count; vcpu; memoryMiB; }` per class) is expanded by
`slots.nix` into a flat table. Each slot gets, deterministically:

```text
name         agent-<class>-<i>          (also the guest hostname)
tap          vm-<class>-<i>             (<= 15 chars, asserted)
mac          02:00:00:83:00:<10+idx>
ip           192.168.83.<10+idx>
vsock cid    8300+idx                   (reserved; see below)
sizing       the class's vcpu / memoryMiB
```

`<idx>` is the pool-wide index (classes walked alphabetically). One
`microvm.vms.<name>` — i.e. one fully evaluated, prebuilt guest — exists per
slot. **There is no per-job Nix evaluation:** a job never changes the guest
closure, only the *content* of the paths mounted into it.

Every per-slot host-side directory is keyed by the slot **name**:

```text
/var/lib/microvms/<slot>/workspace          bind target -> the task's clone
/var/lib/agent-microvms/hostkeys/<slot>/    the slot's SSH host identity
/var/lib/agent-microvms/jobs/<slot>/        the batch job (spec, prompt, out/)
/var/lib/agent-microvms/state/slots/<slot>  bind target -> task-scoped agent state
```

## Workspace indirection

The guest's `/workspace` share source is a **fixed** per-slot path, because the
guest config is prebuilt. The launcher makes that path *mean* the task's
workspace:

```text
git clone --no-local <repo> /var/lib/agent-microvms/workspaces/<task>
mount --bind          …/workspaces/<task>  /var/lib/microvms/<slot>/workspace
virtiofsd            (that path)  ->  guest /workspace
```

So the guest only ever sees one repository — a **standalone clone** (no
hardlinks, no alternates, no shared git metadata with your checkout) — and the
clone survives everything except an explicit `workspace-remove`. The same
indirection is reused for the job directory and for task-scoped agent state.

virtiofsd passes ownership through unchanged, so the launcher `chown -R
guestAgentUid:guestAgentGid`s the clone (default `1000:1000`). On a workstation
that is the primary interactive user, so the same tree is agent-owned inside the
guest and operator-owned on the host — no privileged host id is ever reachable
from the guest, and only the per-task clone is chowned.

## Self-contained guest store

`microvm.storeOnDisk` stays at its default, so each guest boots its **own**
erofs store image built with the system. The host `/nix/store` is never shared,
there is no Nix daemon socket in the guest, and the guest cannot build or fetch
anything — every binary it can run was baked in at host build time. Agent CLIs
come from the registry, so "which agents exist" is a build-time fact.

## Network path

```text
guest agent
  → 127.0.0.1:4000                (guest-side systemd-socket-proxyd)
  → 192.168.83.1:4000             (bridge-only host endpoint, BindToDevice=agentbr0)
  → 127.0.0.1:4000                (the real, loopback-only LiteLLM proxy)
  → upstream provider             (the only place a real API key exists)
```

The guest presents the *same* loopback address the host does, so the operator's
copied agent configs work verbatim. Everything else on the bridge is denied by
the `AGENT_MICROVM_INPUT/_FORWARD/_OUTPUT` chains, whose content is rendered from
the selected [network profile](./agent-microvm.md#network-profiles); guest↔guest
traffic is additionally impossible at layer 2 (`bridge link … isolated on`).

## Credential boundary

- Upstream API keys exist **only** in the host LiteLLM proxy. The guest gets
  placeholder values plus endpoint URLs.
- The guest has no host home, no `~/.ssh`, no SSH/GPG agent socket, no cloud
  credentials, no container sockets, no Nix daemon socket, no host D-Bus.
- Dotfiles are copied through an **allowlist** of already-rendered store paths
  (`guestDotfiles.*`), never by re-evaluating host home modules, so
  secret-bearing paths cannot be dragged in by accident.
- The per-slot SSH **host** key is delivered read-only and root-only; the guest
  agent user cannot read it, so it cannot impersonate its own slot to the
  operator either.

## Interactive execution path

```text
agent-microvm run --attach --agent <a> --name <task> --repository <repo>
  validate (task name, repo, branch, agent, class)
  → allocate a free slot of the requested class   (flock, token, marker)
  → git clone --no-local                          (standalone clone)
  → mount --bind clone -> slot workspace
  → clear any stale job / agent-state
  → systemctl start microvm@<slot>
  → wait for SSH readiness (strict host-key verification)
  → ssh -t … agent-run <agent>                    (guest-side dispatch)
  → on exit: stop VM, unmount, drop marker; KEEP the clone
```

Detached mode (`run` without `--attach`) stops after starting the VM and leaves
the slot allocated for `ssh`/`console`/`stop`.

## Batch execution path

```text
agent-microvm submit --agent <a> --prompt-file <f> --timeout <s> …
  … same allocation/clone/mount …
  → write /var/lib/agent-microvms/jobs/<slot>/{spec.json,prompt.md}  (root:root 0444)
  → systemctl start microvm@<slot>
  guest: agent-job.service (ConditionPathExists=spec.json)
     validate spec → write out/result.json: starting → running
     → timeout(1) <timeout> <agent batch argv>       (registry-generated)
     → write out/result.json: completed | failed | timed-out
  host: poll result.json until terminal or deadline (timeout + grace)
  → archive result to state/results/<task>.json
  → stop VM, unmount, clear job data; KEEP the clone
  → exit 0 / 1 / 124 / 70
```

The prompt never travels as an argument and never enters the Nix store; the spec
cannot name an executable (the agent is resolved through the registry).

## State persistence

Three lifetimes, by design:

| Scope | Lifetime |
| --- | --- |
| guest root, `/home/agent` | **disposable** — tmpfs, rebuilt every boot |
| `/workspace` | the task's standalone clone; kept until `workspace-remove` |
| declared agent state | **opt-in** per run (`--persist-agent-state`), scoped to `<task>/<agent>`, bind-mounted only while that slot runs |

The guest-side linker symlinks `~/<dir>` → the share for each declared directory
the host prepared, and refuses to replace a non-empty directory.

## Allocation ownership

Each allocation marker (`/var/lib/agent-microvms/slots/<slot>/session.json`)
records the task, slot, workspace, VM unit, mode (`attached` / `detached` /
`batch`), the owning launcher's **pid plus that pid's start time** (so a recycled
pid cannot impersonate the owner) and a **random allocation token**. Operations
that act on a slot they did not allocate (`cancel`, `recover`) compare the
**token**, never just the slot name, so a slot that has meanwhile been
re-allocated to another task is never touched.

## Resource classes and limits

Sizing is a property of the class (prebuilt), and limits are enforced twice:

```text
guest  agent-job:      CPUQuota = vcpu*100%, MemoryMax = classRAM - headroom,
                       TasksMax, RuntimeMaxSec
host   microvm@<slot>: MemoryMax = classRAM + hypervisor overhead,
                       TasksMax, CPUWeight/IOWeight (relative, yield to the operator)
```

Timeouts are enforced three times: `timeout(1)` per job in the guest, the guest
unit's static `RuntimeMaxSec` ceiling, and the host's own deadline
(`timeout + job.gracePeriodSeconds`).

## Control-channel identities

- **Interactive/SSH**: one stable ed25519 host key per slot, delivered read-only,
  pinned in a host `known_hosts`; the launcher verifies strictly
  (`StrictHostKeyChecking=yes`). `agent-microvm-hostkeys.service` generates and
  keeps them on the host — not in the world-readable Nix store, and not as
  agenix secrets (they are host-local, per-slot, regenerable identities):

  ```text
  /var/lib/agent-microvms/hostkeys/<slot>/ssh_host_ed25519_key      root:root 0400
  /var/lib/agent-microvms/hostkeys/<slot>/ssh_host_ed25519_key.pub  root:root 0444
  /var/lib/agent-microvms/known_hosts                               root:root 0444
  ```

  The guest generates no host keys of its own
  (`services.openssh.generateHostKeys = false`), so the pinned identity is the
  only one it can present; `known_hosts` holds public keys only, so a non-root
  operator can verify strictly too.
- **VSOCK**: a unique CID is *reserved* per slot (`8300+idx`) for a future
  noninteractive control channel. It is deliberately **not** yet passed to
  `microvm.vsock.cid`, because that flips `microvm@<slot>` to `Type=notify` — a
  startup change that can only be validated by booting on real KVM.
