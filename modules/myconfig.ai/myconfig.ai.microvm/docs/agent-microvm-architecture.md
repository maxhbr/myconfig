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
| `default.nix` | option namespace, assertions, deprecation/migration decisions; instantiates the shared module args (`agentRegistry`, `agentNetwork`, `agentResourceClasses`, `agentCapabilities`) |
| `agents.nix` | **the** supported-agent registry (packages, guest env, interactive + batch invocation, declared state) |
| `slots.nix` | the deterministic slot table derived from the resource classes |
| `network-profiles.nix` | the network capability table (`offline`/`proxy-only`/`package-access`/`internet`) |
| `guest.nix` | microvm.nix host integration, one prebuilt VM per slot, the guest NixOS config, `agent-run`, host-side hypervisor limits |
| `config-seed.nix` | runtime, allowlisted configuration staging — the host-side stager, the per-slot read-only seed share and the guest-side seeding oneshot; the ONLY way a guest home is provisioned (guest home-manager activation was removed) |
| `session.nix` | **the** per-session layout table (paths, owners, modes, per-capability presence, the trust policy), the host tmpfiles rules, the pre-launch verifier and the `/workspace` bind mount |
| `network.nix` | private bridge, TAP enslavement + L2 isolation, firewall chains, NAT, bridge-only LiteLLM forwarder |
| `hostkeys.nix` | per-slot SSH host identities + the host `known_hosts` |
| `job.nix` | versioned batch job format, per-slot job dirs, the TRUSTED guest job controller, the UNTRUSTED guest worker unit, the guest-side permission assertions and the HOST-side result verifier |
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

Everything a slot needs lives in ONE writable per-session tree plus ONE
read-only tree (lightweight plan phase 4 — the historical four/five separate
shares are gone), so a slot has two virtiofsd instances:

```text
/var/lib/agent-microvms/sessions/<slot>/            root:root 0755  ONE writable share
  workspace/                                        agent           bind target -> the clone
  input/                                            root:root 0755  spec 0400, prompt 0444   [batch]
  controller/                                       root:root 0700  the AUTHORITATIVE result [batch]
  worker/                                           agent     0755  untrusted artifacts      [batch]
  worker-logs/                                      root:root 0755  root-opened worker logs  [batch]
  state/                                            agent     0755  bind target -> agent state
/var/lib/agent-microvms/sessions-ro/<slot>/         root:root 0700  ONE read-only share
  hostkeys/                                         root:root 0700  the slot's SSH identity  [interactive]
  config-seed/                                      root:root 0500  staged host configuration
```

The `[batch]` / `[interactive]` subdirectories exist only when the host selects
that capability (`capabilities`, lightweight plan phase 5). The two shares, their
tags, mount points and every owner/mode are the same either way: `session.nix`'s
table carries the per-capability marking, so the host tmpfiles rules, the
pre-launch verifier, the launcher's tree preparation and the tests follow it
without a second decision.

Guest side: `/run/agent-session` and `/run/agent-session-ro`, with
`/run/agent-session/workspace` bind-mounted to `/workspace` so `agent-run` and
every agent keep the path they expect. The owners and modes are unchanged from
the four-share layout; `session.nix` is the ONE place they are declared, and the
launcher enforces them with `agent-microvm-verify-session` before each launch.
What *does* change is the **mount** boundary — the agent-writable and the
root-owned directories now share one filesystem — which is analysed in
[the security model](./agent-microvm-security-model.md#the-one-genuine-security-delta-of-consolidation).

The guest units that depend on one of the two mounts say so with
`RequiresMountsFor=` on the **subpath** they actually use
(`/run/agent-session/state`, `/run/agent-session-ro/hostkeys`,
`…/config-seed`). That is deliberate and correct: systemd turns
`RequiresMountsFor=` into a dependency on the mount unit of *every* path prefix,
so naming the subdirectory pulls in the share's mount unit and documents which
part of the share the unit needs.

## Workspace indirection

The guest's `/workspace` share source is a **fixed** per-slot path, because the
guest config is prebuilt. The launcher makes that path *mean* the task's
workspace:

```text
git clone --local --no-hardlinks <repo> /var/lib/agent-microvms/workspaces/<task>
mount --bind          …/workspaces/<task>  /var/lib/microvms/<slot>/workspace
virtiofsd            (that path)  ->  guest /workspace
```

So the guest only ever sees one repository — a **standalone clone** (no
hardlinks, no alternates, no shared git metadata with your checkout) — and the
clone survives everything except an explicit `workspace-remove`. The same
indirection is reused for the job directory and for task-scoped agent state.

The bind targets are INSIDE the per-session tree
(`<runtimeRoot>/sessions/<slot>/workspace` and `.../state`), and the guest
reaches the clone through the `/workspace` bind mount of the one writable
share — the indirection itself is the historical one.

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

There are two, and the module resolves which one a host uses ONCE (in
`default.nix`, from the table in `network-profiles.nix`), handing it to every
consumer as `agentNetwork.transport` / `.transportCaps`.

**`tap`** — the default shape:

```text
guest agent
  → 127.0.0.1:4000                (guest-side systemd-socket-proxyd)
  → 192.168.83.1:4000             (bridge-only host endpoint, BindToDevice=agentbr0)
  → 127.0.0.1:4000                (the real, loopback-only LiteLLM proxy)
  → upstream provider             (the only place a real API key exists)
```

Everything else on the bridge is denied by the
`AGENT_MICROVM_INPUT/_FORWARD/_OUTPUT` chains, whose content is rendered from
the selected [network profile](./agent-microvm.md#network-profiles); guest↔guest
traffic is additionally impossible at layer 2 (`bridge link … isolated on`).

**`vsock`** — the `vsock` capability together with `networkProfile =
"proxy-only"` (lightweight plan phase 6, the literal objective). The guest then
has **no network interface at all**:

```text
guest agent
  → 127.0.0.1:4000                (guest-side socat, per connection)
  → AF_VSOCK CID 2, port 4000     (the host; cloud-hypervisor's per-VM mux socket
                                   <stateRoot>/<slot>/notify.vsock_4000)
  → 127.0.0.1:4000                (per-VM host forwarder → the loopback LiteLLM proxy)
  → upstream provider             (the only place a real API key exists)
```

No TAP device, no bridge, no static IP, no guest networkd/dhcpcd, no
`AGENT_MICROVM_*` chain and no bridge-only socket exist on such a host; the
host-side components are exactly one `agent-litellm-vsock-<slot>` socket +
service PER VM, each bound to a `root:kvm 0660` Unix socket inside that VM's own
state directory and destination-fixed to `127.0.0.1:<litellmPort>`. LAN, VPN,
metadata, DNS, other guests and other host ports are unreachable by
CONSTRUCTION rather than by firewall verdict, and one slot cannot use another
slot's model path (there is no shared namespace and no CID to spoof).

In BOTH cases the guest presents the *same* loopback address the host does, so
the operator's copied agent configs work verbatim — switching transport
reconfigures no agent.

## Credential boundary

- Upstream API keys exist **only** in the host LiteLLM proxy. The guest gets
  placeholder values plus endpoint URLs.
- The guest has no host home, no `~/.ssh`, no SSH/GPG agent socket, no cloud
  credentials, no container sockets, no Nix daemon socket, no host D-Bus.
- Dotfiles are copied through an **allowlist** of exact host paths staged at
  launch time (`configSeed`, plus a credential denylist — applied to both the
  path's own name and its resolved target — escape rejection and a host-side,
  per-session manifest), so secret-bearing paths cannot be dragged in by
  accident. Nothing is baked into the guest image and no host home module is
  re-evaluated inside the guest.
- The per-slot SSH **host** key is delivered read-only and root-only; the guest
  agent user cannot read it, so it cannot impersonate its own slot to the
  operator either.

## Capabilities: which halves a host builds

`capabilities` (default `[ "interactive" "batch" ]`) selects which of the two
execution paths below a host's guests carry — a SET over the ONE guest shape, not
a second profile axis (lightweight plan phase 5). `vsock` (lightweight plan
phase 6) is a THIRD token, default OFF: it adds a VSOCK control channel (the
`sshd-vsock@` unit) so a batch-only host can still be driven over VSOCK, and —
together with `networkProfile = "proxy-only"` — makes AF_VSOCK the MODEL
transport as well, which removes the guest's network interface entirely (see
[Network path](#network-path)). It is resolved once in
`default.nix` and handed to the sibling modules as `_module.args
.agentCapabilities`; each module then makes the decision in the ONE place that
already owns the concern:

| Module | What the capability set decides there |
| --- | --- |
| `session.nix` | which layout-table entries exist — hence the host tmpfiles rules, the pre-launch verifier, the launcher's `prepare_session`, the guest mounts and the tests. `hostkeys/` needs `interactive` OR `vsock` (the VSOCK sshd reuses the per-slot host identity) |
| `job.nix` | whether the guest module (controller unit, worker template, the three job programs), the worker's endpoint environment and the host result archive exist at all |
| `hostkeys.nix` | whether the per-slot key pair and the `known_hosts` database are provisioned (for `interactive` OR `vsock`) |
| `guest.nix` | whether the guest carries the interactive `agent-run` entry point; whether `microvm.vsock.cid` is set and the TCP `sshd.service` is suppressed (any host whose TCP sshd is unreachable — no `interactive`, or no network interface under the `vsock` transport) |
| `network.nix` | whether the host builds the bridge + TAP attach units + firewall chains + bridge-only socket (`tap`) or ONE destination-fixed AF_VSOCK forwarder per VM (`vsock`) — decided by the transport, which is itself decided by `vsock` + the profile |
| `workmux.nix` | whether the `microvm-<agent>` panes are registered |
| `launcher.nix` | which subcommands the ONE launcher accepts (`run` needs `interactive`; `ssh` needs `interactive` OR `vsock`; `submit`/`cancel` need `batch`; `console` is never gated), and which lines `usage` reports |
| `default.nix` | the batch-capable-agent assertion, the `enableSsh` reconciliation, and the `vsock` + network-profile / `sshPublicKeyFile` rejections |

Nothing else branches on it: there is one launcher, one share pair, one session
tree, one staging path and one guest shape in every case. `vsock` is an AXIS, not
a fork: the guest-visible model endpoint, the session layout, the staging path,
the units that carry the agent and the launcher's interface are identical under
both transports — what changes is whether a network interface exists at all, and
that decision is expressed ONCE as transport capability flags.

Two properties keep the selector from weakening anything:

- the trust POLICY and every `modeOf` lookup read the FULL layout table, not the
  selected slice, so an owner/mode a host happens not to create is still asserted;
- the launcher SWEEPS undeclared top-level entries of a slot's two trees before
  every launch and the generated verifier `die`s on any that survives, so a stale
  subdirectory of a previous selection is neither exported nor able to brick the
  slot.

The set itself is unconditional configuration: `agent-microvm capabilities`
prints it (`capabilities:` / `declared:`) on every host, which is what
`runtime-validation.sh` reads to decide what it may exercise.

## Interactive execution path

Requires the `interactive` capability.

```text
agent-microvm run --attach --agent <a> --name <task> --repository <repo>
  validate (task name, repo, branch, agent, class)
  → allocate a free slot of the requested class   (flock, token, marker)
  → git clone --local --no-hardlinks              (standalone clone)
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

Requires the `batch` capability.

The batch path has **two guest identities**, because the host acts on the result:
a TRUSTED controller (guest root) owns the result channel, and an UNTRUSTED
worker (guest `agent`) runs the coding agent and therefore whatever the
repository asks for.

```text
agent-microvm submit --agent <a> --prompt-file <f> --timeout <s> …
  … same allocation/clone/mount … (the allocation gets a 256-bit token)
  → sessions/<slot>/input/spec.json  root:root 0400  (carries the token)
    sessions/<slot>/input/prompt.md   root:root 0444
    sessions/<slot>/controller/       root:root 0700  (empty; controller-only)
    sessions/<slot>/worker/           agent-owned     (empty)
    sessions/<slot>/worker-logs/      root:root 0755  (systemd opens the logs as root)
  → systemctl start microvm@<slot>
  guest: agent-job-controller.service   (root; ConditionPathExists=input/spec.json)
     assert the share's ownership/permissions (incl. every parent of controller/)
     validate the spec (version, token, slot, agent, paths, bounds, no extra keys)
     controller/state.json: validating → starting-worker → running   (progress only)
     → systemctl start agent-job-worker@<agent>.service  (uid agent, cgroup-killable)
          guest: the registry's batch argv, cwd /workspace,
                 stdout/stderr → worker-logs/{stdout,stderr}.log (root-owned
                 files, UNTRUSTED content), ProtectProc=invisible
     supervise: deadline | cancellation (input/cancel.json, token-bound) | exit
     → collect ExecMainCode/ExecMainStatus/Result from systemd
     → controller/result.json (0600, tmp+rename): the ONE terminal verdict
  host: poll controller/result.json through agent-job-verify-result
        (ownership + strict schema + version + task + token + slot + agent)
  → archive the VALIDATED document to results/<task>.json (0600 in a 0700 dir,
    source:"controller")
  → stop VM, unmount, clear job data; KEEP the clone
  → exit 0 / 1 / 124 / 70
```

The prompt never travels as an argument and never enters the Nix store, and
neither does the allocation token: every helper that needs it gets it in its
ENVIRONMENT, because `/proc/<pid>/cmdline` is world-readable while
`/proc/<pid>/environ` is `0400`. The spec
cannot name an executable (the agent is resolved through the registry, keyed by
the worker unit's instance name). Nothing the worker writes is ever read as a
result, and a document that fails verification is an infrastructure error — never
a success. See the [security model](./agent-microvm-security-model.md#the-batch-result-channel)
for why atomic rename alone would not be enough.

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
pid cannot impersonate the owner) and a **256-bit random allocation token**.
Operations that act on a slot they did not allocate (`cancel`, `recover`) compare
the **token**, never just the slot name, so a slot that has meanwhile been
re-allocated to another task is never touched.

For batch tasks the same token also crosses into the guest (in the root-only
`input/spec.json`) and must reappear in `controller/result.json`, which is what
makes stale results and replayed cancellations harmless. It is never logged.

## Resource classes and limits

Sizing is a property of the class (prebuilt), and limits are enforced twice:

```text
guest  agent-job-worker@<a>: CPUQuota = vcpu*100%, MemoryMax = classRAM - headroom,
                            TasksMax, TimeoutStartSec (static ceiling)
host   microvm@<slot>:       MemoryMax = classRAM + hypervisor overhead,
                            TasksMax, CPUWeight/IOWeight (relative, yield to the operator)
```

The limits sit on the **worker** unit, so an OOM kills the agent rather than the
trusted controller or the guest.

Timeouts are enforced three times: the controller's own deadline (it stops the
worker's whole `control-group`, so double-forked repository processes die too),
the worker unit's static `TimeoutStartSec` ceiling, and the host's own deadline
(`timeout + job.gracePeriodSeconds`). Only the controller's observation ends up
in the result — a timeout the untrusted worker reported about itself would be
worthless as evidence.

## Control-channel identities

- **Interactive/SSH** (the `interactive` capability only): one stable ed25519
  host key per slot, delivered read-only, pinned in a host `known_hosts`; the
  launcher verifies strictly (`StrictHostKeyChecking=yes`).
  `agent-microvm-hostkeys.service` generates and keeps them on the host — not in
  the world-readable Nix store, and not as agenix secrets (they are host-local,
  per-slot, regenerable identities):

  ```text
  /var/lib/agent-microvms/sessions-ro/<slot>/hostkeys/ssh_host_ed25519_key      root:root 0400
  /var/lib/agent-microvms/sessions-ro/<slot>/hostkeys/ssh_host_ed25519_key.pub  root:root 0444
  /var/lib/agent-microvms/known_hosts                                          root:root 0444
  ```

  A host that does not select `interactive` has none of this: no key pair, no
  `known_hosts`, no provisioning unit, no `hostkeys/` subdirectory in the
  read-only tree and no sshd in the guest.

  The guest generates no host keys of its own
  (`services.openssh.generateHostKeys = false`), so the pinned identity is the
  only one it can present; `known_hosts` holds public keys only, so a non-root
  operator can verify strictly too.
- **VSOCK** (lightweight plan phase 6): a unique CID is *reserved* per slot
  (`8300+idx`) and, when the host selects the `vsock` capability, **passed to
  `microvm.vsock.cid`** so the guest gets a VSOCK device. For cloud-hypervisor
  that also flips `microvm@<slot>` to `Type=notify` and starts the socat <->
  vsock bridge backing the device with `<stateRoot>/<slot>/notify.vsock` — the
  socket the host's `ssh vsock-mux/<path>` reaches the guest's `sshd-vsock@`
  (vsock::22) through. The `sshd-vsock@` unit itself is auto-created by NixOS'
  systemd-ssh-generator (reused, not reinvented) whenever `services.openssh`
  is enabled AND a VSOCK device is present. The per-slot host key is pinned under
  the VSOCK mux path in `known_hosts` too, so the VSOCK channel is host-key-
  verified exactly like the TAP one. A batch+vsock guest SUPPRESSES the TCP
  `sshd.service` (`systemd.services.sshd.enable = false`) AND closes its TAP
  firewall opening for 22 (`services.openssh.openFirewall = false`): the VSOCK
  sshd is the ONLY listener, reachable solely from the host (CID 2), and needs no
  TAP firewall rule. `vsock` is a NEW AXIS over the one guest shape (a
  capability token, not a `transport` enum), additive to the unchanged
  TAP/bridge/firewall on a host whose profile is not `proxy-only`. With
  `proxy-only` the SAME CID additionally carries the MODEL API (port
  `litellmPort`), and then the TAP/bridge/firewall are gone entirely — the
  literal phase-6 design. A host that does not select `vsock` leaves
  `microvm.vsock.cid` at its default `null`, so its guest closure is
  byte-for-byte unchanged.
