<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Agent microVM sandboxes (`myconfig.ai.microvm`)

A second, stronger isolation tier for autonomous coding agents, alongside the
QEMU/SLiRP `flake.sandboxed-pi.nix` tier and the process-jail /
dedicated-host-user tiers. Each agent session runs inside a **Cloud Hypervisor
microVM** (via the `microvm.nix` flake input) with:

- its **own kernel** and a self-contained guest store disk (the host
  `/nix/store` is **not** shared),
- a **disposable** root and `/home/agent`; **only `/workspace` persists** (plus,
  opt-in and task-scoped, an agent's declared state directories),
- a writable **virtiofs `/workspace`** that is a standalone git clone of your
  repository (the `workspace/` subdirectory of the ONE writable per-session
  share, bind-mounted to `/workspace` in the guest),
- a dedicated **private bridge** (`agentbr0`, `192.168.83.0/24`) with **per-TAP
  layer-2 isolation**, and
- model-API access restricted to the **host LiteLLM proxy** through a
  bridge-only forwarding endpoint — no upstream API key ever reaches the guest.

Both an **interactive** (`run --attach`) and an **unattended batch** (`submit`)
execution path exist, with structured results, hard timeouts, cancellation and
recovery. They are **independently selectable** per host
(`capabilities`, see [Capabilities](#capabilities)); the default selects both.

Every agent process and guest workload is treated as potentially hostile; the
secure default prioritises **isolation over convenience**.

> **Verification boundary.** The eval/build tier (`nix flake check`,
> `tests/microvm.nix`) covers the module's configuration; the shell components
> were additionally exercised against stubbed systemd/mounts. **Runtime**
> properties (firewall enforcement, L2 isolation, credential absence, cgroup
> limits) are designed and eval-asserted but must be *measured* with the
> [runtime validation guide](./agent-microvm-runtime-validation.md) on real
> KVM. Read [Limitations](#limitations) before trusting the tier.

## This document set

| Document | Contents |
| --- | --- |
| [How-to](./agent-microvm-howto.md) | **start here**: one full, linear journey — `doctor`, interactive `run`/workmux, one batch `submit`, importing the branch, cleanup |
| this file | activation, option reference, agent registry, network profiles, the per-slot SSH host identity, the dedicated SSH key, batch job format, limitations |
| [Architecture](./agent-microvm-architecture.md) | module map, slot pool, workspace indirection, network path, credential boundary, execution paths, state lifetimes |
| [Operator guide](./agent-microvm-operator-guide.md) | exact procedures: start, submit, status, attach, cancel, collect, remove, recover, logs |
| [Workspace layout](./workspace-layout.md) | where a task's clone is created (`central` vs `beside-repo`), the task -> clone index, and the guards that replace the central root |
| [Security model](./agent-microvm-security-model.md) | trusted vs untrusted, what the boundary protects, mitigated attacks, residual risks |
| [Runtime validation](./agent-microvm-runtime-validation.md) | the real-KVM test procedure for `runtime-validation.sh` |

> Module comments still cite `§<n>` sections of the original implementation
> spec and numbered improvement tickets. Those planning documents are gone;
> the markers are historical only.

---

## Activation

The module is **disabled by default** and produces zero config side effects
while disabled: no microvm.nix host module, no bridge/firewall, no VM slots, no
guest build, no Workmux agents.

It is enabled **only per host, and explicitly** — never via the broad
`myconfig.ai.enable`. On `f13` (`hosts/host.f13/ai.f13.nix`):

```nix
myconfig.ai.microvm = {
  enable = true;
  # Fixed, prebuilt resource classes → slots agent-<class>-<i>.
  resourceClasses = lib.mkForce {
    small  = { count = 1; vcpu = 2; memoryMiB = 4096; };
    normal = { count = 1; vcpu = 4; memoryMiB = 8192; };
  };
  networkProfile = "proxy-only";   # the secure default
  passwordlessControl = true;      # operator convenience on an interactive laptop
  sshPublicKeyFile = ./dedicated-agent-vm-key.pub;
};
```

### Options

| Option | Default | Meaning |
| --- | --- | --- |
| `enable` | `false` | Turn the whole tier on for a host. |
| `capabilities` | `[ "interactive" "batch" ]` | Which execution capabilities the guests carry. A deselected capability is *absent*, not disabled. See [Capabilities](#capabilities). |
| `enabledAgents` | `null` | **Selected** agents (registry tokens). `null` = every declared agent (the module-wide default — and the biggest guest closure). A deselected agent is *absent* from the guest closure. See [Selected agents](#selected-agents). |
| `resourceClasses` | `{ normal = { count = 4; vcpu = 4; memoryMiB = 8192; }; }` | Fixed, **prebuilt** resource classes. See [Resource classes](#resource-classes). |
| `bridgeName` | `agentbr0` | Private bridge name. |
| `subnet` | `192.168.83.0/24` | Private subnet. |
| `gatewayAddress` | `192.168.83.1` | Host-side bridge address + LiteLLM forwarder bind address. |
| `litellmPort` | `4000` | LiteLLM proxy port. |
| `workspaceRoot` | `/var/lib/agent-microvms/workspaces` | Storage root of the `central` layout: clones are grouped as `<workspaceRoot>/<repoSlug>__agent-microvm/<task>`. |
| `workspaceLayout` | `"central"` | Where a task's clone is created: `central` (under `workspaceRoot`) or `beside-repo` (next to the source repository, `<repo-parent>/<repo>__agent-microvm/<task>`, mirroring workmux's `<project>__worktrees`). Either way the clone is registered in the root-owned index `<runtimeRoot>/workspace-index/<task>`. See [workspace-layout.md](workspace-layout.md). |
| `runtimeRoot` | `/var/lib/agent-microvms` | Runtime state (locks, markers, jobs, results, logs). |
| `stateRoot` | `/var/lib/microvms` | microvm.nix per-VM state / bind-mount source. MUST equal `config.microvm.stateDir` (asserted) — the VSOCK ssh target and the per-slot `known_hosts` entry key on it. |
| `guestAgentUid` / `guestAgentGid` | `1000` | Numeric ids of the guest `agent` user, and the host-side owner of every guest-writable path. Asserted unprivileged. |
| `job.defaultTimeoutSeconds` / `job.maxTimeoutSeconds` / `job.gracePeriodSeconds` | `3600` / `86400` / `120` | Batch-job timeouts. |
| `enableSsh` | `true` | Guest SSH server, private interface only. Rejected at eval unless `capabilities` includes `interactive` (see [Capabilities](#capabilities)). |
| `sshPublicKeyFile` | `null` | **Required** when `enableSsh` OR the `vsock` capability is set (the VSOCK `sshd-vsock@` authorises the same dedicated key). See [The dedicated SSH key](#the-dedicated-ssh-key). |
| `passwordlessControl` | `false` | Scoped `NOPASSWD`+`SETENV` sudo rule for exactly `agent-microvm`, for members of the `agent-microvm` group. |
| `configSeed.hostHome` | the primary user's home (`config.users.users.<myconfig.user>.home`) | The host home the **runtime configuration staging** resolves its allowlist against. Never mounted — only the allowlisted paths are read. See [Runtime configuration staging](#runtime-configuration-staging). |
| `configSeed.extraPaths` | `.config/git/attributes`, `.config/git/config` | Agent-independent additions to the staging allowlist. Same validation as the registry's per-agent `configPaths`. |
| `guestModelConfig.enable` | `true` | Guest boot-time model discovery: query the loopback LiteLLM endpoint and render the **live** model list into pi + opencode config. See [Boot-time model discovery](#boot-time-model-discovery). |
| `guestModelConfig.providerKey` / `providerName` | `litellm` / `LiteLLM (microVM)` | Provider key/name written into the generated configs. The key matches the host-side generators, so the runtime list *replaces* the build-time one. |
| `guestModelConfig.defaultContextWindow` / `maxTokensCap` | `131072` / `65536` | Context-window fallback for models whose real value the endpoint does not expose, and the upper bound for the reported per-model output budget (`min(contextWindow / 4, maxTokensCap)`). |
| `guestModelConfig.attempts` / `retryDelaySeconds` / `timeoutSeconds` | `5` / `2` / `5` | Endpoint query retry/timeout budget. Exhausting it is **not** an error (fail soft). |
| `networkProfile` | `"proxy-only"` | Named guest network policy. See [Network profiles](#network-profiles). |
| `packageProxyPort` | `null` | **Required** by `networkProfile = "package-access"`: the one host proxy port guests may reach. |
| `dnsServers` | `[ ]` | Explicit DNS policy for `networkProfile = "internet"` (empty = the host on the bridge). |
| `acknowledgeInsecureNetwork` | `false` | **Required** by the insecure profiles (`package-access`, `internet`). |

Removed spellings (setting one is now an *unknown option* error; see
[Migration](#migration)): `profile`, `session.enable`, `configSeed.enable`,
`guestDotfiles.*`, `slotCount`, `defaultVcpu`, `defaultMemoryMiB`,
`allowPublicInternet`, `allowPrivateNetworks`, `allowInterVmTraffic`.

### Boot-time model discovery

The agent configs a sandbox receives are **copies of host dotfiles**, staged at
launch time (see [Runtime configuration staging](#runtime-configuration-staging)):
pi's
`~/.pi/agent/extensions/myconfig-providers.ts` and opencode's
`~/.config/opencode/opencode.json` both freeze
`services.litellm.settings.model_list` as of the last host rebuild. The host
proxy decides its real model list at *runtime*, so those copies drift.

`agent-model-config.service` (guest, oneshot, user `agent`, ordered after the
config-seed provisioning oneshot and before the batch-job controller) queries
`http://127.0.0.1:<litellmPort>/v1/models` — the same loopback forwarder every
guest agent talks to — optionally enriches context windows from
`/model/info`, and renders:

| Agent | Written to | Picked up because |
| --- | --- | --- |
| pi | `~/.pi/agent/extensions/zz-microvm-models.ts` | pi auto-discovers `~/.pi/agent/extensions/*.ts`; the `zz-` prefix loads after `myconfig-providers.ts` and re-registering the same provider key wins. |
| opencode | `/run/agent-model-config/opencode.json` (`OPENCODE_CONFIG`) | opencode loads `$OPENCODE_CONFIG` *in addition to*, and after, the global config, deep-merging it. |

The staged copies themselves are never modified — the unit only writes NEXT to
them, inside the disposable, agent-owned guest home. No
secrets are involved: only model IDs are discovered and the API key stays the
`not-needed` placeholder (§17).

Fail-soft: if the endpoint is unreachable (profile `offline`, host proxy down,
boot race) the unit logs and exits 0, leaving the copied configs in place. Under
`networkProfile = "offline"` there is no forwarder at all, so the unit is not
even created. Inspect a live sandbox with:

```bash
agent-microvm ssh agent-normal-0 -- systemctl status agent-model-config
agent-microvm ssh agent-normal-0 -- agent-model-config   # re-render on demand
```

### The guest shape

The module has exactly ONE shape: the lightweight one described in
[`myconfig-ai-microvm-lightweight-plan.md`](./myconfig-ai-microvm-lightweight-plan.md).
There is no `profile` option any more — the historical (`full`) spelling of every
item below was deleted once the lightweight path had been reviewed, so there is
one code path to read, test and audit.

| Aspect | What every guest gets |
| --- | --- |
| Pool | from `resourceClasses` |
| Guest store disk | pinned `microvm.optimize.enable = true`, `microvm.storeDiskType = "erofs"` |
| Selected agents | `enabledAgents`; `null` (the default) means every agent `../agents.nix` declares |
| Guest toolset | the minimal documented set (POSIX toolbox, git, diffutils/patch, ripgrep, jq, less, procps, util-linux, openssh when `enableSsh`) plus a **bash** login shell, with NixOS' `environment.defaultPackages` dropped. The opt-in `guestShellConvenience` (default off) adds neovim + fish and, when `guestShellConvenience.shell = "fish"`, switches the login shell to fish and enables `programs.fish` in the guest |
| Guest home | **runtime configuration staging** (`configSeed`): an allowlisted, root-owned copy staged per launch; **no** home-manager inside the guest |
| virtiofs shares per slot | two: ONE writable per-session share at `/run/agent-session` and ONE read-only share at `/run/agent-session-ro` — see [The session share](#the-session-share) |
| Capabilities | `capabilities`; both by default. Narrowing it removes units, guest programs and session subdirectories — see [Capabilities](#capabilities) |

Every package in this set has a documented consumer (see the comment above
`guestCommonPackages` in `../guest.nix`). Agent-specific runtimes belong in the
registry's per-agent `extraPackages`, so they are added only while that agent is
selected.

Sizing is entirely the host's decision: an explicit `resourceClasses` table is
what the pool is built from.

### Capabilities

`myconfig.ai.microvm.capabilities` selects which **capabilities** a host's
guests carry (lightweight plan phase 5; `vsock` added in phase 6). It is a SET
over the ONE guest shape — deliberately not a three-valued
`interactive | batch | combined` mode, which would be a compatibility profile
crossed with that shape. The default is **both** `interactive` and `batch`, i.e.
the historical behaviour; `vsock` is OFF by default, so a default host's guest
closure is byte-for-byte unchanged.

The set mixes **two kinds** of token, and the difference is load-bearing:

* `interactive` and `batch` are **workload** capabilities — they say WHAT the
  guest can be asked to do (be entered by an operator / be given a job).
  **At least one of them must be selected.**
* `vsock` is a **transport** capability — it says HOW the host reaches the guest
  (and, on a `proxy-only` host, how the model API reaches it). It carries no
  workload of its own, so it can **never be selected alone**:
  `capabilities = [ "vsock" ]` is rejected at evaluation time. Accepting it
  would produce a guest that has a VSOCK SSH channel handing out shells while
  declaring neither execution capability — an undocumented third execution
  mode. The module fails CLOSED here rather than inferring `interactive` from
  the transport.

| Capability | Kind | What it adds |
| --- | --- | --- |
| `interactive` | workload | the SSH server (`enableSsh`) and the per-slot SSH host identity with its read-only `hostkeys/` subdirectory, the `known_hosts` database and its provisioning unit, the guest `agent-run` entry point, the Workmux `microvm-<agent>` panes, and the launcher's `run` / `ssh` subcommands |
| `batch` | workload | the TRUSTED guest job controller, the UNTRUSTED `agent-job-worker@` template, the guest job-protocol programs, the `input/`, `controller/`, `worker/` and `worker-logs/` subdirectories of the session tree (with their host tmpfiles rules), the host-side result archive `<runtimeRoot>/results`, and the launcher's `submit` / `cancel` subcommands |
| `vsock` | transport | **AF_VSOCK** as the guest's channel to the host (plan phase 6), in two layers. (1) A VSOCK CONTROL channel, always: the guest gets a VSOCK device (`microvm.vsock.cid` = the slot's deterministic CID) and a VSOCK-only `sshd-vsock@` unit (SSH over AF_VSOCK, reused from upstream), reachable ONLY from the host (CID 2) and NOT from any TCP interface, host-key-verified through the per-slot identity (`hostkeys/`) and a `known_hosts` entry keyed by the VSOCK mux path — which is what lets a batch-only host, with no TCP sshd, still be driven by `agent-microvm ssh` and the runtime-validation suite. (2) Together with `networkProfile = "proxy-only"` it ALSO becomes the MODEL transport, and then the guest has **no network interface at all**: no TAP, no address, no route, no networkd, and the host builds no bridge, no firewall chain and no bridge socket — only ONE destination-fixed AF_VSOCK forwarder per VM. See [VSOCK versus TAP transport](#vsock-versus-tap-transport). Only allowed with `networkProfile = "proxy-only"`/`"offline"`, and requires an `sshPublicKeyFile` (the VSOCK sshd authorises the same dedicated key). |

A deselected capability is **absent**, not merely unused: the units are not in
the guest, the programs are not in its closure, and the directories are never
created. What stays identical in every case is the SHAPE — one writable share,
one read-only share, one launcher, the disposable home, the staged
configuration, the standalone clone and the proxy-only network:

```nix
# an interactive sandbox host: no batch controller/worker, no job protocol,
# no input/controller/worker/worker-logs, no result archive, `submit` refused
myconfig.ai.microvm.capabilities = [ "interactive" ];

# a batch-only worker host: no sshd, no host key, no known_hosts, no readiness
# polling, no workmux panes, `run`/`ssh` refused (no control channel)
myconfig.ai.microvm.capabilities = [ "batch" ];
myconfig.ai.microvm.enableSsh = false;   # REQUIRED (see below)

# a batch-only worker host WITH a VSOCK control channel (plan phase 6): no TCP
# sshd, but the VSOCK `sshd-vsock@` lets `agent-microvm ssh` and the
# runtime-validation suite reach the guest — closing the batch-only coverage hole
myconfig.ai.microvm.capabilities = [ "batch" "vsock" ];
myconfig.ai.microvm.enableSsh = false;
myconfig.ai.microvm.sshPublicKeyFile = ./dedicated-agent-vm-key.pub;

# THE LIGHTEST SECURE SHAPE (plan phase 6, the literal objective): an
# interactive Codex-only host whose guest has NO network interface at all. The
# model API goes guest 127.0.0.1:4000 -> AF_VSOCK -> a per-VM host forwarder ->
# 127.0.0.1:4000, `agent-microvm ssh` / `run --attach` go over the VSOCK control
# channel, and the host builds no bridge, no TAP and no firewall chain.
myconfig.ai.microvm.capabilities = [ "interactive" "batch" "vsock" ];
myconfig.ai.microvm.networkProfile = "proxy-only";   # the module default
myconfig.ai.microvm.enabledAgents = [ "codex" ];
myconfig.ai.microvm.sshPublicKeyFile = ./dedicated-agent-vm-key.pub;
```

`enableSsh` and `capabilities` are reconciled explicitly: `enableSsh` remains
the authoritative switch for the SSH server *within* the `interactive`
capability (an interactive host may legitimately run without it and use
`agent-microvm console`), and it is **meaningless-and-rejected** without that
capability. It is deliberately not silently forced to `false`: the server, the
host identity, the `known_hosts` database and the launcher's `ssh` /
`run --attach` paths would then all disappear behind an option that still read
`true`.

Rejected at evaluation time: an empty capability set, a set with no WORKLOAD
capability (the transport-only `[ "vsock" ]`), an unknown token,
`enableSsh = true` without `interactive`, `vsock` without an `sshPublicKeyFile`,
and `vsock` paired with an insecure network profile (`package-access`/`internet`).

```console
$ nixos-rebuild build   # with myconfig.ai.microvm.capabilities = [ "vsock" ]
error: Failed assertions:
- myconfig.ai.microvm.capabilities selects no WORKLOAD capability
  (currently: vsock).
  "interactive" and "batch" are the workload capabilities — what the
  guest can be asked to do; "vsock" is only a TRANSPORT (how the host
  reaches the guest) and cannot be selected alone. Add "interactive",
  "batch", or both.
```

The accepted matrix is therefore exactly:

| selection | accepted |
| --- | --- |
| `[ ]` | no — no capability at all |
| `[ "vsock" ]` | **no — transport only** |
| `[ "interactive" ]` | yes |
| `[ "batch" ]` | yes |
| `[ "interactive" "batch" ]` | yes (the default) |
| `[ "interactive" "vsock" ]` | yes |
| `[ "batch" "vsock" ]` | yes |
| `[ "interactive" "batch" "vsock" ]` | yes |

Every cell is exercised by `checks.microvm-capabilities`.

The launcher of a narrowed host **refuses** the other capability's subcommands
up front, naming the option to change:

```console
$ sudo agent-microvm submit --name t --repository /r --agent codex --prompt-file p
agent-microvm: 'submit' needs the 'batch' capability, which this host does not
select (myconfig.ai.microvm.capabilities = [ interactive ]); add "batch" to
that list and rebuild to enable it
```

Every host — narrowed or not — can be ASKED what it selects. The answer is
machine-readable, needs no root and starts nothing:

```console
$ agent-microvm capabilities
capabilities: interactive
declared: interactive batch vsock
network-transport: tap
```

This is how tooling decides what can be exercised on a host; nothing has to infer
the set from an error message. `agent-microvm usage` likewise reports no batch
result archive on a host without `batch` (the directory is never created), and
`agent-microvm console` is deliberately NOT gated — it is the journal of the
host's `microvm@<slot>` unit, so it works for every guest and is the only way to
debug a batch-only one.

The real-KVM validation suite honours the same split: it reads
`agent-microvm capabilities` (and hard-aborts if it cannot parse the answer),
every section that drives a guest needs a **control channel** — the `transport`
requirement, met by `interactive` (the TCP sshd) OR `vsock` (the VSOCK
`sshd-vsock@`, plan phase 6) — `lifecycle` and `forgery` additionally require
`batch`, and a section whose requirement the host does not meet is SKIPPED (or
hard-aborts when asked for explicitly) rather than reporting vacuous passes. A
batch-only host without `vsock` can therefore only run the `seed` section; a
batch+vsock host runs ALL eight. See
[runtime validation](./agent-microvm-runtime-validation.md).

### VSOCK versus TAP transport

There are exactly TWO transports for the guest's **model-API** traffic, and the
module resolves which one a host uses ONCE, from the network profile plus the
capability set (`./network-profiles.nix` -> `agentNetwork.transport`):

| transport | when | what the guest has | what the host builds |
| --- | --- | --- | --- |
| `tap` | every host except the one below | a TAP on the private bridge, the slot's static IPv4, a default route, systemd-networkd | the bridge + gateway address, the per-TAP L2 isolation units, the `AGENT_MICROVM_*` firewall chains, the bridge-only `agent-litellm-proxy` socket |
| `vsock` | the `vsock` capability **and** `networkProfile = "proxy-only"` | **NO network interface at all** (loopback only): no TAP, no address, no route, no networkd, no dhcpcd | ONE `agent-litellm-vsock-<slot>` socket + service PER VM, and nothing else — no bridge, no TAP, no firewall chain, no bridge socket |

Under the `vsock` transport the model API travels:

```text
guest agent -> 127.0.0.1:<litellmPort>              (UNCHANGED endpoint)
  -> guest socat bridge (litellm-forwarder, ONE unit: `ACCEPT-FD:3,fork`)
  -> AF_VSOCK CID 2 (the host), port <litellmPort>
  -> cloud-hypervisor's per-VM mux socket
     <stateRoot>/<slot>/notify.vsock_<litellmPort>
  -> the per-VM host forwarder (systemd-socket-proxyd)
  -> 127.0.0.1:<litellmPort>                        (the loopback LiteLLM proxy)
```

The guest-visible endpoint is deliberately the SAME `127.0.0.1:<litellmPort>`
the TAP transport presents, so the host-provisioned agent configuration
(`OPENAI_BASE_URL`, `ANTHROPIC_BASE_URL`, the staged dotfiles) works verbatim —
switching transport reconfigures no agent.

Why this is STRICTLY stronger than `tap` + `proxy-only`:

- the guest cannot address the host LAN, the VPN, cloud metadata, DNS, another
  guest or another host port, because it has no interface to address them WITH.
  Invariant 6 becomes an ABSENT DEVICE instead of a firewall verdict;
- the host forwarder is **one listener per VM**, bound to a Unix socket inside
  that VM's own state directory (root-owned, `0660` to **the VMM's group**
  `kvm`, so root, the VMM and any other host user already in `kvm` may connect —
  no escalation, since such a user can already `curl 127.0.0.1:<litellmPort>`;
  and **no TCP listener exists anywhere on this path**) and
  **destination-fixed** to `127.0.0.1:<litellmPort>`. It is
  not a CONNECT proxy: the guest cannot name a host, a port or a CID, and the
  forwarder itself is confined with `IPAddressAllow=localhost` +
  `IPAddressDeny=any`, `DynamicUser`, `ProtectSystem=strict`;
- slot A cannot use slot B's model path: there is no shared namespace and no CID
  to spoof, only per-VM socket paths.

With **no** guest interface, the TCP sshd could never be reached either, so a
`vsock`-transport host masks `sshd.service` (and keeps 22 out of the guest
firewall) whatever `enableSsh` says, and every launcher path — `ssh`,
`run --attach`, the readiness poll, `status` — uses the VSOCK control channel
(`SSH_ENABLED=0`, `VSOCK_ENABLED=1`). The VSOCK `sshd-vsock@` is host-key
verified exactly like the TAP one: the launcher reaches it as
`ssh vsock-mux/<stateRoot>/<slot>/notify.vsock`, a hostname that resolves only
through the `Host vsock-mux/*` ProxyCommand nixpkgs'
`programs.ssh.systemd-ssh-proxy.enable` (default true) supplies; the module
**asserts** that option is enabled whenever `vsock` is selected, so a host that
disables it fails at evaluation rather than at runtime.

`vsock` WITHOUT `proxy-only` (i.e. with `offline`, the only other profile it is
allowed with) keeps the `tap` transport and adds the VSOCK **control** channel
only: an `offline` guest has no model API to carry, so there is nothing to move
off the TAP. `package-access`/`internet` need ordinary IP networking and are
rejected together with `vsock` at evaluation.

`agent-microvm capabilities` reports the resolved transport machine-readably
(`network-transport: tap|vsock`) next to the capability set, and the real-KVM
suite reads it: its `net` section then asserts the guest has NO non-loopback
link, no default route and no global address instead of testing a firewall that
does not exist, and its `l2` section asserts the same for both guests and states
that the bridge-port/ARP/impersonation subtests do not apply. An absent or
unknown transport HARD-ABORTS the run, exactly like an unparseable capability
set — the suite must never turn "the guest has no interface" into a silent
vacuous pass.

Stale directories from a PREVIOUS capability selection are handled, not ignored:
the launcher sweeps every top-level entry of a slot's two trees that the current
table does not declare (refusing to remove one with a live mount underneath),
and the generated pre-launch verifier `die`s on any that survives. A leftover
root-owned `input/` can therefore neither be exported to a guest unverified nor
brick the slot.

### Selected agents

`myconfig.ai.microvm.enabledAgents` selects which of the registry's declared
agents a host actually builds. The selection is applied **once**, inside the
authoritative registry (`../agents.nix`), so every derived artefact follows it:

- the guest closure's agent packages and per-agent guest environment;
- the guest `agent-run` dispatch and its usage/error text;
- the batch worker's and controller's dispatch, and `submit`'s validation;
- the host launcher's `--agent` validation and `--help` listing;
- the Workmux `microvm-*` registrations;
- the agent-state directories the guest-side linker knows about.

A deselected agent is therefore **absent from the guest image** (its runtime is
not in the closure at all), and `--agent <name>` for it is rejected on the host
before anything is started. Rejected at evaluation time: an unknown token, an
empty selection, and — **only on a host that selects the `batch` capability** —
a selection without any batch-capable agent. On an interactive-only host that
last guard does not apply, because such a guest carries no batch machinery at
all (see [Capabilities](#capabilities)).

### Runtime configuration staging

The guest home is **not** produced by home-manager activation inside the guest
(that path was removed). The host stages an **allowlisted** copy of its own agent
configuration on every launch:

```text
host allowlisted config
        │  agent-microvm-stage-config, as root, at launch time
        ▼
<runtimeRoot>/sessions-ro/<slot>/config-seed  root:root 0500/0400
        │  the per-slot READ-ONLY virtiofs share (the payload is a
        │  subdirectory of the read-only session tree, and the staging
        │  MANIFEST stays outside every share)
        ▼
/run/agent-session-ro/config-seed
        │  agent-config-seed.service (guest root oneshot), ordered BEFORE
        │  sshd, the batch job controller, the agent-state linker and the
        │  boot-time model discovery
        ▼
/home/agent                                  agent-owned, disposable
```

Why: editing an allowlisted host instruction/skill/config file affects the
**next launch** without rebuilding the guest, and the guest carries no
home-manager activation machinery at all.

**What may cross the boundary** is an explicit, positive allowlist: the
`configPaths` of the *selected* agents (`../agents.nix`, so the staged set
shrinks with `enabledAgents`) plus `configSeed.extraPaths`. Exact files and
exact directories only — never a whole agent configuration root, because those
mix configuration with credentials (`~/.codex/auth.json`, `~/.hermes/.env`, …).
Hermes deliberately declares **no** `configPaths` for that reason.

The rules, enforced at evaluation time *and* again by the generated stager:

- paths are relative to `configSeed.hostHome`; `..`, absolute paths, a leading
  `-`, a trailing `/` and anything outside `[A-Za-z0-9._-/]` are **rejected at
  eval**;
- a **credential denylist** (`auth.json`, `credentials*`, `*.pem`, `*.key`,
  `id_rsa`, `id_ed25519`, `.env`, `.netrc`, `.ssh`, `*token*`, `*secret*`,
  `*session*`, `cookies*`, …) is matched against every path component — at eval
  against the allowlist itself, at runtime against every file found inside an
  allowlisted directory, **and against the RESOLVED target** of every entry,
  file and subdirectory, so a benignly *named* symlink
  (`.codex/config.toml` → `.codex/auth.json`, `.agents/skills/x` → `~/.ssh`)
  cannot smuggle credential material past a name-only check;
- a symlink that resolves **outside** the host home is refused; a symlink into
  `/nix/store` is **dereferenced** into a plain copy. That is how home-manager
  renders dotfiles, but the exception is not limited to them: any home symlink
  into the store is followed (store content is world-readable anyway, and the
  budgets below bound how much of it can be copied);
- only regular files and directories are copied — never sockets, FIFOs, device
  nodes or setuid/setgid files;
- the staged tree is **root-owned and root-only** (0500/0400, and the share is
  mounted read-only), so neither the untrusted guest `agent` user nor another
  unprivileged *host* user can read or modify what the host staged — the guest
  agent only ever gets the copy the guest root seeder hands it;
- the per-slot destination is **cleaned before every launch**, and again on
  teardown, so nothing from a previous task can leak into the next one;
- per-file (1 MiB), per-launch (32 MiB) and file-count (1024) budgets bound what
  one launch copies — the last one also bounds how long staging can delay a
  launch; content deeper than 12 directory levels is not considered and the
  truncation is recorded in the manifest;
- the guest seeding oneshot runs **before** sshd, the batch job controller, the
  agent-state linker and the boot-time model discovery (which writes into the
  same home), so nothing races the copy.

What is **not** defended against (it is out of scope, and an attacker who can
do it can simply edit an allowlisted file instead): an attacker with write
access *inside the trusted host home* can defeat the name-based denylist with a
**hardlink** to a credential, or by swapping a resolved path between the check
and the copy (TOCTOU).

Model-provider credentials are **never** staged: they stay in the host LiteLLM
proxy, and the guest only learns the endpoint (see
[Boot-time model discovery](#boot-time-model-discovery)).

Inspect what a session actually got — the stager writes a manifest recording the
policy, everything staged and everything skipped **with a reason**. It lives in
the host-only `<runtimeRoot>/config-seed/<slot>/`, `root:root 0400`, deliberately
*outside* every share: it names the host home and every skipped,
credential-shaped host file name, which the untrusted guest has no business
learning.

```bash
sudo jq . /var/lib/agent-microvms/config-seed/agent-normal-0/manifest.json
sudo find /var/lib/agent-microvms/sessions-ro/agent-normal-0/config-seed -ls
agent-microvm ssh agent-normal-0 -- systemctl status agent-config-seed
```

The guest copy is the agent's own: it is writable, lives on the disposable
tmpfs home and disappears with the session, and changing it cannot affect any
host file.

The stager itself (`agent-microvm-stage-config`, root-only, takes a slot name
and nothing else — the whole policy is baked in) is on the host `PATH` whenever
staging is enabled, so an operator can stage and audit a slot by hand. The
`seed` section of `runtime-validation.sh` is the repeatable, root-run proof that
the policy is actually *enforced* (CI can only prove it is baked in, because the
Nix sandbox is not root).

### The session share

A slot has **one writable** virtiofs share and **one read-only** one (the
historical four/five separate shares are gone) — fewer virtiofsd processes, one
guest mount set, and one host tree to create, verify and remove:

```text
<runtimeRoot>/sessions/<slot>/          root:root 0755   ONE WRITABLE SHARE  -> /run/agent-session
  workspace/                            agent            bind target -> the task's clone
  input/                                root:root 0755   spec.json 0400, prompt.md 0444        [batch]
  controller/                           root:root 0700   the AUTHORITATIVE result              [batch]
  worker/                               agent     0755   the untrusted worker's own area       [batch]
  worker-logs/                          root:root 0755   worker stdout/stderr (root-opened)    [batch]
  state/                                agent     0755   bind target -> task-scoped state

<runtimeRoot>/sessions-ro/<slot>/       root:root 0700   ONE READ-ONLY SHARE -> /run/agent-session-ro
  hostkeys/                             root:root 0700   the slot's ed25519 host key (0400)    [interactive]
  config-seed/                          root:root 0500   the staged host agent configuration
```

The `[batch]` / `[interactive]` entries exist only when the host selects that
capability (see [Capabilities](#capabilities)); the two SHARES, their tags,
their mount points and every mode are the same either way. `../session.nix`'s
layout table carries that per-capability marking, so the host tmpfiles rules,
the pre-launch verifier, the launcher's tree preparation and the tests all
follow it without a second decision.

Inside the guest, `/run/agent-session/workspace` is **bind-mounted to
`/workspace`**, so `agent-run`'s mount/writability checks and every agent's
expectation of `/workspace` are unchanged.

The trust boundaries are **identical** to the historical four-share layout,
because they were never expressed by the share split but by **ownership and
modes**, which
virtiofsd passes through unchanged: the session root, `input/`, `controller/`
(root-**only** `0700`) and `worker-logs/` are root-owned, and only `workspace/`,
`worker/` and `state/` belong to the unprivileged guest `agent` user.

`../session.nix` is the single source of truth for that table; `../job.nix`,
`../state.nix`, `../config-seed.nix`, `../hostkeys.nix`, the host tmpfiles rules,
the generated pre-launch verifier and the launcher all derive from it.

Two things are deliberately **not** in the writable tree (the plan's own
invariants 7 and 8 outrank its layout sketch, which suggested folding the config
seed into the writable share):

- the slot's **SSH private host key** — the plan states this explicitly, and
- the **staged host configuration** — host-decided input the guest must not be
  able to modify. It stays in a share virtiofsd mounts `--readonly`, root-owned
  `0500`/`0400`, with its `manifest.json` outside every share (it moves to the
  host-only `<runtimeRoot>/config-seed/<slot>/`).

Per launch the launcher

1. **prepares** the tree (`install -d` with the table's exact owner/mode, which
   also *resets* a directory the guest agent chmodded in an earlier session),
2. stages the configuration and bind-mounts the clone and (optionally) the state,
3. **verifies** it with `agent-microvm-verify-session` — every directory's owner
   and mode, no symlinked component, every parent root-owned and not
   group/other-writable, and no host-key material anywhere in the writable tree
   — and **refuses to start the VM** on any mismatch, and
4. on teardown **removes the complete tree** — but only after proving both bind
   mounts are gone *and* that `findmnt` reports no other mount anywhere at or
   below the session root (deleting through a live bind would destroy the clone
   or the task's persisted state, and `rm --one-file-system` is no protection
   because a same-filesystem bind shares its `st_dev`), then recreates the empty
   skeleton virtiofsd needs.

The host-key sweep in step 3 deliberately **prunes** `workspace/` and `state/`:
those two are the user's own clone and the task's agent state, which the host
never writes key material into and the agent may write to freely — without the
prune an ordinary `…/ssh_host_ed25519_key.pub` in a NixOS/agenix repository
would refuse the launch, and a hostile agent could deny every future launch of
its slot by creating one. Step 2 also normalises the **mode of the clone root**
to the table's value for the mount point, because a mount point shows the
mounted tree's root mode and `git clone` (root's umask, the source repository's
`core.sharedRepository`) is not something the launcher controls.

Inspect a slot's tree by hand with the same policy the launcher uses:

```bash
sudo agent-microvm-verify-session agent-normal-0
```

### Resource classes

Each class contributes `count` slots named `agent-<class>-<i>`, sized by the
class:

```nix
myconfig.ai.microvm.resourceClasses = lib.mkForce {
  small  = { count = 2; vcpu = 2; memoryMiB = 4096; };
  normal = { count = 4; vcpu = 4; memoryMiB = 8192; };
  large  = { count = 1; vcpu = 8; memoryMiB = 16384; };
};
```

- `mkForce` matters: defining a single class otherwise **merges** with the
  module's default `normal` class instead of replacing the pool.
- The allocator only ever considers the **requested** class; it never
  substitutes a smaller one. If the class is full it fails, or waits at most
  `--wait <sec>`.
- Class names must match `[a-z][a-z0-9-]*` and be short enough that
  `vm-<class>-<i>` stays within the 15-character interface-name limit — both
  asserted at eval time.
- Slot **names** are stable, but the pool-wide index (which drives MAC / IPv4 /
  VSOCK CID) walks classes alphabetically, so adding or resizing a class
  re-numbers the *addresses* of later classes. Every host-side directory is
  keyed by name, not address.
- Old per-slot state left over from a resized or renamed pool
  (`slots/`, `sessions/`, `sessions-ro/` entries — plus the pre-consolidation
  `/var/lib/microvms/<slot>/workspace`, `jobs/`, `hostkeys/` and `state/slots/`
  residue, which `recover` still scans on purpose) is unused, but no longer
  *silent*:
  `agent-microvm recover` reports it as a `foreign:` finding, and
  `agent-microvm recover --prune-foreign` removes it (unmounting a stale foreign
  bind through the same verified path as any other unmount). It is never removed
  implicitly. Workspace clones are keyed by **task**, so none of this touches
  them.

### Agent state persistence

The guest home is a tmpfs, so a sandbox starts with **no** agent memories,
skills, sessions or caches by default. Agents may declare, in the registry,
which directories are worth keeping (`persistentState.directories`; today only
hermes, `~/.hermes`).

```bash
sudo agent-microvm run    --agent hermes --persist-agent-state …
sudo agent-microvm submit --agent hermes --persist-agent-state …
sudo agent-microvm --help    # lists the declared directories per agent
```

```text
/var/lib/agent-microvms/state/tasks/<task>/<agent>/<dir>   per task+agent, kept
/var/lib/agent-microvms/state/slots/<slot>                 the share source
```

- **Opt-in only** — without the flag the slot's share source stays empty and the
  agent uses its disposable home.
- **Task-scoped** — the per-task directory is bind-mounted onto the slot's share
  source only while that slot runs, so task B never sees task A's state.
- **Only declared directories** are linked, and the guest-side linker refuses to
  replace a **non-empty** directory in the home, so provisioned dotfiles are
  never clobbered.
- Requesting persistence for an agent that declares nothing is an **error**, not
  a silent no-op.

### Resource and abuse limits

Sized per resource class, enforced on **both** sides:

| Where | Limit | Value |
| --- | --- | --- |
| guest `agent-job-worker@<a>` | `CPUQuota` | `vcpu × 100 %` |
| guest `agent-job-worker@<a>` | `MemoryMax` | class RAM − `job.guestMemoryHeadroomMiB` (never below half), so an OOM kills the **agent**, not the guest or the trusted controller |
| guest `agent-job-worker@<a>` | `TasksMax` | `job.tasksMax` (default 4096) — fork-bomb bound |
| guest `agent-job-worker@<a>` | `TimeoutStartSec` | `job.maxTimeoutSeconds + job.gracePeriodSeconds` — static ceiling (`Type=oneshot` ignores `RuntimeMaxSec`) |
| guest `agent-job-worker@<a>` | `KillMode` | `control-group` — the controller kills the whole worker tree, not just a pid |
| guest `agent-job-controller` | `TimeoutStartSec` | `job.maxTimeoutSeconds + 2 × job.gracePeriodSeconds` — the controller's own ceiling. Also `TimeoutStartSec`, because for `Type=oneshot` `RuntimeMaxSec` has no effect and the start timeout would otherwise be **infinite** |
| host `microvm@<slot>` | `MemoryMax` | class RAM + `hypervisorMemoryOverheadMiB` (never below guest RAM + overhead) |
| host `microvm@<slot>` | `TasksMax` | `hypervisorTasksMax` |
| host `microvm@<slot>` | `CPUWeight` / `IOWeight` | `50` — sandboxes yield to interactive host work but may use idle capacity |

Guest root and `/tmp` are tmpfs, so they are bounded by the class's RAM.
Retained growth is on disk (clones + task state) and is reported by
`agent-microvm usage`.

---

## Network profiles

`networkProfile` replaces three ambiguous booleans with four named policies. The
capability table ([`network-profiles.nix`](../network-profiles.nix)) is resolved
**once** in `default.nix` and drives both the host firewall (`network.nix`) and
the guest-side proxy/DNS configuration (`guest.nix`), so host policy and guest
configuration cannot disagree.

| Profile | Additionally allowed | Guest-side effect |
| --- | --- | --- |
| `offline` | nothing — only host→guest control traffic and its replies | no loopback LiteLLM forwarder |
| `proxy-only` **(default)** | `guest → <gatewayAddress>:<litellmPort>` (the model API) | loopback LiteLLM forwarder |
| `package-access` | additionally one explicit host proxy port `<packageProxyPort>`. **No routing, NAT or DNS** — this is *not* unrestricted internet | `http_proxy`/`https_proxy` point at that proxy |
| `internet` | routing **plus** NAT/masquerading, DNS restricted to `dnsServers`, rate-limited drop logging | guest resolvers set to `dnsServers` |

In **every** profile:

- guest↔guest traffic is blocked — per-TAP L2 `isolated` *and* an IPv4 inter-VM
  `FORWARD` DROP; there is no option to relax it;
- the cloud-metadata IP `169.254.169.254` is dropped first, in `INPUT` and
  `FORWARD`;
- private/special-use IPv4 ranges (host LAN, VPN peers, RFC1918, CGNAT,
  loopback, link-local, multicast, reserved) are dropped — the only exception is
  a resolver explicitly listed in `dnsServers`;
- `INPUT` and `FORWARD` end in a terminal `DROP` (fail closed);
- host→guest control traffic is allowed (the host is trusted).

`package-access` and `internet` require `acknowledgeInsecureNetwork = true`.

The profile is deliberately a **single global option** — per-resource-class
profiles were planned but never implemented; see the historical
[per-resource-class network plan](./plan-per-resource-class-network.md) for
the constraints that couple every slot to the one value.

---

## Supported agents — the authoritative registry

[`agents.nix`](../agents.nix) is the **single source of truth** for which agents
a sandbox supports: guest closure packages, guest environment, interactive and
batch argv, `--agent` validation, the `microvm-*` workmux agents and the
declared state paths are all generated from it. It is instantiated exactly once
(in `default.nix`) and passed to consumers via `_module.args.agentRegistry`.

```nix
<name> = {
  package = pkgs.<attr>;       # baked into the guest closure, never fetched at runtime
  executable = "<bin>";        # what `agent-run <name>` execs inside the guest
  workmuxType = "<type>";      # optional, defaults to <name>
  interactiveArgs = [ ];       # optional extra argv for the interactive session
  batchArgs = [ ];             # argv for unattended batch execution
  batchStdin = false;          # pass the prompt on stdin instead of as argv
  guestEnvironment = { };      # endpoint plumbing only — NEVER a credential
  persistentState = {
    enabledByDefault = false;  # the guest home stays DISPOSABLE by default
    directories = [ ];         # verified paths, relative to the guest home
  };
};
```

`workmuxName` is derived (`microvm-<name>`), so adding an agent is a one-entry
change. `sudo agent-microvm --help` lists the currently supported agents.

Batch invocations are verified against each pinned build's own `--help`:
`claude -p <prompt>`, `codex exec -` (prompt on **stdin**),
`opencode run <prompt>`, `pi --print <prompt>`,
`hermes --model <m> --oneshot <prompt>`. `herdr` has **no** batch mode (see
below), so `submit --agent herdr` is rejected.

### herdr specifics

`herdr` (`pkgs.herdr`, the same package the host `programs.herdr` and the
tier-3 `sandboxed-herdr` runner install) is the odd one out: it is not itself a
coding agent but the **agent multiplexer** — a terminal TUI that launches the
*other* agents (pi, opencode, claude, codex, hermes) in its panes. Inside a
guest, `agent-run herdr` drops the operator into a herdr session from which
those agents can be started, exactly like the tier-3 `sandboxed-herdr` variant
execs `herdr` over SSH in its QEMU microVM. The agents herdr can launch are the
ones the host also SELECTS via `enabledAgents`, because those are the runtimes
baked into the guest closure and on `PATH`.

Being a multiplexer, herdr is **interactive-only**: it has no unattended
one-shot mode, so it declares no `batchArgs` and `submit --agent herdr` is
rejected. Its `--attach` / workmux path is unaffected — a `microvm-herdr` pane
launches `agent-microvm run --attach --agent herdr` like every other
interactive agent. Only the rendered keybinding config
(`~/.config/herdr/config.toml`, no credentials) is staged, so a herdr guest
gets the same `ctrl+b` prefix / pane-focus bindings the host uses. workmux has
no `herdr` profile, so the pane falls back to workmux's default profile.

### Hermes specifics

Hermes resolves its endpoint as `config.yaml` `base_url` → `CUSTOM_BASE_URL` →
`OPENROUTER_BASE_URL` → `openrouter.ai`, so the registry sets
`OPENROUTER_BASE_URL` to the guest loopback LiteLLM endpoint and pins `--model`
to `myconfig.ai.hermes.model.default`. The placeholder `OPENAI_API_KEY` is what
hermes picks for a non-OpenRouter `base_url` and also satisfies its first-run
"any provider configured?" guard, so the setup wizard never appears. All hermes
state lives under `$HERMES_HOME` (`~/.hermes`), which is why that single
directory is the declared `persistentState`. workmux has no `hermes` profile, so
the pane falls back to workmux's default profile.

---

## Unattended batch jobs

A batch job runs as **two guest identities**: a TRUSTED controller
(`agent-job-controller.service`, guest root) and an UNTRUSTED worker
(`agent-job-worker@<agent>.service`, guest `agent`). The controller validates the
job, starts the worker, enforces the deadline/cancellation and is the ONLY writer
of the authoritative result. See the
[security model](./agent-microvm-security-model.md#the-batch-result-channel).

### Job directory (runtime only — never in the Nix store)

```text
/var/lib/agent-microvms/sessions/<slot>/               root:root 0755
                              input/                   root:root 0755
                              input/spec.json          root:root 0400  the job spec (v2)
                              input/prompt.md          root:root 0444  the prompt TEXT
                              input/cancel.json        root:root 0400  cancellation request
                              controller/              root:root 0700  CONTROLLER ONLY
                              controller/state.json    root:root 0600  trusted progress
                              controller/result.json   root:root 0600  AUTHORITATIVE result
                              worker/                  1000:1000 0755  UNTRUSTED output
                              worker/artifacts/
                              worker-logs/             root:root 0755  log dir (root-owned)
                              worker-logs/{stdout,stderr}.log  root:root 0644  UNTRUSTED content
```

The per-slot job data IS `<runtimeRoot>/sessions/<slot>/`, surfaced into the
guest at `/run/agent-session` through the ONE writable share (see
[The session share](#the-session-share)). It is read-**write**, but
*who* may write *what* is decided by ownership and modes, which virtiofsd passes
through unchanged:

- `input/` is root-owned, so the guest cannot lift its own timeout or swap its
  own agent. `spec.json` is `0400` because it carries the **allocation token**
  (256 bits from `/dev/urandom`), which the untrusted worker must not be able to
  read.
- `controller/` is `root:root 0700`: the worker can neither write nor read it,
  and cannot rename or shadow it (its parent is root-owned `0755`, and the worker
  unit masks it via `InaccessiblePaths`).
- `worker/` is the only worker-writable part. Everything in it is untrusted.
- `worker-logs/` holds the two files the guest's systemd opens for the worker
  with `append:`. systemd opens them **as root and follows symlinks**, so they
  live in a root-owned directory *outside* `worker/` — otherwise anything running
  as uid 1000 could rename `worker/` contents and plant a symlink that redirects a
  root-opened fd. The worker may read its logs; it cannot truncate, replace or
  redirect them, and their content is untrusted regardless.

Prompts never travel as process arguments from the host and never enter the Nix
store. Neither does the **allocation token**: `/proc/<pid>/cmdline` is
world-readable (`0444`) while `/proc/<pid>/environ` is `0400`, so every helper
that needs the token (`jq` on both sides, the host result verifier) receives it
in its ENVIRONMENT. The verifier reads `AGENT_JOB_EXPECTED_TOKEN` and refuses a
`--token` argument outright; the worker unit additionally sets
`ProtectProc=invisible`, so it cannot see the controller's processes at all.

`spec.json` (schema `version = 2`, validated on **both** sides):

```json
{
  "version": 2,
  "taskId": "fix-parser",
  "allocationToken": "…64 hex chars…",
  "slot": "agent-normal-0",
  "agent": "opencode",
  "workspace": "/workspace",
  "promptFile": "/run/agent-session/input/prompt.md",
  "timeoutSeconds": 3600,
  "resourceClass": "normal",
  "persistAgentState": false
}
```

The guest controller rejects (as `infrastructure-error`) an unknown schema
version, **any unknown field**, an invalid `taskId`, a malformed or missing
`allocationToken`, a `slot` that is not this guest, an agent that is not
batch-capable, a `workspace` other than `/workspace`, a `promptFile` that is not
*exactly* `/run/agent-session/input/prompt.md`, an out-of-range `timeoutSeconds`, a
malformed `resourceClass`, a non-boolean `persistAgentState`, and **any** attempt
to name an executable (`command` / `exec` / `executable`). If the share's
ownership/permissions are not exactly as above it refuses to run at all.

`controller/result.json` — written by the controller only, with tmp-file +
`rename` (which gives *consistency*; **authenticity** comes from ownership plus
the allocation token) — always carries a TERMINAL state:

```json
{
  "version": 2, "controllerVersion": 1, "taskId": "fix-parser",
  "allocationToken": "…", "slot": "agent-normal-0", "agent": "opencode",
  "state": "completed", "exitCode": 0,
  "startedAt": "…Z", "finishedAt": "…Z", "timedOut": false, "message": ""
}
```

Terminal states: `completed`, `failed`, `timed-out`, `cancelled`,
`infrastructure-error`. Progress lives in `controller/state.json` as a `phase`
(`validating`, `starting-worker`, `running`, `timing-out`, `cancelling`,
`finished`) and is never an outcome.

The host reads the result through ONE verifier (`agent-job-verify-result`), which
requires a regular, non-symlink, root-owned, size-bounded file in a root-owned,
non-group/other-writable directory and a document whose `version`,
`controllerVersion`, `taskId`, `allocationToken`, `slot` and `agent` match the
ACTIVE allocation, with a valid terminal state, exit code and timestamps.
Anything else — including a v1 result (there is **no** compatibility mode) — is an
infrastructure error, never a success. The validated document is archived at
`/var/lib/agent-microvms/results/<task>.json` (outside every guest share) tagged
`source: "controller"`; a record the host had to invent itself is tagged
`source: "host"`. The archive directory is `0700` and each archived file `0600`,
because the document still carries that run's allocation token.

The controller's deadline is measured against a **clock**, not a count of poll
iterations: each iteration also spends one or two `systemctl show` round-trips, so
counting iterations would drift by a few percent — at the 24 h ceiling by tens of
minutes, enough for the HOST deadline to fire first and take the verdict away
from the controller. A worker that never starts at all is an
`infrastructure-error`, not a `timed-out`, even when the job's own timeout is
shorter than the 60 s worker-startup grace.

`submit` exit codes: **0** completed, **1** the agent failed, **124** timed out,
**70** infrastructure error (no/invalid/unauthentic result).

---

## Per-slot SSH host identity: lifecycle and recovery

Every slot has its OWN stable ed25519 **host** key. It authenticates the GUEST
to the host, and is the reason `agent-microvm ssh` / `run --attach` /
the readiness poll can run with `StrictHostKeyChecking=yes` instead of accepting
any key. (The *dedicated* key of the next section is the opposite direction: it
authenticates the operator to the guest.)

### Where it lives

| Path | Owner / mode | Notes |
| --- | --- | --- |
| `<runtimeRoot>/sessions-ro/<slot>/hostkeys/ssh_host_ed25519_key` | `root:root 0400` | the slot's PRIVATE host key |
| `<runtimeRoot>/sessions-ro/<slot>/hostkeys/ssh_host_ed25519_key.pub` | `root:root 0444` | its public half |
| `<runtimeRoot>/known_hosts` | `root:root 0444` | the aggregated database the launcher pins `ssh` to |
| `<runtimeRoot>/hostkeys.lock` | `root:root` | the provisioner's `flock` |

Never in the Nix store (a store path is world-readable, so every local user
could impersonate a slot) and never in the WRITABLE session tree — the
pre-launch verifier refuses to launch a slot whose writable tree contains key
material. The guest sees the directory through the per-slot READ-ONLY virtiofs
share, and virtiofsd passes ownership through unchanged, so inside the guest the
private key is still `root:root 0400`: the unprivileged `agent` user cannot read
it, and no other slot's directory is exposed.

The `known_hosts` alias depends on the CONTROL TRANSPORT:

| Transport | alias |
| --- | --- |
| TAP (`interactive`, guest has an interface) | the slot's deterministic IPv4 |
| VSOCK (`vsock` is the only control channel) | `vsock-mux/<stateRoot>/<slot>/notify.vsock` |

### Provisioning is idempotent and self-healing

`agent-microvm-hostkeys.service` (a `RemainAfterExit` oneshot,
`wantedBy = multi-user.target`) provisions the identities. It is safe to run at
any time, and it is run **before every launch** of a slot whose control
transport is SSH-based — i.e. whenever the host has `interactive` (TAP sshd) OR
`vsock` (the VSOCK `sshd-vsock@`).

What it does, in order, per slot:

1. **repairs the MODE** of an existing, already-`root:root` private key back to
   `0400`, and says so on stderr if it had to. This happens BEFORE the key is
   judged, because `ssh-keygen` refuses to read an over-permissive private key —
   judging first would mistake mode drift for corruption and silently re-key the
   slot. A key that is *not* `root:root` is never chowned into trust; it is
   replaced;
2. **keeps a valid private key untouched.** Validity = non-empty +
   `root:root` + loadable by `ssh-keygen`. A slot's identity is therefore stable
   across reboots, rebuilds, and repairs of OTHER slots;
3. **generates** a key pair only when the private key is missing or unusable;
4. **derives the public key from the private key** (`ssh-keygen -y`) when it is
   missing, truncated, or CONFLICTS with the private key. The private key is
   authoritative: discarding it would invalidate every `known_hosts` entry
   already distributed for that slot;
5. **sets ownership and modes explicitly** (`0400` / `0444`), and **never through
   a symlink**: `chmod`/`chown`/`stat` follow links, so a symlink planted at
   either key path is refused as key material (`-f` and `! -L`), unlinked, and
   replaced by a real file — its target keeps its own mode and content;
6. **rebuilds `known_hosts`** deterministically, at most one entry per alias
   (`ssh` refuses a file offering two keys for one host), and installs it
   ATOMICALLY (temp file + rename), so a reader never sees a partial database.

The whole body runs under one exclusive `flock` on
`<runtimeRoot>/hostkeys.lock`, so the boot-time unit and a concurrent
pre-launch repair cannot interleave.

### What the launcher does before a launch

`ensure_host_identity <slot>` validates the ACTUAL FILES — not the unit's
activation state:

* private key present and non-empty; public key present and non-empty;
* private key `root:root 0400` (hence not group/world readable), public key
  `root:root 0444`;
* the `known_hosts` database itself `root:root 0444` — symmetric with the two key
  checks, since a drifted owner or a group/world-WRITABLE mode on the database
  `StrictHostKeyChecking=yes` verifies against would let a local user swap a
  pinned key;
* EXACTLY ONE `known_hosts` entry for the alias of this host's transport, looked
  up with `ssh-keygen -F` (the matcher `ssh` itself uses). Two entries are a
  CONFLICT and are treated as broken, not accepted;
* that entry's key type and body match the public key exactly;
* the private key is LOADABLE — `ssh-keygen` can parse it and print its public
  half; a truncated or garbled key is caught here rather than by the guest's
  sshd failing to start;
* the public key file is that private key's PUBLIC HALF (compared as
  `<type> <body>`, the same normalisation the provisioner uses). Without this the
  chain is self-consistent but anchored to nothing: `known_hosts` is checked
  against the `.pub`, and the `.pub` was never checked against the key the guest
  actually serves.

If anything is off it logs `repairing SSH host identity for slot <slot>`, runs
`systemctl **restart** agent-microvm-hostkeys.service`, re-validates, and
**aborts the launch** if the identity is still incomplete. It is a repair, not a
fallback: there is no code path that relaxes verification.

> **Why `restart` and not `start`.** The unit is a `RemainAfterExit = true`
> oneshot and is normally already active on a booted host, so `systemctl start`
> returns success WITHOUT re-running `ExecStart`. A key directory deleted by hand
> would never be recreated. Use `restart` in manual recovery too.

### Operator recovery

```bash
# re-provision every slot's identity and rebuild known_hosts (idempotent):
sudo systemctl restart agent-microvm-hostkeys.service
sudo journalctl -u agent-microvm-hostkeys.service -n 20

# what the host expects for a slot, and what it recorded:
sudo ssh-keygen -y -f /var/lib/agent-microvms/sessions-ro/<slot>/hostkeys/ssh_host_ed25519_key
ssh-keygen -F <slot-ip> -f /var/lib/agent-microvms/known_hosts
# ... or, under the vsock transport:
ssh-keygen -F "vsock-mux//var/lib/microvms/<slot>/notify.vsock" \
    -f /var/lib/agent-microvms/known_hosts

# per-slot key directories present?
sudo agent-microvm doctor
```

Symptoms and what they mean:

| Symptom | Cause | Action |
| --- | --- | --- |
| `slot <s> still has no complete SSH host identity ... refusing to launch` | the provisioner ran and the identity is still broken | read its journal; a full-disk or a hand-created root-owned file in the way is the usual cause |
| `failed to provision per-slot SSH host keys` | the unit itself failed | `journalctl -u agent-microvm-hostkeys.service` |
| `missing host-key database <path>; run: systemctl restart agent-microvm-hostkeys.service` | `known_hosts` absent/unreadable | run exactly that |
| `agent-microvm-hostkeys: normalised over-permissive mode NNN on <key>` | a slot's private key had been left group/world readable | the mode is fixed automatically; decide whether to ROTATE the key (`rm` that slot's key dir, then restart the unit) |
| `ssh` fails host-key verification | the guest presents a key the host did not record | do **not** bypass it. Restart the unit; if it persists, the slot is not what the host expects — investigate |

**To rotate a slot's identity deliberately**, delete only that slot's directory
and re-provision; other slots are untouched:

```bash
sudo rm -rf /var/lib/agent-microvms/sessions-ro/<slot>/hostkeys
sudo systemctl restart agent-microvm-hostkeys.service
```

Rotation invalidates any `known_hosts` copy an operator made by hand; the
host-managed database is rebuilt automatically.

---

## The dedicated SSH key

`sshPublicKeyFile` must point at a **dedicated** public key that authorises
**only the guest `agent` user** — never the host, and never a host
`authorized_keys` file (asserted intent).

- The **public** key is committed in-repo (e.g.
  `hosts/host.f13/dedicated-agent-vm-key.pub`); a public key is not a secret.
- The matching **private** key lives in the separate `../priv` repository and is
  never committed here.
- **Recommended: inject the private key via agenix.** `secrets.nix` declares a
  `myconfig.secrets` **stub** `dedicated-agent-vm-key` with no `source` and a
  stable `dest = /run/agenix/dedicated-agent-vm-key` (root-owned, `0400`). Fill
  the source from the priv repo:

  ```nix
  # in ../priv (host.<hostname> module)
  myconfig.secrets."dedicated-agent-vm-key".source =
    ./secrets/dedicated-agent-vm-key;
  ```

  The launcher **defaults `AGENT_MICROVM_SSH_KEY` to that path** when the caller
  set none and the file exists, so `run --attach` / `ssh` (which run as root
  under `sudo`, losing any user-set env var) find the key automatically with no
  sudoers `--preserve-env` rule required. Until the source is provisioned,
  `myconfig.secrets` warns and no key is decrypted.
- To use a *specific* private key, export
  `AGENT_MICROVM_SSH_KEY=/path/to/private-key` (this overrides the agenix
  default). Under `sudo` **without** the agenix secret, `env_reset` strips the
  variable — rely on the workmux launcher's
  `--preserve-env=AGENT_MICROVM_SSH_KEY` passthrough, or give **root** a key
  matching the dedicated pubkey.

Generate the pair with the helper in this module directory (it writes the
private key into the priv repo, stages the public key here, and refuses to
overwrite an existing private key):

```bash
./modules/myconfig.ai/myconfig.ai.microvm/mk-dedicated-agent-vm-key.sh [<hostname>]
# hostname defaults to the current machine's hostname; override the priv repo
# location with PRIV_ROOT (default: ~/myconfig/priv). Result:
#   private -> $PRIV_ROOT/hosts/host.<hostname>/secrets/dedicated-agent-vm-key
#   public  -> hosts/host.<hostname>/dedicated-agent-vm-key.pub  (git add-ed)
```

Then commit the private key inside the priv repo separately.

> **`git add` reminder.** Nix evaluates from the git tree, so both the `.pub`
> file and the `ai.<host>.nix` change must be `git add`-ed or evaluation fails
> with a "path does not exist" error.

---

## Migration

| Removed | Replacement | Behaviour |
| --- | --- | --- |
| `profile` | *(none — there is one shape)* | the `lite` behaviour is unconditional; see [The guest shape](#the-guest-shape). |
| `session.enable`, `configSeed.enable` | *(none — both are unconditional)* | ONE writable + ONE read-only share, and a guest home staged at launch time. |
| `guestDotfiles.enable`, `.homeFilePrefixes`, `.xdgConfigPrefixes` | `configSeed.extraPaths` + the registry's per-agent `configPaths` | guest home-manager activation is gone; the guest home is staged per launch. |
| `slotCount`, `defaultVcpu`, `defaultMemoryMiB` | `resourceClasses` | the migration shim (a synthesized single `normal` class plus an ambiguity assertion) was removed once no host used it. |
| `allowPublicInternet` | `networkProfile = "internet"` | the translate/warn shim was removed. |
| `allowPrivateNetworks` | *(none — no profile grants it)* | use `networkProfile = "package-access"` with an explicit host proxy if a guest needs packages. |
| `allowInterVmTraffic` | *(none — isolation is unconditional)* | guest↔guest is blocked at layer 2 in every profile, so the flag could never do anything. |

Every one of them is now an **unknown option**: an eval error naming the option,
which is louder than the warning or assertion it replaces.

The interactive commands themselves are unchanged; `submit`, `cancel`,
`recover`, `usage` and the flags `--resource-class`, `--wait`,
`--persist-agent-state` are additions that default to previous behaviour. So is
`capabilities`: its default selects BOTH capabilities, so a host that says
nothing keeps every unit, path and subcommand it had.

---

## Limitations

- **IPv6 is disabled, not policed.** No equivalent IPv6 firewall policy exists,
  so IPv6 is switched off on the bridge. L2 link-local IPv6 between guests is
  out of scope.
- **Runtime properties are not measured.** The firewall ordering (including the
  `br_netfilter` dependency of the IPv4 inter-VM rule), the per-TAP `isolated`
  flag, `/workspace` writability, credential absence and cgroup containment are
  asserted at eval time only. Run
  [`runtime-validation.sh`](../runtime-validation.sh) on real KVM before
  trusting them; a successful build is not proof.
- **The hypervisor is the boundary.** Cloud Hypervisor, KVM, the guest kernel
  and virtiofsd are all in the trusted computing base; an escape through any of
  them defeats the tier.
- **Writable workspace + model disclosure.** `/workspace` is writable by the
  (hostile) agent, and prompts plus source are disclosed to whatever the LiteLLM
  proxy forwards to. Review diffs before importing.
- **Sudo policy.** The launcher requires root via `sudo`. With
  `passwordlessControl = true` the `agent-microvm` group gets a
  `NOPASSWD`+`SETENV` rule for **exactly**
  `/run/current-system/sw/bin/agent-microvm`; with the secure default sudo
  prompts, which the workmux panes will surface on first launch.
- **CI cannot exercise KVM.** The `test-f13` configuration *is* `f13` with the
  feature enabled, which is what makes the eval/build checks meaningful, but no
  boot, bridge traffic, forwarder socket or guest-to-host packet path is
  exercised.
- **Not a hardened multi-tenant cloud sandbox.** See the
  [security model](./agent-microvm-security-model.md#why-this-is-not-a-hardened-multi-tenant-cloud-sandbox).
