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
  repository,
- a dedicated **private bridge** (`agentbr0`, `192.168.83.0/24`) with **per-TAP
  layer-2 isolation**, and
- model-API access restricted to the **host LiteLLM proxy** through a
  bridge-only forwarding endpoint — no upstream API key ever reaches the guest.

Both an **interactive** (`run --attach`) and an **unattended batch** (`submit`)
execution path exist, with structured results, hard timeouts, cancellation and
recovery.

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
| this file | activation, option reference, agent registry, network profiles, the dedicated SSH key, batch job format, limitations |
| [Architecture](./agent-microvm-architecture.md) | module map, slot pool, workspace indirection, network path, credential boundary, execution paths, state lifetimes |
| [Operator guide](./agent-microvm-operator-guide.md) | exact procedures: start, submit, status, attach, cancel, collect, remove, recover, logs |
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
| `profile` | `"full"` | Overall **shape** of the tier: `full` (existing behaviour) or `lite` (one 2 vCPU / 4 GiB slot, pinned optimized EROFS guest store). See [Profiles](#profiles). |
| `enabledAgents` | `null` | **Selected** agents (registry tokens). `null` = whatever `profile` selects (`full` → all declared agents). A deselected agent is *absent* from the guest closure. See [Selected agents](#selected-agents). |
| `resourceClasses` | `{ normal = { count = 4; vcpu = 4; memoryMiB = 8192; }; }` | Fixed, **prebuilt** resource classes. See [Resource classes](#resource-classes). |
| `bridgeName` | `agentbr0` | Private bridge name. |
| `subnet` | `192.168.83.0/24` | Private subnet. |
| `gatewayAddress` | `192.168.83.1` | Host-side bridge address + LiteLLM forwarder bind address. |
| `litellmPort` | `4000` | LiteLLM proxy port. |
| `workspaceRoot` | `/var/lib/agent-microvms/workspaces` | Where per-task standalone clones are created. |
| `runtimeRoot` | `/var/lib/agent-microvms` | Runtime state (locks, markers, jobs, results, logs). |
| `stateRoot` | `/var/lib/microvms` | microvm.nix per-VM state / bind-mount source. |
| `guestAgentUid` / `guestAgentGid` | `1000` | Numeric ids of the guest `agent` user, and the host-side owner of every guest-writable path. Asserted unprivileged. |
| `job.defaultTimeoutSeconds` / `job.maxTimeoutSeconds` / `job.gracePeriodSeconds` | `3600` / `86400` / `120` | Batch-job timeouts. |
| `enableSsh` | `true` | Guest SSH server, private interface only. |
| `sshPublicKeyFile` | `null` | **Required** when `enableSsh`. See [The dedicated SSH key](#the-dedicated-ssh-key). |
| `passwordlessControl` | `false` | Scoped `NOPASSWD`+`SETENV` sudo rule for exactly `agent-microvm`, for members of the `agent-microvm` group. |
| `guestDotfiles.enable` | `true` | Provision the guest `agent` user with the host primary user's fish + coding-agent dotfiles (home-manager in the guest). |
| `guestDotfiles.homeFilePrefixes` | `.pi/`, `.codex/`, `.agents/`, `.qwen/`, `.config/git/`, `.gitconfig` | Allowlist of `home.file` keys copied from the host primary user. |
| `guestDotfiles.xdgConfigPrefixes` | `fish/`, `opencode/` | Allowlist of `xdg.configFile` keys copied from the host primary user. |
| `guestModelConfig.enable` | `true` | Guest boot-time model discovery: query the loopback LiteLLM endpoint and render the **live** model list into pi + opencode config. See [Boot-time model discovery](#boot-time-model-discovery). |
| `guestModelConfig.providerKey` / `providerName` | `litellm` / `LiteLLM (microVM)` | Provider key/name written into the generated configs. The key matches the host-side generators, so the runtime list *replaces* the build-time one. |
| `guestModelConfig.defaultContextWindow` / `maxTokens` | `131072` / `4096` | Fallbacks for models whose real values the endpoint does not expose. |
| `guestModelConfig.attempts` / `retryDelaySeconds` / `timeoutSeconds` | `5` / `2` / `5` | Endpoint query retry/timeout budget. Exhausting it is **not** an error (fail soft). |
| `networkProfile` | `"proxy-only"` | Named guest network policy. See [Network profiles](#network-profiles). |
| `packageProxyPort` | `null` | **Required** by `networkProfile = "package-access"`: the one host proxy port guests may reach. |
| `dnsServers` | `[ ]` | Explicit DNS policy for `networkProfile = "internet"` (empty = the host on the bridge). |
| `acknowledgeInsecureNetwork` | `false` | **Required** by the insecure profiles (`package-access`, `internet`). |

Deprecated / removed spellings (each warns or fails with a pointer; see
[Migration](#migration)): `slotCount`, `defaultVcpu`, `defaultMemoryMiB`,
`allowPublicInternet`, `allowPrivateNetworks`, `allowInterVmTraffic`.

### Boot-time model discovery

The agent configs a sandbox receives are **copies of build-time-rendered host
dotfiles** (see `guest-home.nix`): pi's
`~/.pi/agent/extensions/myconfig-providers.ts` and opencode's
`~/.config/opencode/opencode.json` both freeze
`services.litellm.settings.model_list` as of the guest image build. The host
proxy decides its real model list at *runtime*, so those copies drift.

`agent-model-config.service` (guest, oneshot, user `agent`, ordered after the
guest home-manager activation and before the batch-job controller) queries
`http://127.0.0.1:<litellmPort>/v1/models` — the same loopback forwarder every
guest agent talks to — optionally enriches context windows from
`/model/info`, and renders:

| Agent | Written to | Picked up because |
| --- | --- | --- |
| pi | `~/.pi/agent/extensions/zz-microvm-models.ts` | pi auto-discovers `~/.pi/agent/extensions/*.ts`; the `zz-` prefix loads after `myconfig-providers.ts` and re-registering the same provider key wins. |
| opencode | `/run/agent-model-config/opencode.json` (`OPENCODE_CONFIG`) | opencode loads `$OPENCODE_CONFIG` *in addition to*, and after, the global config, deep-merging it. |

The build-time copies are read-only store symlinks and are never modified. No
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

### Profiles

`myconfig.ai.microvm.profile` is the compatibility boundary between the
existing, full-featured tier and the lightweight one described in
[`myconfig-ai-microvm-lightweight-plan.md`](./myconfig-ai-microvm-lightweight-plan.md).
The authoritative table lives in `../profiles.nix`.

| Profile | Pool (when the host defines no sizing) | Guest store disk |
| --- | --- | --- |
| `full` **(default)** | derived from `resourceClasses` — or, for a host still on the deprecated spelling, from `slotCount` / `defaultVcpu` / `defaultMemoryMiB` | microvm.nix defaults |
| `lite` | one slot `agent-lite-0`, 2 vCPU, 4096 MiB | pinned `microvm.optimize.enable = true`, `microvm.storeDiskType = "erofs"` |

| Profile | Selected agents (when the host sets no `enabledAgents`) | Guest toolset |
| --- | --- | --- |
| `full` **(default)** | every agent `../agents.nix` declares | historical set (curl, fd, file, gnumake, rsync, tree, unzip, which, …) + fish login shell + NixOS `defaultPackages` |
| `lite` | `[ "codex" ]` | minimal documented set (POSIX toolbox, git, diffutils/patch, ripgrep, jq, less, procps, util-linux, openssh when `enableSsh`) + bash login shell, no NixOS `defaultPackages` |

Every package in the minimal set has a documented consumer (see the comment
above `guestMinimalPackages` in `../guest.nix`). Agent-specific runtimes belong
in the registry's per-agent `extraPackages`, so they are added only while that
agent is selected.

The profile only supplies **defaults**: an explicit `resourceClasses` always
outranks the profile's class table. Combining a profile that carries its own
table (`lite`) with the deprecated `slotCount` / `defaultVcpu` /
`defaultMemoryMiB` spelling is **rejected** as ambiguous rather than silently
resolved.

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
empty selection, and a selection without any batch-capable agent (the batch
machinery is still built into every guest — see plan phase 5).

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
  (`/var/lib/microvms/<slot>/workspace` and the matching `slots/`, `hostkeys/`,
  `jobs/`, `state/slots/` entries) is unused, but no longer *silent*:
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
`hermes --model <m> --oneshot <prompt>`.

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
/var/lib/agent-microvms/jobs/<slot>/                   root:root 0755
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

Surfaced into the guest at `/run/agent-job`. The share is read-**write**, but
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
  "promptFile": "/run/agent-job/input/prompt.md",
  "timeoutSeconds": 3600,
  "resourceClass": "normal",
  "persistAgentState": false
}
```

The guest controller rejects (as `infrastructure-error`) an unknown schema
version, **any unknown field**, an invalid `taskId`, a malformed or missing
`allocationToken`, a `slot` that is not this guest, an agent that is not
batch-capable, a `workspace` other than `/workspace`, a `promptFile` that is not
*exactly* `/run/agent-job/input/prompt.md`, an out-of-range `timeoutSeconds`, a
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

| Deprecated / removed | Replacement | Behaviour |
| --- | --- | --- |
| `slotCount`, `defaultVcpu`, `defaultMemoryMiB` | `resourceClasses` | still honoured as a synthesized single `normal` class (warns). Setting **both** spellings is **rejected** as ambiguous. |
| `allowPublicInternet` | `networkProfile = "internet"` | translated **with a warning** when `networkProfile` is unset; **rejected** as ambiguous when combined with a different explicit profile. |
| `allowPrivateNetworks` | *(none — no profile grants it)* | `true` is **rejected**; use `networkProfile = "package-access"` with an explicit host proxy if a guest needs packages. |
| `allowInterVmTraffic` | *(none — isolation is unconditional)* | `true` is **rejected**: guest↔guest is blocked at layer 2 in every profile, so honouring the flag would misrepresent the policy. |

The interactive commands themselves are unchanged; `submit`, `cancel`,
`recover`, `usage` and the flags `--resource-class`, `--wait`,
`--persist-agent-state` are additions that default to previous behaviour.

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
