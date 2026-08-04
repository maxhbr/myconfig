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
| `networkProfile` | `"proxy-only"` | Named guest network policy. See [Network profiles](#network-profiles). |
| `packageProxyPort` | `null` | **Required** by `networkProfile = "package-access"`: the one host proxy port guests may reach. |
| `dnsServers` | `[ ]` | Explicit DNS policy for `networkProfile = "internet"` (empty = the host on the bridge). |
| `acknowledgeInsecureNetwork` | `false` | **Required** by the insecure profiles (`package-access`, `internet`). |

Deprecated / removed spellings (each warns or fails with a pointer; see
[Migration](#migration)): `slotCount`, `defaultVcpu`, `defaultMemoryMiB`,
`allowPublicInternet`, `allowPrivateNetworks`, `allowInterVmTraffic`.

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
- Old per-slot state left over from a resized pool (`/var/lib/microvms/<slot>`
  and the matching `hostkeys/`, `jobs/`, `state/slots/` entries) is simply
  unused and can be deleted; workspace clones are keyed by **task**.

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
| guest `agent-job` | `CPUQuota` | `vcpu × 100 %` |
| guest `agent-job` | `MemoryMax` | class RAM − `job.guestMemoryHeadroomMiB` (never below half), so an OOM kills the **agent**, not the guest |
| guest `agent-job` | `TasksMax` | `job.tasksMax` (default 4096) — fork-bomb bound |
| guest `agent-job` | `RuntimeMaxSec` | `job.maxTimeoutSeconds + job.gracePeriodSeconds` |
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

### Job directory (runtime only — never in the Nix store)

```text
/var/lib/agent-microvms/jobs/<slot>/            root:root 0755   guest: read-only*
/var/lib/agent-microvms/jobs/<slot>/spec.json   root:root 0444   the job spec (v1)
/var/lib/agent-microvms/jobs/<slot>/prompt.md   root:root 0444   the prompt TEXT
/var/lib/agent-microvms/jobs/<slot>/out/        1000:1000 0755   guest-writable
/var/lib/agent-microvms/jobs/<slot>/out/result.json              the guest's result
```

Surfaced into the guest at `/run/agent-job`. \* The share is read-**write**
because the guest must write `out/result.json`, but spec and prompt are
root-owned `0444` and virtiofsd passes ownership through — so a guest cannot
lift its own timeout or swap its own agent. Prompts never travel as process
arguments and never enter the Nix store.

`spec.json` (schema `version = 1`, validated on **both** sides):

```json
{
  "version": 1,
  "taskId": "fix-parser",
  "agent": "opencode",
  "workspace": "/workspace",
  "promptFile": "/run/agent-job/prompt.md",
  "timeoutSeconds": 3600
}
```

The guest runner rejects (as `infrastructure-error`) an unknown schema version,
an invalid `taskId`, an agent that is not batch-capable, a `workspace` other
than `/workspace`, a `promptFile` that is not *exactly*
`/run/agent-job/prompt.md`, an out-of-range `timeoutSeconds`, and **any**
attempt to name an executable (`command` / `exec` / `executable`).

`result.json`, written with tmp-file + `rename`:

```json
{
  "version": 1, "taskId": "fix-parser", "agent": "opencode",
  "state": "completed", "exitCode": 0,
  "startedAt": "…Z", "finishedAt": "…Z", "timedOut": false, "message": ""
}
```

States: `starting`, `running`, `completed`, `failed`, `timed-out`, `cancelled`
(written by the host), `infrastructure-error`. The final result is archived at
`/var/lib/agent-microvms/results/<task>.json` — outside every guest share — so
`status <task>` still reports the outcome after the slot was released.

`submit` exit codes: **0** completed, **1** the agent failed, **124** timed out,
**70** infrastructure error (no/invalid result).

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
