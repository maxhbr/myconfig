<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Agent microVM sandboxes (`myconfig.ai.microvm`)

A second, stronger isolation tier for autonomous coding agents, alongside the
existing QEMU/SLiRP `flake.sandboxed-pi.nix` tier and the process-jail /
dedicated-host-user tiers. Each agent session runs inside a **Cloud
Hypervisor microVM** (via the `microvm.nix` flake input) with:

- its **own kernel** and a self-contained guest store disk (the host
  `/nix/store` is **not** shared),
- a **disposable** root and `/home/agent`; **only `/workspace` persists**,
- a single writable **virtiofs `/workspace`** mount that is a standalone git
  clone of your repo,
- a dedicated **private bridge** (`agentbr0`, `192.168.83.0/24`), and
- model-API access restricted to the **host LiteLLM proxy** through a
  bridge-only forwarding endpoint — no upstream API key ever reaches the
  guest.

Every agent process and guest workload is treated as potentially hostile. The
secure default prioritises **isolation over convenience**.

> **Status / maturity.** The module has been built and evaluated
> (`nix flake check`, `test-f13` toplevel, real-`f13` eval). The guest
> `/workspace` virtiofs share and the §11 UID/GID ownership strategy are now
> **wired up in config** (see [The `/workspace` share & ownership](#the-workspace-share--ownership)),
> and locked down by an eval test. The end-to-end *runtime* path (actually
> booting a guest and writing to `/workspace` on live KVM) has **not** yet
> been exercised. Read the [Limitations](#limitations) section before relying
> on it — several controls are still only eval-tested.

---

## Activation

The module lives in `modules/myconfig.ai/myconfig.ai.microvm/` and is
**disabled by default**. While disabled it produces zero config side effects:
it does not import the microvm.nix host module, create the bridge/firewall,
define VM slots, build the guest, or register Workmux agents.

It is enabled **only on `f13`**, and **explicitly** — never via the broad
`myconfig.ai.enable`. See `hosts/host.f13/ai.f13.nix`:

```nix
myconfig.ai.microvm = {
  enable = true;
  slotCount = 4;          # agent-0 .. agent-3 (max concurrent sandboxes)
  defaultVcpu = 4;        # vCPUs per guest
  defaultMemoryMiB = 8192;# guest RAM (MiB) per guest
  allowPublicInternet = false;
  allowPrivateNetworks = false;
  allowInterVmTraffic = false;
  sshPublicKeyFile = ./dedicated-agent-vm-key.pub;
};
```

### Knobs

| Option | Default | Meaning |
| --- | --- | --- |
| `enable` | `false` | Turn the whole tier on for a host. |
| `slotCount` | `4` | Fixed pool size (`agent-0 .. agent-<n-1>`); bounds max concurrency. |
| `defaultVcpu` | `4` | vCPUs per guest. |
| `defaultMemoryMiB` | `8192` | Guest RAM per guest (MiB). |
| `bridgeName` | `agentbr0` | Private bridge name. |
| `subnet` | `192.168.83.0/24` | Private subnet. |
| `gatewayAddress` | `192.168.83.1` | Host-side bridge address + LiteLLM forwarder bind address. |
| `litellmPort` | `4000` | LiteLLM proxy port. |
| `workspaceRoot` | `/var/lib/agent-microvms/workspaces` | Where per-task standalone clones are created. |
| `runtimeRoot` | `/var/lib/agent-microvms` | Runtime state (locks, slot session metadata). |
| `stateRoot` | `/var/lib/microvms` | microvm.nix per-VM state / bind-mount source. |
| `enableSsh` | `true` | Guest SSH server on the private interface only. |
| `sshPublicKeyFile` | `null` | **Required** when `enableSsh`. One dedicated key. |
| `guestDotfiles.enable` | `true` | Provision the guest `agent` user with the host primary user's fish + coding-agent dotfiles (home-manager in the guest). |
| `guestDotfiles.homeFilePrefixes` | `.pi/`, `.codex/`, `.agents/`, `.qwen/`, `.config/git/`, `.gitconfig` | Allowlist of `home.file` keys copied from the host primary user. |
| `guestDotfiles.xdgConfigPrefixes` | `fish/`, `opencode/` | Allowlist of `xdg.configFile` keys copied from the host primary user. |
| `networkProfile` | `"proxy-only"` | Named guest network policy: `offline`, `proxy-only`, `package-access`, `internet`. See [Network profiles](#network-profiles). |
| `packageProxyPort` | `null` | **Required** by `networkProfile = "package-access"`: the explicit host proxy port guests may reach. |
| `dnsServers` | `[ ]` | Explicit DNS policy for `networkProfile = "internet"` (empty = the host on the bridge). |
| `acknowledgeInsecureNetwork` | `false` | **Required** by the insecure profiles (`package-access`, `internet`). |
| `allowPublicInternet` | `false` | **DEPRECATED** → `networkProfile = "internet"`. Translated with a warning when `networkProfile` is unset; rejected as ambiguous when it contradicts an explicit profile. |
| `allowPrivateNetworks` / `allowInterVmTraffic` | `false` | **REMOVED** — setting either to `true` is rejected by an assertion. See [Network profiles](#network-profiles). |

### Network profiles

`myconfig.ai.microvm.networkProfile` replaces the three ambiguous booleans with
four named, coherent policies. The authoritative capability table is
[`network-profiles.nix`](../network-profiles.nix); it is resolved **once** (in
`default.nix`) and drives both the host firewall (`network.nix`) and the
guest-side proxy/DNS/forwarder configuration (`guest.nix`), so host policy and
guest configuration can never disagree.

| Profile | Additionally allowed | Guest-side effect |
| --- | --- | --- |
| `offline` | nothing — only host→guest control traffic (ssh/console) and its replies | no loopback LiteLLM forwarder |
| `proxy-only` **(default)** | `guest → <gatewayAddress>:<litellmPort>` (the model API) | loopback LiteLLM forwarder |
| `package-access` | additionally `guest → <gatewayAddress>:<packageProxyPort>`, one explicit host proxy port. **No routing, NAT or DNS**, so it is *not* unrestricted internet | `http_proxy`/`https_proxy` point at that proxy |
| `internet` | routing **plus** NAT/masquerading, DNS restricted to `dnsServers`, rate-limited drop logging | guest resolvers set to `dnsServers` |

In **every** profile:

- guest↔guest traffic is blocked — per-TAP L2 `isolated` *and* the IPv4 inter-VM
  `FORWARD` DROP. There is no way to relax it (hence the removal of
  `allowInterVmTraffic`);
- the cloud-metadata IP `169.254.169.254` is dropped first, in `INPUT` and
  `FORWARD`;
- private/special-use IPv4 ranges (host LAN, VPN peers, RFC1918, CGNAT,
  loopback, link-local, multicast, reserved) are dropped — the only exception is
  a resolver the operator **explicitly** lists in `dnsServers` (hence the
  removal of `allowPrivateNetworks`);
- `INPUT` and `FORWARD` end in a terminal `DROP` (fail closed);
- host→guest control traffic is allowed (the host is trusted).

`package-access` and `internet` require `acknowledgeInsecureNetwork = true`.

**Migration.** `allowPublicInternet = true` is still honoured: with no explicit
`networkProfile` it is translated to `internet` and a warning is emitted;
combined with a *different* explicit profile it is rejected as ambiguous rather
than silently resolved. Merely *defining* any of the three booleans emits a
deprecation warning. `allowPrivateNetworks = true` / `allowInterVmTraffic = true`
are rejected outright — no profile grants those, and silently ignoring them
would misrepresent the policy. Every case is locked down by
`microvm-eval-rejects-invalid` and `microvm-network-profiles`.

### The dedicated SSH key

`sshPublicKeyFile` must point at a **dedicated** public key that authorises
**only the guest `agent` user** — never the host, and never a host
`authorized_keys` file (asserted intent, plan §18).

- The **public** key is committed in-repo at
  `hosts/host.f13/dedicated-agent-vm-key.pub` (a public key is not a secret).
- The matching **private** key is **not** in this repo. It is managed
  out-of-band and lives in the separate `../priv` repository. Never commit a
  private key here.
- **Recommended: inject the private key via agenix.** When the feature is
  enabled (and `enableSsh`), `secrets.nix` declares a `myconfig.secrets`
  **stub** `dedicated-agent-vm-key` with **no `source`** and a stable
  `dest = /run/agenix/dedicated-agent-vm-key` (root-owned, `0400`). Fill the
  `source` from the **priv** repo:

  ```nix
  # in ../priv (host.<hostname> module)
  myconfig.secrets."dedicated-agent-vm-key".source =
    ./secrets/dedicated-agent-vm-key;
  ```

  agenix then decrypts the private key to that `dest`, and the launcher
  **defaults `AGENT_MICROVM_SSH_KEY` to it** when the caller set none and the
  file exists — so the `run --attach` / `ssh` readiness paths (which run as
  root under `sudo`, losing any user-set env var) find the dedicated key
  automatically, with **no** sudoers `--preserve-env` rule required. Until the
  source is provisioned, `myconfig.secrets` emits its standard
  "source is missing for: dedicated-agent-vm-key" warning and no key is
  decrypted.
- To use a *specific* private key with the launcher's `ssh` / `--attach`
  paths, export `AGENT_MICROVM_SSH_KEY=/path/to/private-key` (this overrides
  the agenix default above).
- For `run --attach` under `sudo` **without** the agenix secret: `sudo`'s
  `env_reset` strips the variable, so either export `AGENT_MICROVM_SSH_KEY`
  and rely on the workmux launcher's `--preserve-env=AGENT_MICROVM_SSH_KEY`
  passthrough (the sudoers policy must permit it), or give **root** an ssh key
  matching the dedicated pubkey (e.g. via `/root/.ssh` or an `ssh_config`
  `IdentityFile` entry).

Generate the pair with the helper script in this module directory. It writes
the **private** key into the priv repo and the **public** key into this repo
(staging it), and refuses to overwrite an existing private key:

```bash
./modules/myconfig.ai/myconfig.ai.microvm/mk-dedicated-agent-vm-key.sh [<hostname>]
# hostname defaults to the current machine's hostname; override the priv
# repo location with PRIV_ROOT (default: ~/myconfig/priv). Result:
#   private -> $PRIV_ROOT/hosts/host.<hostname>/secrets/dedicated-agent-vm-key
#   public  -> hosts/host.<hostname>/dedicated-agent-vm-key.pub  (git add-ed)
```

Then commit the private key inside the priv repo separately (never here).

Manual equivalent (throwaway, private key deleted immediately):

```bash
tmp=$(mktemp -d)
ssh-keygen -t ed25519 -N '' -C 'agent-microvm@f13' -f "$tmp/key"
cp "$tmp/key.pub" hosts/host.f13/dedicated-agent-vm-key.pub
# store "$tmp/key" (the PRIVATE key) in ../priv, then:
rm -rf "$tmp"
git add hosts/host.f13/dedicated-agent-vm-key.pub
```

> **`git add` reminder.** Nix evaluates from the git tree, so both the `.pub`
> file and the `ai.<host>.nix` change must be `git add`-ed or evaluation
> fails with a "path does not exist" error. The script stages the `.pub` for
> you.

---

## Supported agents — the authoritative registry

[`agents.nix`](../agents.nix) is the **single source of truth** for which
coding agents a sandbox supports. Everything agent-shaped is generated from
it; there is no second list to keep in sync:

| Consumer | What is generated |
|---|---|
| `guest.nix` | the agent packages baked into the immutable guest closure, the per-agent guest environment, and the `agent-run` dispatch table |
| `launcher.nix` | `--agent` validation (`validate_agent_name`) and the `agent-microvm --help` agent listing |
| `workmux.nix` | the `myconfig.ai.workmux.agents.microvm-*` entries and their pane launchers |
| `default.nix` | assertions that every registry entry is well-formed |
| `tests/microvm.nix` | the `microvm-agent-registry` / shellcheck checks |

A registry entry is:

```nix
<name> = {
  package = pkgs.<attr>;       # baked into the guest closure (never installed at runtime)
  executable = "<bin>";        # what `agent-run <name>` execs inside the guest
  workmuxType = "<type>";      # optional, defaults to <name>
  interactiveArgs = [ ];       # optional extra argv for the interactive session
  guestEnvironment = { };      # optional endpoint plumbing (NEVER a credential)
  persistentState = {          # verified state paths, relative to the guest home
    enabledByDefault = false;  # guest home stays DISPOSABLE by default
    directories = [ ];
  };
};
```

`workmuxName` is derived (`microvm-<name>`), so adding an agent is a one-entry
change. To list the currently supported agents:

```bash
sudo agent-microvm --help      # "Supported agents (--agent)" section
```

The registry is instantiated **exactly once** (in `default.nix`) and passed to
the other modules through `_module.args.agentRegistry`, so no consumer can
re-instantiate it with different context (LiteLLM port, model name).

### Hermes

Hermes (`--agent hermes`, workmux `microvm-hermes`) is the same
`inputs.hermes-agent` package the host `myconfig.ai.hermes` backends use, baked
into the guest closure — never fetched by the upstream `curl | bash` installer,
`pip` or `npm` at boot (§8).

```bash
# Directly:
sudo agent-microvm run --attach --agent hermes \
  --name my-task --repository ~/src/my-repo

# Or through workmux (the normal frontend):
workmux add --agent microvm-hermes my-feature
```

- **Model routing.** Hermes resolves its endpoint as `config.yaml` `base_url` →
  `CUSTOM_BASE_URL` → `OPENROUTER_BASE_URL` → `openrouter.ai`. The registry
  therefore sets `OPENROUTER_BASE_URL=http://127.0.0.1:4000/v1` (the guest
  loopback LiteLLM endpoint) and pins `--model` to
  `myconfig.ai.hermes.model.default` — the same LiteLLM route the host hermes
  backends use. No upstream provider credential exists in the guest: the
  placeholder `OPENAI_API_KEY` is what hermes picks for a non-OpenRouter
  `base_url`, and it also satisfies hermes' first-run "any provider
  configured?" guard, so the setup wizard never appears.
- **State (verified, not guessed).** All hermes state lives under one root,
  `$HERMES_HOME` (default `~/.hermes`; see `hermes_constants.py`
  `get_hermes_home()`): `config.yaml`, `.env`, `auth.json`, `state.db`,
  `sessions/`, `memories/`, `skills/`, `logs/`, `plugins/`, `cron/`,
  `scripts/`. It is declared in the registry as
  `persistentState.directories = [ ".hermes" ]` with
  `enabledByDefault = false`, i.e. **disposable**: the guest home is a tmpfs
  rebuilt every boot, so a fresh sandbox starts with no memories, skills or
  sessions and nothing hermes writes escapes the VM (opt-in, task-scoped
  persistence is ticket 5).
- **Workmux profile.** workmux has no `hermes` profile, so the pane falls back
  to workmux's default profile (no prompt injection / resume flags). The pane
  still launches and status hooks still work.

#### Interactive smoke test

```bash
sudo agent-microvm run --name hermes-smoke --repository ~/src/my-repo \
  --agent hermes --attach
# inside the guest (or: sudo agent-microvm ssh hermes-smoke):
command -v hermes                  # -> /run/current-system/sw/bin/hermes
hermes version                     # prints the baked-in hermes version
env | grep -E 'OPENROUTER|OPENAI'   # endpoint + placeholder key only
env | grep -Ei 'anthropic_api|token|secret'  # must show no real credential
ls -a "$HOME"                      # ~/.hermes appears only after first run
curl -sS http://127.0.0.1:4000/v1/models | head -c 200   # LiteLLM reachable
```

---

## Launching via Workmux

Workmux stays the **frontend** — it owns the worktree, the tmux pane, task
naming, status hooks and cleanup (`workmux merge` / `workmux remove`). The
host launcher `agent-microvm` is only the **backend**.

```bash
workmux add --agent microvm-claude feature-name
workmux add --agent microvm-pi      feature-name
workmux add --agent microvm-codex   feature-name
workmux add --agent microvm-opencode feature-name
```

The `microvm-*` agent set is generated from
[the agent registry](#supported-agents--the-authoritative-registry); the list
above is illustrative, not a second source of truth.

Each agent's pane command:

1. resolves the linked main repository from the worktree's shared git dir,
2. maps the workmux branch to a launcher-safe task name
   (`[a-zA-Z0-9._-]`, `<= 64` chars), and
3. execs, as separate argv (never a shell string):

   ```bash
   sudo agent-microvm run --attach \
     --name <task> --repository <main-repo> --agent <bin>
   ```

No network-relaxation flags are passed, so the guest runs under the host's
configured `networkProfile` — the secure **proxy-only** default unless the host
deliberately chose otherwise.

> The pane runs `sudo agent-microvm …`; there is currently **no**
> passwordless-sudoers rule, so the first launch **will prompt for a
> password** (see [Limitations](#limitations)).

---

## Unattended batch jobs

Interactive `run --attach` is unchanged. In addition, a slot can run a job
**unattended** from a versioned job specification (improvement ticket 4). The
host-side `submit` / `cancel` / `recover` commands are documented in the next
section; the format and the guest side are described here.

### Job directory (runtime only — never in the Nix store)

```text
/var/lib/agent-microvms/jobs/<slot>/            root:root 0755   guest: read-only*
/var/lib/agent-microvms/jobs/<slot>/spec.json   root:root 0444   the job spec (v1)
/var/lib/agent-microvms/jobs/<slot>/prompt.md   root:root 0444   the prompt TEXT
/var/lib/agent-microvms/jobs/<slot>/out/        1000:1000 0755   guest-writable
/var/lib/agent-microvms/jobs/<slot>/out/result.json              the guest's result
```

The directory is surfaced into the guest at `/run/agent-job` by a third
virtiofs share. \* The share is read-**write** because the guest must write
`out/result.json`, but the spec and prompt are root-owned `0444` inside a
root-owned `0755` directory and virtiofsd passes ownership through — so the
untrusted `agent` user can only **read** them (and can only write inside
`out/`). A guest therefore cannot lift its own timeout or swap its own agent.

Prompts never travel as process arguments and never enter the Nix store.

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

The guest runner rejects (as `infrastructure-error`): an unknown schema
version, an invalid `taskId`, an agent that is not batch-capable in the
registry, a `workspace` other than `/workspace`, a `promptFile` that is not
*exactly* `/run/agent-job/prompt.md` (so traversal and symlink games fail), a
non-integer/out-of-range `timeoutSeconds`, and **any** attempt to name an
executable (`command` / `exec` / `executable`) — the agent is always resolved
through [the registry](#supported-agents--the-authoritative-registry).

### `result.json`

Written with tmp-file + `rename`, so the host never reads a half-written
result:

```json
{
  "version": 1, "taskId": "fix-parser", "agent": "opencode",
  "state": "completed", "exitCode": 0,
  "startedAt": "…Z", "finishedAt": "…Z", "timedOut": false, "message": ""
}
```

States: `starting`, `running`, `completed`, `failed`, `timed-out`,
`cancelled` (written by the host), `infrastructure-error`.

### Guest service

`agent-job.service` is **inert unless a job is present**
(`ConditionPathExists=/run/agent-job/spec.json`), waits for both the workspace
and job mounts (`RequiresMountsFor`), and runs the generated batch dispatch as
the unprivileged `agent` user in `/workspace` with
`NoNewPrivileges`, `PrivateDevices`, `PrivateTmp`, `ProtectKernelTunables`,
`ProtectKernelModules`, `ProtectControlGroups` and `RestrictSUIDSGID`.

The timeout is enforced **three** times: per-job `timeout(1)` in the guest, the
unit's static `RuntimeMaxSec` ceiling
(`job.maxTimeoutSeconds + job.gracePeriodSeconds`), and the host's own wait
(`job.gracePeriodSeconds` beyond the job's timeout).

> **No guest-side power-off.** microvm.nix runs `microvm@<slot>` with
> `Restart = "always"`, so a guest that powered itself off after the job would
> be rebooted immediately. Stopping the VM is the host's part of the lifecycle.

Per-agent batch invocations come from the registry (`batchArgs` / `batchStdin`),
verified against each pinned build's own `--help`:
`claude -p <prompt>`, `codex exec -` (prompt on **stdin**),
`opencode run <prompt>`, `pi --print <prompt>`,
`hermes --model <m> --oneshot <prompt>`.

---

## Listing & status

```bash
sudo agent-microvm list             # one line per slot
sudo agent-microvm status           # detailed, all slots
sudo agent-microvm status agent-0   # a single slot
sudo agent-microvm status <task>    # resolve a running task to its slot
```

`status` reports slot, service state, IP, MAC, VSOCK CID, task, workspace path,
bind-mount status, agent type, start time, SSH readiness, session state, a
`stale` flag, and the lock owner — **never** secrets.

> A slot with a persisted session marker but an inactive unit is flagged
> `stale: yes` (e.g. after a hard kill / power loss where the cleanup trap did
> not run). Reclaim it with `agent-microvm destroy <slot>`.

---

## Connecting

```bash
sudo agent-microvm ssh agent-0            # interactive shell as guest `agent`
sudo agent-microvm ssh agent-0 -- id      # run a command
sudo agent-microvm console agent-0        # follow the serial console (journal)
```

`ssh` verifies the guest **strictly**
(`StrictHostKeyChecking=yes`, `UserKnownHostsFile=/var/lib/agent-microvms/known_hosts`):
every slot has a **stable, per-slot ed25519 host identity**, so a wrong or
unknown host key aborts the connection instead of being accepted. Set
`AGENT_MICROVM_SSH_KEY` to pick the client private key.

### Per-slot SSH host identities

`agent-microvm-hostkeys.service` (see `hostkeys.nix`) provisions, on the host
and at runtime:

```text
/var/lib/agent-microvms/hostkeys/<slot>/ssh_host_ed25519_key      root:root 0400
/var/lib/agent-microvms/hostkeys/<slot>/ssh_host_ed25519_key.pub  root:root 0444
/var/lib/agent-microvms/known_hosts                               root:root 0444
```

- **One key per slot, never shared.** Keys are generated once and kept, so a
  slot's identity is stable across reboots and rebuilds. They are **not** in
  the Nix store (which is world-readable) and **not** agenix secrets (they are
  host-local, per-slot, regenerable identities).
- **Delivered through a read-only, per-slot virtiofs share** mounted at
  `/var/lib/agent-hostkey` in the guest. virtiofsd passes ownership through,
  so the private key stays `root:root 0400` inside the guest: the untrusted
  `agent` user cannot read it, the guest cannot rewrite its own identity, and
  no other slot's directory is visible. This is a deliberate, documented
  amendment to the original "exactly one share" rule — see
  [The `/workspace` share](#the-workspace-share--ownership).
- **The guest generates no host keys of its own**
  (`services.openssh.generateHostKeys = false`), so the identity in
  `known_hosts` is the only one it can present.
- `known_hosts` holds public keys only and is world-readable, so a non-root
  operator can also run `agent-microvm ssh` with strict verification.

If the file is missing the launcher fails closed with a pointer to
`systemctl start agent-microvm-hostkeys.service` (which `run` also invokes
before booting a slot).

### Reserved VSOCK control-channel identity

Every slot additionally owns a unique, deterministic AF_VSOCK context id
(`cid = 8300 + <index>`, reported by `agent-microvm status`), avoiding the
reserved CIDs `0`/`1`/`2` and `VMADDR_CID_ANY`. It is **reserved, not yet
wired**: handing it to `microvm.vsock.cid` flips `microvm@<slot>.service` to
`Type=notify` (microvm.nix adds a socat↔vsock systemd-notify bridge), a startup
change that can only be validated by booting a guest on KVM. It is therefore
activated together with the noninteractive control channel that uses it
(batch job readiness / status / cancellation / results).

---

## Logs

Everything is supervised by systemd, so use the journal:

```bash
journalctl -u microvm@agent-0.service     # the guest VM (Cloud Hypervisor + serial console)
journalctl -u agent-litellm-proxy.service # the bridge-only LiteLLM forwarder
journalctl -u agent-microvm-agentbr0-disable-ipv6.service  # bridge IPv6-disable oneshot
journalctl -u agent-microvm-attach-agent-0.service         # enslave + L2-isolate TAP vm-agent-0
journalctl -u agent-microvm-hostkeys.service               # per-slot SSH host keys + known_hosts
```

`agent-microvm console <slot>` is a shortcut for
`journalctl -f -u microvm@<slot>.service`. No secrets, env dumps, prompts,
source or tokens are logged.

---

## Stop vs destroy vs remove workspace

These have **distinct** semantics (plan §26/§35). **None of them delete your
clone except `workspace-remove`.**

| Command | VM | bind mount | slot transient state | workspace / git / patches |
| --- | --- | --- | --- | --- |
| `stop <slot\|task>` | stopped | unmounted | removed | **kept** |
| `destroy <slot\|task>` | stopped | unmounted | removed | **kept** |
| `workspace-remove <task> [--force]` | (must already be stopped) | — | — | **deleted** |

```bash
sudo agent-microvm stop agent-0             # end the session, keep everything on disk
sudo agent-microvm destroy agent-0          # clear ephemeral slot runtime, keep the clone
sudo agent-microvm workspace-remove feature-name   # delete the standalone clone
```

`workspace-remove` is separate and **guarded**:

- it **refuses** if the clone has **uncommitted changes** (`git status`) or
  **unexported commits** (commits on local branches not in any remote) unless
  you pass `--force`;
- it **refuses** while the clone is still bind-mounted into (or recorded as
  in use by) any running slot — stop that slot first.

`--attach` sessions (the Workmux path) tear the VM down automatically on exit
via a cleanup trap, always **keeping** the workspace clone. Interrupted
launches also clean up the VM / bind mount / lock / TAP but keep the clone.

---

## Inspecting & importing changes

The workspace is a **standalone clone** (`git clone --no-local`), so the
original repo is never shared into the guest and there is **no** shared git
common dir. Work happens on branch **`agent/<task>`** by default. The launcher
performs **no** auto push/merge/commit/delete — importing is always explicit.

From the workspace clone:

```bash
clone=/var/lib/agent-microvms/workspaces/<task>
git -C "$clone" diff
git -C "$clone" format-patch "origin/HEAD..agent/<task>"
```

> **Note.** `format-patch` here assumes `origin/HEAD` resolves in the
> standalone clone. If it is unset (e.g. the remote never advertised a default
> branch), substitute the concrete base branch, e.g.
> `git -C "$clone" format-patch "origin/main..agent/<task>"`.

To pull the branch back into your original repo:

```bash
# from your original repo checkout
git fetch "$clone" "agent/<task>:refs/heads/agent/<task>"
git log agent/<task>          # review, then merge/cherry-pick as you see fit
```

`agent-microvm run` prints the exact `diff` / `format-patch` commands for the
slot when it starts in detached mode.

---

## The `/workspace` share & ownership

**§10 — exactly one *writable* share.** Each slot's guest declares exactly
**one** read-write `microvm.shares` entry: a **read-write virtiofs** share
tagged `workspace`, mounted at `/workspace`. Its host `source` is
`/var/lib/microvms/<slot>/workspace` (`${stateRoot}/<slot>/workspace`) — the
**same** path the launcher uses as its `mount --bind` target
(`mount_point()` in `launcher.nix`). So the launcher bind-mounts the
standalone clone onto that host directory, and virtiofsd surfaces it into the
guest as the single writable `/workspace`.

microvm.nix keeps the guest `/nix/store` on its own **storeDisk**
(`microvm.storeOnDisk` defaults to `true` unless a share's source is
`/nix/store`, which this one is not), so it does **not** add a store share.
The guest therefore has **exactly two shares** — the writable `/workspace`
above and the **read-only, per-slot SSH host-key share** at
`/var/lib/agent-hostkey` (only when `enableSsh` is true; see
[Per-slot SSH host identities](#per-slot-ssh-host-identities)). No `/nix`, no
`/home`, no host sockets, no cross-slot paths. The
`microvm-eval-workspace-share` check asserts exactly this: two virtiofs
shares, the workspace read-write at the expected source, the hostkey share
**read-only** at the per-slot source, and no other mount point.

> The hostkey share is a deliberate amendment to the original "exactly one
> share" rule, required to give each slot a verifiable SSH identity
> (improvement ticket 3 B). It is read-only, root-only (`0400` passed through
> by virtiofsd), single-purpose and per-slot, so it exposes no host state to
> the untrusted agent.

**§11 — UID/GID ownership.** virtiofsd passes file ownership through
unchanged (no `--translate-uid/--translate-gid`), so the numeric owner of the
host clone tree is exactly what the guest sees. The guest `agent` user is
**uid/gid 1000** (`guest.nix` `users.users.agent`). Therefore, right after
creating the clone (and its `agent/<task>` branch), the launcher runs:

```bash
chown -R 1000:1000 -- "$clone"
```

Inside the guest, `/workspace` then appears owned by `agent` and is
read-write, so `agent-run`'s `test -w /workspace` check passes.

- **Why 1000:1000 and not a new dedicated host user?** On `f13`, uid/gid 1000
  is already the primary **unprivileged interactive user** (the human who
  inspects/exports the agent's result, plan §25). Creating a *new* host user
  at uid 1000 would collide with them, and picking a different guest uid would
  add moving parts for no gain. Chowning to 1000:1000 makes the clone owned by
  that same human on the host — who can then `git -C "$clone" diff` / import
  the branch directly — while appearing `agent`-owned in the guest.
- **No privileged mapping.** uid/gid 1000 is **not** a privileged id, so no
  guest id maps to a privileged host id (plan §11). The guest agent cannot,
  via the share, create host files owned by root or any system account.
- **Scope.** Only the per-task clone under
  `/var/lib/agent-microvms/workspaces/<task>` is chowned; no other host
  permissions are touched.

## Guest dotfiles & the loopback LiteLLM endpoint

The guest `agent` user is provisioned with the **same shell and coding-agent
dotfiles as the host primary user**, so a sandboxed agent has the familiar
fish prompt/abbreviations/functions and the pi / opencode / codex
configuration, skills and prompts. This is done by `guest-home.nix`, which
runs **home-manager inside the guest** and copies the host primary user's
*already-evaluated* `home.file` / `xdg.configFile` entries (their `source`
fields are store paths, so no host home module is re-evaluated in the guest;
home-manager bakes the sources into the guest's own store disk).

The copy is an **allowlist**, never a denylist — the same fail-closed posture
as `myconfig.agentUsers`. Only the prefixes in
`myconfig.ai.microvm.guestDotfiles.{homeFilePrefixes,xdgConfigPrefixes}` cross
the boundary, so host secrets (tokens, credentials, keys) are never dragged
into the sandbox by accident. Defaults cover fish, opencode, pi, codex,
`.agents/`, and git config. Set `guestDotfiles.enable = false` for a bare
home.

### Why the agents "just work" against `192.168.83.1:4000`

Every host-provisioned agent config (e.g.
`~/.pi/agent/extensions/myconfig-providers.ts`,
`~/.config/opencode/opencode.json`) hardcodes the host's **loopback** LiteLLM
address `http://127.0.0.1:4000/v1`. To make those copied configs reach the
real proxy from inside the guest — where LiteLLM is only reachable at the
bridge gateway `192.168.83.1:4000` — the guest runs a **reverse** of the
host's bridge-only forwarder: a socket-activated `systemd-socket-proxyd`
(`litellm-forwarder`) listens on `127.0.0.1:4000` and forwards to
`${gatewayAddress}:${litellmPort}` (`192.168.83.1:4000`). So the guest
presents the *same* loopback endpoint the host does, the copied configs work
verbatim, and `OPENAI_BASE_URL` is likewise set to `http://127.0.0.1:4000/v1`.
Net effect: pi and every other agent transparently "rely on"
`192.168.83.1:4000` (the host on the agent bridge) without any per-agent
config rewrite.

## Security properties

What the module actually enforces (plan §5, §13–§18, §45):

- **Own kernel & disposable state.** Cloud Hypervisor guest with its own
  kernel; root and `/home/agent` are disposable; **only `/workspace`
  persists** across stop/destroy/restart.
- **Standalone clone only.** The single writable host-backed path is the
  per-task `git clone --no-local` at `/workspace`, shared via **virtiofs**
  (mounted via `mount --bind`, not a symlink). No primary checkout, no linked
  worktree, no shared git metadata.
- **Non-root guest user.** The agent runs as `agent` (uid 1000, no extra
  groups, locked password). `agent-run` **refuses to run as root** and verifies
  `/workspace` is a mounted, writable mount before `cd`-ing in and exec-ing the
  agent.
- **Hardened SSH, private interface only.** `PermitRootLogin=no`,
  `PasswordAuthentication=false`, `KbdInteractiveAuthentication=false`,
  `AllowAgentForwarding=no`, `X11Forwarding=false`, `PermitTunnel=no`,
  `AllowTcpForwarding=no`; exactly one dedicated public key; not reachable from
  the LAN.
- **Authenticated control channel.** Each slot has its own stable ed25519 host
  key (read-only per-slot share; the guest generates none) and the launcher
  connects with `StrictHostKeyChecking=yes` against the host-generated
  `known_hosts` — so the operator always talks to *the* slot, not to whatever
  answers on its address. See
  [Per-slot SSH host identities](#per-slot-ssh-host-identities).
- **No host creds / sockets / home.** The guest gets no host home, no host SSH
  keys, no SSH/GPG agent sockets, no password store, no Docker/Podman sockets,
  no D-Bus/systemd sockets, no host Nix daemon socket, and no writable host Nix
  store. The guest environment does not receive `SSH_AUTH_SOCK`,
  `GPG_AGENT_INFO`, `AWS_*`, `GOOGLE_*`, `AZURE_*`, `KUBECONFIG`,
  `GITHUB_TOKEN`, `GH_TOKEN` or `GITLAB_TOKEN`.
- **TAP enslaved to the bridge.** microvm.nix's `type = "tap"` only *creates*
  the per-slot tap (`vm-agent-<n>`) and brings it up — it does **not** attach
  it to any bridge. A per-slot `agent-microvm-attach-<slot>` oneshot (ordered
  after `microvm-tap-interfaces@<slot>`, before `microvm@<slot>`, `partOf` the
  VM) runs `ip link set vm-agent-<n> master agentbr0`, giving the guest its L2
  path to the gateway. Without it the guest boots and runs sshd but is
  unreachable (SSH readiness times out).
- **Per-TAP Layer 2 isolation.** The same oneshot then runs
  `bridge link set dev vm-agent-<n> isolated on`. The kernel bridge refuses to
  forward frames **between isolated ports**, in either direction and for
  *every* EtherType — so guest↔guest ARP spoofing, IPv6 ND and non-IP traffic
  are impossible even though iptables cannot filter them. Isolated ports can
  still reach non-isolated ports and the bridge itself, so guest↔host
  (gateway, LiteLLM forwarder, SSH) is unaffected; the bridge's host-facing
  side is deliberately **not** isolated. This makes the IPv4 inter-VM
  `FORWARD` DROP a second line of defence rather than the only one, and
  removes the co-resident-guest MITM risk for the unpinned
  `agent-microvm ssh` / `--attach` sessions. Verify at runtime with
  `bridge link show` (each active guest TAP reports `isolated on`).
- **Deny-all, proxy-only network.** The default `networkProfile = "proxy-only"`
  firewall policy on `agentbr0` is deny-all-except-proxy: the only egress a
  guest gets is `guest -> 192.168.83.1:4000` (see
  [Network profiles](#network-profiles) for the other three). Dedicated chains `AGENT_MICROVM_INPUT` /
  `_FORWARD` / `_OUTPUT` (built on the existing NixOS firewall, no nftables
  migration) block all other host ports, the host LAN, RFC1918 / CGNAT /
  loopback / link-local / multicast / reserved ranges, inter-VM (TAP-to-TAP)
  traffic, and the general internet. The **cloud-metadata IP
  `169.254.169.254` is blocked unconditionally and first**, so no later ACCEPT
  can shadow it. The FORWARD chain ends in a **terminal `DROP` (fail
  closed)**.
- **LiteLLM boundary.** The main LiteLLM proxy stays **loopback-only**
  (`127.0.0.1:4000`). A **bridge-only** `systemd-socket-proxyd` endpoint
  (`agent-litellm-proxy`) binds **only** to `192.168.83.1:4000`
  (`BindToDevice=agentbr0`, never `0.0.0.0`/LAN) and forwards to the loopback
  proxy. It runs `DynamicUser`, `NoNewPrivileges`, `PrivateTmp`,
  `ProtectSystem=strict`, `ProtectHome`.
- **No upstream key in the guest.** `OPENAI_BASE_URL` points at the guest
  loopback endpoint `127.0.0.1:4000`, which the guest `litellm-forwarder`
  socket-proxy forwards to the bridge-only host endpoint `192.168.83.1:4000`
  (the only reachable model-API peer); `OPENAI_API_KEY=not-needed` is a
  placeholder. No real upstream key, and no secrets in the Nix store / flake /
  scripts / argv / logs / images / workspaces. Guest dotfiles are copied by
  an **allowlist** (`guestDotfiles.*`), so no credential-bearing path leaks
  into the guest.
- **Bounded & lock-protected.** Fixed vCPU/mem and a fixed slot pool bound
  resource use; a global allocator lock plus per-slot `flock`s prevent
  double-allocation.

---

## Limitations

Honest caveats — read these before trusting the tier:

- **IPv6 disabled (MVP).** No equivalent IPv6 firewall policy exists, so IPv6
  is simply disabled on the bridge. L2 link-local IPv6 between guests is out of
  scope for the MVP.
- **TAP-to-bridge enslavement was missing (now fixed).** Earlier revisions
  created the per-slot tap but never enslaved it to `agentbr0`, so a guest
  booted and ran sshd yet was unreachable and every `run`/`--attach` timed
  out at the SSH-readiness wait. This is now handled by the per-slot
  `agent-microvm-attach-<slot>` oneshot (see Security properties). The rest
  of the packet path (firewall ordering, proxy egress) is still only
  lightly exercised on live KVM — see the next bullets.
- **`/workspace` runtime write is config-wired but not KVM-verified.** The
  guest virtiofs share and the §11 `chown -R 1000:1000` ownership strategy are
  now in place (see [The `/workspace` share & ownership](#the-workspace-share--ownership))
  and eval-tested, so the previously-known `run --attach` breakage (root-owned
  clone vs. uid-1000 guest) is **fixed in config**. But actually **booting** a
  guest and confirming `/workspace` is mounted, writable and correctly-owned
  is a runtime step (plan §41/§42) that has **not** been executed on live KVM
  here.
- **No passwordless sudoers yet.** The Workmux pane runs `sudo agent-microvm`
  with no dedicated sudoers rule, so it will **prompt for a password** on first
  launch. Acceptable for an interactive tmux workflow; a rule is left to a
  later phase.
- **Runtime attack surface.** Cloud Hypervisor, KVM, the guest kernel and
  virtiofsd are all part of the trusted computing base; a guest escape through
  any of them defeats the isolation.
- **Writable workspace + disclosure.** `/workspace` is writable by the (hostile)
  agent, and the agent's prompts and your source are disclosed to whatever the
  LiteLLM proxy forwards to. This tier does not change that.
- **Firewall-ordering-dependent, eval-only so far.** The network controls
  depend on iptables rule ordering and on `br_netfilter` +
  `bridge-nf-call-iptables` for inter-VM enforcement. They have been
  **eval-tested only** — the packet path has **not** yet been verified from a
  live guest on real KVM. Do not treat a successful build as proof the runtime
  firewall is secure.
- **`test-f13` vs real `f13`.** The `test-f13` configuration builds with the
  feature **disabled**; the real `f13` enables it. CI eval/build does not
  exercise KVM, the bridge, the forwarder socket or the guest-to-host packet
  path.
