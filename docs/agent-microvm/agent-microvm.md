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
| `allowPublicInternet` / `allowPrivateNetworks` / `allowInterVmTraffic` | `false` | Insecure network relaxations. Enabling any one **also** requires `acknowledgeInsecureNetwork = true`. Keep them **false**. |

### The dedicated SSH key

`sshPublicKeyFile` must point at a **dedicated** public key that authorises
**only the guest `agent` user** — never the host, and never a host
`authorized_keys` file (asserted intent, plan §18).

- The **public** key is committed in-repo at
  `hosts/host.f13/dedicated-agent-vm-key.pub` (a public key is not a secret).
- The matching **private** key is **not** in this repo. It is managed
  out-of-band and lives in the separate `../priv` repository. Never commit a
  private key here.
- To use a specific private key with the launcher's `ssh` / `--attach` paths,
  export `AGENT_MICROVM_SSH_KEY=/path/to/private-key`.

Regenerate the pair (throwaway, private key deleted immediately) with:

```bash
tmp=$(mktemp -d)
ssh-keygen -t ed25519 -N '' -C 'agent-microvm@f13' -f "$tmp/key"
cp "$tmp/key.pub" hosts/host.f13/dedicated-agent-vm-key.pub
# store "$tmp/key" (the PRIVATE key) in ../priv, then:
rm -rf "$tmp"
git add hosts/host.f13/dedicated-agent-vm-key.pub
```

> **`git add` reminder.** Nix evaluates from the git tree, so both the `.pub`
> file and the `ai.f13.nix` change must be `git add`-ed or f13 evaluation
> fails with a "path does not exist" error.

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

Each agent's pane command:

1. resolves the linked main repository from the worktree's shared git dir,
2. maps the workmux branch to a launcher-safe task name
   (`[a-zA-Z0-9._-]`, `<= 64` chars), and
3. execs, as separate argv (never a shell string):

   ```bash
   sudo agent-microvm run --attach \
     --name <task> --repository <main-repo> --agent <bin>
   ```

No network-relaxation flags are passed, so the guest always runs under the
secure **proxy-only** profile.

> The pane runs `sudo agent-microvm …`; there is currently **no**
> passwordless-sudoers rule, so the first launch **will prompt for a
> password** (see [Limitations](#limitations)).

---

## Listing & status

```bash
sudo agent-microvm list             # one line per slot
sudo agent-microvm status           # detailed, all slots
sudo agent-microvm status agent-0   # a single slot
sudo agent-microvm status <task>    # resolve a running task to its slot
```

`status` reports slot, service state, IP, MAC, task, workspace path,
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

`ssh` uses `StrictHostKeyChecking=no` with a `/dev/null` known-hosts file on
purpose: slots are ephemeral guests with regenerated host keys on a
host-controlled private bridge; the trust boundary is the bridge/firewall, not
SSH host identity. Set `AGENT_MICROVM_SSH_KEY` to pick the private key.

---

## Logs

Everything is supervised by systemd, so use the journal:

```bash
journalctl -u microvm@agent-0.service     # the guest VM (Cloud Hypervisor + serial console)
journalctl -u agent-litellm-proxy.service # the bridge-only LiteLLM forwarder
journalctl -u agent-microvm-agentbr0-disable-ipv6.service  # bridge IPv6-disable oneshot
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

**§10 — exactly one share.** Each slot's guest declares exactly **one**
`microvm.shares` entry: a **read-write virtiofs** share tagged `workspace`,
mounted at `/workspace`. Its host `source` is
`/var/lib/microvms/<slot>/workspace` (`${stateRoot}/<slot>/workspace`) — the
**same** path the launcher uses as its `mount --bind` target
(`mount_point()` in `launcher.nix`). So the launcher bind-mounts the
standalone clone onto that host directory, and virtiofsd surfaces it into the
guest as the single writable `/workspace`.

microvm.nix keeps the guest `/nix/store` on its own **storeDisk**
(`microvm.storeOnDisk` defaults to `true` unless a share's source is
`/nix/store`, which this one is not), so it does **not** add a store share.
The guest therefore has **exactly this one share** — no `/nix`, no `/home`, no
host sockets (verified by the `microvm-eval-workspace-share` check, which
asserts `microvm.vms.agent-0.….microvm.shares` is exactly one virtiofs
`/workspace` entry with the expected source).

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
- **No host creds / sockets / home.** The guest gets no host home, no host SSH
  keys, no SSH/GPG agent sockets, no password store, no Docker/Podman sockets,
  no D-Bus/systemd sockets, no host Nix daemon socket, and no writable host Nix
  store. The guest environment does not receive `SSH_AUTH_SOCK`,
  `GPG_AGENT_INFO`, `AWS_*`, `GOOGLE_*`, `AZURE_*`, `KUBECONFIG`,
  `GITHUB_TOKEN`, `GH_TOKEN` or `GITLAB_TOKEN`.
- **Deny-all, proxy-only network.** Default firewall policy on `agentbr0` is
  deny-all-except-proxy: the only egress a guest gets is
  `guest -> 192.168.83.1:4000`. Dedicated chains `AGENT_MICROVM_INPUT` /
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
- **No upstream key in the guest.** `OPENAI_BASE_URL` points at the bridge
  forwarder; `OPENAI_API_KEY=not-needed` is a placeholder. No real upstream
  key, and no secrets in the Nix store / flake / scripts / argv / logs /
  images / workspaces.
- **Bounded & lock-protected.** Fixed vCPU/mem and a fixed slot pool bound
  resource use; a global allocator lock plus per-slot `flock`s prevent
  double-allocation.

---

## Limitations

Honest caveats — read these before trusting the tier:

- **IPv6 disabled (MVP).** No equivalent IPv6 firewall policy exists, so IPv6
  is simply disabled on the bridge. L2 link-local IPv6 between guests is out of
  scope for the MVP.
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
