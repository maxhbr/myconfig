<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Agent sandboxing tiers

Coding agents in this repo are treated as **untrusted code that runs with your
credentials in your checkout**. Four sandboxing techniques are implemented,
forming a ladder from cheap-and-convenient to strong-and-heavy:

| Tier | Technique | Isolation boundary | Entry point | Cost |
| --- | --- | --- | --- | --- |
| 1 | [`agent-tmux`](#1-agent-tmux--dedicated-unix-user) | Unix user + file permissions | `agent-tmux` | ~none |
| 2 | [`agent-bubblewrap-pi`](#2-agent-bubblewrap-pi--bubblewrap-process-jail) | Linux namespaces (bubblewrap) | `agent-bubblewrap-pi` | ~none |
| 3 | [`agent-qemu-pi`](#3-agent-qemu-pi--qemuslirp-microvm) | own kernel (QEMU microVM, SLiRP NAT) | `agent-qemu-pi` | seconds to boot |
| 4 | [`agent-microvm`](#4-agent-microvm--cloud-hypervisor-microvm-fleet) | own kernel + own store + private bridge | `agent-microvm run\|submit` | host config + prebuilt slots |

Tiers 2–4 share one design rule: **the working directory is the only thing the
agent can write**, and secrets are forwarded at launch rather than baked into
the Nix store. Tier 1 works differently — it moves the agent to a *different
identity* instead of restricting what one identity may see.

The tiers are largely orthogonal and compose: an agent user (tier 1) can run
`agent-bubblewrap-pi` (tier 2) in its own session.

All tiers are agent-agnostic in principle; `pi` is the reference agent. Tier 2
also exists as `agent-bubblewrap-opencode` / `agent-bubblewrap-claude`, and tier 4 carries a whole
registry of agents inside its guests. Tier 3 also has a `agent-qemu-herdr`
variant (below) that drops into a `herdr` multiplexer instead of a single
`pi`.

---

## 1. `agent-tmux` — dedicated Unix user

The classic, kernel-free technique: run the agent **as somebody else**.
Isolation is plain Unix uid/gid separation and file permissions, so it costs
nothing and survives any tooling change — but it only protects what the
filesystem permissions protect.

**Implementation**: [`modules/myconfig.agentUsers.nix`](../../myconfig.agentUsers.nix)
(`myconfig.agentUsers.names`, default `[ "agent" ]`).

**Entry points**, generated per declared agent and installed for the primary
user:

- `<name>-tmux` — `sudo -u <name> -i -- tmux new-session -A -s <name>` in the
  current terminal (so `agent-tmux` for the default agent);
- `<name>-alacritty-tmux` — the same session in a fresh Alacritty window
  (Alacritty itself stays as the primary user so it can reach the display);
  these are also registered as Wayland `launcherCommands`.

**The user model**

- Each agent gets its **own primary group** and a **static uid/gid from a
  dedicated block at 31000+** — deliberately above the nixbld range and below
  `nobody`, so it can never collide with a dynamically allocated user. (A
  previous base of 1001 collided with the remote-build user and broke the nix
  daemon's `authPeer()` trust check.)
- Home is **ephemeral**: state is lost on reboot (no impermanence persistence),
  except a single persistent `workdir/` under the work impermanence tree.
- Home mode `0750` plus "the primary user is a member of every agent group"
  means: **you can read agent data; agents cannot read you or each other.**
- `hashedPassword = "!"` (no password login), `linger = true` (user services
  run without a login session), and **deliberately no `extraGroups`** — no
  `wheel`, no `keys`, no `docker`. The only opt-in escape hatch is
  `myconfig.agentUsers.extraGroups`, restricted per-host to device access
  groups (e.g. `dialout`/`plugdev` for the `agnt-embedded` agent on host
  `thing`, so it can flash boards over `/dev/ttyACM*` and SWD/JTAG probes).
- Agents inherit the primary user's home-manager `sharedModules` (tmux, shell,
  dev tools, coding agents), plus an explicitly opt-in list of
  `sessionVariables`, `home.file` entries and config subtrees via
  `myconfig.agentUsers.inheritFromMainUser`. **Secrets are not inherited.**
- `nix.settings.allowed-users` includes the agents, but **not**
  `trusted-users` — a compromised agent cannot redirect builds to a malicious
  substituter or import unsigned store paths.

**The `offline` agent**: an agent literally named `offline` is automatically
network-isolated — an `iptables`/`ip6tables` `OUTPUT` rule matching its uid
rejects all egress except loopback. This is fail-closed, with no per-host
opt-in, by design. Caveat: iptables does not filter unix sockets, so the
root-owned nix daemon can still fetch store paths on its behalf; it is not a
true air gap.

**Limits**: same kernel, same machine, and the agent user can still read
world-readable files across the system. It defends *your* home, keys and the
other agents — not the host.

---

## 2. `agent-bubblewrap-pi` — bubblewrap process jail

The cheapest tier. `pi` runs as **your user on your kernel**, confined by
bubblewrap namespaces via the vendored
[`jail.nix`](../fns/jail-app.nix) library (`vendor/alexdavid-jail.nix`).

**Implementation**

- `modules/myconfig.ai/fns/jail-app.nix` — the reusable wrapper factory
  (`jail-app { name; pkg; userDataDirs; ... }`). Every jailed agent wrapper in
  the repo is one call to it.
- `modules/myconfig.ai/programs.pi-coding-agent/default.nix` — the `agent-bubblewrap-pi`,
  `agent-bubblewrap-pi-tmp` and `agent-bubblewrap-pi-worktree` instantiations.
- `modules/myconfig.ai/myconfig.ai.jail.nix` — the shared
  `myconfig.ai.jail.fwdEnvs` option (host env vars forwarded into *every*
  wrapper, on top of the always-forwarded `OPENAI_API_KEY`).

**What the agent sees**

- `$PWD` bound read-write (`mount-cwd`) — and, if the CWD is a git repo with a
  sibling `../<basename>__worktrees`, that directory too.
- `~/.pi` read-write, so session/credential/extension state persists.
- A curated read-only set of config dirs (`~/.config/git`, `~/.config/bat`,
  `~/.agents`, …), each bound only if it exists (`try-ro-bind`).
- A fixed dev-tool closure on `PATH` (git, ripgrep, fd, jq, nix, python3, …).
- Network access (the `network` combinator: resolv.conf + CA bundle).

**What it does not see**: the rest of `$HOME`, `~/.ssh`, `~/.gnupg`, agent
sockets, `$TMUX`. The environment is cleared (`--clearenv`) and repopulated
only from the explicit forward lists.

**Guardrails**

- `rejectHomeCwd` — refuses to start in `$HOME`, because `mount-cwd` would
  otherwise bind your entire home read-write.
- The worktree variant is a *separate* wrapper
  (`agent-bubblewrap-pi-worktree-inner`), so a normal invocation cannot obtain a
  writable bind into another repository merely by setting an env var.
- `PI_JAIL_MARKER=1` is set inside the jail; the `myconfig-jail-marker.ts`
  extension paints an obvious red border when pi runs **un**jailed.
- workmux status updates escape via a `jail-to-host-channel` FIFO shim rather
  than exposing the tmux socket to the jail.

**Limits**: same kernel, same user, no VM boundary. A kernel or bubblewrap
escape, or a `nix`-mediated write, is not defended against. It is a strong
*accident* barrier and a moderate *malice* barrier.

Session-wide variant: `agent-bubblewrap-alacritty-workmux-tmux`
(`myconfig.ai.workmux.jail`) jails an entire tmux/workmux session — main
checkout plus its `__worktrees` sibling — in one bubblewrap.

---

## 3. `agent-qemu-pi` — QEMU/SLiRP microVM

Same ergonomics as `agent-bubblewrap-pi` (`cd` into a project, run it, arguments are
forwarded to `pi`), but the agent runs **in its own kernel** as an
unprivileged `agent` user with an ephemeral root filesystem.

**Implementation**

- `modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix` — `mkAgentQemuPiRunner` builds a one-shot
  [microvm.nix](https://github.com/microvm-nix/microvm.nix) QEMU runner (guest
  NixOS system + kernel + virtiofsd + run script).
- `myconfig.ai.qemu-agent-sandbox.runnerExpression` evaluates that builder
  **impurely** from `AGENT_QEMU_PI_*` env vars, so the workspace path never
  lands in a tracked file or flake output. It is not a flake package.
- The host wrapper `agent-qemu-pi` lives in
  `modules/myconfig.ai/programs.pi-coding-agent/default.nix`.
- Full write-up: [`../programs.pi-coding-agent/agent-qemu-pi.README.md`](../programs.pi-coding-agent/agent-qemu-pi.README.md).

**Launch sequence**: validate CWD (refuses `$HOME`) → generate a throwaway
ed25519 keypair → pick a random `127.0.0.1` port → `nix build --impure` the
runner → boot the VM → wait for guest SSH → forward credentials **over the SSH
session environment** → exec `pi` in `/workspace`. On exit the VM is killed and
the runtime dir removed.

**Sharing**: CWD read-write at `/workspace` (virtiofs) and the host
`/nix/store` **read-only** (hence no disk image and a fast boot). Not shared:
home, `~/.ssh`, `~/.gnupg`, D-Bus/systemd/nix-daemon/container sockets, `/run`,
`/dev`.

**Networking**: QEMU SLiRP user-mode NAT — outbound only, plus one forwarded
SSH port on loopback. No bridge, TAP, NAT or firewall changes on the host,
which is what keeps it a self-contained user-space command. (This is also why
it is QEMU and not Cloud Hypervisor: CH has no user-mode networking.)

**Credentials**: `OPENAI_API_KEY`, `OPENAI_BASE_URL`, `OPENROUTER_BASE_URL`,
`ANTHROPIC_API_KEY` are pushed over SSH env at launch, only when set — never
into the store, never on a command line.

**Requires**: `/dev/kvm` (otherwise slow TCG), `nix-command`.

Session-wide variant: `agent-qemu-workmux-tmux` (in-terminal, like
`agent-bubblewrap-workmux-tmux`) and `agent-qemu-alacritty-workmux-tmux` (Alacritty
popup, like `agent-bubblewrap-alacritty-workmux-tmux`) — both under
`myconfig.ai.workmux.sandbox`, off by default — put a whole workmux session in
one VM. The in-terminal `agent-qemu-workmux-tmux` is the reusable entry point; the
Alacritty variant is a thin popup around it.

**`agent-qemu-herdr` variant**: the same microVM, but instead of exec'ing `pi`
it exec's `herdr` (the agent multiplexer), and the guest carries `herdr` plus
the coding-agent CLIs enabled on the host so the user can start `pi` /
`opencode` / `claude-code` / … from *inside* the `herdr` session. It reuses
the same `mkSandboxedRunner` factory (no parallel guest builder) and shares
`agent-qemu-pi`'s workspace handling, credential forwarding and refuse-`$HOME`
guard. See
[`../programs.herdr.nix`](../programs.herdr.nix) (wrapper) and
[`../agent-qemu-herdr.README.md`](../agent-qemu-herdr.README.md).

**Limits**: the host store is visible read-only; the guest still shares the
host store closure and reaches the network via the host. See the status note
in its README regarding live-boot validation.

---

## 4. `agent-microvm` — Cloud Hypervisor microVM fleet

The strongest tier, and the only one designed for **unattended, autonomous**
agent runs. Each session gets a **Cloud Hypervisor** microVM from a pool of
prebuilt slots.

**Implementation**: `modules/myconfig.ai/myconfig.ai.microvm/` (module split
across `guest.nix`, `network.nix`, `session.nix`, `job.nix`, `launcher.nix`,
`state.nix`, `agents.nix`, `hostkeys.nix`, `workmux.nix`, …).
Documentation set:

| Document | Contents |
| --- | --- |
| [How-to](../myconfig.ai.microvm/docs/agent-microvm-howto.md) | **start here** — one linear journey: `doctor`, `run`, `submit`, import, cleanup |
| [Reference](../myconfig.ai.microvm/docs/agent-microvm.md) | activation, options, agent registry, network profiles, batch job format, limitations |
| [Architecture](../myconfig.ai.microvm/docs/agent-microvm-architecture.md) | module map, slot pool, workspace indirection, credential boundary |
| [Operator guide](../myconfig.ai.microvm/docs/agent-microvm-operator-guide.md) | exact start/submit/status/attach/cancel/collect/recover procedures |
| [Workspace layout](../myconfig.ai.microvm/docs/workspace-layout.md) | `central` vs `beside-repo` task clones and the task -> clone index |
| [Security model](../myconfig.ai.microvm/docs/agent-microvm-security-model.md) | trusted vs untrusted, mitigated attacks, residual risks |
| [Runtime validation](../myconfig.ai.microvm/docs/agent-microvm-runtime-validation.md) | the real-KVM measurement procedure |
| [Lightweight plan](../myconfig.ai.microvm/docs/myconfig-ai-microvm-lightweight-plan.md) | historical phased implementation plan, kept for its per-phase implementation-status record |
| [Per-resource-class network plan](../myconfig.ai.microvm/docs/plan-per-resource-class-network.md) | historical planning artifact; per-class network profiles were **not** implemented, superseded by the single `networkProfile` option |

**Boundary**

- Own kernel **and a self-contained guest store disk** — the host
  `/nix/store` is *not* shared (unlike tier 2).
- Root and `/home/agent` are disposable; **only `/workspace` persists**, plus
  opt-in task-scoped agent state dirs.
- `/workspace` is a **standalone git clone** of your repository on the single
  writable per-session share — the agent never touches your checkout, you
  import the resulting branch afterwards.
- A dedicated private bridge (`agentbr0`, `192.168.83.0/24`) with **per-TAP
  layer-2 isolation** between slots.
- Model access is restricted to the **host LiteLLM proxy** via a bridge-only
  forwarding endpoint: **no upstream API key ever reaches the guest.**

**Two execution paths**, independently selectable per host via `capabilities`:

- `agent-microvm run --attach` — interactive, also surfaced as workmux
  `microvm-<agent>` panes;
- `agent-microvm submit` — unattended batch with structured results, hard
  timeouts, cancellation (`cancel`, exit `130`) and recovery.

Like tier 3's `agent-qemu-herdr` variant, a guest can run **`herdr`** (the agent
multiplexer) instead of a single agent: `herdr` is a selectable registry agent
(`enabledAgents = [ … "herdr" ]`, on by default in the `null`
"all declared agents" mode and on `f13`), so `agent-microvm run --attach
--agent herdr` (or its `microvm-herdr` workmux pane) drops the operator into a
`herdr` session from which the *other* selected agents can be launched. Being a
multiplexer it is **interactive-only** — it has no batch mode, so `submit
--agent herdr` is rejected.

Operational commands: `doctor`, `status`, `ssh`, `collect`, `remove`,
`recover --dry-run`.

**Activation** is explicit per host and never implied by `myconfig.ai.enable`;
while disabled the module produces **zero** config side effects. On `f13`:

```nix
myconfig.ai.microvm = {
  enable = true;
  resourceClasses = lib.mkForce {
    small  = { count = 1; vcpu = 2; memoryMiB = 4096; };
    normal = { count = 1; vcpu = 4; memoryMiB = 8192; };
  };
  networkProfile = "proxy-only";
  passwordlessControl = true;
  sshPublicKeyFile = ./dedicated-agent-vm-key.pub;
};
```

**Verification boundary**: eval/build coverage comes from `nix flake check` and
`tests/microvm.nix`; runtime properties (firewall enforcement, L2 isolation,
credential absence, cgroup limits) are eval-asserted but must be *measured* on
real KVM with `runtime-validation.sh`. Read the reference doc's *Limitations*
section before trusting the tier.

---

## `agent-qemu-herdr` vs. `agent-microvm run --agent herdr`

Both tier 3 and tier 4 can put the `herdr` multiplexer inside a microVM, which
looks like duplication but isn't: they sit at different rungs of the ladder
above and solve different problems. Full comparison, with file/line
references:
[`doc/agent-qemu-herdr-vs-agent-microvm-herdr.md`](../../../doc/agent-qemu-herdr-vs-agent-microvm-herdr.md).
Condensed:

| | `agent-qemu-herdr` (tier 3) | `agent-microvm --agent herdr` (tier 4) |
| --- | --- | --- |
| Purpose | ad hoc, zero-config sandbox for one project directory | one selectable agent inside the prebuilt, policy-hardened fleet |
| Host `/nix/store` | shared into guest read-only | not shared — guest has its own store disk |
| Workspace | host CWD bind-mounted read-write (live edits) | standalone git clone; host checkout untouched, import the branch after |
| Model-API credentials | real keys forwarded over the SSH session environment | never reach the guest — proxied through the host LiteLLM service |
| Network | SLiRP NAT, no host bridge/firewall | dedicated bridge + per-TAP L2 isolation (or no NIC at all under `vsock`) |
| Activation | always installed with any coding agent enabled | explicit per-host opt-in (`myconfig.ai.microvm.enable`); currently f13 only |
| Execution modes | interactive only | interactive **and** unattended batch (batch not available for herdr itself) |

**Overlap**: both reuse the same config-seeding allowlist/denylist library
(`../fns/seed-agent-config.nix`) and near-identical "why herdr" rationale text
(kept manually in sync between `../agent-qemu-herdr.README.md` and
`../myconfig.ai.microvm/docs/agent-microvm.md`). `agent-microvm`'s herdr guest
is a strict security superset of `agent-qemu-herdr`'s on every shared axis
(store exposure, credential exposure, network egress, workspace isolation),
but `agent-qemu-herdr` is not redundant: it needs zero host configuration and
shares the live working directory, which `agent-microvm` deliberately does
not offer.

**Recommendation**: keep both — they are intentionally orthogonal tiers, not
a superset/subset pair to collapse. Use `agent-qemu-herdr` for the common
ad hoc case; use `agent-microvm` when the task is untrusted enough to keep
the model key off the guest, needs a throwaway workspace, or is unattended.
The only worthwhile cleanup is deduplicating the copy-pasted rationale text
and the thrice-repeated "which coding-agent CLIs to bake in" logic
(`../programs.herdr.nix`, `../myconfig.ai.microvm/agents.nix`,
`../myconfig.ai.gvisor-agent-sandbox/default.nix`'s `agentPackagesByFlag`) —
a documentation/DRY change, not a behavior change.

## Choosing a tier

- **Keep the agent out of your home directory, keys and shell history, at zero
  runtime cost and with a normal interactive session** → `agent-tmux` (or a
  network-isolated `offline` agent user).
- **Interactive work in a repo you trust, fastest loop** → `agent-bubblewrap-pi`.
- **Same loop, but you want a kernel boundary** (untrusted repo, sketchy
  dependency, `npm install` in the agent's path) → `agent-qemu-pi`.
- **Autonomous / batch runs, several agents in parallel, no upstream API key
  exposure, results you collect later** → `agent-microvm`.

When in doubt, compose: run `agent-bubblewrap-pi` *inside* an `agent-tmux` session.

## Runtime model registry

Hosts that declare `myconfig.ai.llama-cpp` models also render a Markdown
overview of every declared model (devices, params, aliases, variants) to
`/run/myconfig/docs/models.md` at activation time — generated by
[`../myconfig.ai.llama-cpp/docs.nix`](../myconfig.ai.llama-cpp/docs.nix)
from the same model definitions that build the servers, so it doubles as a
quick reference for which models a host serves.

## Related

A fifth, container-based tier — rootless Podman with the **gVisor** runtime
(`agent-gvisor`) — lives in
[`../myconfig.ai.gvisor-agent-sandbox/`](../myconfig.ai.gvisor-agent-sandbox/)
and enabled per host (`myconfig.ai.gvisor-agent-sandbox.enable`). In isolation
strength it sits between the bubblewrap jail (tier 2) and the QEMU microVM
(tier 3): like the jail it runs on the host kernel (no full VM), but `runsc`
interposes a user-space kernel between the sandbox and the host kernel, which
plain namespaces do not.
