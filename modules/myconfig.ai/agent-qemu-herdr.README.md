<!-- Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT -->

# `agent-qemu-herdr` — `herdr` inside a QEMU/SLiRP microVM

`agent-qemu-herdr` is the `herdr`-driven counterpart of `agent-qemu-pi`. It reuses
**the same** microVM/runner machinery (`mkSandboxedRunner` from
`modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix`) — it does **not** fork a parallel guest builder. The
only difference is what runs at the end of the SSH session: instead of exec'ing
`pi`, it exec's `herdr`, the agent multiplexer. From inside that `herdr`
session the user starts `pi` / `opencode` / `claude-code` / … as panes — all
running **inside** the VM.

## Why

`agent-qemu-pi` drops you straight into a single `pi` session inside a
disposable VM. `agent-qemu-herdr` drops you into a `herdr` multiplexer inside
the same disposable VM, so you can run several agents (and shells) side by
side in one sandbox, switching between them with the same keybindings you use
for `herdr` on the host. The agents themselves run *inside* the VM, not via
the host wrapper.

## Usage

```bash
cd ~/some/project      # NOT $HOME — it refuses to run there
agent-qemu-herdr        # launches a VM, opens herdr in /workspace
agent-qemu-herdr --help # arguments are forwarded to herdr unchanged
```

Inside the VM you are dropped at a `herdr` prompt in `/workspace`. Open a new
pane and run `pi`, `opencode`, `claude-code`, … exactly as you would on the
host (see [What the guest sees](#what-the-guest-sees) for which agents are
available). On exit the VM is torn down and all guest state is discarded.
Only the files under the shared working directory persist (they are the same
inodes on the host — edits made by the agents are immediately visible on the
host).

## What the guest sees

Shared into the guest:

- The current working directory, read-write, at `/workspace` (virtiofs). The
  guest `agent` user's uid/gid is pinned to the invoking host user's own
  uid/gid (forwarded as `AGENT_QEMU_HERDR_UID`/`_GID`), because the rootless
  virtiofsd daemon backing this share runs without `--translate-uid` and so
  passes the workspace's real host ownership straight through; without a
  matching guest uid, the guest kernel's permission check would deny writes
  (`EACCES`) while reads kept working via the usual world-readable bits.
- The host `/nix/store`, **read-only** (so the VM needs no disk image and
  boots fast).
- The allowlisted host configuration for every registered agent, **copied**
  into the guest `/home/agent` at launch (see
  [Agent-configuration seeding](#agent-configuration-seeding)). Credentials are
  never copied.

On the guest `PATH`:

- `herdr` (the agent multiplexer the user is dropped into).
- The coding-agent CLIs that are enabled on the **building** host — the same
  set the gVisor sandbox image bakes in (see
  `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/default.nix`,
  `agentPackagesByFlag`): `pi-coding-agent`, `opencode`, `claude-code`,
  `codex`, `github-copilot-cli`, `qwen-code`. Only agents whose
  `myconfig.ai.<name>.enable` flag is true on the host are included, so the
  guest closure stays minimal. The list of enabled agent store paths is baked
  into the wrapper at build time and forwarded to the impure runner expression as
  `AGENT_QEMU_HERDR_AGENT_PACKAGES` (a JSON array); only public store paths are
  baked in — never credentials.

Plus the standard minimal coding-agent environment every `sandboxed-*` guest
carries (bash, coreutils, git, ripgrep, fd, jq, curl, …).

Deliberately **not** shared:

- The host home directory, `~/.ssh`, `~/.gnupg`, credential/agent sockets.
- The host D-Bus / systemd / nix-daemon / container-runtime sockets.
- `/run`, `/dev`, a writable host `/nix/store`.

## Networking

Identical to `agent-qemu-pi`: qemu SLiRP user-mode networking. The guest gets
outbound NAT through the host (so the agents can reach LLM endpoints), but
nothing on the host LAN can reach the guest and the guest reaches the host
only through the single forwarded SSH port on `127.0.0.1`. No host bridge,
firewall or NAT configuration is required.

## Credentials

Identical to `agent-qemu-pi`: `OPENAI_API_KEY`, `OPENAI_BASE_URL`,
`OPENROUTER_BASE_URL`, `ANTHROPIC_API_KEY` are forwarded **at launch over the
SSH session environment** — only if they are set on the host. They are never
baked into the Nix store, never passed on a process command line, and never
written to a tracked file. The agents launched from inside `herdr` inherit
them from the SSH session environment.

## Agent-configuration seeding

The guest home is an ephemeral tmpfs, so without seeding every agent `herdr`
can launch would start with empty/default configuration. To avoid that,
`agent-qemu-herdr` copies the relevant, allowlisted host configuration for
**every** registered agent into the guest `/home/agent` over the SSH channel
**at launch**, after the VM boots and before `herdr` is exec'd. This mirrors
the heavyweight `myconfig.ai.microvm` config-seed mechanism
(`modules/myconfig.ai/myconfig.ai.microvm/config-seed.nix`), adapted for the
user-space qemu tier (no privileged host daemon: staging + transfer happen
entirely in the launcher, over the already-established SSH channel).

The seeded agent set is the union of the per-agent `configPaths` for every
registered agent (from `modules/myconfig.ai/fns/seed-agent-config.nix`, whose
path lists **mirror** the tier-4 registry `myconfig.ai.microvm/agents.nix` for
every agent that registry knows — including `herdr` itself):

- `pi` — `~/.pi/agent/{agents,extensions,keybindings.json,prompts,themes}`.
- `opencode` — `~/.config/opencode/{agents,commands,opencode.json,tui.json,skills}`.
- `claude` (`claude-code`) — `~/.claude/settings.json`, `~/.claude/skills`.
- `codex` — `~/.codex/{config.toml,hooks.json,skills}`.
- `qwen-code`, `github-copilot-cli` — the shared `~/.agents/skills` tree.
- `hermes` — nothing (its config root mixes credentials; the guest gets its
  endpoint from the SSH environment).
- `herdr` — `~/.config/herdr/config.toml` (the rendered keybindings; the
  guest's entry point therefore uses the host's `ctrl+b` prefix / pane-focus
  bindings). The exact file only, never the `.config/herdr` directory.
- Plus the module-wide `.config/git/{config,attributes}` and `.agents/skills`.

What is **never** seeded:

- **Credentials.** `.pi/agent/auth.json`, `.codex/auth.json`,
  `.claude/.credentials.json`, and any file matching the credential denylist
  (applied to every path component **and** the resolved symlink target):
  `auth.json`, `credentials*`, `*.pem`, `*.key`, `id_rsa`, `id_ed25519`,
  `.env`, `.netrc`, `cookies*`, `*session*`, `*token*`, …. Model keys keep
  flowing over the SSH environment (`SetEnv`), exactly as before.
- The host home directory as a whole — only the exact allowlisted paths are
  copied.

Nothing is baked into the Nix store: the allowlist + denylist are, but the
host home contents are read at launch. The seeder script
(`seed-agent-config`) is built into the runner output and invoked by the
host wrapper as `seed-agent-config <ssh-port> <identity> 127.0.0.1 agent`.

## How it works

- `modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix` exports `mkAgentQemuHerdrRunner`, a thin wrapper
  around the shared `mkSandboxedRunner` that provisions one workspace share
  plus `herdr` and the enabled coding-agent CLIs as guest packages. This is
  the same factory `mkAgentQemuPiRunner` uses; the guest system, kernel,
  virtiofsd and run script are built by the identical code path.
- `myconfig.ai.qemu-agent-sandbox.runnerExpression` evaluates that builder
  **impurely** from `AGENT_QEMU_HERDR_*` environment variables, so the
  workspace path never lands in a tracked file or flake output. The runner is
  not exported as a flake package.
- The host wrapper `agent-qemu-herdr` lives in
  `modules/myconfig.ai/programs.herdr.nix`. It validates the working directory
  (refuses `$HOME`), generates a throwaway SSH keypair, picks a random
  `127.0.0.1` port, `nix build --impure`s the runner for the current
  directory, starts the VM, waits for guest SSH, forwards credentials over the
  SSH environment, **seeds the in-guest agent configuration for every
  registered agent from the host** (see [Agent-configuration seeding](
  #agent-configuration-seeding)), and execs `herdr` in `/workspace`. On exit it
  kills the VM and removes the runtime directory.

## How it differs from `agent-qemu-pi`

| | `agent-qemu-pi` | `agent-qemu-herdr` |
| --- | --- | --- |
| In-guest entry point | `pi` | `herdr` |
| Guest packages | `pi` only | `herdr` + enabled coding-agent CLIs |
| Multiple agents in one VM | no (one `pi` session) | yes (via `herdr` panes) |
| Workspace sharing | CWD rw at `/workspace`, host store ro | identical |
| Credential forwarding | same 4 env vars over SSH env | identical |
| Config seeding | `pi` allowlist only (`~/.pi`) | union for every registered agent |
| Refuse `$HOME` | yes | yes |
| Runner factory | `mkAgentQemuPiRunner` | `mkAgentQemuHerdrRunner` (same `mkSandboxedRunner`) |

## Requirements

Same as `agent-qemu-pi`:

- Access to `/dev/kvm` for the invoking user (KVM acceleration). Without it,
  qemu falls back to slow TCG emulation.
- `nix` with `nix-command`; the wrapper runs `nix build --impure --file`
  against the module-owned runner expression.

## Status / validation

The runner builds and evaluates successfully (`nix flake check`, pure-eval
placeholder, impure runner build, and the guest closure confirmed to carry
`herdr` plus the enabled agent CLIs). `agent-qemu-pi` is provably unchanged
(snapshot/diff of its runner drvPath is identical before and after this
change). A live VM boot has not yet been exercised here for the same reason
as `agent-qemu-pi` (no `/dev/kvm` in the build environment); run one
`agent-qemu-herdr` session on a KVM-capable host to complete runtime
validation.
