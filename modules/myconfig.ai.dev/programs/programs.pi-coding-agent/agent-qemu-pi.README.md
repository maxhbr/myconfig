# `agent-qemu-pi` — the microVM counterpart of `agent-bubblewrap-pi`

`agent-qemu-pi` runs the `pi` coding agent with the same ergonomics as
`agent-bubblewrap-pi`, but inside a real [microvm.nix](https://github.com/microvm-nix/microvm.nix)
virtual machine (its own kernel) instead of a bubblewrap jail.

Like `agent-bubblewrap-pi`, you run it **from a project subdirectory**; the working
directory is the only writable thing the agent can see. Unlike `agent-bubblewrap-pi`, the
agent runs in a separate kernel as an unprivileged `agent` user with an
ephemeral root filesystem that is discarded when the VM stops.

## Usage

```bash
cd ~/some/project      # NOT $HOME — it refuses to run there
agent-qemu-pi           # launches a VM, opens pi in /workspace
agent-qemu-pi --help    # arguments are forwarded to pi unchanged
```

On exit the VM is torn down and all guest state is discarded. Only the files
under the shared working directory persist (they are the same inodes on the
host — edits made by the agent are immediately visible on the host).

## What the guest can and cannot see

Shared into the guest:

- The current working directory, read-write, at `/workspace` (virtiofs).
- The host `/nix/store`, **read-only** (so the VM needs no disk image and
  boots fast).
- The allowlisted host `~/.pi` configuration, **copied** into the guest
  `/home/agent` at launch (see [Agent-configuration seeding](
  #agent-configuration-seeding)). Credentials are never copied.

Deliberately **not** shared:

- The host home directory, `~/.ssh`, `~/.gnupg`, credential/agent sockets.
- The host D-Bus / systemd / nix-daemon / container-runtime sockets.
- `/run`, `/dev`, a writable host `/nix/store`.

## Networking

qemu SLiRP user-mode networking: the guest gets outbound NAT through the host
(so `pi` can reach LLM endpoints), but nothing on the host LAN can reach the
guest and the guest reaches the host only through the single forwarded SSH
port on `127.0.0.1`. No host bridge, firewall or NAT configuration is required.

## Credentials

LLM credentials (`OPENAI_API_KEY`, `OPENAI_BASE_URL`, `OPENROUTER_BASE_URL`,
`ANTHROPIC_API_KEY`) are forwarded **at launch over the SSH session
environment** — only if they are set on the host. They are never baked into
the Nix store, never passed on a process command line, and never written to a
tracked file. If none are set, `pi` starts without credentials.

## Agent-configuration seeding

The guest home is an ephemeral tmpfs, so without seeding `pi` would start
with empty/default configuration. To avoid that, `agent-qemu-pi` copies the
relevant, allowlisted host `~/.pi` configuration into the guest `/home/agent`
over the SSH channel **at launch**, after the VM boots and before `pi` is
exec'd. This mirrors the heavyweight `myconfig.ai.microvm` config-seed
mechanism (`modules/myconfig.ai/myconfig.ai.microvm/config-seed.nix`), adapted
for the user-space qemu tier (no privileged host daemon: staging + transfer
happen entirely in the launcher, over the already-established SSH channel).

What is seeded (the `pi` allowlist, from
`modules/myconfig.ai/fns/seed-agent-config.nix`, mirroring
`myconfig.ai.microvm/agents.nix`):

- `.pi/agent/agents` — sample subagent definitions.
- `.pi/agent/extensions` — the auto-generated provider extension, the
  jail-marker extension, the subagent/handoff examples, etc.
- `.pi/agent/keybindings.json`, `.pi/agent/themes`, `.pi/agent/prompts`.
- `.agents/skills` — the handcrafted skills tree.
- `.config/git/{config,attributes}` — git identity/tools.

What is **never** seeded:

- **Credentials.** `.pi/agent/auth.json`, session state under
  `.pi/agent/sessions/`, and any file matching the credential denylist
  (applied to every path component **and** the resolved symlink target):
  `auth.json`, `credentials*`, `*.pem`, `*.key`, `id_rsa`, `id_ed25519`,
  `.env`, `.netrc`, `cookies*`, `*session*`, `*token*`, …. Model keys keep
  flowing over the SSH environment (`SetEnv`), exactly as before.
- The host home directory as a whole — only the exact allowlisted paths are
  copied.

Nothing is baked into the Nix store: the allowlist + denylist are, but the
host home contents are read at launch. A file whose name matches the denylist
is skipped even if it lives inside an allowlisted directory (defence in
depth). A small, explicit `denyOverrides` allowlist exempts trademark /
name-collision exceptions from the INFIX checks only — e.g. the
`trustedtokens-provider` pi extension ("TrustedTokens" is a TNG inference-
service trademark; the directory holds only TypeScript source + metadata,
no API key) is staged despite the deny infix "token". Exact-name and suffix
checks still apply everywhere, so a real `auth.json` or `*.pem` placed inside
an overridden directory is still refused. This matches the heavyweight tier's
behaviour. The seeder script (`seed-agent-config`) is built into the runner
output and invoked by the host wrapper as
`seed-agent-config <ssh-port> <identity> 127.0.0.1 agent`.

## How it works

- `modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix` exports `mkAgentQemuPiRunner`, which builds a qemu
  microvm.nix runner for one session (guest NixOS system + kernel + virtiofsd
  + run script).
- `myconfig.ai.qemu-agent-sandbox.runnerExpression` evaluates that builder
  impurely, reading the workspace path, forwarded SSH port, and throwaway
  authorized-keys file from `AGENT_QEMU_PI_*` environment variables. The
  workspace path therefore never appears in a tracked file or flake output.
  The runner is not exported as a flake package.
- The host wrapper `agent-qemu-pi` (in `default.nix`) validates the working
  directory (refuses `$HOME`), generates a throwaway SSH keypair, picks a
  random `127.0.0.1` port, `nix build --impure`s the runner for the current
  directory, starts the VM, waits for guest SSH, forwards credentials over the
  SSH environment, **seeds the in-guest `pi` configuration from the host**
  (see [Agent-configuration seeding](#agent-configuration-seeding)), and execs
  `pi` in `/workspace`. On exit it kills the VM and removes the runtime
  directory.

## Why qemu and not Cloud Hypervisor

Cloud Hypervisor only supports `tap`/`macvtap` network interfaces — it has no
user-mode networking and no port forwarding. Using it would require a
dedicated host bridge, per-VM TAP device, NAT and firewall rules **plus a
system rebuild** before the guest could reach the network or be reachable over
SSH. That contradicts the `agent-bubblewrap-pi`-like goal of a self-contained
user-space wrapper that works from any project directory with no host changes.
qemu's user-mode networking + `forwardPorts` (the same combination the in-repo
hermes microVM uses) provides exactly that. Moving to Cloud Hypervisor with a
dedicated bridge/subnet + NAT/firewall is the natural follow-up once that host
networking is provisioned; the guest module isolates the hypervisor-specific
bits (`hypervisor`, `interfaces`, `forwardPorts`).

## Related: `agent-qemu-alacritty-workmux-tmux`

The same microVM approach is available for the *whole* workmux/tmux session as
the counterpart of `agent-bubblewrap-alacritty-workmux-tmux` (the bubblewrap
`myconfig.ai.workmux.jail`). `agent-qemu-alacritty-workmux-tmux` runs the main
checkout, its `<basename>__worktrees` sibling, tmux, workmux and the agents it
launches inside one VM, popped up in an Alacritty window. It is defined in
`modules/myconfig.ai/myconfig.ai.workmux/sandbox.nix` and gated behind
`myconfig.ai.workmux.sandbox.enable` (off by default). The main checkout is
shared read-write at `/workspace` and the worktrees sibling at
`/workspace__worktrees`, so workmux's sibling-directory convention resolves
inside the guest. See `modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix` (`mkSandboxedWorkmuxRunner`).

## Requirements

- Access to `/dev/kvm` for the invoking user (KVM acceleration). Without it,
  qemu falls back to slow TCG emulation.
- `nix` with the `nix-command` feature; the wrapper runs
  `nix build --impure --file` against the module-owned runner expression.

## Status / validation

Built and evaluated successfully (`nix flake check`, host toplevel build, and
a full build of the guest runner including kernel/initrd/virtiofsd/run script,
with the forwarded port and workspace source confirmed baked into the run
script). **A live VM boot has not yet been exercised** because the build
environment used has no `/dev/kvm`; run one `agent-qemu-pi` session on a
KVM-capable host (e.g. f13) to complete runtime validation.
