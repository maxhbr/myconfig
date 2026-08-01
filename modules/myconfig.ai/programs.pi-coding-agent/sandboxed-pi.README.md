# `sandboxed-pi` — the microVM counterpart of `jailed-pi`

`sandboxed-pi` runs the `pi` coding agent with the same ergonomics as
`jailed-pi`, but inside a real [microvm.nix](https://github.com/microvm-nix/microvm.nix)
virtual machine (its own kernel) instead of a bubblewrap jail.

Like `jailed-pi`, you run it **from a project subdirectory**; the working
directory is the only writable thing the agent can see. Unlike `jailed-pi`, the
agent runs in a separate kernel as an unprivileged `agent` user with an
ephemeral root filesystem that is discarded when the VM stops.

## Usage

```bash
cd ~/some/project      # NOT $HOME — it refuses to run there
sandboxed-pi           # launches a VM, opens pi in /workspace
sandboxed-pi --help    # arguments are forwarded to pi unchanged
```

On exit the VM is torn down and all guest state is discarded. Only the files
under the shared working directory persist (they are the same inodes on the
host — edits made by the agent are immediately visible on the host).

## What the guest can and cannot see

Shared into the guest:

- The current working directory, read-write, at `/workspace` (virtiofs).
- The host `/nix/store`, **read-only** (so the VM needs no disk image and
  boots fast).

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

## How it works

- `flake.sandboxed-pi.nix` exports `mkSandboxedPiRunner`, which builds a qemu
  microvm.nix runner for one session (guest NixOS system + kernel + virtiofsd
  + run script).
- The flake output `packages.<system>.sandboxed-pi-runner` evaluates that
  builder impurely, reading the workspace path, forwarded SSH port and a
  throwaway authorized-keys file from `SANDBOXED_PI_*` environment variables.
  The workspace path therefore never appears in a tracked file or flake
  output. Under pure evaluation (`nix flake check`) the output is a harmless
  placeholder.
- The host wrapper `sandboxed-pi` (in `default.nix`) validates the working
  directory (refuses `$HOME`), generates a throwaway SSH keypair, picks a
  random `127.0.0.1` port, `nix build --impure`s the runner for the current
  directory, starts the VM, waits for guest SSH, forwards credentials over the
  SSH environment and execs `pi` in `/workspace`. On exit it kills the VM and
  removes the runtime directory.

## Why qemu and not Cloud Hypervisor

Cloud Hypervisor only supports `tap`/`macvtap` network interfaces — it has no
user-mode networking and no port forwarding. Using it would require a
dedicated host bridge, per-VM TAP device, NAT and firewall rules **plus a
system rebuild** before the guest could reach the network or be reachable over
SSH. That contradicts the `jailed-pi`-like goal of a self-contained
user-space wrapper that works from any project directory with no host changes.
qemu's user-mode networking + `forwardPorts` (the same combination the in-repo
hermes microVM uses) provides exactly that. Moving to Cloud Hypervisor with a
dedicated bridge/subnet + NAT/firewall is the natural follow-up once that host
networking is provisioned; the guest module isolates the hypervisor-specific
bits (`hypervisor`, `interfaces`, `forwardPorts`).

## Related: `alacritty-sandboxed-workmux-here`

The same microVM approach is available for the *whole* workmux/tmux session as
the counterpart of `alacritty-workmux-here` (the bubblewrap
`myconfig.ai.workmux.jail`). `alacritty-sandboxed-workmux-here` runs the main
checkout, its `<basename>__worktrees` sibling, tmux, workmux and the agents it
launches inside one VM, popped up in an Alacritty window. It is defined in
`modules/myconfig.ai/myconfig.ai.workmux/sandbox.nix` and gated behind
`myconfig.ai.workmux.sandbox.enable` (off by default). The main checkout is
shared read-write at `/workspace` and the worktrees sibling at
`/workspace__worktrees`, so workmux's sibling-directory convention resolves
inside the guest. See `flake.sandboxed-pi.nix` (`mkSandboxedWorkmuxRunner`).

## Requirements

- Access to `/dev/kvm` for the invoking user (KVM acceleration). Without it,
  qemu falls back to slow TCG emulation.
- `nix` with flakes; the wrapper runs `nix build --impure` against the pinned
  flake revision it was built from.

## Status / validation

Built and evaluated successfully (`nix flake check`, host toplevel build, and
a full build of the guest runner including kernel/initrd/virtiofsd/run script,
with the forwarded port and workspace source confirmed baked into the run
script). **A live VM boot has not yet been exercised** because the build
environment used has no `/dev/kvm`; run one `sandboxed-pi` session on a
KVM-capable host (e.g. f13) to complete runtime validation.
