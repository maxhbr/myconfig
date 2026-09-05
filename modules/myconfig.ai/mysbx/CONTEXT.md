<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# mysbx

The sandboxing CLI of this repo: one command that confines a process to a
declared set of host resources for the repository you are standing in. It is
the intended successor of the other sandboxing tiers in `modules/myconfig.ai/`
(see [`docs/feature-comparison.md`](./docs/feature-comparison.md)).

## Language

**Repo**:
The git work-tree root `mysbx` builds a sandbox around. Resolved from the
current directory, never passed as a flag.
_Avoid_: project, checkout, workspace, CWD

**Sidecar**:
The directory `<repo>.mysbx/` next to the repo, holding the per-repo
`config.toml` and disposable state. Outside the repo and outside the sandbox.
_Avoid_: state dir, `.mysbx`, project config dir

**User config**:
`$XDG_CONFIG_HOME/mysbx/config.toml`, the host-wide layer. Decides what the
user allows into *any* sandbox; a sidecar can only narrow it.
_Avoid_: global config, system config

**Layer**:
One source of configuration (built-in defaults, user config, sidecar, flags).
Layers are merged in a fixed precedence, never concatenated blindly.

**Backend**:
The technology that actually confines the process (bubblewrap, podman+gVisor,
qemu, microvm). Always chosen explicitly, never auto-detected.
_Avoid_: driver, engine, runtime

**Payload**:
The process `mysbx` starts inside the sandbox — an interactive shell for the
bare form, or the command after `--`. Its exit code is propagated unchanged.
_Avoid_: inner command, guest command, job

**Base**:
The mounts, namespaces and environment every sandbox gets before any
configuration is applied (`/nix/store`, `/proc`, `/dev`, the dev-tool
closure, …). Configuration adds to the base; it cannot remove from it.
_Avoid_: defaults, boilerplate, preamble

**Mount**:
One declared host path exposed inside the sandbox, `ro` or `rw`, at the same
path or at an explicit destination. The repo mount is implicit, not a mount
entry.
_Avoid_: bind, share, volume
