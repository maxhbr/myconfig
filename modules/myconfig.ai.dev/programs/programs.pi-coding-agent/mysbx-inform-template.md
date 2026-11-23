---
name: mysbx-inform
description: Explain the mysbx sandbox this session runs in — its backend, confinement model, network policy and persistence rules. Use when the agent wants to know where it runs, why egress fails or must go through a proxy, or how to reason about its environment.
---

# mysbx sandbox briefing

This session runs inside a **mysbx sandbox**. The facts below describe
THIS run; they were read from the `MYSBX_*` environment variables the
sandbox backend set (config.md D14's infrastructure class — a config
layer cannot spoof them) and rendered into this file at session start,
so everything here is the current run's state, not a generic table.

## Identity

- **Backend**: `{{backend}}` — this is `MYSBX_BACKEND` of the run.
- **Host**: {{host}} — the OUTER host's hostname (`MYSBX_HOST`); the
  sandbox's own hostname is backend cover and names nothing.
{{multiplexer_line}}
{{session_line}}

## What `{{backend}}` means

{{backend_description}}

## Network

The run's policy is **`{{network}}`** (`MYSBX_NETWORK`):

{{network_description}}

{{egress_section}}

## Persistence

Only host-backed binds survive this sandbox's exit:

- **The repository** — the repo at its real path is the host's own
  live checkout, bound read-write (config.md D13): every commit and
  every working-tree change is host state the moment it is written.
- **The `<repo>__worktrees` sibling** — bound rw the same way
  (config.md D13): worktrees created inside (`git worktree add`,
  `workmux add`) persist on the host like the repo itself.
- **State directories** — `state-dirs` entries (config.md D15) are
  backed by the sidecar (`<repo>.mysbx/state/<entry>`) and survive
  across runs, so the coding agents' sessions and caches persist
  per repository (plus the implicit `.ssh` keypair). Read-write
  mounts and `--rw` grants write host state directly too.

Everything else is **ephemeral**: the sandbox home (`/mysbx-home`)
and any scratch directory not listed above are a tmpfs that dies
with the run. Credentials, scratch files and unfinished notes are
NOT expected to survive the exit — finish work by committing it to
the repository, not by leaving files in the home. A `mysbx-once`
run is the narrower exception: its sidecar and worktrees sibling
are temporary as well, so only the repo (and approved mounts)
keep your work.

{{persistence_note}}

## Variables of record

These were read to render this file; they stay set in the environment
as the authoritative values if this file is stale:

- `MYSBX_BACKEND` — `{{backend}}`
- `MYSBX_NETWORK` — `{{network}}`
{{env_var_lines}}

## Digging deeper

The design docs of the mysbx deployment live with the myconfig
repository this sandbox serves:

- `mysbx/docs/design/config.md` — D13
  (the implicit repo/worktrees binds), D14 (identity/infra env
  vars), D15 (state-dirs persistence), D20 (the `network` profile),
  D21 (allowlists), D27 (nested virt)
- `mysbx/docs/design/workspace.md` — the
  clone-session lifecycle and its handoff verbs
- `mysbx/docs/design/backends.md` — the
  backend table and per-backend contracts
- `mysbx/README.md` — the sandbox's identity
  and network variables, operator view

Read them only when a decision needs the exact mechanism; the facts
above are enough for everyday work.
