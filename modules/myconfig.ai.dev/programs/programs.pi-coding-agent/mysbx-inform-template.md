---
name: mysbx-inform
description: Explain the mysbx sandbox this session runs in — its backend, confinement model and network policy. Use when the agent wants to know where it runs, why egress fails or must go through a proxy, or how to reason about its environment.
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

## Variables of record

These were read to render this file; they stay set in the environment
as the authoritative values if this file is stale:

- `MYSBX_BACKEND` — `{{backend}}`
- `MYSBX_NETWORK` — `{{network}}`
{{env_var_lines}}

## Digging deeper

The design docs of the mysbx deployment live with the myconfig
repository this sandbox serves:

- `modules/myconfig.ai.dev/mysbx/docs/design/config.md` — D14
  (identity/infra env vars), D20 (the `network` profile), D21
  (allowlists), D27 (nested virt)
- `modules/myconfig.ai.dev/mysbx/docs/design/backends.md` — the
  backend table and per-backend contracts
- `modules/myconfig.ai.dev/mysbx/README.md` — the sandbox's identity
  and network variables, operator view

Read them only when a decision needs the exact mechanism; the facts
above are enough for everyday work.
