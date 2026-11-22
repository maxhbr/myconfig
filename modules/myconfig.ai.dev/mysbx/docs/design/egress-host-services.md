<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the broker's host-service rows — named host loopback services on the broker-channel backends

Status: IMPLEMENTED (bd myconfig-db56, the successor decision of
[egress.md](./egress.md) E1/E9 and [egress-connect.md](./egress-connect.md)
E7) — the design record of what now ships, in the same shape the
CONNECT record has: the decisions, the fail-closed rules and the audit
surface, each anchored where the code holds it.

## The problem, and why the LiteLLM row alone is not enough

The broker table has exactly one loopback row — the upstream
(`--upstream 127.0.0.1:<litellm port>`), which E9 of egress-connect.md
exempts from the address-class refusal by construction: it is
argv-pinned, never resolved, never chosen by the payload. Every other
loopback dial is refused (`host_service_class`), and that is the rule
that keeps an allowed name's A record from turning the broker into a
host-service client.

That leaves a use case with no honest spelling: a sandboxed agent that
should reach exactly one controlled HOST service — e.g. an HTTP MCP
server listening on host loopback (bd myconfig-9y4c's `url` servers) —
must either expose the service on a non-loopback address (widening the
host's attack surface to the whole LAN) or drop the isolation
(`network = "shared"`). E9's refusal is the correct rule for CONNECT
rows; what is missing is a row class that is EXEMPT from it the same
way the LiteLLM row is — pinned by the wrapper, never chosen by the
payload, name-bound rather than address-resolved.

## E15: host-service rows — argv-pinned loopback forwards, name-keyed

The broker's table grows a third row class next to the CONNECT table
and the public lane:

```text
mysbx-egress-broker --listen <run>/<name> --upstream 127.0.0.1:<port> \
    --decision-log <path> \
    [--allow-domain <host>]... [--allow-connect-port <port>]... \
    [--public] \
    [--host-service <name>=127.0.0.1:<port>]...
```

- **The row is an argv-pinned forward, not a policy grant.** Each
  `--host-service name=127.0.0.1:<port>` names ONE host loopback
  endpoint the broker will dial when a client asks for `name` — the
  same shape as the LiteLLM row (a pinned loopback upstream), with the
  name as the in-band selector. The address is LITERAL in the argv:
  never resolved, never influenced by anything inside the sandbox, so
  the E9 rebinding/metadata concern has no foothold — the exemption is
  earned the same way the upstream's is.
- **The name is the policy, exactly as for `allow-domains**: plain
  host-name grammar, lower-cased at validation, deduplicated, and the
  first-win rule on merge. A row's name may not collide with another
  row's, may not be an IP literal, may not carry scheme/path/port
  syntax, and may not be `localhost` — `localhost` inside the sandbox
  is the in-sandbox loopback (`egressEtc` binds `127.0.0.1 localhost`
  over `/etc/hosts`), so a row under that name would shadow the
  relay's own listener with a forward to the host.
- **Only loopback upstreams.** The row's target must be
  `127.0.0.1:<port>` or `[::1]:<port>`; a non-loopback target is a
  refusal (exit 70, before the socket). This is the blast-radius
  fence: the rows can name HOST services — services that chose the
  natural safe way to listen — and nothing else. No non-loopback host
  address is reachable through the new rows.
- **The fence is exact (E10's rule, applied to the new rows).** The
  Landlock port fence carries the upstream port plus every row's
  target port, so the fence, the `listen` line and the dial are
  exactly equal — the same correction review finding 3 made of the
  CONNECT table: a claim of a widening the kernel does not grant must
  not survive. Host services on DIFFERENT ports are refused by the
  kernel with `EACCES`, and the log says only what the fence grants.
- **The lane picker stays byte-exact.** A connection whose first bytes
  are not a `CONNECT` request line keeps its two existing outcomes:
  relayed to the model upstream (the phase-B lane) or, now, when the
  CONNECT authority's host NAMES A ROW, tunneled to the row's target.
  The preface classifier and the tunnel tail are reused unchanged; a
  `CONNECT <row name>:<port>` where the port does not match the row's
  port is a `deny` (the row pins the port too — `CONNECT row:443`
  against a row pinned for port 8080 is refused, the conjunction the
  table lane already applies).
- **The rows ride the proxy-only transport only** (E7's pairing, now a
  three-way rule): the schema key is legal ONLY under
  `network = "proxy-only"` on the broker-channel backends
  (`Allowlist::Broker` rows: bubblewrap, podman-gvisor, krun,
  microvm-qemu, microvm-ch); `shared`/`none`/`public` + the key is
  refused, nono stays refused (open decision 12), podman-krun stays
  refused. A plain profile run with rows keeps the empty CONNECT
  table — the rows are forwards, not grants, so they do not need
  `connect-ports` and do not inject proxy variables.
- **No proxy-variable collision (E8/E13).** The rows do not trigger
  the proxy-variable injection: a row is reached by CONNECTing
  `<row name>:<row port>` — the payload's client must spell the row's
  name — not via `HTTP_PROXY`-mediated CONNECT to an allowlisted
  domain. A run with BOTH rows and a CONNECT table keeps the table's
  injection unchanged; a run with rows and NO table stays
  byte-identical to phase B.
- **Audit surface.** Every admitted row dial logs `tunnel` with the
  row's name and target (the same vocabulary E4 fixes); a refused
  port/conjunction match logs `deny` naming the row; the dry-run
  broker argv block prints the rows one `--host-service` per line;
  `mysbx diagnose egress` counts the events unchanged (the vocabulary
  is closed); `mysbx doctor`'s broker probe is unchanged (a run's
  rows are per-run config, not a host pin).

## The schema key: `host-services` (config.md D34)

A table array, merged per the D15 rule (user layer first, sidecar
adds, first occurrence wins by NAME):

```toml
[[host-services]]
name = "my-mcp"
port = 8765
```

- `name` — required; the plain host-name grammar of the row
  (`[a-zA-Z0-9-]` plus intra-label dots, case-insensitive, never
  `localhost`), duplicated rows are a schema error naming the file
  and the name.
- `port` — required; the HOST port the service listens on
  (1–65535). The address is fixed: host loopback, always — the key
  cannot spell an address, because a spelleable address invites the
  non-loopback spellings the design just refused.
- Strict schema (D11): wrong types, unknown keys and a missing
  `name`/`port` are errors naming the file, the key and the row,
  never warnings.

## Not decided here

- TLS termination or credential injection for the forwarded services
  (n8kv.3 stays undecided).
- Any form of LAN-reachable row (a named non-loopback host): refused
  by this record; a future need gets its own record.
- The MCP epic's rendering of row names into `mcp.json` (bd
  myconfig-9y4c's subtasks own that; this record provides the
  transport).
