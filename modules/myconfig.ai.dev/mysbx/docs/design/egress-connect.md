<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the broker's CONNECT mode — the D21 allowlist on the bubblewrap backend

Status: IMPLEMENTED (bd myconfig-ew9s, phase C of bd myconfig-n8kv.2) —
the design record of what now ships, in the 4-commit order its own
E-decomposition lists (broker lane, nono reservation half, registry
enum, the lift). Every code anchor below was verified against
aa763dd073 by symbol, and the kernel-level claims were probed live
with bubblewrap 0.13.1 (see "What this sandbox proved"). The
implementation's docs commit supersedes the one paragraph of
egress.md E4 this record makes stale (E9) and config.md D20/D21's
bubblewrap rows (E7/E12); the validation log records what was proven
end to end and what this sandbox cannot prove.

## The two problems, verified

- **FENCE (exists).** `bwrap --unshare-all` without `--share-net`
  leaves an empty netns; the ONE host channel is the broker's AF_UNIX
  socket, bound read-only at `/run/mysbx-egress` (`bwrap.rs`, the
  `egress` arm of the argv builder). Re-probed live: `ip route` inside
  is empty, and a host loopback listener a client reaches fine under
  `--share-net` is `ECONNREFUSED` inside the fence. A proxy on a
  shared-network sandbox is advisory — also probed live: with
  `HTTPS_PROXY` set, `curl --noproxy '*'` walks past it and connects
  directly.
- **POLICY (missing).** The broker (`bin/mysbx-egress-broker.rs`) has
  exactly one upstream row and no decision code: `relay_one` dials the
  upstream unconditionally after `accept`; there is no CONNECT parsing,
  no deny path in production code (`deny` exists only in test fixtures),
  and egress.md E4 says so ("There is no deny event").

## E7: the pairing — the allowlist rides the proxy-only transport

The load-bearing decision is WHICH config spelling selects the broker
transport. It cannot be left implicit, because today no spelling can
reach a fenced broker with an allowlist (verified, and the concrete
defect the bead's ordering insight predicts):

- `allow-domains`/`connect-ports` + `network = "shared"` (the default)
  is refused by the registry gate (lib.rs, the second
  `if !allowlist_keys.is_empty()` block, predicate
  `backends::Backend::network_allowlist`), and the argv builder would
  emit `--share-net` for a shared run (`bwrap.rs`,
  `cfg.network.is_shared()`) — the fence would die.
- the same keys + `network = "proxy-only"` are refused by the
  profile-contradiction block (lib.rs, the first `allowlist_keys`
  block) and are a same-file schema error (`config.rs`, the
  `Network::ProxyOnly` guard) — "an allowlist contradicts it".
- the channel itself is gated on `cfg.network == ProxyOnly`
  (`bwrap.rs`, `let egress`), and `resolve_egress` runs only for the
  profile.

The decision:

- `network = "proxy-only"` + keys on **bubblewrap** = the profile's
  transport (empty netns, broker) with the broker's table = the
  LiteLLM row PLUS one row per merged allowlist entry. The
  mutual-exclusion refusal is lifted for this backend only, and its
  message flips from "contradicts it" to naming the pairing.
- keys + `network = "shared"` on **bubblewrap** stays REFUSED, with
  the new reason: the keys select broker policy; set
  `network = "proxy-only"` to get the transport. Never `--share-net`
  with an advisory proxy.
- `network = "proxy-only"` alone is byte-identical to today: the table
  is the one LiteLLM row; phase B is the degenerate case. This is "one
  fixed upstream ⇒ no rotation" made concrete — a small table, argv
  audited, and the phase-B row is never removed.
- The D28 credential rules are keyed on the profile
  (`merged.network == ProxyOnly`, lib.rs credential gate) and keep
  applying to the paired run unchanged.
- The layered nono backend keeps its own D21 enforcement for
  `shared`; `proxy-only` + keys on nono stays refused in this bead.
  The registry enum (E12) leaves room to extend the pairing there.

## E8: two lanes on one socket; the relay is untouched

The broker keeps serving the phase-B lane byte-for-byte: bytes that
are not a `CONNECT` request are relayed to the LiteLLM row, so the
agents' baked `http://127.0.0.1:<port>/v1` URLs keep working with no
CONNECT awareness. A `CONNECT host:port` request line selects the
policy lane. The in-sandbox relay needs no change (verified: it is a
dumb byte pipe with `--listen`/`--connect`).

For the payload to USE the CONNECT lane, mysbx injects the proxy
variables itself: `HTTP_PROXY`/`HTTPS_PROXY` = `http://127.0.0.1:<relay
port>`, `NO_PROXY` = `127.0.0.1,localhost`, plus the lower-case forms
— set after `[env]` like `TMUX_TMPDIR`, so no configuration layer
overrides them by precedence; and the reservation (E13) drops payload
overrides instead of trusting that precedence at all.

A `--dry-run` today never prints the broker argv (the broker is
spawned after the dry-run return). Because E7 makes the table part of
what an operator audits, the dry run gains the broker argv block after
the backend argv, the same shape the detached-dtach block uses
(program, then one argument per line). Goldens that pin the broker
channel are updated with it.

## E9: the deny and the dial rules

- A `deny` event joins the E4 vocabulary; the "There is no deny event"
  paragraph of egress.md E4 is superseded (the implementation's docs
  commit rewrites it), and `mysbx diagnose egress` counts it like
  `allow`.
- **Matching.** Name-form `CONNECT` targets match `allow-domains`
  entries exactly. URL/path-form allow-domains entries are refused at
  the gate exactly as nono's `validate_allowlist` refuses them — the
  gate's nono-only call of that predicate generalizes to every
  enforcing backend, so `listen-ports`-only and URL forms stay refused
  on bubblewrap too.
- **IP literals.** With `allow-domains` set, an IP-literal `CONNECT`
  target is refused: the name is the policy, an IP literal has no name
  to match. With `connect-ports` alone (nono's "ports without domains"
  semantics, honestly mapped — the empty netns denies every direct
  connect, so the ports can only mean the CONNECT lane) any host on an
  allowed port is admitted.
- **Conjunction, deliberately unlike nono.** On nono, domains and
  ports are NOT an intersection (the supervisor denies direct remote
  connects, so the port grants do not widen the domain policy). On the
  broker there is no direct-connect path at all, and Landlock grants
  PORTS — so `connect-ports` is the CONNECT lane's port filter:
  intersecting with `allow-domains` when both are set, the only row
  class when domains are empty. Emitting port rules into the fence
  while admitting allowed domains on any port would make the fence a
  lie.
- **Resolution.** Host-side, at connect time, one address chosen and
  pinned for the life of the connection; the resolved address is
  logged in the `allow` line. This is where the rebinding and
  metadata-range concerns of bd myconfig-fvi become applicable —
  egress.md's "the rebinding and metadata-range rules do not apply
  yet" sentence gets its successor here.
- **Address classes.** For CONNECT rows the broker refuses to dial
  loopback, link-local (`169.254.0.0/16`, `fe80::/10`), unspecified
  and multicast targets. Only the LiteLLM row may dial loopback.
  Without this rule an allowed name whose A record points at
  `127.0.0.1` or `169.254.169.254` turns the broker into a
  host-service and metadata client. Private ranges stay allowed —
  `allow-domains = ["proxy.internal", …]` is a documented use.
- **Fail closed.** A broker that cannot parse its table, or cannot
  apply its Landlock fence, refuses before the socket exists (the
  phase-B refusal order, exit 70). It never serves unmapped.

## E10: the Landlock fence under CONNECT

The phase-B fence is one port (`landlock.rs`,
`confine_to_tcp_port`). Under CONNECT the rule set extends to one rule
per allowed port. `connect-ports` empty + `allow-domains` non-empty
means the port axis is unrestricted (D21 semantics), so the fence
widens to every port — the ONE sanctioned widening, and it is written
into the `listen` line of the decision log, never silent. The host
axis is carried by E9's matching and address-class refusals, not by
Landlock, which grants ports and never addresses. Handling
`ACCESS_NET_BIND_TCP` as well (deny all TCP bind for the broker) is
optional defense in depth; the security claim does not rest on it.

## E11: why a permitted destination cannot become a relay

The threat the bead names — re-aim an already-admitted socket at a new
destination with `IP_TRANSPARENT`/`IP_FREEBIND` and `TCP_REPAIR`, then
splice — has no foothold in this design. What blocks it is NOT a
blanket "the payload has no capabilities, so every spoofing setsockopt
fails": one of the three options SUCCEEDS inside the fence, and the
design must say so rather than imply otherwise. The claim rests on two
primitives, each verified live against bubblewrap 0.13.1:

1. **The netns has no route.** Observed directly inside the fence:
`ip route` shows nothing. A socket — whatever its source address —
has nowhere to go.
2. **There is no admitted socket to re-aim.** The payload's only
channel is the broker's AF_UNIX socket; the payload owns no routed
socket whose re-aiming would inherit an admission. The routed
sockets (the broker's) live in another pid namespace (primitive 3
below) and are never shared as fds.

The setsockopt inventory, measured on the host, inside
`--unshare-all`, and inside `--unshare-all --share-net`:

| option | needs | result in all three |
| --- | --- | --- |
| `IP_TRANSPARENT` | `CAP_NET_ADMIN`/`CAP_NET_RAW` (absent: `CapEff: 0`, `NoNewPrivs: 1`) | `EPERM` |
| `TCP_REPAIR` | `CAP_NET_ADMIN` | `EPERM` |
| `IP_FREEBIND` | none capability-gated | **`ALLOWED` — and it works: `bind(192.0.2.77)`, a non-local source, succeeds** |

`IP_FREEBIND` is the option a reader would expect to be the hole, so
it is named here deliberately: the setsockopt is NOT capability-gated
(a user namespace satisfies the check with its own caps), the bind to
a spoofed source SUCCEEDS — and the `connect(2)` from that source dies
with `ENETUNREACH` (errno 101), inside the fence and under
`--share-net`. The handshake never leaves the netns; primitives 1 and
2 are what make the allowed-but-useless option useless. Without the
empty netns (a `--share-net` payload with `IP_FREEBIND`), the option
would need a route — and on this offline sandbox that errno is
unattributable between "the netns has no route" and "the host has no
route at all" (the host's own `connect(1.1.1.1)` is `ENETUNREACH`
too), so the design's fence claim rests on the DIRECTLY observed
empty route table inside `--unshare-all`, never on the share-net
errno.

3. **The broker is unreachable as a process.** The payload's PID
namespace hides the broker from its `/proc`; the one channel is the
AF_UNIX socket; no fd is ever passed in either direction. The
payload cannot ptrace or inject into a process it cannot name. This
is a property of `--unshare-all`, NOT of the broker design itself:
if a later refactor ever runs the broker and the payload in ONE pid
namespace, this primitive is silently GONE while the empty netns
still holds — the fence test must assert the pid-namespace
separation explicitly, so the refactor fails a test instead of
quietly removing a named primitive.

So the implementation does NOT rely on a seccomp `IP_TRANSPARENT`
filter or on de-privileging beyond what bubblewrap already does. The
test that pins this (AC2) is a fence-child test in the
`tests/egress_broker.rs` pattern: a child inside the fence asserts
`EPERM` on the two capability-gated setsockopts, that `IP_FREEBIND`
bind to a non-local source succeeds and its `connect(2)` fails
`ENETUNREACH`, that a non-loopback plain connect fails, and that the
broker's pid is not visible in the child's `/proc`.

The residual risk, named honestly: an allowed host that itself runs a
forward proxy IS a relay. That is inherent to every name allowlist —
nono's has the same property — and no transport design removes it; it
is out of scope. The rule that keeps the socket-level variant dead:
the CONNECT table never grows "via"/gateway rows. That is the
gateway-rotation trap restated as a constraint on this table.

## E12: the registry restructure

The two booleans cannot express "enforceable only with a broker
pinned" (verified: bubblewrap's row would need `network_allowlist` to
be true with the pin and false without it, while nono's is
unconditionally true, and `network_proxy_only` already says something
else about both). Replace the allowlist bool with an enum on the row:

```rust
pub enum Allowlist {
    /// The backend cannot enforce the keys; the run is refused (D21).
    Refused,
    /// nono enforces them itself (its proxy, Landlock and seccomp).
    NonosEgress,
    /// The bwrap-built tiers enforce them through the per-run broker,
    /// and only when the wrapper pinned one (MYSBX_EGRESS_BROKER);
    /// without the pin the run is refused, never advisory.
    Broker,
}
```

- `network_proxy_only` stays a bool: it still expresses exactly "the
  bwrap-built tiers", and the D20/D21 distinction survives (AC3).
- The gate asks the row AND the pin: `NonosEgress` ⇒ enforced;
  `Broker` ⇒ enforced iff the pin exists. The pin must be read BEFORE
  the gate — today `resolve_egress` (which reads `MYSBX_EGRESS_BROKER`)
  runs after the allowlist block; hoist the pin read, and keep every
  refusal before the session clone and before the `--dry-run` return,
  so a dry run audits them.
- The table-pinning test (backends.rs, "network_allowlist must match
  config.md D21's table") is rewritten against the enum and still pins
  the doc table: bubblewrap `Broker`, nono `NonosEgress`, the five VM
  and podman rows `Refused`.

## E13: the proxy-variable reservation, implemented (the D1 caveat)

backends.md D1's "documented protection, not implemented" becomes
code. Under nono's domain-allowlist mode and under the bubblewrap
CONNECT lane, the proxy-control names — `HTTP_PROXY`, `HTTPS_PROXY`,
`NO_PROXY`, their lower-case forms, `NONO_PROXY_TOKEN`,
`NONO_NO_PROXY`, `NODE_USE_ENV_PROXY` — are reserved: `[env]` and
`forward-env` entries with those names are dropped, not applied, and
reported as `[config, ignored — set by the proxy]` /
`[host, ignored — set by the proxy]` (the report's ignored-marking
vocabulary already exists in `report.rs`).

The harm is verified real on both lanes: the layered builder applies
`host_env` and `cfg.env` in the pinned `env` AFTER nono's injection
(`bwrap.rs`, the `inner` arm), so a configured `HTTPS_PROXY`
overwrites nono's injected value; and the CONNECT lane injects its own
variables (E8), which an unreserved `[env]` entry would silently
override. The honest scope is unchanged from backends.md's own words:
on bubblewrap an override breaks the payload's own egress (the fence
holds — nothing else is reachable); on nono it breaks connectivity (the
supervisor still denies direct remote connects). The reservation is
correctness and marking, not the fix of a demonstrated policy bypass.

## Decomposition (verified ordering)

The bead's commit order holds, with one coupling made explicit: the
reservation is not independent of the lift, because the CONNECT lane
INJECTS proxy variables — they must be un-overridable in the same
change or the D1 caveat reappears on the new lane on day one. The
nono-side half of the reservation can still land alone (it fixes the
documented gap for nono); the bubblewrap-side half lands with the
lift.

1. **Broker** (standalone, no callers yet): CONNECT lane, table
   parsing, `deny` event, Landlock extension, address-class refusals;
   binary tests for admit/deny/the two lanes/the rotation reasoning.
2. **Reservation, nono half**: drop + mark the reserved names under
   nono's domain-allowlist mode; report tests.
3. **Registry**: the `Allowlist` enum, the hoisted pin read, the
   re-pinned table test. No behavior change yet — bubblewrap still
   refuses keys (the gate now refuses them via `Broker` + no pin).
4. **The lift**: the E7 pairing — the two lib.rs refusal blocks and
   the config.rs schema twin, the row emission into the broker argv,
   the injected + reserved proxy variables (E8/E13), the generalized
   `validate_allowlist` call, the dry-run broker-argv block, goldens,
   CLI tests for every refusal (keys + shared; listen-ports-only;
   URL-form; keys without a broker pin).
5. **Docs**: config.md D20/D21 rows, backends.md D1, the egress.md E4
   paragraph, feature-comparison.md §3.
6. **Validation log**: the live probe below, or the honest
   "not provable in this sandbox" entry (q8zg precedent) naming what
   remains unproven: a real CONNECT round trip through the broker to
   an allowlisted host.

## What this sandbox proved (and what it cannot)

Proven live, bubblewrap 0.13.1, this sandbox:

- the fence: empty netns, no route (observed with `ip route`), host
  loopback unreachable inside;
- the advisory-proxy proof under `--share-net` (`--noproxy '*'`
  bypasses `HTTPS_PROXY` and connects);
- the E11 setsockopt inventory: `EPERM` on `IP_TRANSPARENT` and
  `TCP_REPAIR` (empty `CapEff`, `NoNewPrivs 1`), and `IP_FREEBIND`
  allowed with a working `bind(192.0.2.77)` whose `connect(2)` fails
  `ENETUNREACH`.

Attribution caveat, recorded honestly: this sandbox's HOST has an
empty route table (an offline OS), so a `--share-net` `ENETUNREACH`
cannot here be attributed to the fence rather than the offline host —
the host's own `connect(1.1.1.1)` is `ENETUNREACH` too. The fence
claim rests on the directly observed empty route table and failed
plain connect INSIDE `--unshare-all`, which is the only netns this
design ever runs.

Not provable here: an end-to-end CONNECT round trip (no network, no
LiteLLM forwarder). The validation-log entry must say so explicitly.

## Not decided here

- `listen-ports` beyond the existing refusals (listen-ports-only
  stays refused; the key riding along with allow-domains remains
  in-sandbox loopback, unchanged by this design).
- The layered nono backend's `proxy-only` + keys pairing.
- The `"public"` network value, datagram/raw denial (bd
  myconfig-n8kv.4), TLS interception and credential injection (bd
  myconfig-n8kv.3).
- A seccomp `IP_TRANSPARENT` filter as belt-and-braces on top of E11.
