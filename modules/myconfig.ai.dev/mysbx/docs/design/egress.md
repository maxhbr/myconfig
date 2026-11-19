<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the per-run egress broker — phase B of the network policy

Status: agreed design (bd myconfig-n8kv.1, phase B of that epic;
implementation in its children n8kv.1.1–.6).
Phase A shipped the `network = "proxy-only"` profile with a static,
socket-activated forwarder ([config.md](./config.md) D20/D28, live run in
[../egress-validation-log.md](../egress-validation-log.md)). This record
replaces that one static host channel with a process mysbx owns per run:
the egress broker, the single policy point every backend will eventually
use (n8kv ideas 2, 4, 9 and 12).

Vocabulary, on top of [`../../CONTEXT.md`](../../CONTEXT.md):

- **egress broker** — the host process mysbx spawns per run, which
  accepts the sandbox's one channel and dials the host's LiteLLM
  endpoint itself. It makes the allow/deny decision and writes it down.
- **run directory** — `$XDG_RUNTIME_DIR/mysbx-egress/<pid>/`, the
  broker's own directory: its listening socket, its decision log.
- **upstream** — the one destination the broker dials,
  `127.0.0.1:<litellm port>`. Literal, never resolved.
- **decision log** — `decisions.jsonl` in the run's LOG directory
  (E4: the sidecar's `egress/<pid>/`), written by the broker: one JSON
  object per line, appended, flushed per line. Never in the bound run
  directory: the payload reads that one through the read-only bind. The
  line format is fixed in E4.
- **static forwarder** — the phase-A `agent-litellm-forward-unix`
  socket unit (`myconfig.ai.dev.litellm-forwarder`), which the broker
  replaces in the sequence E6 fixes.

## The problem

Phase A's channel is a systemd unit: root starts it, it outlives every
sandbox, and the sandbox binds its socket directory read-only. That was
the cheapest correct fence, and it carries three limits the phase-B work
exists to remove:

- **No policy point.** Which sandbox reached the proxy, when, and with
  what result is not recorded anywhere. `doctor` can only ask whether the
  socket answers, not what it answered for whom.
- **Root-owned state.** The socket directory must be root-owned and
  non-writable by the user, which is why the socket itself is mode 0666
  and why the whole channel lives at a fixed path in `/run`. Every sandbox
  user needs that path, so path and ownership are host-wide facts.
- **Not the shape other backends need.** Phase C gives every backend a
  channel (vsock, hybrid vsock, a bound socket in a `--network none`
  container). A host-wide static unit cannot express "for this run, this
  target, this policy" — the broker can.

## Decisions

### E1: The broker is one host process per run, spawned by mysbx

Not a daemon, not a systemd unit, not a long-lived host service. mysbx
spawns `mysbx-egress-broker` for the run and owns the child, exactly as
it owns the waypipe client (`start_waypipe_channel`, `lib.rs`): per-run
directory, `prctl(PR_SET_PDEATHSIG, SIGKILL)` through the existing
`die_with_parent` fork-race guard, a bounded wait for its socket before
the backend starts, an unlink of a stale socket in the run directory
before it binds (the waypipe rule: a directory named after a pid belongs
to a run that is gone), kill-and-report on failure.

The broker is a host process with the run's lifetime, chosen over a unit
for four reasons:

1. **Lifetime is the run's.** A detached run survives its terminal and
   dies with its inner mysbx; a unit would outlive both and need its own
   supervision.
2. **It runs as the invoking user**, so no world-writable socket is
   needed: the phase-A `SocketMode = "0666"` relaxation exists only
   because root had to serve every user's sandbox. A user-owned channel
   in a 0700 directory needs no such relaxation.
3. **Each run gets its own channel, policy and log.** The pins are
   wrapper-provided settings (`.litellm-forwarder.nix`). What is
   host-global today is the ONE fixed directory `/run/mysbx-egress`,
   which E3/E4 replace with a per-run directory — the change this record
   exists to make.
4. **It is the shape phase C needs** (vsock, container-local sockets):
   the backend builds the fence and the transport, the broker owns the
   policy — for every backend, in one place.

The upstream table has exactly one row in phase B:

| listen | upstream | policy |
| --- | --- | --- |
| the run directory's `<name>` socket | `127.0.0.1:<litellm port>` | admit, then log |

Allowlist rows (`allow-domains`, `connect-ports`, D21) ride the CONNECT
lane since bd myconfig-ew9s (docs/design/egress-connect.md): the
phase-B table above is the degenerate case of it, and a plain
`network = "proxy-only"` run keeps it byte-for-byte. With a table
the broker RESOLVES names at connect time, host-side, and pins one
address per connection — so the rebinding and metadata-range
concerns (bd myconfig-fvi) become applicable there; the
address-class refusals (E9 of egress-connect.md) are the rule that
keeps an allowed name's A record from turning the broker into a
host-service and metadata client.

The broker is a Rust binary in this crate, std only (the
`mysbx-egress-relay` precedent: no runtime, no async), and it confines
itself: it accepts only the one upstream, over TCP, on the pinned port.
`myconfig-n8kv.1`'s open decision 9 asks for self-confinement with
Landlock network rules rather than a systemd `IPAddressAllow` (a user
process has no unit). The concrete rule set — and the Landlock ABI 4
floor under which port rules exist — is fixed in n8kv.1.2 **before** the
binary accepts its first connection; a kernel below it refuses the run.
This record fixes the choice (Landlock, in-process, fail closed); the
program against it is the child bead's.

### E2: Lifecycle, per run mode

Three run shapes, one rule: **the broker is a child of the process that
spawns the backend.**

- **exec mode** (the bare form, cli.md D2): mysbx execs the backend, so
  mysbx's own pid becomes the backend's. The broker's parent is that pid,
  and `PR_SET_PDEATHSIG` delivers `SIGKILL` when the backend exits. No
  explicit kill is possible — mysbx no longer exists at that point — and
  none is needed.
- **result mode** (`run --result`): mysbx waits for the backend, then
  kills and reaps the broker, exactly as it reaps the waypipe client.
- **detach** (detach.md B4, B7): the broker is spawned by the INNER mysbx
  — the one under the `dtach` master that spawns the backend. It must not
  be spawned by the outer pre-flight: the outer's whole contract is that a
  refusal (and `--dry-run`) creates nothing, so a broker spawned there
  would leave a run directory behind for a run that never started. The
  inner's death (the sandbox ending) is therefore the broker's death.

The broker must be up before the backend starts. The socket wait is
bounded (5 s, the waypipe budget), and a broker that dies or never binds
refuses the run: **no broker, no egress** — never a silent fall back to a
shared network.

### E3: The run directory is user-owned, not root-owned

The phase-A rule — "the bind source must be a non-symlink, root-owned
directory, not group- or world-writable" — exists because the sandbox
bound a root-owned `sockets.target` unit's directory read-only. Under the
broker the bind source is a directory the invoking user just created, so
the rule becomes its honest phase-B form:

> the run directory is a REAL directory (never a symlink), owned by the
> invoking user's effective uid, with no group or other permission bits
> (`mode & 0o077 == 0`).

The existing pure predicate (`egress_dir_metadata_ok`) takes the expected
uid and the forbidden mode bits as parameters instead of hard-coding
root and `0o022`, so its unit tests keep covering both rules. It splits
into the two checks the phases need: a SHAPE check on the path the run
would use, which `--dry-run` runs too (the D20 rule: a dry run checks the
path, not the filesystem), and a REAL-RUN metadata check, which now says
"this is our own directory" instead of "root made this directory".

What does NOT change: the sandbox still binds ONE directory read-only (at
the fixed in-sandbox path `bwrap::EGRESS_DIR`), so the "never a shared
directory" argument still holds. It now holds by construction: the
directory is created per run at mode 0700 by the user who runs the
sandbox, and it holds nothing but the socket. It is removed when mysbx
reaps the broker (result mode, detach) and, in exec mode, by the next
run's empty-directory sweep of `$XDG_RUNTIME_DIR/mysbx-egress` — a
directory whose pid is gone and which holds nothing is debris, the
`pid_gone` idea applied to a root that may hold several runs.

### E4: The socket lives under `$XDG_RUNTIME_DIR`, and the name comes from the name pin

`<run dir>/<name>`, where `<run dir>` is
`$XDG_RUNTIME_DIR/mysbx-egress/<pid>/` and `<name>` is the
`MYSBX_EGRESS_SOCKET_NAME` pin, validated by the
`validate_egress_socket` shape rule (one plain file-name component,
never `.`/`..`).

Why this root and not `<sidecar>/egress/<pid>/` (the location the
parent bead's B1 line names, `pid_gone`-swept):

- **`SUN_LEN`.** A Unix socket path is bounded (107 bytes,
  `detach::SUN_PATH_LIMIT`). A checkout path plus `.mysbx/egress/<pid>/`
  plus a 20-character file name can exceed it, and bd myconfig-wui7 is
  the question that asks exactly this. The precedent is inside this
  repo: bd myconfig-lxiw is the same failure on nono's own
  `supervisor.sock` in a long build root (120 bytes). `$XDG_RUNTIME_DIR`
  is short by construction, and a computed path over the limit is
  REFUSED (exit 70) with an error naming the limit and the offending
  path, in both mysbx and the broker — never silently shortened.
- **Lifetime.** The channel is per run and per login session; a tmpfs
  that disappears at logout matches that, and a stale directory cannot
  outlive its session for longer than one sweep.
- **The detach registry stays closed.** detach.md B8 refuses a bind
  source that IS, lies below or contains `$XDG_RUNTIME_DIR/mysbx`. The
  sibling spelling `mysbx-egress` is none of those, and it must stay a
  SIBLING — nesting the run directory under `mysbx/` would make the
  channel's bind source trip the registry-exposure check.
4. **The fit is checked before the run starts.** Both the resolved path's
  length and the `$XDG_RUNTIME_DIR` root itself are validated in step 4b,
  so a missing or relative root (`None`, never a `/tmp` guess) and an
  over-long path refuse the run with a dry run still auditing it.

An unset or relative `$XDG_RUNTIME_DIR` refuses the run (exit 70) rather
than falling back, the same rule detach.md B3 applies to detached runs
(a `/tmp` fallback would put the channel in a world-writable directory).

That is also why item 4 above sits in step 4b: the root is resolved
once, early, and the pins carry the finished directory, so the argv
builder and the broker start cannot disagree about the path.

The wrapper pins the NAME directly since the static unit went away (bd
myconfig-ps3i, E6 step 3): `MYSBX_EGRESS_SOCKET_NAME` is one plain
file-name component (`[A-Za-z0-9._-]+`, not `.`/`..`, no `/`), sourced
from the host option `myconfig.ai.dev.litellm-forwarder.unixSocketName`
(the old `unixSocket.name` spelling went with the unit). A run that
finds the pin malformed refuses exactly as it does now; a path-shaped
value is not a name and is refused.

The DECISION LOG does not live in this directory. The sandbox binds the
run directory read-only, so anything in it is readable by the payload;
idea 9 wants the log out of the payload's reach. It goes to the sidecar's
`egress/<pid>/decisions.jsonl` — the location the bead names — and the
run directory keeps exactly one entry, the socket. The log directory is
sidecar debris like the waypipe token directories (config.md D18): the
next run of the same repo sweeps the entries of gone pids, and a refused
or dry run creates none.

**The line format is fixed here** (bd myconfig-n8kv.1.4). One JSON object
per line, appended with a single `write_all` of the whole line plus a
`flush`, so a reader never sees a half line and a crash loses at most the
line being written. The four fields are always present:

```json
{"ts":1794583068,"pid":9671,"event":"listen","why":"/run/user/1000/mysbx-egress/9669/litellm.sock -> 127.0.0.1:4777, confined by Landlock to that one port"}
```

- `ts` — whole seconds since the epoch (`0` if the clock is before it).
- `pid` — the BROKER's pid, not the run's and not the peer's.
- `event` — one of the vocabulary below.
- `why` — free text, JSON-escaped (`"`, `\`, and control characters; other
  bytes pass through). It names peers and paths, so nothing machine-parses
  it beyond displaying it.

The vocabulary separates the ADMISSION decision from what happened after
it, because a reader must not mistake a stopped model proxy for a policy
refusal:

| `event` | meaning |
| --- | --- |
| `listen` | the channel opened; `why` is `<socket> -> <upstream>` |
| `allow` | a peer was admitted; `why` carries its pid and uid |
| `deny` | a CONNECT request was refused by the table (bd myconfig-ew9s); `why` names the rule |
| `unreachable` | after admission, the upstream could not be dialed |
| `io` | after admission, a stream could not be split |
| `fatal` | the accept loop died; the process exits `70` |

So one connection produces `allow` and MAY then produce `unreachable` or
`io`: `allow` counts ATTEMPTS, not successes, and the counts therefore do
not sum to a connection total.

There is a `deny` event — but only since the CONNECT lane (bd
myconfig-ew9s, docs/design/egress-connect.md E9). In phase B nothing
refuses a peer — the fence is the empty network namespace plus the one
bound socket — so a `deny` in a phase-B log would be a broker bug, not
a policy decision. A paired run (allowlist keys under the profile)
adds the CONNECT table: a `CONNECT host:port` request the table does
not admit produces `deny` with a reason naming the rule that refused
it, and `mysbx diagnose egress` counts it. Transport failures stay
separate — a resolution or dial failure after a CONNECT admission is
`unreachable`, never `deny`.

`mysbx diagnose egress` (n8kv.1.4, cli.md D29) is the reader. A line that
does not parse is SKIPPED, not treated as an error: a broker killed
mid-write leaves exactly that, and the rest of the log is still the
record.

### E5: Fail closed, and what a dry run can audit

Every failure is exit 70, in the order that already exists (D20): the
pin checks run in step 4b, BEFORE anything is created (`ensure_state_dirs`,
the SSH key, the session clone) and before the `--dry-run` early return,
so a dry run audits them: a missing or malformed `MYSBX_EGRESS_SOCKET_NAME` /
`_PORT`, an unset or relative `$XDG_RUNTIME_DIR` (when the broker is
pinned), a computed socket path over `SUN_PATH_LIMIT`.

A build WITHOUT the broker pin is a refusal too, since E6 step 3: the
static unit is gone, so "no broker, no egress" governs every build.

The checks that need the filesystem or a live process come AFTER that
return, on a real run only, and are still refusals, not warnings: the run
directory is not ours (missing after creation, a symlink, another uid,
group/other bits), the broker fails to spawn, its socket does not appear
within the budget, it exits early, the kernel's Landlock ABI is below 4.
All of them refuse before the backend starts, so nothing runs half-fenced.

`--dry-run` prints the channel it WOULD create — the run directory shape,
the broker's binary, the upstream — and creates nothing, including no run
directory and no sidecar log directory. The test for this is the `mkdir`
sentinel: a dry run in a fresh `$XDG_RUNTIME_DIR` and a fresh sidecar
leaves both empty.

### E6: The static forwarder unit is gone (the sequenced removal, executed)

Decision 3 of the epic ("keep-as-fallback after the broker exists:
decide in myconfig-n8kv.1") was decided as a sequenced removal and has
run to the end:

1. The broker landed first (n8kv.1.2, .1.3) while the static unit still
   existed, and the wrapper pinned both spellings.
2. The live checks (n8kv.1.6, and the vpi9.1 nono run) validated the
   broker path on real hardware — bubblewrap (§1–§4) and a brokered
   nono run (§5) on host 'thing', 2026-11-14, in the
   [validation log](../egress-validation-log.md).
3. bd myconfig-ps3i then dropped the static unit in ONE commit
   (revertable as one), together with the name-source rewrite it was
   waiting on: `MYSBX_EGRESS_SOCKET` (the `/run/mysbx-egress/<name>`
   path shape) became `MYSBX_EGRESS_SOCKET_NAME` (a plain name), and
   the broker pin became REQUIRED — a build without it refuses the
   profile, so there is no fallback channel to keep.

What went dead, named so the removal commit can be checked against it:

- `systemd.sockets.agent-litellm-forward-unix` /
  `systemd.services.agent-litellm-forward-unix` and their
  `DirectoryMode`/`SocketMode` block in
  `myconfig.ai.dev.litellm-forwarder.nix`, plus the
  `mkIf cfg.unixSocket.enable` half of that module;
- the `unixSocket.enable` option (and what it asserted) — the surviving
  option is `unixSocketName`, the NAME source;
- the `egressSocket` argument of `nix/mysbx.nix` (now `egressSocketName`)
  and the `egressPinGate` in `nix/config-eval-test.nix` (now gating the
  name);
- the root-owned-directory rule of D20 (replaced by E3 in phase B).

The TCP half of the forwarder is NOT part of this: it serves the podman
backends (bd myconfig-jq2) and is hardened separately in n8kv.15.

## Not decided here

- The CONNECT proxy, hostname/port allowlists at the broker, and the
  `"public"` value — bd myconfig-n8kv.2.
- Credential injection and TLS interception, per-binary identity,
  interactive approval, seccomp-notify interception — bd myconfig-n8kv.3.
- Datagram/raw-socket/`io_uring` denial — bd myconfig-n8kv.4.
- nono's own grants — bd myconfig-vpi9.1 (LANDED: `--block-net` plus one
  `--allow-connect-port`; the broker path covers it unchanged).
