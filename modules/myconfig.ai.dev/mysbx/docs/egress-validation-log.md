# mysbx egress live-validation log

> **Scope / current status:** the dated entries below are host-run evidence
> for the configuration named in each entry, not acceptance of every later
> backend or policy. The enforcing backend today is `bubblewrap`; every
> other backend refuses `network = "proxy-only"`. See
> [`backend-comparison.md`](./backend-comparison.md) for the current backend
> guide and [D20](./design/config.md) for the decision the runs verify.
> Podman-gVisor's endpoint reachability is a separate item (bd
> myconfig-jq2) and is not covered here.

One block per host run: host, date, kernel, PASS/FAIL per section. A FAIL
invalidates the decision row — a bead goes against the decision document,
not this log.

The phase-A run below was transcribed from a bead's closing record. The
phase-B run (the per-run egress broker, [design](./design/egress.md)) has
a SCRIPTED half, so its probes are reproducible rather than prose:

```bash
MYSBX=<path-to-mysbx> ./nix/egress-broker-live-validation.sh <repo-dir>
```

Run it on f13 with the host's LiteLLM proxy up and the rebuilt wrapper in
place. It covers: the pins, a dry run that creates nothing and names the
per-run channel, a real run that starts the broker and logs to the
sidecar, the run directory's removal after a waited run, `mysbx diagnose
egress` reading the log back, a broken broker pin refusing the run
create-nothing, and every other backend still refusing the profile. It
prints the remaining HUMAN steps (an agent's model request from `pi` and
`opencode`, killing the broker mid-run, and `--detach` survival) rather
than pretending to press keys. Paste its output into the dated block.

## 2026-11-13, host 'f13' — `network = "proxy-only"` on bubblewrap (bd myconfig-mo3.2.8)

Provenance: the run was executed by hand on f13 after commit `df45536b73`
(the `localhost` relay fix). This block is transcribed from the bead's
closing record (bd myconfig-mo3.2.8, close reason and notes); the probes were
**not** re-executed while writing this file. Where the record names a section
as passed without listing every probe, the entry says so instead of
inventing output.

Host kernel is not recorded. Backends in scope: `bubblewrap` (enforcing) plus
the other six (refusals only).

### §1 audit surface — PASS

`mysbx --dry-run --verbose` prints the egress line naming the pinned socket
`/run/mysbx-egress/litellm.sock` and the relay. The dry run creates nothing.

### §2 reachability through the forwarder — PASS

- `curl http://127.0.0.1:<litellm port>/v1/models` succeeds inside the
  sandbox.
- The same request via `localhost` succeeds — this is the `df45536b73` fix:
  the fixed `/etc/hosts` (`127.0.0.1 localhost`) and `/etc/nsswitch.conf`
  (`hosts: files`) come from the store (`MYSBX_EGRESS_ETC`), so no resolver
  is needed for the loopback name.
- `pi` and `opencode` each complete one model request inside the sandbox.

### §3 blocked probes (everything but the proxy) — PASS

All of the following fail inside the sandbox as intended: `curl
https://example.com`, `getent hosts github.com`, `curl 127.0.0.1:22`, a host
`python -m http.server` port, and `nc -u 1.1.1.1 53`.

### §4 sandbox contents — PASS

`ip -o addr` shows only `lo`; `/nix/var/nix` is absent; host abstract Unix
sockets are unreachable; the sandbox environment carries no credential names.

### §5 socket-path refusals — PASS

`MYSBX_EGRESS_SOCKET` set to `/run/litellm.sock` or to
`/run/x/../litellm.sock`: both `--dry-run` and a real run exit `70`, naming
`MYSBX_EGRESS_SOCKET` and the required `/run/mysbx-egress/<name>` form.

### §6 directory-metadata refusal — PASS

A real run refuses unless `/run/mysbx-egress` is a real (non-symlink),
root-owned directory that is not group- or world-writable. systemd creates it
with the unit's `DirectoryMode=0755`.

### §7 forwarder stopped — PASS

With `agent-litellm-forward-unix.socket` stopped, a real run exits `70`
(the socket does not exist / is not a Unix socket), while `--dry-run` still
succeeds. The unit carries `--exit-idle-time=5min`, so an idle forwarder
exits without widening anything.

### §8 create-nothing ordering — PASS

A refused real run leaves no state directory, no SSH key and no session
clone behind: every refusal precedes `ensure_state_dirs`, the key and the
clone.

### §9 merge rules — PASS

A sidecar `network = "shared"` (or `true`) over a user `proxy-only` is a
`NetworkUpgrade` error; a sidecar `network = false` over a user `proxy-only`
runs with no network at all.

### §10 other backends' refusals — PASS

`podman-gvisor`, `podman-krun`, `krun`, `microvm-qemu` and `microvm-ch` (and
`nono`) each print their backend-specific refusal with exit `70` and do not
run. Endpoint reachability behind podman-gVisor stays bd myconfig-jq2.

### Not covered by this run

No kernel version, no timings, and no per-probe transcript are recorded — the
closing record names the sections as passed, not their raw output. A later
re-run should fill those in rather than treat this block as a template for
them.

## 2026-11-14, host 'thing' — the per-run egress broker (bd myconfig-n8kv.1.6)

Run after PR #108 merged to `main` and the host was rebuilt, so this is
the first evidence from a real wrapped `mysbx` on a real kernel — the
earlier rounds could only exercise the code against a hand-built shim.

Host kernel not recorded. The build under test is
`/nix/store/031zjvy1jlc6ydmnr7l6ijxjwg5mhf17-mysbx-0.1.0`.

### §1 doctor — PASS

`mysbx doctor bubblewrap | grep -i egress` reported the whole channel
through the wrapper's own pins:

```
OK   egress name lookup: /nix/store/9abasx0d9fi6p95jwglrq49lvvcwmcs4-mysbx-egress-etc (localhost = 127.0.0.1, no DNS)
OK   egress relay: /nix/store/031zjvy1jlc6ydmnr7l6ijxjwg5mhf17-mysbx-0.1.0/bin/mysbx-egress-relay (listens on 127.0.0.1:4000 in a proxy-only run)
OK   egress broker: /nix/store/031zjvy1jlc6ydmnr7l6ijxjwg5mhf17-mysbx-0.1.0/bin/mysbx-egress-broker (one process per proxy-only run)
OK   egress broker confinement: Landlock ABI 7 confines the broker to one TCP port
OK   egress channel probe: through the per-run broker: GET /v1/models answered (HTTP/1.1 200 OK)
```

Two things this settles beyond the PASS:

- **The wrapper's pin split is real.** The harness's fail-closed probe
  depends on `MYSBX_EGRESS_BROKER` being `--set` (immovable) while
  `MYSBX_EGRESS_SOCKET`/`_PORT` are `--set-default`; that was verified
  only against `make-binary-wrapper.sh` until now.
- **The probe reached a live model endpoint** — `HTTP/1.1 200 OK`, not
  the 404 the sandbox's stand-in servers answer, so the through-the-broker
  path carried a real request.

### §2 dry run — PASS

```
## network:        proxy-only (no --share-net; 127.0.0.1:4000 -> /run/mysbx-egress/litellm.sock via the relay from /run/user/1000/mysbx-egress/1670032)
```

The source is the per-run directory, not the static
`/run/mysbx-egress` — the distinction the harness's guard exists to
check, now demonstrated on a real report (verbose lines are `## `-prefixed,
which is what made an earlier `^network:` anchor vacuous).

A `bubblewrap` warning about `memory`/`cpus` being unenforced appeared in
the same output; it is a pre-existing backend limitation, unrelated to the
channel.

### §3 scripted runbook — PASS, 9/9

`nix/egress-broker-live-validation.sh` passed every scripted probe: pins,
dry run creating nothing, a real run starting the broker and logging to
the sidecar, the waited run's directory removal (E2/E3),
`mysbx diagnose egress` reading the log back, the broken-pin refusal
(exit 70, nothing created, E5), both bwrap-built tiers accepting the
profile, and the five refusing backends.

### §4 manual probes — PASS (operator)

- one model request each from `pi` and from `opencode` inside the sandbox;
- the broker killed mid-run: the sandbox's requests fail with no fallback,
  and the decision log gains no new entries — a dead broker appends
  nothing (the `unreachable` event is a LIVING broker's connect failure
  to the upstream, logged after its `allow`; broker death and upstream
  failure are two different failures, and only the second is visible
  in the log);
- a `--detach` / `mysbx attach` cycle: the broker outlives the outer run
  and dies with the inner one.

### §5 nono — PASS (real brokered run, later the same day)

`mysbx --backend nono run --result -- true` produced nono's own capability
banner with `net  outbound blocked`, then started the broker:

```
run 1677220  (/home/mhuber/myconfig/myconfig.mysbx/egress/1677220/decisions.jsonl)
  channel:      /run/user/1000/mysbx-egress/1677220/litellm.sock -> 127.0.0.1:4000, confined by Landlock to that one port
  decisions:    1 (0 admissions, 0 unreachable, 0 io, 0 fatal)
```

The `listen` line with `decisions: 1` is the evidence: nono's run created
a per-run channel, started a broker on it, and the broker bound its
socket. The channel source is the per-run directory, and nono's own deny
(`--block-net`) is visible in its banner.

The `0 admissions` is expected, not a gap: `run -- true` never opens a
connection, so nothing crossed the channel. The cross-the-channel half is
§4's model requests and §1's `HTTP/1.1 200 OK`.

**Limits of this entry.** §4 and §5's model-request probes are the
operator's report of passing, not transcribed command output. The runs
above executed in the main checkout rather than a scratch repo, so its
sidecar holds normal run state.

### Consequence for design decision E6

Both bwrap-built tiers are now evidenced through the broker on real
hardware: bubblewrap (§1–§4) and nono (§5). That closes E6's stated
precondition.

The static unit is nonetheless still **load-bearing**, and E6 as originally
written overstated the risk. What the code actually does with a broker
pinned (`lib.rs`): the bind source is the per-run directory, and the
static directory is neither bound nor even stat'ed — so a brokered run
does not touch it. But the static socket path stays the NAME source for
every run (`MYSBX_EGRESS_SOCKET` is parsed for its file-name component and
its exact-shape rule), and a build WITHOUT the broker pin serves the whole
channel from the static unit, which remains the documented fallback state
(`egress.md` E6 step 1).

So the removal is a smaller, narrower decision than "both tiers must pass":
it needs a host that always pins the broker (the Nix module already
defaults `egressBroker ? true`), plus the name-source rewrite. It is not
blocked on any further live evidence.

## 2026-11-14, nested sandbox — supervisor environment (bd myconfig-q8zg)

Provenance: executed in the dev sandbox that authored the fix, NOT on a
host with the LiteLLM forwarder. This entry records what that environment
could and could not prove, and is explicit about the gap.

### What was measured

`network = "none"` and `network = "shared"` on `bubblewrap`, driven
through the real binary with a stub backend that dumps
`/proc/$$/environ` (the supervisor's own environment — the thing a
payload reads as `/proc/1/environ`):

| run | supervisor env entries | canaries present (`HF_TOKEN`, `OPENAI_API_KEY`, `DISPLAY`, `SSH_AUTH_SOCK`) |
| --- | --- | --- |
| `network = "none"` | 0 | none |
| `network = "shared"` | 0 | none |

Two separate measurements back that up, and they are different things:

- **mysbx, pre-fix** (the `#[ignore]`d test of commit `346de0cb8f`, plus a
  hand-run `mysbx --detach` with a stub backend): the supervisor reported
  the full invoking environment — 13 entries in the cli-suite run, the
  canaries among them. That is the mysbx measurement, and it is a FAIL.
- **a bare `bwrap` control**, with no mysbx in the loop, showing the
  mechanism the bead described:

```
HF_TOKEN=CANARY bwrap --clearenv --unshare-all … -- cat /proc/1/environ
  -> 74 entries, HF_TOKEN=CANARY present
env -i PATH=/usr/bin bwrap --clearenv --unshare-all … -- cat /proc/1/environ
  -> 1 entry (PATH), the inherited HF_TOKEN gone
```

The `0` in the table's `network = "shared"` row needs one caveat: that
probe ran with `env -i` and set no `TERM`/`LANG`, so the forwarded set
contributed nothing. It shows the supervisor is empty, not that a
forwarded name is absent from a run that had one — the crate test
`a_detached_run_forwards_the_payload_environment_like_the_foreground`
covers that pair explicitly.

`--clearenv` governs the PAYLOAD; the supervisor keeps whatever the
spawn handed it. The fix clears the spawn.

### The gap: `proxy-only` was not re-run live here

The egress channel is pinned to `/run/mysbx-egress/litellm.sock` by
design (D20), and this nested sandbox has no `/run` at all, so a real
`proxy-only` run refuses with the documented exit `70` naming the
forwarder (`MYSBX_EGRESS_SOCKET = /run/mysbx-egress/litellm.sock`). That
refusal is the same one an operator sees on a host without the
forwarder, not a defect of this change.

The supervisor-environment property does not depend on the network
profile: the environment is cleared at the spawn, before any channel
exists, which is why the `none` and `shared` rows above are valid
evidence for it. What remains un-executed is the combination
"`proxy-only` AND `/proc/1/environ clean AND relay reachable" in one
run — the AC's "re-validated live" item. It needs a host carrying the
LiteLLM forwarder; the scripted probe in this file's header already
covers the channel half, and the supervisor half is now asserted by the
crate's `run_env_never_reaches_the_backend_supervisor` test.

## 2026-11-15, nested sandbox — the CONNECT pairing, unit and integration level (bd myconfig-ew9s)

Provenance: executed in the offline dev sandbox that implemented the
pairing (commits `821ccd39f6`…`53a6c680b3`), NOT on a host with the
LiteLLM forwarder. This entry follows the q8zg precedent: it records
what this environment could and could not prove, and is explicit
about the gap.

### What was measured

**The broker's CONNECT lane, against the real binary.** The
`egress_broker` integration suite (19 tests, 2 `#[ignore]`d probe
children) drives `mysbx-egress-broker` end to end over its AF_UNIX
socket: a `CONNECT api.openai.com:443` against a table row is admitted
(`allow`) and relayed; against an empty table or a non-matching name it
is refused (`deny`, 403, the reason naming the rule); an address-class
target (loopback, link-local, unspecified, multicast) is refused as
`unreachable`, never `deny` — the admission and transport vocabularies
stay separate. A bad table (URL forms, whitespace, port 0) makes the
broker exit `70` BEFORE its socket exists. The model lane is asserted
byte-equivalent: non-CONNECT bytes reach the LiteLLM row unchanged.

**The rotation reasoning, under real bubblewrap 0.13.1.** The
`#[ignore]`d probe child runs inside `bwrap --unshare-all` (the store
bound read-only, the probe binary bound at its own path): `IP_TRANSPARENT`
and `TCP_REPAIR` are `EPERM` at setsockopt time; `IP_FREEBIND` is
allowed and a `bind(192.0.2.77)` on it succeeds — but every `connect`
fails (`ENETUNREACH` inside the fence; `EADDRNOTAVAIL` for the plain
bind). The broker's pid is invisible in the child's `/proc` (pid
namespace). This pins E11's primitives: the empty netns and the
absent admitted socket, not a blanket capability claim.

**The gates and argv, through the real CLI binary.** Seven new cli
tests cover every refusal path of the pairing and the audit: keys +
shared refused with the empty-netns reason; keys + profile without a
broker pin refused naming the pin; the paired dry run emitting the
broker argv with one `--allow-domain` row per entry plus the port
filter, AND the backend argv injecting `HTTP(S)_PROXY`/`NO_PROXY`
pointing at the relay AFTER `[env]`, with a configured override
dropped from `[env]` and marked
`[config, ignored — set by the proxy]` in the report; the plain
profile run byte-identical (no table flags, no proxy variables);
`listen-ports`-only and URL forms refused under the pairing rules;
nono + profile + keys still refused.

### The gap: no end-to-end CONNECT round trip here

This sandbox is offline at the OS level (the host's own route table
is empty), and it carries no LiteLLM forwarder, so a live
`CONNECT github.com:443` that resolves, dials and relays bytes to a
real upstream cannot be executed here. That remained the
HUMAN/host step, the same gap the phase-B entry records for the model
lane: run the pairing on a host with an allowlisted upstream
reachable, and confirm (a) the `allow` + `tunnel` lines in the
sidecar log for an admitted CONNECT, (b) the `deny` line with the
rule name for a non-admitted one, (c) `mysbx diagnose egress` counting
both. The unit and integration layers below that round trip — request
parsing, table validation, admission, address classes, the fence —
are what the suites above assert. **Closed by the 2026-11-16 entry
below, which executed exactly that round trip in a networked
sandbox.**

## 2026-11-16, nested sandbox with network — the CONNECT round trip, end to end (bd myconfig-ew9s)

Provenance: executed in a mysbx-nested sandbox (kernel 6.12.117,
bubblewrap 0.13.1 from nixpkgs, uid 0) whose OS has a live default
route and working DNS — the environment the 2026-11-15 entry lacked.
The build under test is the crate at commit `5d5a9fc9d9` (the ew9s
implementation tip: the CONNECT lane, the E10 fence, the E13
reservation) built with `cargo build` (unoptimized), driven both
directly and through the real `mysbx run --` under bwrap. The host
LiteLLM forwarder was stood in by a local TCP listener answering
`HTTP/1.1 200` on the pinned loopback port; no model request rode the
model lane beyond that fixture. This entry records the round trip the
previous entry named as its gap, plus the probes the offline host
made unattributable.

### §1 the CONNECT round trip against real upstreams — PASS

Directly against the built broker, over its AF_UNIX socket, with a
real DNS resolver and a live route:

- `CONNECT github.com:443` against the table
  (`--allow-domain github.com --allow-domain example.com
  --allow-connect-port 443 --allow-connect-port 80`) answered
  `HTTP/1.1 200 Connection Established`; a full TLS 1.3 handshake
  followed through the tunnel with `server_hostname = github.com` and
  a verified certificate (CN `github.com`, chain to the system
  trust store), and a `HEAD /` over it answered `HTTP/1.1 200 OK`.
  The log carried the designed event pair: `allow` naming
  `CONNECT github.com:443`, then `tunnel` naming the pinned resolved
  address `github.com:443 -> 140.82.121.4:443`.
- The deny paths, same table: `evil.example:443` → 403, `deny` with
  `host evil.example is not in the allowed domains (github.com,
  example.com)`; `github.com:22` → 403, `deny` with
  `port 22 is not in the allowed connect-ports`; an IP literal
  `140.82.121.4:443` → 403, `deny` naming
  `the CONNECT lane's policy is the NAME`; `EXAMPLE.com:443` admitted
  case-insensitively and tunneled to the resolved address.
- A ports-only table (`--allow-connect-port 443 --allow-connect-port
  8080`, no domains): an IP literal is ADMITTED by the port rule, and
  the refused address classes answered `502` with `unreachable`
  events naming the class — `127.0.0.1:443`, `127.0.0.2:443`, the
  IPv4-mapped `::ffff:127.0.0.1:443` and the metadata address
  `169.254.169.254:443` all refused, never `deny`, never a tunnel;
  `example.com:443` admitted and tunneled with real TLS bytes
  relayed; `github.com:22` → 403 policy deny.
- The model lane on the same broker: a plain
  `GET /v1/models` reached the upstream listener unchanged
  (`HTTP/1.1 200 OK`) — the two lanes coexist on one socket.
- The fail-closed gates, live: a domains-only table made the broker
  exit 70 before its socket existed (E10's corrected rule), and a
  bad row (URL form, whitespace, port 0) did the same — matching the
  suite's assertions on this kernel.

### §2 the full stack through `mysbx run --` under bwrap — PASS

A real repo with a sidecar config
(`backend = "bubblewrap"`, `network = "proxy-only"`,
`allow-domains = ["github.com", "example.com"]`,
`connect-ports = [443, 80]`) and the egress pins
(`MYSBX_EGRESS_{SOCKET,PORT,RELAY,ETC,BROKER}`) pointing at the built
binaries, run with bwrap 0.13.1 as the backend:

- The paired run's payload (`curl`, driven by the injected
  `HTTPS_PROXY`/`HTTP_PROXY`/`NO_PROXY` the pairing sets after
  `[env]`) fetched `https://github.com/` and got
  `HTTP/1.1 200`, exit 0 — through the relay's loopback listener, the
  broker's CONNECT lane, a real DNS resolution, a real dial and a
  verified TLS exchange.
- A second run fetching `https://example.com/`: 200 as well.
- The deny side of the stack: a run fetching `https://www.wikipedia.org/`
  got curl's `CONNECT tunnel failed, response 403`; the sidecar log
carries `deny` naming the rule (`host www.wikipedia.org is not in
the allowed domains`), and `mysbx diagnose egress` reports it —
item (c) of the 2026-11-15 gap. One combined run fetched both
sides in one broker session: `https://github.com/` → 200 through
the tunnel, `https://www.wikipedia.org/` → 403, and `mysbx diagnose
egress` read the single sidecar log back as
`decisions: 4 (1 admissions, 1 denials, 0 unreachable, 0 io,
0 fatal)`, with `--json` agreeing — allow and deny counted in one
run, exactly the (a)+(b)+(c) triple the gap named.
- The exec-mode lifecycle held: each `run --` leaves its per-run
`$XDG_RUNTIME_DIR/mysbx-egress/<pid>` directory behind (exec cannot
clean up; PDEATHSIG + the next run's sweep own it — egress.md E4),
the broker process was dead after the run, and the NEXT run swept the
gone pid's directory, exactly the documented model.

### §3 the fence and the rotation reasoning, attributed — PASS

The 2026-11-15 entry's attribution caveat (its host had an empty
route table, so `--share-net` failures were unattributable) resolves
here, on a host with a live route:

- Inside `bwrap --unshare-all` the route table is empty and a direct
  connect to an IP literal (`http://140.82.121.4/`, no DNS involved)
  fails with exit 7 (connect failed); the SAME connect under
  `--share-net` answers 301 from the same address. The fence, not
  the offline host, was the blocker — now attributed.
- The advisory-proxy harm, live: under `--share-net` with
  `HTTPS_PROXY` set, `curl --noproxy '*'` walks past the proxy and
  connects (301) — the demonstration that a shared-network proxy is
  advisory, which is the reason the pairing refuses `--share-net`.
- The `#[ignore]`d fence tests ran live:
  `the_fence_allows_the_pinned_port_and_refuses_every_other` passed
  (allowed port: CONNECTED, forbidden port: REFUSED, EPERM — Landlock
  ABI 4+ on this kernel); the rotation probe child under bwrap
  printed `IP_FREEBIND: ALLOWED`, a successful `bind(192.0.2.77)`, and
  `freebind-connect: FAILED` / `plain-connect: FAILED`.
  One environment deviation, recorded honestly: this nested sandbox
  runs as uid 0 with full capabilities, so `IP_TRANSPARENT` and
  `TCP_REPAIR` were ALLOWED inside the user namespace (the offline
  dev sandbox ran uid 1000 and saw EPERM); the probe's assertions on
  those two spellings fail HERE for that reason, while the decisive
  claims — the spoofed-source connect never leaves the netns even
  WITH capabilities, and no routed socket exists to re-aim — held.
  On a real host mysbx runs as the user, so the EPERM lines hold
  there; this entry's stronger form (caps present, connect still
  dead) is the E11 claim under a harder condition.

### §4 the suites on this kernel — PASS, with the known environmental failures

Full `cargo test` on the ew9s tip (elevated TMPDIR and target dir so
the git-metadata and bwrap-bind tests can run; bwrap NOT on the
suite's PATH, so the argv goldens keep their literal `bwrap`
spelling): 405 + 0 + 24 + 7 + 215 + 415 = 1066 tests, 3 failures —
`doctor_routes_the_krun_backend_away_from_the_podman_checks` and
`doctor_routes_the_microvm_qemu_backend_to_its_own_checks` (this
sandbox has a writable `/dev/kvm`, so doctor's VM checks pass here and
fail the tests' degraded-environment expectations) and
`merge_passes_the_strategy_through` (this sandbox's git rejects
`--squash --no-ff` together). All three are the same
known-environmental set the 2026-11-15 entry recorded, none in an
egress path, and `cargo fmt --check` was clean.

### Limits of this entry

- The upstream on the model lane was a local fixture, not a real
  LiteLLM forwarder with a model behind it; an agent's actual model
  request through the paired run remains the f13 operator step it
  always was.
- The payload was curl, not `pi`/`opencode`; nothing here exercises
  an agent toolchain's proxy behavior beyond the variables the
  pairing injects.
- The sandbox ran as uid 0 (a nested-mysbx artifact); a
  capability-dropping payload was not directly represented — only
  probed from inside, see §3's deviation note.
