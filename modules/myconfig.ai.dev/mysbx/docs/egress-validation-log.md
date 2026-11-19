# mysbx egress live-validation log

> **Scope / current status:** the dated entries below are host-run evidence
> for the configuration named in each entry, not acceptance of every later
> backend or policy. The enforcing backends today are `bubblewrap`, the
> layered `nono` backend, `podman-gvisor`, `microvm-qemu` and `krun`; every
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

## 2026-11-19, offline dev sandbox — the public lane, unit and integration level (bd myconfig-fvi, fvi.1+fvi.2)

Provenance: executed in the offline dev sandbox that implemented the
lane (commits `1135cf45b1`, `7e504a636d`). This entry follows the
q8zg precedent: it records what this environment could and could not
prove. The live entry (a real CONNECT to a public target through a
`--public` broker on a networked host, and a denied private /
metadata target) needs a networked host — same gap the 2026-11-15
entry had before the 2026-11-16 networked entry closed it.

### §1 the deny set at unit level — PASS

The broker's `private_or_special_use` and the metadata-name admission
are pinned by the bin's tests
(`the_public_lane_refuses_the_private_and_special_use_ranges`,
`the_public_lane_admits_every_target_but_the_metadata_names`):
RFC1918, CGNAT 100.64/10, 240/4, loopback, unspecified, link-local,
multicast; ULA fc00::/7; the IPv4-mapped and NAT64-embedded spellings
decode to their embedded v4 and admit-or-deny by it
(`nat64_embedded_v4`), the rest of 64:ff9b:1::/48 refused. These are
pure address-class functions — no network needed, so this is the
same level of proof the offline sandbox can give for the table
lane's own rules.

### §2 admission and event honesty — PASS

The integration suite drives a real `--public` broker through a real
socket: `the_public_lane_denies_the_metadata_names_with_a_403`
(policy `deny`, 403, the name denied at admission),
`the_public_lane_listen_line_names_the_deny_set_rule` (the fence
summary names the rule instead of ports — the lane carries no
Landlock port fence, E14),
`a_mixed_public_lane_argv_is_a_usage_error` (`--public` and the
table flags are mutually exclusive, exit 2). The resolved-address
dial path (`resolve_connect_host`: resolve one address, dial THAT
address) is the table lane's shared helper — no DNS rebinding
window exists at admission time by construction.

### §3 the pipeline and argv shape — PASS

The dry-run audit of a `network = "public"` run on bubblewrap is
pinned by the CLI test (`public_is_parsed_and_reports_the_broker_lane`):
no `--share-net`, the broker argv carries `--public` and no table
flag, `MYSBX_NETWORK=public`, the CONNECT proxy variables injected;
the argv golden `egress-public.txt` pins the same shape byte-for-byte.
The refusals are pinned too: allowlist-under-public, every
non-bubblewrap backend, the nono builder's `PublicRefused` defense
in depth. The merge order (a wider sidecar is the same
`NetworkUpgrade` hard error) is pinned by
`public_sits_in_the_merge_order_and_still_cannot_widen_around_it`.

### Limits of this entry

- No end-to-end CONNECT round trip: this sandbox's host has no
  network, so a public target cannot be dialed here. The 2026-11-16
  entry's shape (a networked host closing exactly this gap for the
  table lane) is the template for the follow-up.
- The metadata-name denial is proven by the trusted-resolver
  admission path, not against a real cloud metadata service.
- The VM transports are out of scope by design (bd
  myconfig-nakf): every non-bubblewrap backend refuses the value,
  and for qemu/ch the refusal is permanent — the candidate
  mechanisms cannot express the lane.

## 2026-11-19, offline dev sandbox (kernel 6.12.117) — podman-gvisor's channel, live runsc probes (bd myconfig-eov2)

Provenance: run inside the dev sandbox (no podman installed, so the
probes drive `runsc` directly — the same binary the backend invokes
through podman's `--runtime-flag`; the podman layer itself is pinned
by the argv goldens). runsc is the flake's pinned build
`gvisor 20260817.0` (nix/gvisor-overlay.nix, rev `b1b5614`),
platform systrap, rootless (`--ignore-cgroups`; the module sets the
same flag on hosts without a delegated cgroup). Probes 2-4 use a
hand-built OCI bundle (rootfs: bash + the crate's own
`mysbx-egress-relay`), because the production shape is exactly
`--network none` + the broker-dir bind + `--host-uds=open`.

### §1 the production shape round-trips — PASS

```
runsc --host-uds=open --network=none run --bundle <b> <id>
  probe 1: dial 127.0.0.1:9            -> Connection refused (empty netns, no route)
  probe 2: relay --listen 127.0.0.1:4000 --connect /run/mysbx-egress/litellm.sock
           payload reads /dev/tcp/127.0.0.1/4000
                                        -> RELAY-ROUND-TRIP-REPLY: BOUND-SOCKET-REPLY
                                           (a host-side UDS server behind the bind
                                            answered through the relay), exit 0
```

So the C5 design is live-proven: a `--network none` gVisor sandbox
has NO route out, and the single bound host socket reaches the
payload's loopback client through the in-image relay.

### §2 the host-uds gate is real — PASS

```
runsc --host-uds=none --network=none, same bundle:
  relay -> /run/mysbx-egress/litellm.sock:
     cannot reach ...: Connection refused (os error 111)
```

The bind alone is not the fence: `--host-uds=none` (the default!)
refuses connect(2) to the socket inode even though the gofer serves
the directory. The channel exists only because the backend appends
`--runtime-flag host-uds=open` (pinned in the argv golden).

### §3 the reachable set is the declared binds, not the host — PASS

```
--host-uds=open, TWO binds (hostsock -> /run/mysbx-egress, otherbind -> /run/other):
  relay -> /run/other/other.sock      -> OTHER-BIND-REPLY (second bind's socket answers)
  relay -> /run/other/MISSING.sock    -> ENOENT, connection dropped, relay stays up
  relay -> /run/no-such-mount/x.sock  -> ENOENT (no bind covers it)
```

`--host-uds=open` does NOT open the host's sockets wholesale: outside
the mounts the pathnodes do not exist for the sentry at all. The
fence is the *combination* — default-deny netns + gofer-reachable set
(= the binds) + the host-uds gate — which is exactly the claim the
design doc makes for the backend's socket audit (the recursive
no-symlink-follow audit of every bind source before spawn).

### Limits of this entry

- podman itself was not driven (not installed in this sandbox): the
  `--network none`, `--runtime-flag host-uds=open`, `-v <brokerdir>:/run/mysbx-egress:ro`
  argv shape is pinned by the golden `podman-egress-proxy-only.txt`
  and the CLI tests, and the runsc layer below it is what these
  probes exercise.
- The relay probe used a stand-in UDS server, not the host's LiteLLM
  forwarder (offline sandbox); the bubblewrap entries above prove the
  broker+relay protocol, this entry proves the gVisor transport.
- `runsc do` (test-only, whole host mounted ro) showed the same gate
  (`none` -> refused, `open` -> connects), confirming the flag's
  semantics independent of the bundle shape.


## 2026-11-19 · microvm-qemu restricted NIC, live KVM probe (bd myconfig-krab, n8kv.2 C4)

Host: the offline dev sandbox (kernel 6.12.117, /dev/kvm rw, rootless
user, no networked upstreams). Stack: the `nix/mysbx.nix`-wrapped
`mysbx` with the real microvm-qemu pins — launcher
(`nix/microvm-qemu-launcher.nix`, its own wrapper carrying
`MYSBX_MICROVM_QEMU_EGRESS_RELAY` from the same crate closure as the
bwrap channel's `MYSBX_EGRESS_RELAY`), the shared `mysbx-krun-rootfs`,
the pinned 6.12.117 bzImage, qemu 11.1.2 — against a broker pinning
`litellm.sock` at `127.0.0.1:4000`. The "upstream" is a stand-in echo
server on host port 4000 (started in the background and verified from
the host before every probe — the sandbox has no LiteLLM); the guest
payload is busybox over the shared init. The scratch repo's sidecar
sets `backend = "microvm-qemu"`, `network = "proxy-only"`.

The launcher's netdev (pinned by the `egress-proxy-only.txt` argv
golden and the launcher test):
`-netdev user,id=net0,restrict=on,guestfwd=tcp:10.0.2.2:4000-cmd:<relay> --guestfwd <broker sock>`.

### §1 the restricted guest NIC — PASS

```
guest: ip addr show eth0      -> inet 10.0.2.15/24 scope global eth0
guest: ip route | grep default -> NOTHING (no default route)
guest: cat /etc/resolv.conf   -> "# no nameserver: restricted (mysbx proxy-only)"
```

The init's restricted arm did its three jobs: the address stays
(10.0.2.2 is on-link), the default route is gone, the resolver is
empty — every non-forwarded dial fails at connect, not at resolve.

### §2 the guestfwd channel round-trips to the broker — PASS

```
guest: echo MODEL-PROBE | nc -w 5 10.0.2.2 4000
  -> UPSTREAM-REPLY: MODEL-PROBE          (nc rc=0)
broker decisions.jsonl:
  listen ... /tmp/.../mysbx-egress/<pid>/litellm.sock -> 127.0.0.1:4000, confined by Landlock to that one port
  allow  ... pid <relay> uid 0 -> 127.0.0.1:4000
```

The FULL chain in one round trip: guest -> SLiRP guestfwd (the only
mapped address:port) -> qemu's libslirp execs the crate's
`mysbx-egress-relay --guestfwd <socket>` per connection -> the relay
connects the per-run broker's Unix socket from the launcher's bwrap
view (the ro broker-dir bind) -> the broker's model lane dials its
Landlock-confined upstream -> the stand-in echo answered. The broker
LOGGED the admission — the decision log is the host-side record of a
guest's model traffic, exactly as on bubblewrap.

### §3 the deny side: forwards-only reach — PASS

```
guest: nc 10.0.2.2 80     -> rc=1 (connect refused: restrict=on drops every unmapped outbound SYN)
guest: nc 1.1.1.1 80      -> rc=1 (no default route; nothing to forward)
guest: nc -u 1.1.1.1 53   -> rc=1
guest: nslookup example.com 1.1.1.1 -> "Network unreachable"
```

SLiRP's `restrict=on` semantics (the fvi.3 libslirp source audit:
tcp_input.c drops every outbound SYN whose addr:port is not a guestfwd
target; udp.c bails before any socket work) hold live: the ONE mapped
port answers, every other TCP and all outbound UDP fail. The
forwards-only net IS the profile — no second policy layer needed.

### Notes

- The kernel panic after the payload's clean exit
  ("Attempted to kill init", `exitcode=0x00000000`) is the KNOWN
  PID-1-exit noise the exit channel exists for (backends.md D9):
  the launcher read the payload's code from `mysbx-exit` and reported
  `payload exited with 0`; the panic is the guest kernel observing
  its init exiting after reaping the payload.
- The probe used the wrapped mysbx with real store-pinned binaries;
  an unwrapped run of the same crate had `PATH=/usr/bin` inside the
  guest and could not exec `sh` — the wrapper pins are load-bearing
  for any live probe (recorded so a future probe does not re-derive
  it).
- The stand-in upstream replaces LiteLLM (offline host); the
  bubblewrap entries above prove the broker's model-lane protocol
  against the real stack, this entry proves the qemu transport.

## 2026-11-19 · krun explicit vsock, live KVM probe (bd myconfig-ild7, n8kv.2 C3)

Host: the offline dev sandbox, KVM present. Guest: libkrunfw 6.12.91,
`backend = "krun"`, `network = "proxy-only"`, host-store mode. The
channel: `krun_disable_implicit_vsock` → `krun_add_vsock(0)` →
`krun_add_vsock_port(4000, <per-run broker socket>)` (pinned libkrun
1.19.7, symbols verified with `nm -D` against the stock and withBlk
builds), the baked static `mysbx-vsock-bridge` in the guest rootfs,
started by the init under `MYSBX_KRUN_NET=restricted`. A python
echo upstream on host `127.0.0.1:4000` stands in for LiteLLM.

### §1 The guest has no net device at all

`ip link` shows only `lo` (up — the init's header brings it up) and
the kernel's own `dummy0` (down, no driver); `ip route` is empty;
there is no `/etc/resolv.conf` — `MYSBX_VM_RESOLV` rides only a
shared run, so a lookup falls through to nothing and fails. The
bridge is listening on `127.0.0.1:4000` (guest netstat).

### §2 The one mapped port answers end to end

`nc 127.0.0.1 4000` → `UPSTREAM-REPLY: MODEL-PROBE` (rc 0): the
guest bridge → AF_VSOCK CID 2:4000 → the VMM muxer's `UnixProxy` →
the broker's socket → the Landlock-confined upstream. The broker's
`decisions.jsonl` records the `listen` (the per-run socket → the
one port, Landlock-confined) and the `allow` (the connection
through the muxer's own process). No host relay binary exists —
the VMM itself is the relay, the design difference from C4's
guestfwd.

### §3 Everything else fails

- A second loopback port (`127.0.0.1:80`): refused instantly (rc 1).
- An outside address (`1.1.1.1:80`): refused instantly (rc 1) — no
  net device exists, no TSI (`krun_add_vsock(0)`: no `tsi_hijack` on
  the guest cmdline, verified at the pinned libkrun source before
  the run).
- An UNMAPPED vsock port (a second bridge on 4001, connecting
  `VSOCK-CONNECT:2:4001`): times out — the muxer drops requests for
  ports with no map entry, no data path is created; the mapped
  port still answers afterwards.
- DNS (`nslookup api.openai.com`): instant failure (no resolver
  file, nothing listens on the guest's loopback :53).

### Notes

- The first probe attempt failed with `connect vsock: Invalid
  argument`: the locally-spelled `struct sockaddr_vm` missed the
  trailing `svm_flags` byte, making the sockaddr 12 bytes where the
  guest kernel's `vsock_addr_cast` requires 16 — connect(2) EINVAL
  before the CID is even read. Fixed in `krun-vsock-bridge.c`
  (recorded so a future relay author does not re-derive it).
- The inherited outer-sandbox `MYSBX_VM_STORE_DISK=1` pin attached
  the store disk and pushed the IRQ census to 12 of 11 slots with
  piped stdio — the launcher's census refused the run by name
  before any libkrun call (the bd myconfig-rn4e guard working as
  designed); unsetting the inherited markers fixed the probe.
- The stand-in upstream replaces LiteLLM (offline host); the
  bubblewrap entries prove the broker's model-lane protocol against
  the real stack, this entry proves the krun transport.
