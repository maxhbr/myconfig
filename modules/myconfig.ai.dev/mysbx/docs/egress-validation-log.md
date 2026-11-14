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
  and the decision log's next line is `unreachable` rather than `allow`;
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
