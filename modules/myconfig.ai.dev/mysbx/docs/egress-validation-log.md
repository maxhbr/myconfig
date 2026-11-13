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
