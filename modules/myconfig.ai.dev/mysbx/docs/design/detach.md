<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: detached runs — a sandbox that keeps running without a terminal

Status: proposed (bd myconfig-7ioq; acceptance: myconfig-7ioq.1).
This is the decision record for running a mysbx sandbox in the
background and attaching a terminal to it later — the workflow aoe and
workmux give on the host, where the tmux server lives outside any
sandbox and a tmux detach just drops the client.

Vocabulary, on top of [`../../CONTEXT.md`](../../CONTEXT.md):

- **detached run** — an interactive run (the bare form, cli.md D2)
  whose terminal is held by a host-side `dtach` master instead of the
  terminal it was started from. It survives that terminal closing.
- **attach** — connect the current terminal to a detached run
  (`mysbx attach`). **detach** — drop that connection again (the
  detach key, `^\`); the run keeps going.
- **workspace** — as in [workspace.md](./workspace.md): the live repo,
  or one session `NAME`. A detached run is identified by its repo and
  its workspace.
- **runtime root** — `$XDG_RUNTIME_DIR/mysbx/`, where the sockets and
  state files of detached runs live.

## The problem

Today the sandbox lives exactly as long as the foreground process:

- `lib.rs` ends a run in `exec` of the backend (bubblewrap with
  `--die-with-parent`, `podman run --rm`, the krun launcher).
- The multiplexer entries (`nix/*-entry.nix`) end in
  `exec tmux attach` / `exec herdr` / `exec aoe`.
- So a multiplexer detach ends the client, the payload, the sandbox and
  with it the multiplexer server. Closing the terminal does the same
  through `SIGHUP`.
- The multiplexer sockets live in the sandbox home tmpfs
  (`/mysbx-home/.mysbx-tmux`, config.md D16/D17). The host cannot reach
  them, by design; on podman-gvisor and the krun backends a socket
  cannot cross the host/guest boundary at all (backends.md).

## Decisions

### B1: The terminal is held on the host, by `dtach`, outside the sandbox

A detached run is a normal run whose controlling terminal is a pty owned
by a `dtach` master process on the host. mysbx starts that master; the
sandbox is started inside it exactly as it is started from a terminal.

Why this layer, and not another:

- **Backend-independent.** The pty is outside the sandbox, so bubblewrap,
  nono, podman-gvisor, podman-krun and krun all behave the same. Nothing
  crosses the sandbox boundary that does not cross it already (the
  terminal).
- **Multiplexer-independent.** It works for every `multiplexer` value,
  including `none`: a plain shell becomes detachable too.
- **The D16/D17 socket isolation stays.** No multiplexer socket is
  exposed to the host.
- **`dtach`, not tmux/screen.** dtach has no prefix key, no windows and
  no scrollback, so it does not clash with a tmux, workmux, aoe or herdr
  inside. It takes an explicit socket path, so mysbx owns the location
  (B3). It reports exec failures of `-n` synchronously, ignores
  `SIGHUP`, points its stdio at `/dev/null` and unlinks its socket when
  the child exits (dtach 0.9 `master.c`). abduco is the alternative; it
  manages its own session directory and naming, which mysbx would have
  to work around.

Rejected:

- **Expose the inner multiplexer socket to the host** and attach with a
  host tmux client. Breaks D16/D17, needs matching tmux client and
  server versions, cannot work across the gVisor/krun boundary, and
  needs a different client per multiplexer (herdr, orca).
- **`podman run --detach` + `podman attach`.** Only covers the podman
  backends.

### B2: CLI surface — `--detach`, `--detachable`, `mysbx attach`

- **`mysbx --detach [run flags]`** starts the bare form detached and
  returns once the sandbox runs (B4). It prints the attach command on
  stderr: `mysbx: detached; attach with: mysbx attach [--session NAME]`.
  It works without a terminal on stdin, so scripts and agents can start
  one.
- **`mysbx --detachable [run flags]`** does the same, then attaches the
  current terminal immediately. This is the "start working now, detach
  later" form. It needs a terminal on stdin. It is the exact composition
  `mysbx --detach && mysbx attach`, done under one lock (B6).
- **`mysbx attach [--session NAME]`** connects the current terminal to
  the detached run of the live repo, or of session `NAME`. It is a
  host-side verb like `status`: the repo comes from the cwd (cli.md D1),
  no sandbox is started. It ends in
  `exec dtach -a <socket> -r winch -z`, so dtach owns the terminal and
  its exit code propagates (the `hunk` precedent, worktree.md W4). The
  detach key is dtach's default `^\`. `-z` sends `^Z` into the run
  instead of suspending the client.

Where the flags are valid:

| Form | `--detach` / `--detachable` |
| --- | --- |
| bare form, with or without `--session` | accepted |
| `run -- CMD` | refused, usage error `2`. A detached one-shot is the batch form of bd myconfig-dys; it composes with `--result` and must force `--session` (workspace.md D8). It is not decided here. |
| `gui` | refused, usage error `2`. `gui` already detaches its window (cli.md D15). |
| `init`, `edit`, `status`, `doctor`, the session/worktree/handoff verbs, `attach` | refused, usage error `2`, the shared rejection arm of the run-scoped flags |
| `--detach` together with `--detachable` | refused, usage error `2` |

The run-scoped flags (`--backend`, `--multiplexer`, `--ro`, `--rw`, …)
keep their meaning; they configure the detached run. `attach` accepts
only `--session NAME` and `--dry-run`.

A detached run is **not** an unattended run in the sense of
workspace.md D8: it is interactive and somebody attaches to it, so a
live-repo detached run is allowed, the same way a tmux session left
attached on an unwatched screen is.

### B3: One detached run per workspace, registered in the runtime root

The runtime root is `$XDG_RUNTIME_DIR/mysbx/`. Per repo it holds a
directory named by the same `fnv1a10` hash of the repo root the podman
container names use (`podman_gvisor.rs`):

```
$XDG_RUNTIME_DIR/mysbx/<fnv1a10(repo root)>/
  live.sock   live.json   live.lock
  s.NAME.sock s.NAME.json s.NAME.lock
```

- **The directory is the registry**, the `clones/` precedent of
  workspace.md D2. There is no index file.
- **One detached run per workspace.** A second `--detach` for the same
  workspace is refused (`70`) and names `mysbx attach`. Foreground runs
  are not registered and are not affected (but see B9 for podman).
- **Why not the sidecar.** Unix socket paths are limited to 107 bytes.
  A worktree sidecar plus a 64-character session name exceeds that;
  `/run/user/<uid>/mysbx/<10 hex>/s.<64>.sock` stays below it. A
  computed path that is still too long is refused (`70`), never
  shortened silently. The runtime root is also a tmpfs that disappears
  at logout, which matches the lifetime of a detached run.
- **No fallback.** An unset or relative `XDG_RUNTIME_DIR` is refused
  (`70`). `/tmp` is not used, for the reason `TMUX_TMPDIR` has no
  fallback (mux-entry-lib.nix `requireSocketDir`).
- **Permissions.** mysbx creates `mysbx/` and the per-repo directory
  `0700` and refuses to use them when they are not owned by the current
  uid or are group/world accessible.
- **Hash collisions.** The state file (B5) records the repo root. An
  entry whose recorded root differs from the resolved repo is refused
  (`70`), never attached to.

### B4: Pre-flight in the foreground, the run under dtach

`mysbx --detach` runs in two processes:

1. **The outer mysbx** (the one the operator typed) runs the full run
   pipeline up to the argv, without side effects — the same path
   `--dry-run` takes. Every refusal a foreground run would print
   (no sidecar D13, unparsable config, unpinned multiplexer D11,
   forbidden mounts, …) happens here, on the operator's terminal, with
   the D8 exit code. `--verbose` prints its report (D10) here.
2. The outer takes the workspace lock (B6), writes the state file with
   `state: "starting"` (B5) and runs
   `dtach -n <socket> -r winch <self> <args>` with its own cwd and
   environment. `<args>` are the original arguments without `--detach`,
   `--detachable` and `--verbose`. `MYSBX_DETACHED=<socket path>` marks
   the child.
3. **The inner mysbx** (the dtach child, on the dtach pty) runs the
   pipeline again, for real: session clone creation, waypipe client,
   git trust file, krun staging, then it spawns the backend. It checks
   that `MYSBX_DETACHED` equals the socket path it computes itself and
   refuses (`70`) otherwise, so the marker cannot redirect state files.
4. The outer waits until the state file leaves `starting` or the socket
   disappears, then reports: `running` → `0` and the attach hint (or,
   for `--detachable`, `exec dtach -a`); `failed` → the recorded error
   on stderr and `70`; socket gone while `starting` → "the detached run
   died during startup" and `70`. There is no timeout: a first
   `--session` run may clone a large repo, and a dead inner removes the
   socket.

The inner mysbx **waits** for the backend instead of `exec`'ing it (the
`RunMode` of `run --result`, cli.md D17), so that:

- it can record the exit code (B5) — nobody waits on a detached run
  otherwise;
- the per-run host state it owns (git trust file, krun stage and GC
  roots, waypipe client) is cleaned up after the run, which an exec
  cannot do;
- bubblewrap's `--die-with-parent` and the waypipe `PDEATHSIG` keep a
  parent that lives as long as the run.

Signals in the inner mysbx: `SIGINT`, `SIGQUIT` and `SIGTSTP` are
ignored. They come from the pty and belong to the payload, which is in
the same foreground process group. `SIGHUP` (the dtach master went
away) and `SIGTERM` are forwarded to the backend and the run is recorded
as `cancelled`.

The argv the inner builds is the interactive one: the payload always
has a terminal (the dtach pty), whatever the outer's stdin was. For
podman this means `--interactive --tty` always.

Environment: the detached run sees the environment of the shell that
started it, like a foreground run. Attaching from another shell does
not update it (`SSH_AUTH_SOCK`, `TERM`, `WAYLAND_DISPLAY`) — the same
limitation as tmux.

### B5: The state file records the run; the socket proves it is alive

`<workspace>.json`, written atomically (temp file + rename):

| field | meaning |
| --- | --- |
| `version` | `1` |
| `state` | `starting` → `running` → `exited` / `failed` / `cancelled` |
| `repo`, `sidecar`, `session` | the resolved repo, its sidecar, the session `NAME` or `null` |
| `backend`, `multiplexer` | the merged values the run uses |
| `startedAt`, `finishedAt` | ISO-8601 UTC, as in cli.md D17 |
| `exitCode` | the backend's exit code, for `exited` |
| `error` | the `mysbx: ` message of a `failed` startup |

Field names follow cli.md D17 where they mean the same thing.

**Liveness is a `connect(2)` to the socket**, never the state field: a
socket that accepts a connection is a live run. A state that says
`running` without a live socket means the dtach master was killed;
`status` and `attach` report it as `lost`.

Lifecycle: the outer writes `starting`, the inner writes `running`
after the backend was spawned and the final state after it ends. A
finished record stays until `attach` (which prints it once, e.g.
`mysbx: the detached run exited with 1 at <time>`, removes it and exits
`70`) or the next `--detach` for the same workspace replaces it.
`status` only reads.

The output of a payload that fails right after start is lost with the
dtach session; only its exit code remains. A foreground run or
`--detachable` shows it. This is the `gui` precedent (cli.md D15).

### B6: A per-workspace lock serializes the registry

`<workspace>.lock` is `flock`ed by `--detach`/`--detachable` from the
liveness probe until the handshake of B4 ends, and by `attach` while it
removes a finished record. Without it, two concurrent `--detach` runs
could both find a stale socket, and the second could unlink the first
one's fresh socket. dtach's own `bind(2)` is the last line of defense:
it fails on an existing path.

A stale socket (connection refused) is removed under the lock before a
new run starts.

### B7: The inner multiplexer survives its own detach

In a detached run, a multiplexer's own detach (`prefix d` in tmux,
quitting the aoe TUI, leaving the herdr client) still ends the entry
script and with it the sandbox. That is the trap B1 does not close by
itself.

The argv of a detached run therefore carries `MYSBX_DETACHABLE=1`, set
after `[env]` like `TMUX_TMPDIR`, so no configuration layer can set or
unset it. Foreground runs do not carry it; their argv stays byte-identical.

With the variable set, the entries do not `exec` their client. They
loop: attach; when the client returns and the server still has a
session, print one line
(`session still running — Enter: reattach, q: end the sandbox, ^\: detach mysbx`)
and wait; exit when the server has no session left or on `q`. The loop
is one shared fragment in `nix/mux-entry-lib.nix`, parameterized by the
attach command and the liveness probe. The prompt belongs to the
payload, not to the mysbx CLI, so the "no interactive prompts"
non-goal of cli.md still holds.

| multiplexer | attach | still alive |
| --- | --- | --- |
| `tmux`, `workmux` | `tmux -S "$socket" attach-session -t "=$session"` | `tmux -S "$socket" has-session -t "=$session"` |
| `aoe` | `aoe "$@"` | `tmux ls` on the `TMUX_TMPDIR` default socket has a session |
| `herdr` | `herdr "$@"` | the pre-started `herdr server` answers |
| `orca` | not applicable: the payload is the server itself, it has no client detach | — |
| `none` | not applicable: the shell's exit is the end of the run | — |

### B8: The sandbox never sees the runtime root

A process that can connect to a dtach socket can type into that
terminal. If a sandbox could reach `$XDG_RUNTIME_DIR/mysbx/`, it could
type into every other detached run of the user, including runs of other
repos with other grants. So:

- No base mount exposes it. Backends already pin `XDG_RUNTIME_DIR` to
  the sandbox home (`bwrap.rs`, `podman_gvisor.rs`).
- A configured mount, `--ro` or `--rw` whose source is the runtime
  root, below it or above it (`$XDG_RUNTIME_DIR`, `/run/user`, `/run`,
  `/`) is refused (`70`) for every run, detached or not: a foreground
  run could reach detached ones just the same. The refusal names the
  runtime root and this decision. The waypipe socket bind is a sibling
  (`$XDG_RUNTIME_DIR/<display>`), not covered.
- `MYSBX_DETACHED` is not forwarded into the sandbox.

### B9: Interactions with existing decisions

- **`--dry-run` (cli.md D9).** `mysbx --detach --dry-run` prints the
  dtach launcher argv (dtach first, one argument per line), a blank
  line, then the backend argv the inner run would spawn, including
  `MYSBX_DETACHABLE=1`. Nothing is created. `attach --dry-run` prints
  the dtach attach argv.
- **`status` (cli.md D19)** gains a `detached:` section after
  `worktrees:`: one line per entry with the workspace (`live` or the
  session name), the state (`running`, `lost`, or the finished state
  with its exit code), backend, multiplexer and start time — or
  `detached: none`.
- **`session destroy` (workspace.md D7)** refuses while the session has
  a live detached run, with or without `--force`: the clone is bound rw
  into a running sandbox. The message names `mysbx attach --session
  NAME`. `--force` keeps its meaning (unmerged work).
- **`doctor` (cli.md D20)** checks the `MYSBX_DTACH` pin and the runtime
  root (set, absolute, owned, `0700`) in a host section. Problems there
  are `WARN`, not `FAIL`: they disable detached runs, not runs.
- **podman backends (bd myconfig-cad).** The container name is per repo
  root and the argv carries `--replace`, so any later podman run in the
  same checkout kills a detached one. `--detach` on `podman-gvisor` and
  `podman-krun` is refused (`70`, naming the bead) until per-run
  container names land.
- **Parallel runs per backend.** A detached run plus a foreground run
  in one checkout is two parallel runs. Each backend is enabled for
  `--detach` only after that combination is validated live.
- **Non-goals of cli.md.** "No daemon, no background state beyond the
  sidecar directory" changes to: no mysbx daemon — the dtach master is
  per run and ends with it — and no background state beyond the
  sidecar and the runtime root.

### B10: `dtach` is pinned like every other tool

The wrapper (`nix/mysbx.nix`) pins `MYSBX_DTACH` to an absolute store
path, the `MYSBX_BWRAP` idiom. A plain `cargo run` falls back to `dtach`
on `PATH`. An unpinned, missing or non-executable dtach makes
`--detach`, `--detachable` and `attach` fail with `70` naming the
variable; other runs are unaffected.

## Implementation beads

Children of bd myconfig-7ioq, in dependency order: `.1` accept this
record; `.2` registry, state file, lock (B3, B5, B6); `.3` runtime-root
mount refusal (B8); `.4` `--detach` (B2, B4, B10); `.5` `attach` and
`--detachable`; `.6` inner-multiplexer reattach loop (B7); `.7` `status`,
`session destroy`, `doctor` (B9); `.8` live validation per backend,
blocked by myconfig-cad for podman.

## Open follow-ups

1. The batch form `run --result --detach --session NAME -- CMD`
   (bd myconfig-dys), on top of B3–B6.
2. A configurable detach key (dtach `-e`), only if `^\` conflicts in
   practice.
3. A config key that makes every bare run `--detachable`, only after
   the flag has been used for a while.
4. `gui --attach`: open a terminal window attached to a detached run.

## Non-goals

- No multiplexer socket on the host (B1).
- No more than one detached run per workspace (B3).
- No scrollback or output log of a detached run: the inner multiplexer
  has the scrollback, dtach has none.
- No detached `run -- CMD` before myconfig-dys decides it (B2).
