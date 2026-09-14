# Design: the `mysbx` CLI

Status: draft. This file is the authoritative place for CLI design
decisions. Implementation lives in `mysbx-rs/src/` (`lib.rs` dispatcher,
`usage.txt` help text).

## Scope

`mysbx` starts a sandbox for the repository in the current working
directory. The CLI is the only user-facing interface; everything it needs
beyond the command line comes from the configuration (see
[config.md](./config.md)).

## Decisions

### D1: The repository is the unit of work

`mysbx` is always invoked from inside a repository checkout. There is no
`--repo` flag in the base design: the current working directory selects the
repository, its sidecar and its configuration. This keeps the common case
(`cd repo && mysbx`) free of arguments.

### D2: Bare `mysbx` enters a sandbox

Running `mysbx` with no arguments is the primary action: enter an
interactive sandbox shell for the current repository. Subcommands are the
exception, not the rule.

**Superseded, first half.** The bare form used to create the sidecar
implicitly when it was missing, so that `cd repo && mysbx` worked on
the very first run. It no longer does: initialization is explicit
(D13). A bare `mysbx` in a repository that has no sidecar config fails
and names `mysbx init`.

A sidecar config that exists WITHOUT git-dir approvals — written by
`mysbx edit` (D12), by hand, or by an `init` that ran before the
checkout became a linked worktree — can take that trust decision after
the fact with `mysbx init --approve-git-dirs` (review-3 item 5): it
adds the discovered-but-unapproved git metadata directories to the
existing config — idempotently, and never rewriting anything but the
`git-dirs` list. Note the boundary: it is additive, not reverting — a
later run of the flag re-approves an entry an operator had removed (it
is still discovered); plain `init` never touches the config at all.

### D3: Verb subcommands, no nesting

Subcommands are single verbs (`init`, `run`, `edit`, `version`, `help`). No
nested command trees — two exceptions: the `session` noun group
(`session list`, `session destroy`, `session hunk`), a closed set defined by
[workspace.md](./workspace.md) D7, and the `worktree` noun group
(`worktree list`, `worktree diff`, `worktree hunk`), a closed set defined by
[worktree.md](./worktree.md) W1. Rationale: the surface is small and
stays memorable; nesting would only pay off with many more commands —
and the two noun groups are the places where a noun carries its own
meaning (“destroy the session”, not “destroy the repo”; “diff the
worktree”, not “diff the repo”).

Currently implemented: `init`, `edit` (D12), `version`, `help`, the bare
form (entering the sandbox, see D2), `run -- COMMAND` for
non-interactive use, `run --result -- COMMAND` (D17) for the
machine-readable-outcome form, `gui [ARG...]` (D15), the handoff verbs
`fetch NAME` / `merge NAME` / `push NAME [REMOTE]` / `diff NAME`
([workspace.md](./workspace.md) D6 — the host-side git plumbing between
the host repo and a session's clone; no sandbox is started, the session
is the one positional and `--dry-run` prints the git commands), the
session noun group `session list` / `session destroy NAME [--force]` /
`session hunk NAME` ([workspace.md](./workspace.md) D7 — the first D3
exception above), the
session noun group's worktree sibling `worktree list` /
`worktree diff NAME` / `worktree hunk NAME`
([worktree.md](./worktree.md) W1 — the second D3 exception: read-only
inspection of the host's workmux worktrees in `<repo>__worktrees`,
host-side, no sandbox started), the
global flag `--session <name>` ([workspace.md](./workspace.md) D1-D5 —
the clone run mode), plus the global flags `--dry-run` (D9),
`--verbose` (D10), `--result`/`--timeout` (D17) and `--multiplexer`
(D14).

### D4: `--` separates sandbox args from the payload command

Everything after `--` is passed verbatim to the process started inside the
sandbox and is never parsed by `mysbx`.

### D5: Hand-rolled argument parsing, zero dependencies

The crate has no dependencies (see `mysbx-rs/Cargo.toml`). The parser is
hand-written and the help text is a literal file (`src/usage.txt`) included
at compile time. Rationale: the CLI is small and stable, the help output
stays exactly as written, and the Nix build needs no `outputHashes` in
`cargoLock`.

Consequence: every new flag must be added to both the parser and
`usage.txt`; there is no derive macro keeping them in sync. Tests guard the
pairing.

### D6: Flags override config, config overrides defaults

Precedence, highest first:

1. command-line flags
2. sidecar `config.toml`
3. user config (`$XDG_CONFIG_HOME/mysbx/config.toml`)
4. built-in defaults

See [config.md](./config.md) for the layers themselves.

### D7: Backend selection is explicit, not magic

The sandbox technology (bubblewrap, podman+gVisor, qemu, microvm) is chosen
by configuration or an explicit flag, never auto-detected from the host.
Rationale: a silently downgraded isolation level is a security bug.

### D8: Exit codes

**Extended (bd myconfig-0ql) — the full set, per the
agent-microvm precedent (`0/1/124/130/70` + JSON result), recorded
here before the unattended mode lands rather than after**
(feature-comparison.md §6):

| code | meaning |
| --- | --- |
| `0` | success — the payload exited `0` (a `--result` run: state `completed`) |
| `1` | the payload failed — its own non-zero exit code under the exec passthrough, or the `failed` state of a `--result` run |
| `2` | usage error — the command line is wrong |
| `70` | **infrastructure error** — mysbx's own runtime failure: the command line was fine but the run it named could not happen (no sidecar, unparsable config, no backend, backend cannot be started) |
| `124` | a `--result` run exhausted its `--timeout` budget (state `timed-out`) |
| `130`/`143` | a `--result` run was cancelled — SIGINT/SIGTERM arrived while mysbx waited (state `cancelled`; the shell's `128 + signum`) |

The boundary between `2` and `70` is *what is wrong*: `2` means the
command line is wrong, `70` means the command line was fine but the
world it named is not. "No sidecar yet", "no backend configured" and
"this directory is `$HOME`" are all the latter — the argv is exactly
what the operator meant.

**Why `70` and not the old `1`.** `1` is the one code a payload
owns — it is the `failed` state of a `--result` run, and under the exec
passthrough a payload's own `1` lands on the terminal
indistinguishable from the tool's. A batch driver (and an operator)
must be able to tell "the payload failed" from "the tool failed
before the payload could": `70` is agent-microvm's
`infrastructure-error`, adopted verbatim for the same reason.

When `mysbx` runs a payload command, the payload's exit code is propagated
unchanged — the run ends in an `exec` that replaces this process, so
mysbx cannot get in the way even if it wanted to; `mysbx`'s own failures
are reported on stderr with a `mysbx: ` prefix so they are distinguishable from payload output. The one exception is
`run --result` (D17), which waits instead of exec'ing and therefore
interprets.

### D9: Output conventions

Diagnostics go to stderr and are prefixed `mysbx: `. Progress/result lines
for `init` go to stdout and are prefixed `## ` (see the README transcript).
Nothing else is written to stdout, so the tool stays pipe-friendly — the
`--result` pointer line (`mysbx: result: <path>`, D17) goes to stderr for
exactly that reason.

The one deliberate exception is the `--dry-run` argv: it is printed to
stdout **unprefixed, one argument per line**, the backend executable
(argv[0], the pinned `MYSBX_BWRAP` value) first — the executable is part
of what `--dry-run` audits (review-1 finding 7). It is a *result*,
not a diagnostic. Golden tests compare it byte for byte, and
`mysbx run --dry-run -- ls | wc -l` is meaningful.

### D10: `--verbose` prints a `## `-prefixed run report before the run

`--verbose` is a global flag with the same position rule as `--dry-run`:
it is accepted before the subcommand (`mysbx --verbose`), and `run`
accepts it again before its `--` (`mysbx run --verbose -- CMD`). Both
global flags may be combined and may appear in any order, but neither may
be repeated (a repeated flag is a typo, not an intensifier: usage error,
D8). After `--` the token is payload, verbatim, and never a flag (D4).

**What it shows.** The resolved repo root and sidecar path (and whether
the sidecar directory exists), both configuration file paths with whether
each was loaded or absent (an absent file is an empty layer), the merged
backend, the network sense (`shared` / `denied`), every mount in
declaration order — the implicit repo bind first (config.md D13), then the
git metadata directories its `.git` file points at (config.md D13), then
the configured mounts with mode, source, in-sandbox destination and the
layer that contributed it — the forwarded host variables and the `[env]`
variables, the effective `MYSBX_BWRAP` / `MYSBX_SHELL` / `MYSBX_TOOLS_PATH` /
`MYSBX_NIX_CONF` (the sanitized nix configuration, or `(none)`) /
`MYSBX_BINSH` (the sandbox's `/bin/sh`, or `(none)`) /
`MYSBX_CA_BUNDLE` (the pinned CA bundle the TLS env variables point
at, or `(none)`) values after
their fallbacks, the payload, and whether the run will exec or stop at
the argv.

The report cannot say *how* the repo was resolved (sidecar ancestor / git
root / cwd): `repo::resolve` does not return that, and restructuring it
just for a report line is not worth it. It shows the resolved paths
instead.

**Where it goes: stdout, `## `-prefixed, before everything else.** Not
stderr: this is operator-facing information about a run, not an error, and
stderr belongs to the `mysbx: ` failures (D8/D9) and to the payload. The
`## ` prefix is what makes this safe next to the unprefixed `--dry-run`
argv block: with `--verbose --dry-run` the report comes first and the argv
follows, so `mysbx --verbose --dry-run | grep -v '^## '` is byte-identical
to a plain `mysbx --dry-run`. Tests pin that. The report is also printed
before `exec` in a real run, so the operator sees the configuration even
when the payload takes over the terminal — and before the WAIT of a
`--result` run (D17), whose `mode:` line says "waiting for the payload,
the outcome is recorded in result.json" instead of "executing", so the
report never claims a handoff mode the run does not use.

**What is hidden: nothing.** In particular, `[env]` values are printed
verbatim rather than redacted. `--dry-run` already prints them as
`--setenv KEY VALUE`, and anyone who can run `mysbx --verbose` can read
both config files — redaction would buy no secrecy while making the report
lie about the run. The help text says out loud that the values may be
secrets, so nobody pastes a verbose report into a bug tracker unaware.

### D11: the multiplexer session replaces the interactive payload only

With `multiplexer = "tmux" | "workmux" | "herdr" | "aoe" | "orca"` in a
configuration layer (config.md D17 — the generalization of the boolean
`workmux` of D16), the **bare form** does not start a shell: its
payload is the entry pinned for that value
(`MYSBX_MUX_ENTRY_<VALUE>`), a script from mysbx's own closure that
starts the multiplexer on sandbox-internal state and attaches to it.
Everything else about the run is unchanged — same base, same mounts,
same `--chdir`; the payload line of the argv is the only difference,
plus the `TMUX_TMPDIR` variable that names the private socket
directory. `multiplexer = "none"` (and an omitted key) is the plain
interactive shell.

**`run -- CMD` is untouched, deliberately.** A one-shot command that
was wrapped in a multiplexer would write its output into a pane nobody
attaches to, and its exit code would become the multiplexer's, not the
payload's (D8). So the `run` argv is *byte-identical* to the
`multiplexer = "none"` one — no payload swap, no `TMUX_TMPDIR`, and
none of the socket guards (they guard a session this run does not
start). Tests pin the byte identity in both directions, for every
value.

**A missing pin is a refused run, not a silent shell.** A value whose
entry this build did not pin (an unwrapped build, a host that does not
install that multiplexer) exits `70` with a `mysbx: ` message naming the
value and the missing variable. Falling back to a bare shell would be
discovered only after the work happened outside the session it was
supposed to happen in. It is refused while the argv is built, so
`--dry-run` refuses it too — the failure is a configuration error, not
an exec failure inside the sandbox.

`--dry-run` stays side-effect-free with a multiplexer as with
everything else: it prints the entry as the payload and creates no
socket directory — the entry itself is what creates it, inside the
sandbox. `--verbose` reports the selected multiplexer, the socket path
and the entry, so the isolation claim of config.md D17 is checkable
against the argv.

### D12: `mysbx edit` opens the sidecar config in `$EDITOR`

`mysbx edit` opens `<repo>.mysbx/config.toml` in the editor named by
`$EDITOR` (or `$VISUAL`, when `EDITOR` is unset or empty) and takes no
arguments. It creates the file first when it is missing — the same
commented template `init` writes, without the git-dir approvals
(D12/config.md D13) — so the operator always edits a documented file
instead of writing one from memory.

**Why the sidecar and not the user config.** The sidecar is the file a
person is expected to edit by hand: it is the per-repo policy, it lives
outside the repo (config.md D2) and the sandbox cannot write it. The
host-wide user config is generated on myconfig hosts — a symlink into
the immutable `/nix/store` — so an editor pointed at it either fails or
replaces the symlink and silently detaches the layer from Home Manager.
Editing that layer means editing `myconfig.ai.mysbx.config` and
rebuilding. A `--user` flag would have to know the difference between
those two worlds; the verb stays about the file mysbx itself owns.

`edit` is, next to `init`, the second command that may create the
sidecar config — deliberately: writing that file is its whole purpose,
and it is an explicit command (D13). It approves no git dirs, unlike
`init` on a fresh config (config.md D13).

**No editor guess.** With neither variable set the command fails
(exit `70`, `mysbx: ` message naming both). Falling back to `vi` would
open an editor the operator did not choose on a policy file, with no
hint that the variable is unset.

**The value is split on whitespace, not shell-evaluated.**
`EDITOR="code --wait"` and `EDITOR="nvim -u NONE"` work; quoting and
shell metacharacters do not. Running the value through a shell would
make `$EDITOR` a code-execution surface of every `mysbx edit` — the
same reason configuration carries no hooks (config.md D4). A value that
needs more than an argument list can be a wrapper script.

The editor is `exec`d, so it owns the terminal and its exit code
propagates unchanged (D8). It is resolved *before* the sidecar is
created: a run that cannot edit must not leave a sidecar behind as its
only effect. The global flags are not valid with `edit` (there is
nothing to dry-run and no run to report on).

### D13: Initialization is explicit — a run never creates the sidecar

Supersedes the second half of D2. A sandbox run — bare `mysbx`,
`mysbx run -- CMD`, with or without `--dry-run` — requires the sidecar
config `<repo>.mysbx/config.toml` to exist already. When it does not,
the run fails (exit `70`, D8) with a `mysbx: ` message naming the
missing path and the command that creates it:

```text
mysbx: this repository has no sandbox yet:
  /path/to/the/repo.mysbx/config.toml does not exist — run `mysbx init`
  in /path/to/the/repo to create it
```

`mysbx init` (D12/config.md D12) is unchanged and stays the way to
create it; `mysbx edit` creates it too, because writing that file is
what it is for. Both are explicit commands the operator typed for that
purpose.

**Why the implicit init went away.**

- *No silent writes from a command that reads like a read-only-ish
  one.* Bare `mysbx` is what a user types to look at a repository
  inside a sandbox. Creating a host directory and a policy file as a
  side effect of that is a surprise, and it happens in the place the
  user is least likely to be looking: **outside** the repository, next
  to it (config.md D2).
- *Discoverability of the sidecar.* An implicit init made the sidecar
  appear without ever being mentioned. Operators learned about
  `<repo>.mysbx/` when they noticed the sibling directory in `git
  status`' parent, or not at all. `mysbx init` printing `## created:`
  lines is the moment the concept is introduced — and the refusal
  names the path, so even the failure teaches the layout.
- *`--dry-run` honesty.* `--dry-run` promised side-effect-freeness, so
  it could not do the implicit init — which made the *dry* run
  configured differently from the real one it claimed to preview: a
  missing sidecar counted as an empty layer for the dry run and as a
  freshly created one for the real run. With explicit init both forms
  see the same two layers, and `--dry-run` fails in exactly the cases
  the real run would. (This is a deliberate change of the earlier
  documented dry-run behaviour "missing sidecar = empty layer, never
  created": the user config alone no longer defines a run.)
- *One decision per file.* The sidecar config is policy. Policy that
  appears by itself is policy nobody chose — and the trust decisions
  it carries (git-dir approvals, config.md D13) were already refused to
  the implicit init for the same reason.

**What is required is the config FILE, not the directory.** A bare
`<repo>.mysbx/` (a leftover `state/` tree, a hand-made directory) is
not a policy; running with an empty layer instead would hide the fact
that nothing was configured. `lib.rs::require_initialized_sidecar` is
the single gate of both run forms.

**Ordering.** The repo guard (mvp-2: `$HOME`, a directory containing
`$HOME`, `/`) runs *before* this check, so a run inside the home
directory is still diagnosed as the home exposure it is and never
invites the operator to `mysbx init` a tree that must not be bound at
all.

### D14: `--multiplexer <mux>` overrides the configured multiplexer for one run

The bare form accepts `--multiplexer <mux>` with the same position rule
as the other global flags (D10: before the verb; here there is no verb —
the flag belongs to the bare form alone). The value is the same closed
enum as the config key (D17): `tmux` | `workmux` | `herdr` | `aoe` |
`orca` | `none`. It wins over the merged `multiplexer` of both layers for THIS
invocation only, per the precedence of D6 (flags > sidecar > user >
defaults) — nothing is written, and the next `mysbx` runs whatever the
configuration says again.

**Why a flag at all.** The multiplexer of a host is a declarative
default (the module option generates the user layer); the exception is
an operator decision — "this one run wants herdr instead of the
configured workmux", or "this one run wants a bare shell" — and an
exception that must not edit a file to happen. Editing the sidecar to
flip one run would leave the repo's *policy* changed for every later
run, and editing the user layer would change every *repo* of the host.

**`none` is a real value.** `--multiplexer none` forces the plain
interactive shell on a host that configured a session — the payload
swap is skipped entirely, no `TMUX_TMPDIR`, none of the socket guards.

**The same refusal, not a weaker one.** A value this build pinned no
entry for is refused exactly like a config layer selecting it (D11/D17):
exit `70` with the `mysbx: ` message naming the value and the missing
variable — never a silent plain shell. The flag grants no access a
configuration would not have: it selects a payload from mysbx's own
closure either way.

**No verb accepts it.** `run -- CMD` never starts a session (D11), so
`--multiplexer` is a usage error there — accept-and-ignore would let an
operator believe the one-shot ran inside a session it did not. `init`,
`edit`, `version` and `help` reject it with the same words. A repeated
flag, a missing value or an unknown spelling is a usage error (`2`,
D5/D8), and the error names the accepted set.

### D15: `mysbx gui` opens a terminal window with an interactive run in it

`mysbx gui ARG...` starts the terminal emulator (alacritty, pinned by the
Nix wrapper as `MYSBX_TERMINAL` from `myconfig.ai.mysbx.terminal.package`)
with `--working-directory` on the current directory and `--command` on
this same `mysbx` — by absolute path, `current_exe`, never a PATH lookup —
with the whole argument tail passed through verbatim. `mysbx gui ARG1
ARG2` therefore becomes `mysbx ARG1 ARG2` in the window.

**`gui` is not a run.** Nothing of the sandbox pipeline happens in the
outer invocation: no repo resolution, no sidecar guard, no merge, no
backend check. The inner `mysbx` is the run — it reports its own errors
(`no sandbox yet, run mysbx init`, a refused multiplexer) in the window
it opens, which is where an operator wants them. Consequence: `gui` also
works in a repo that has no sidecar, and creates nothing (D13 holds for
the outer form; the inner run is what a D13 refusal would hit).

**The tail is never parsed.** Everything after the verb is the inner
invocation's command line (the same rule `--` gives `run`, D4) — flags
the inner `mysbx` understands (`mysbx gui --multiplexer herdr`),
subcommands (`mysbx gui run -- ls`), anything. Only the inner process
parses it, so only it can reject it.

**No global flag before the verb.** The argv the outer invocation builds
is the terminal's, not the sandbox's: `--dry-run` before `gui` has
nothing to print, `--verbose` no run to report on, and `--multiplexer`
would be a flag the dispatcher parsed half of (D14). All three are usage
errors (`2`) — after the verb they belong to the inner run and pass
through.

**The terminal is pinned, not looked up.** `MYSBX_TERMINAL` is an
absolute store path under Nix (the same wrapper idiom as `MYSBX_BWRAP`);
the `alacritty` fallback serves a plain `cargo run`, like `bwrap` does.
`--working-directory` and `--command` are alacritty's own options — the
one place the crate knowingly names another program's command line. A
terminal that cannot be started is a runtime failure (`1`) naming it.

**The outer invocation does not stay around.** `mysbx gui` detaches
itself: it forks, the half that started the window becomes a session
leader (`setsid`, so it holds no controlling terminal), ignores `SIGHUP`
(so closing the terminal the command was typed in does not take the
window with it), points its stdin/stdout/stderr at `/dev/null` and waits
for the terminal — the exact lifetime and insulation `mysbx gui &
disown` gives, without the operator having to type it. The parent
returns as soon as the window was started, so the shell prompt comes
back immediately. Exactly one failure stays synchronous: a terminal
that cannot be started at all is a runtime failure (`1`) naming it.
One that starts and fails afterwards (no Wayland socket, say) is
silent — nobody waits for its status, the same as the `& disown`
form.

### D16: `--ro <path>` / `--rw <path>` grant one host path for one run

`mysbx --ro <path>... --rw <path>...` (and the same flags after `run`)
binds each named host path into THIS run's sandbox — `--ro` read-only,
`--rw` read-write — at its own canonicalized host path, on top of every
configured mount. The flags are the interactive answer to "the sandbox
needs one more directory": no `mysbx edit`, no config file round trip,
no commit, and nothing persists — the next `mysbx` starts from the
configuration alone.

**Repeatable, run-scoped, position-typed.** Both flags take a value and
may be given any number of times, before the verb (the bare form) and
after it for `run` — the same position rule as `--multiplexer` (D14),
and the same refusal for every other verb (`2`, `is not valid with
...`). `gui` passes its tail verbatim (D15), so `mysbx gui --ro /data`
reaches the inner run. A missing value is a usage error (`2`); an empty
string or a lone `~` is refused at parse time for the same reason.

**One value, three spellings, resolved eagerly.** Each value gets the
exact D8 treatment a `[[mounts]]` path gets: `~/…` expands against
`$HOME`, a relative path resolves against the **cwd** (a config file
resolves against its own directory; the command line's "own directory"
is the one it was typed in), and the result is canonicalized on the
spot. A path that does not exist is a runtime failure (`1`) naming the
flag and the spelling — the run that named it is the honest diagnosis,
and bwrap would refuse it later anyway.

**The guards a config entry answers to apply unchanged.** The addition
is a `Mount` appended to the merged list, so the argv builder's own
checks fire for it: the protected-dest table (`/tmp`, `/run`,
`/nix/store`, ...), the writable-tree symlink-redirect refusal, the
policy-file-writable refusal (an `--rw` of a directory that covers a
trusted policy file steers the next run, exactly like a config entry
would), and the home-exposure refusal (review-3 item 4): no flag may
re-expose the host home, Equal or Contains.

**Applied last, grouped, `--rw` wins a tie.** The additions are appended
AFTER every configured mount, every `--ro` addition before every `--rw`
one, the values of each flag in the order they were given. Mount order
is argv order and a later bind wins, so the flags apply on top of
everything — the same precedence D6 gives every flag (flags > sidecar >
user > defaults) — and `--rw` beats `--ro` for the same path no matter
the typing order, the one predictable rule.

**The report says so.** The mounts of the command line are attributed
`[command line]` in the `--verbose` report (D10), a provenance distinct
from both config layers, so the report never claims a grant came from a
file the operator never edited.

### D17: `run --result -- CMD` records a machine-readable outcome

A one-shot run can be asked to *wait* instead of exec: `mysbx run
--result [--timeout <seconds>] -- CMD...` spawns the backend as a
child, waits for its outcome — bounded by the budget when given —
writes that outcome to `<repo>.mysbx/result.json` and exits by the
interpreted contract of D8 (`0` completed, `1` failed, `124`
timed out, `130`/`143` cancelled, `70` infrastructure error) instead
of passing the payload's own code through. Both flags follow the one
position rule of D10/D16 (before the verb and after it, `run` only);
`--timeout` without `--result` is a usage error — a plain run ends in
an exec, and no budget can apply to a process that is no longer
mysbx.

**Why a file in the sidecar and not JSON on stdout.** stdout is the
payload's (D9): a driver capturing the run's stdout must find exactly
what the payload printed — interleaving a result object into it would
either corrupt the payload's output contract or force the payload's
own stdout into the file with it. The sidecar is also the one place
the payload cannot write (config.md D7): a `result.json` found there
is a result mysbx wrote, which is what makes the file trustworthy
enough to drive automation. The pointer to it goes to stderr
(`mysbx: result: <path>`), where the `mysbx: ` diagnostics live.

**The file.** One `result.json` per repository, atomically REPLACED
by each waited run — the sidecar does not grow without bound and the
file always names the LATEST run; a driver wanting history copies it
after each run. A session run (workspace.md D5) writes
`<repo>.mysbx/clones/<name>.json` instead — one file per session, so
parallel sessions cannot overwrite each other's outcomes; the live
`result.json` is untouched by a session run. Fields: `version` (schema number, `1`), `state`
(`completed`/`failed`/`timed-out`/`cancelled`/`infrastructure-error`),
`exitCode` (the run's own status — always the state's code, the two
cannot disagree), `repo`, `sidecar`, `payload` (the command vector),
`startedAt`/`finishedAt` (ISO-8601 UTC), `durationMs`, and per state:
`payloadExitCode` (or `payloadSignal`, `SIG…` — never both) for a run
that ended on its own, `timeoutSec` when a budget was given,
`cancelledBy` (`SIGINT`/`SIGTERM`) for a cancellation, `error` for an
infrastructure failure. A killed run records NO payload fate: its
outcome is mysbx's kill, not the payload's own.

**Cancellation and timeout are mysbx's own answers, not the
payload's.** SIGINT/SIGTERM to a waiting mysbx kill the whole process
group (the backend and everything it spawned inside the sandbox),
the run is recorded as `cancelled` with the signal named, and the
exit code is the shell's `128 + signum`. The `--timeout` budget does
the same through `alarm(2)`, recorded as `timed-out`. Anything a
plain run does NOT record (a Ctrl-C mid-exec simply kills it) stays
unrecorded in a `--result` run too, for any signal other than
SIGINT/SIGTERM/SIGALRM.

**Earlier refusals write no file.** A run that fails BEFORE the
backend starts — no sidecar, a broken config, an argv that cannot be
laid out — writes nothing: there was no run to record, and the
`mysbx: ` diagnosis on stderr (with exit `70`) is the outcome. Only
a failure of the exec/wait boundary itself is recorded, as
`infrastructure-error` — the run happened, it failed at the edge, and
the driver polling the file learns of it there.

## Non-goals

- No daemon, no background state beyond the sidecar directory.
- No interactive prompts; `mysbx` must be usable from scripts and from
  agents.
