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

When no sidecar exists, bare `mysbx` creates it implicitly — exactly what
`mysbx init` would have done (D12: idempotent, never overwrites an existing
`config.toml`) — and then proceeds. The common case stays argument-free:
`cd repo && mysbx` works on the first run. An operator who wants to review
the generated `config.toml` first can still run `mysbx init` explicitly.

### D3: Verb subcommands, no nesting

Subcommands are single verbs (`init`, `run`, `version`, `help`). No nested
command trees. Rationale: the surface is small and stays memorable; nesting
would only pay off with many more commands.

Currently implemented: `init`, `version`, `help`, the bare form (entering
the sandbox, see D2) and `run -- COMMAND` for non-interactive use, plus the
global flags `--dry-run` (D9) and `--verbose` (D10).

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

- `0` success
- `1` runtime failure (cannot create the sidecar, backend failed to start)
- `2` usage error (unknown command, bad flag, unexpected argument)

When `mysbx` runs a payload command, the payload's exit code is propagated
unchanged; `mysbx`'s own failures are reported on stderr with a `mysbx: `
prefix so they are distinguishable from payload output.

### D9: Output conventions

Diagnostics go to stderr and are prefixed `mysbx: `. Progress/result lines
for `init` go to stdout and are prefixed `## ` (see the README transcript).
Nothing else is written to stdout, so the tool stays pipe-friendly.

The one deliberate exception is the `--dry-run` argv: it is printed to
stdout **unprefixed, one argument per line**, because it is a *result*,
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
variables, the effective `MYSBX_BWRAP` / `MYSBX_SHELL` / `MYSBX_TOOLS_PATH`
values after their fallbacks, the payload, and whether the run will exec
or stop at the argv.

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
when the payload takes over the terminal.

**What is hidden: nothing.** In particular, `[env]` values are printed
verbatim rather than redacted. `--dry-run` already prints them as
`--setenv KEY VALUE`, and anyone who can run `mysbx --verbose` can read
both config files — redaction would buy no secrecy while making the report
lie about the run. The help text says out loud that the values may be
secrets, so nobody pastes a verbose report into a bug tracker unaware.

## Non-goals

- No daemon, no background state beyond the sidecar directory.
- No interactive prompts; `mysbx` must be usable from scripts and from
  agents.
