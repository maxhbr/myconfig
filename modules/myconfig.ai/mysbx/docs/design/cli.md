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

Open question: what bare `mysbx` does when no sidecar exists — fail with a
hint to run `mysbx init`, or init implicitly?

### D3: Verb subcommands, no nesting

Subcommands are single verbs (`init`, `run`, `version`, `help`). No nested
command trees. Rationale: the surface is small and stays memorable; nesting
would only pay off with many more commands.

Currently implemented: `init`, `version`, `help`.
Planned: entering the sandbox (see D2) and `run COMMAND` for non-interactive
use.

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

## Non-goals

- No daemon, no background state beyond the sidecar directory.
- No interactive prompts; `mysbx` must be usable from scripts and from
  agents.
