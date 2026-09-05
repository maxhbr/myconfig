# Design: configuration and the sidecar directory

Status: draft. This file is the authoritative place for configuration
design decisions. See [cli.md](./cli.md) for the command-line surface.

## Vocabulary

- **repo** — the repository checkout `mysbx` is invoked from,
  `/path/to/the/repo`.
- **sidecar** — the directory `/path/to/the/repo.mysbx/`, next to the repo,
  holding per-repo configuration and state.
- **user config** — `$XDG_CONFIG_HOME/mysbx/config.toml`, host-wide
  defaults.
- **backend** — the sandbox technology that actually confines the process
  (bubblewrap, podman+gVisor, qemu, microvm).

## Decisions

### D1: Two configuration layers plus flags

1. built-in defaults
2. user config — `$XDG_CONFIG_HOME/mysbx/config.toml`
3. sidecar config — `<repo>.mysbx/config.toml`
4. command-line flags

Later layers override earlier ones. No project-local config *inside* the
repo is read (see D3).

### D2: The sidecar lives outside the repo

The sidecar is a sibling directory `<repo>.mysbx/`, not `<repo>/.mysbx/`.

Rationale:

- It is not part of the repository, so it never has to be gitignored and
  never leaks into commits, diffs or clones.
- It is outside the sandbox's writable repo mount, so a compromised or
  confused workload inside the sandbox cannot rewrite its own confinement
  configuration.

Consequence: the sidecar path depends on the repo path; moving a checkout
orphans its sidecar. Accepted — the sidecar holds only reproducible
configuration and disposable state.

### D3: Configuration inside the repo is untrusted

Anything under the repo is content the sandbox can write. `mysbx` therefore
never reads sandbox configuration from inside the repo. If repo-provided
defaults are ever supported, they must be opt-in from the sidecar and
clearly marked untrusted.

### D4: TOML, declarative, no code

Configuration is TOML. It declares *what* is available in the sandbox, never
*how* to obtain it — no hooks, no shell snippets, no scripts. Rationale:
config that can execute is config that can escape.

### D5: What the sidecar config decides

- which repos/paths are mounted and with which mode; by default the repo
  itself is available read-write
- the backend and its resource limits
- network policy
- environment forwarded into the sandbox

### D6: What the user config decides

Host-wide defaults, in particular which host (agent) config files are
exposed inside the sandbox — credentials and tool configuration that belong
to the user, not to a repo. Keeping them here means they are declared once,
and a per-repo sidecar cannot silently widen them (see D7).

### D7: Sidecar may narrow, not widen

A sidecar can drop or restrict what the user config grants, but cannot
grant access the user config does not allow. Rationale: a repo-adjacent file
must not be able to pull more of the host into the sandbox than the user has
approved host-wide.

Open question: how a repo requests additional access — a one-off flag, or an
explicit allow-list entry in the user config keyed by repo path.

### D8: Paths are absolute and resolved eagerly

Every path in the configuration is absolute and is canonicalized when the
config is loaded, before the backend starts. Broken paths fail fast with a
clear error instead of producing a sandbox with a silently missing mount.

### D9: Default deny

Nothing is available inside the sandbox unless it is declared: no network,
no host paths besides the repo, no host environment. New backends must
uphold this even when the backend's own default is permissive.

### D10: The sidecar also holds state

Beside `config.toml`, the sidecar has room for backend state, caches and
mounts standing in for host directories (e.g. `~/.local/share`). State is
disposable: deleting the sidecar and re-running `mysbx init` must yield a
working setup again.

### D11: Strict parsing, hand-rolled TOML subset

The parser is hand-written (`mysbx-rs/src/toml.rs`), because the crate stays
zero-dependency (see cli.md D5). It covers key/value pairs, dotted and
quoted keys, tables, arrays of tables, basic and literal strings, integers,
floats, booleans, arrays and inline tables. Multi-line strings and datetimes
are rejected with a located error rather than misparsed.

Loading is strict (`mysbx-rs/src/config.rs`): unknown keys, wrong types,
unknown enum values (`mode` other than `ro`/`rw`) and missing required keys
are errors, never warnings. Rationale: a sandbox built from a
half-understood configuration has unknown confinement, which is the one
failure mode this tool must not have.

The example configurations in `mysbx-rs/tests/assets/` are part of the
specification: `valid/` shows what users may write, `invalid/` pins down
what must be rejected.

### D12: `mysbx init` is idempotent

`init` creates the sidecar and a default `config.toml`. Re-running it never
overwrites an existing `config.toml`; it reports what already exists.

## Non-goals

- No global registry of sandboxes; the filesystem layout *is* the registry.
- No config-file includes or inheritance chains beyond the two layers in D1.
