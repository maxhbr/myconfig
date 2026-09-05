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

- additional host paths mounted into the sandbox (`[[mounts]]`), with mode
- the backend and its resource limits
- network policy (`network = false` is the deny switch; the network is
  shared by default)
- environment forwarded into the sandbox (`[env]`)

It does not decide the repo itself: the repo is implicit and always mounted
read-write (see D13).

### D6: What the user config decides

Host-wide defaults, in particular which host (agent) config files are
exposed inside the sandbox — credentials and tool configuration that belong
to the user, not to a repo. Keeping them here means they are declared once,
and a per-repo sidecar cannot silently widen them (see D7).

On myconfig hosts the user config is not hand-written: the NixOS module
(`../../default.nix`) generates `~/.config/mysbx/config.toml` from the
`myconfig.ai.mysbx.config` option, with a read-only baseline of grants for
the host tool config this repo manages (`~/.config/{git,ripgrep,bat,fish}`).
Other modules extend it by appending to `myconfig.ai.mysbx.config.mounts`.
Outside myconfig the file stays an ordinary hand-written file; mysbx itself
knows nothing about where it came from.

### D7: Sidecar may narrow, not widen

A sidecar can drop or restrict what the user config grants, but cannot
grant access the user config does not allow. Rationale: a repo-adjacent file
must not be able to pull more of the host into the sandbox than the user has
approved host-wide.

When several user-config grants cover the same path, the **deepest** (most
specific) grant decides the allowed mode: a narrow `ro` grant beside a
broad `rw` one cannot be upgraded through the broad one. Mode may equal the
granted mode or downgrade `rw` → `ro`, never upgrade.

For `[env]` the rule is asymmetric: a sidecar may introduce variables the
user config never mentions (an invented variable is a value the repo
already controls), but may not override a variable the user config sets.

When the user config is absent it grants nothing: every sidecar
`[[mounts]]` entry is an error telling the user to grant the path in the
user config first. There is no implicit allow-all.

Open question: how a repo requests additional access — a one-off flag, or an
explicit allow-list entry in the user config keyed by repo path.

### D8: Paths are absolute and resolved eagerly

Every path in the configuration is absolute and is canonicalized when the
config is loaded, before the backend starts. Broken paths fail fast with a
clear error instead of producing a sandbox with a silently missing mount.

### D9: A strong accident barrier, a moderate malice barrier

The MVP's base is deliberately permissive: the network is shared by default
and the backend base exposes the usual tool environment. The claim is
therefore **not** deny-by-default confinement — it is the one the sandboxing
ladder already makes for its bubblewrap tier: *a strong accident barrier and
a moderate malice barrier*. A sandboxed process cannot stumble into host
state it should not touch, and an attacker inside the sandbox does not
trivially escape — but the shared network and the permissive base are real
exposure, and the doc says so instead of overstating the confinement.

What does hold in every backend, without exception: **nothing from the host
filesystem is available unless it is declared** — the repo itself (D13) and
the explicit `[[mounts]]` entries. New backends must uphold this even when
the backend's own default is permissive.

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

### D13: The repo is implicit

The repo is the repo the sidecar belongs to. It is always mounted
read-write, at its real host path, inside the sandbox — and it is not
expressible in configuration: the schema has no repository table (a
`repo` key at top level is an unknown key and therefore a schema error,
D11).

Rationale: the sidecar is named after the repo (`<repo>.mysbx/`, D2), so
the repo path is already fixed by where the sidecar sits. A config that
could name a different repo would create a contradiction class between the
sidecar's location and its content — two sources of truth for "which
checkout is this sandbox for", of which only one is visible in the
filesystem layout. Making the repo inexpressible removes that class
entirely; the only way to point `mysbx` at another checkout is to stand in
it (cli.md D1).

## Non-goals

- No global registry of sandboxes; the filesystem layout *is* the registry.
- No config-file includes or inheritance chains beyond the two layers in D1.
