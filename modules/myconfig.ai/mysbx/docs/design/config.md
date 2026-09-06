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
  (each written absolute, `~/…`, or relative to the sidecar's own
  directory — see D8)
- which external git metadata directories may be bound (`git-dirs`,
  see D13): the approval list for the targets a repo's `.git` FILE
  points at
- the backend and its resource limits
- network policy (`network = false` is the deny switch; the network is
  shared by default)
- environment forwarded into the sandbox (`[env]`)

It does not decide the repo itself: the repo is implicit and always mounted
read-write (see D13).

### D6: What the user config decides

Host-wide defaults, in particular which host (agent) config files are
exposed inside the sandbox — credentials and tool configuration that belong
to the user, not to a repo. It may also pre-approve external git metadata
host-wide (`git-dirs`, D13) — e.g. a checkout root under which every
worktree's metadata is acceptable. Keeping them here means they are declared once,
and a per-repo sidecar cannot silently widen them (see D7).

On myconfig hosts the user config is not hand-written: the NixOS module
(`../../default.nix`) generates `~/.config/mysbx/config.toml` from the
`myconfig.ai.mysbx.config` option, with a read-only baseline of grants for
the host tool config this repo manages, written with the `~/` prefix
(`~/.config/{git,ripgrep,bat,fish}`) so the generated file needs no
home-directory lookup at build time — mysbx expands it at run time (D8).
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

`git-dirs` (D13) is the one deliberate exception to "narrow, not
widen": a sidecar entry approves external git metadata the user config
never mentioned. The rationale is that the approval is *per repo* by
nature — every worktree points at a different metadata directory, so a
host-wide list could only be a coarse checkout root — and that the
sidecar is not repo-controlled: it lives outside the repo (D2) and is
never mounted into the sandbox, so the payload cannot write it. What
the exception does NOT do is let the repository approve itself: the
`.git` pointer inside the repo grants nothing, an implicit init records
nothing, and turning a discovered directory into an approval is an
explicit `mysbx init` (D12) that prints every path it records.

When the user config is absent it grants nothing: every sidecar
`[[mounts]]` entry is an error telling the user to grant the path in the
user config first. There is no implicit allow-all.

Open question: how a repo requests additional access — a one-off flag, or an
explicit allow-list entry in the user config keyed by repo path.

### D8: Paths are resolved eagerly, to absolute paths

A `[[mounts]]` host path may be written in three ways:

- **absolute** — taken as written;
- **`~/…`** — expanded against the invoking user's `$HOME`. Only the `~/`
  prefix is supported: `~` alone and `~user/…` are schema errors, because
  "another user's home" is a passwd lookup this tool deliberately does not
  do;
- **relative** — resolved against the directory of *the config file that
  declared it*. The same string therefore means `~/.config/mysbx/state`
  in the user config and `<repo>.mysbx/state` in a sidecar config; a
  sidecar path never resolves against the user config's directory.

`..` is allowed in every form: canonicalization resolves it, and the D7
grant check then compares canonicalized absolute paths on both sides — so
no `~/…` or `../…` spelling can slip a path past a grant.

Resolution and canonicalization happen when the config is loaded, before
the merge and before the backend starts. Broken paths fail fast with a
clear error — naming the file, the path *as written* and, when it
differs, the resolved path — instead of producing a sandbox with a
silently missing mount.

Parsing itself stays string-level and knows nothing about `$HOME` or the
file it is reading (`mysbx-rs/src/config.rs` stores the path verbatim);
`mysbx-rs/src/merge.rs` owns the resolution, because it is the only place
that knows both. Resolution runs on the **host**, before the backend is
executed, so the sandbox's own (cleared) environment never influences it.

`dest` is the exception: it stays absolute-only. It names a path in the
sandbox's filesystem view, where there is no host home to expand and no
config file to be relative to, and it is never canonicalized against the
host.

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
filesystem is available unless it is declared** — the repo itself (D13),
the git metadata directories its `.git` file points at when the repo is a
linked worktree or submodule (D13: they are part of the repo's own git
data, discovered with it and shown in the report), and the explicit
`[[mounts]]` entries. New backends must uphold this even when the
backend's own default is permissive.

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

When the repo root carries a `.git` FILE (linked worktree, submodule),
the git metadata that file points at is part of the repo's own data:
the gitdir and, when a `commondir` file names one, the common dir are
discovered with the repo, listed in the report, and are inexpressible
in configuration: a mount that would cover them is refused like one
that covers the repo root.

**The pointer itself grants nothing (review-2 item 1).** The `.git`
file lives *inside* the repo, so it is content the sandbox can rewrite
(D3) — treating it as a mount specification would let a repository
name any host directory and have it bound read-write (`gitdir: /`,
`gitdir: $HOME`). The metadata is therefore bound only when all of
these hold:

- the target resolves to a directory that *looks* like git metadata
  (`HEAD` and `refs/`); anything else is ignored, and `git` inside the
  sandbox gives the authoritative error,
- it is neither `/`, nor the home directory, nor an ancestor of either,
  nor a directory containing the repo root — refused at repo
  resolution, hard, because no configuration can make those safe,
- it is not related to a protected sandbox path (the base table of
  `plan.md`): a git dir under `/tmp`, say, would land inside the
  sandbox's own tmpfs,
- and it is **approved**: at or below an entry of the `git-dirs` list
  of a *trusted* layer — the user config or the sidecar, both of which
  live outside the repo and are unreachable from inside the sandbox
  (D2).

An **explicit** `mysbx init` records what it discovered into the
`git-dirs` list of the fresh sidecar config and prints every recorded
path, so the common worktree/submodule case works without hand-editing
while the trust decision stays an operator action, written to a file
the repository cannot rewrite. The *implicit* init of the bare form and
of `run` (cli.md D2) deliberately records nothing: a first run in a
freshly cloned repository must not turn that repository's own pointer
into an approval. A `.git` file edited *later* points somewhere
unapproved and is refused, with the offending path named.

The sidecar itself is never approvable as git metadata, in either
direction (a pointer at it, or at anything containing or inside it):
binding it would hand the payload the file that decides what may be
bound at all.

## Non-goals

- No global registry of sandboxes; the filesystem layout *is* the registry.
- No config-file includes or inheritance chains beyond the two layers in D1.

### D14: The sandbox has its own `$HOME`, and it is infrastructure

Inside the sandbox `HOME` is `/mysbx-home`, a fresh, empty, writable
tmpfs created with the other base mounts. The host home directory is
still **not** mounted, and the host's `HOME` *value* is never forwarded
(it is not in the forwarded list, plan.md "Environment").

Rationale, in the order the constraints bite:

- **Something must be there.** With `--clearenv` and no `HOME`, `cd ~`
  fails with `bash: cd: HOME not set`, and git, shells and editors that
  derive paths from `$HOME` fail or write to `/`. An unset `HOME` is not
  a confinement property, it is a broken sandbox.
- **Not the host home.** Mounting it would hand the payload `~/.ssh`,
  `~/.aws` and every agent credential in one bind — the exact thing the
  base table refuses. Exposing *parts* of the host home stays what it
  was: an explicit `[[mounts]]` grant of the user layer (D6), which the
  sidecar may only narrow (D7). Such a mount may point its `dest` into
  `/mysbx-home` to seed dotfiles (`~/.gitconfig`); the tmpfs is created
  before the configured mounts, so they land on top of it.
- **Not the repo root.** `HOME = <repo>` would make every tool that
  writes to `~` (shell history, caches, `.gitconfig` edits, agent state)
  pollute the checkout, and would make `~` and the work tree
  indistinguishable to the payload.
- **Not under `/home`.** A path such as `/home/<user>` inside the
  sandbox would mirror a host path that is deliberately absent; a payload
  (or a reviewer of `--dry-run`) could not tell the two apart. The
  literal invariant "no `/home/` anywhere in the argv" is worth keeping
  checkable, so the sandbox home is namespaced instead: `/mysbx-home`.
- **Ephemeral.** A tmpfs dies with the sandbox. Persisting the sandbox
  home is a phase-2 question (the sidecar has room for state, D10); it is
  not decided here.

**`HOME` and `PATH` are not configurable.** Both name paths the argv
builder itself created — the tmpfs above and the shipped tool closure —
so a layer that repointed them would break the sandbox rather than
configure it. Both are therefore emitted *after* `[env]`, and bubblewrap
lets the later `--setenv` win: an `[env] HOME` (or `PATH`) entry parses
and appears in `--dry-run`, but never reaches the payload. `--verbose`
marks such an entry `[config, ignored — set by mysbx]` rather than
pretending it applies. This is not an error, on purpose: rejecting it
would turn a harmless (often inherited) config into a hard failure of
every run, and the report already says what happens.
