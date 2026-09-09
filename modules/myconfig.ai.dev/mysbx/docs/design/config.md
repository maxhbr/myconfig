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
- which sandbox-home subdirectories persist across runs
  (`state-dirs`, see D15): entries backed by the sidecar's `state/`
  tree
- the backend and its resource limits
- network policy (`network = false` is the deny switch; the network is
  shared by default)
- environment forwarded into the sandbox (`[env]`)
- which terminal multiplexer the interactive payload is
  (`multiplexer`, see D17) — the one key where the sidecar overrides
  the user config's value outright, because it grants no host access

It does not decide the repo itself: the repo is implicit and always mounted
read-write (see D13).

### D6: What the user config decides

Host-wide defaults, in particular which host (agent) config files are
exposed inside the sandbox — credentials and tool configuration that belong
to the user, not to a repo. It may also pre-approve external git metadata
host-wide (`git-dirs`, D13) — e.g. a checkout root under which every
worktree's metadata is acceptable. Keeping them here means they are declared once,
so they are there in every sandbox, whatever a sidecar says (see D7).

On myconfig hosts the user config is not hand-written: the NixOS module
(`../../default.nix`) generates `~/.config/mysbx/config.toml` from the
`myconfig.ai.mysbx.config` option, with a read-only baseline for the
host tool config this repo manages, written with the `~/` prefix
(`~/.config/{git,ripgrep,bat,fish}`) so the generated file needs no
home-directory lookup at build time — mysbx expands it at run time (D8).
Each entry carries an explicit `dest` under `/mysbx-home` (review-2
item 6): inside the sandbox `HOME` is `/mysbx-home`, so a config bound
at its host path would be invisible to the tools that want it. A NixOS
assertion refuses any entry that would still land inside the host home,
in every spelling D8 allows. The generated `[env]` additionally carries
`RIPGREP_CONFIG_PATH` pointing at the mounted ripgreprc (review-3 item
6): the sandbox clears the host variable (`--clearenv`, and it is not
in the forwarding allowlist), so the mount alone would leave ripgrep
running with its defaults — the variable is the activation, and the
module sets it only when Home Manager itself enables ripgrep with
arguments, so it never points at a file that does not exist. A
hand-written user config outside myconfig must reproduce the
`[env]` entry itself — the mount alone is inert, and mysbx will not
invent a variable the config never set.
Other modules extend it by appending to `myconfig.ai.mysbx.config.mounts`.
Outside myconfig the file stays an ordinary hand-written file; mysbx itself
knows nothing about where it came from.

### D7: The sidecar config is trusted; mounts come from both layers

The sidecar `config.toml` is **trusted**, at the same level as the user
config. Rationale: it is not repo content. It lives *outside* the repo
(D2), is never mounted into the sandbox and can therefore never be
written by the payload; the person who wrote it is the operator who
chose to clone this repo and work on it — the same person the user
config belongs to. A plain file in the operator's own filesystem does
not need a second file in the same filesystem to bless it.

So **`[[mounts]]` is a direct declaration in either layer**: a sidecar
entry may name any host path, in either mode (`ro` and `rw` alike), and
needs no covering entry in the user config — which may be absent
entirely. The two lists simply concatenate: the user config's entries
first, in declaration order, then the sidecar's, in its declaration
order. Nothing is sorted and nothing is deduplicated. When both layers
name the same path with different modes, **both binds are emitted and
the later one wins inside the sandbox** — bubblewrap applies binds in
argv order (see `cli.md`/`bwrap.rs`), and mysbx does not second-guess
that: `--verbose` shows every entry with the layer that declared it, so
the effective result stays readable.

This replaces the earlier "the sidecar may narrow, not widen" rule,
which required every sidecar mount to sit at or below a user-config
grant and forbade `ro` → `rw`. That rule made the common case — a repo
that needs one adjacent directory, with no user config on the machine —
fail with an error telling the operator to grant the path to themselves
first. **The security claim of mysbx does not rest on a mount
allow-list** but on the shape of the sandbox itself (D9): a fixed base
table, a tmpfs `/tmp`, no `/run`, no host `$HOME`, a cleared
environment, and nothing from the host filesystem inside unless a
trusted config declared it. Removing the cross-layer grant check does
not weaken that claim; it only stops one trusted file from having to
ask another for permission. (The question this rule left open — how a
repo requests access the user config does not grant — is answered by
this decision: it declares it in its own sidecar.)

**User entries are mounts, not offers** (review-2 item 6). A
`[[mounts]]` entry in the user config is mounted in every sandbox of
that user; a sidecar changes nothing about it. Dropping a user mount
for one suspicious repository stays an open question (a per-repo
opt-out); the user config is the place to make that call today.

What the sidecar still may **not** do:

- **`[env]`**: it may introduce variables the user config never
  mentions (an invented variable is a value the repo already
  controls), but it may not override a variable the user config sets —
  overriding is how a repo would redirect a tool at something the user
  did not choose, and the user config is the more global statement.
- **`network`**: it may set `false`, never `true` over a user-config
  `false`. `network` is a host-wide policy switch with no per-entry
  granularity: a user who denied the network host-wide means it for
  every sandbox, and a repo re-enabling it is exactly the thing that
  switch exists to prevent.

`backend` passes through from whichever layer named it (the sidecar
wins when both do, per the layer precedence of D6); it is not an access
question.

`git-dirs` (D13) works the same way as mounts: an entry in either layer
approves external git metadata. What that does NOT do is let the
repository approve itself: the `.git` pointer inside the repo grants
nothing, a run records nothing (it creates nothing at all — cli.md
D13), and turning a discovered directory into an approval is an
explicit `mysbx init` (D12) that prints every path it records. That "cannot write the policy" is
enforced, not assumed (review-3 item 3): an `rw` mount — or the repo
bind, or a git dir — whose source contains the sidecar config or the
user config is refused with a policy-file error, because a policy
file the sandbox can write steers the NEXT run of itself:
`git-dirs` approvals can be added, the `.git` pointer rewritten to
match. Read-only mounts of the sidecar stay allowed (reviewing it
from inside the sandbox is legitimate; `ro` cannot write it in place).

What counts as "contains the config" is the whole PATHNAME, not only
the file the pathname currently resolves to (review-4 item 1). mysbx
finds its policy by walking a path, so every directory entry on that
walk decides which file the next run reads — including the final entry
and any symlink in between. Home Manager generates
`~/.config/mysbx/config.toml` as a symlink into the immutable
`/nix/store`: a writable bind of the directory holding that symlink
cannot touch the store target, but it can unlink the symlink and put a
policy of the payload's own there. Both halves are therefore guarded —
the resolved target *and* every traversed directory entry — and a
writable source covering either is refused.

Protection that is independent of the config layers and keeps holding:
no mount `dest` may overwrite a protected sandbox path (`/`,
`/nix/store`, `/usr/bin`, `/proc`, `/dev`, `/etc/localtime`, `/tmp`,
`/run` — see D8 and `bwrap.rs`), no `dest` may hide an earlier mount or
resolve through writable content, and the repo root itself may not be
`$HOME`, a directory CONTAINING `$HOME` (review-4 item 2 — the repo is
bound `rw`, so a marker above the home would expose the whole home) or
`/` (`repo.rs`).

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

`..` is allowed in every form: canonicalization resolves it, so what is
mounted is the real target of a `~/…`, `../…` or symlinked spelling,
not the spelling itself.

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

A `dest` may also not lie **below a writable bind** — the repo work
tree, a git metadata directory, an `rw` mount, or a `ro` alias of any of
those (review-2 item 2). bubblewrap resolves a destination against the
sandbox it has built so far and follows symlinks in its parent
components, so a symlink planted in writable content
(`<repo>/jump -> /`) redirects the bind to any path, protected ones
included. mysbx cannot see that: canonicalizing the dest on the host
would model the wrong tree and would race with the payload. The whole
class is refused instead.

A `ro` bind stays usable as a parent — the sandbox cannot rewrite host
state it only reads — **unless it re-exposes content that is writable
elsewhere in the sandbox**: `ro` stops writes through that bind, not
writes to the same host inode through the repo bind next door, so a
`ro` mount of a path inside the repo (or inside an `rw` mount) counts
as writable too. "Inside" holds in both directions (review-3 item 1):
an `ro` alias of a tree that *contains* the repo, or a parent of an
`rw` mount's source, exposes the same planted symlinks through the
wider window. The analysis is therefore order-independent — the
declaration order of the mounts does not matter, only the composed
writable set does — because the symlink is exploited on the *next*
run, when the order is identical. The tmpfs `$HOME` stays seedable
(D14): bubblewrap creates it empty in the same run, so nothing can
have planted a symlink in it.

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

"Declared" means: written in one of the two **trusted** configuration
files (D7) — the user config and the repo's sidecar, both outside the
repo work tree and both unreachable from inside the sandbox. The claim
is *not* that a per-repo file can only reach what a host-wide file
pre-approved; it is that the sandbox contains nothing the operator did
not write down, and that the base table itself (tmpfs `/tmp`, no
`/run`, no host `$HOME`, `--clearenv`) is not configurable at all.

### D10: The sidecar also holds state

Beside `config.toml`, the sidecar has room for backend state, caches and
mounts standing in for host directories (e.g. `~/.local/share`). State is
disposable: deleting the sidecar and re-running `mysbx init` must yield a
working setup again. `state-dirs` (D15) is the schema's way to ask for
exactly that: each entry's backing store lives at `<sidecar>/state/`.

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

It is also the *only* way a repository becomes usable: a sandbox run
creates nothing and refuses an uninitialized repository (cli.md D13).
The one other command that writes this file is `mysbx edit` (cli.md
D12), whose purpose is exactly that file.

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
the repository cannot rewrite. A run itself records nothing, because it
creates nothing: an uninitialized repository is refused with the
`mysbx init` hint (cli.md D13), and `mysbx edit` writes the template
without approvals. A first contact with a freshly cloned repository can
therefore never turn that repository's own pointer into an approval. A
`.git` file edited *later* points somewhere unapproved and is refused,
with the offending path named.

The sidecar itself is never approvable as git metadata, in either
direction (a pointer at it, or at anything containing or inside it):
binding it would hand the payload the file that decides what may be
bound at all.

The explicit-after-the-fact form of that trust decision is `mysbx init
--approve-git-dirs` (review-3 item 5): against a config that exists
without approvals — written by `mysbx edit`, by hand, or by an `init`
that ran before the checkout became a linked worktree — it adds the
discovered-but-unapproved entries —
idempotently, and never touching anything but the `git-dirs` list.
It is additive, not reverting: an entry an operator deliberately
REMOVED is rediscovered on a later run, so re-running the flag
re-approves it — the flag is the operator's word each time it runs.
Plain `init` (D12) never touches an existing config at all, which is
what keeps a deliberately removed entry removed until the operator
says otherwise.

The edit itself is a TABLE- and STRING-aware splice (review-4 item 3,
`toml.rs::add_git_dirs`), not an append: TOML never returns to the root
table, so a `git-dirs` line written at the end of a config that ends in
`[env]` would be an `env.git-dirs` key and one written after
`[[mounts]]` a mount field — both rejected by the strict parser on the
next run. A missing key is therefore inserted before the first table
header (below any comment block documenting that table), an existing
one is recognised in both its bare and its quoted spelling and only at
the top level, and `]` or `#` inside a quoted path is read as data, not
as structure. The rewritten document is validated with the real parser
before it replaces anything, and the replacement is a temp-file +
`rename(2)`, so an interruption can never leave a truncated policy.

## Non-goals

- No global registry of sandboxes; the filesystem layout *is* the registry.
- No config-file includes or inheritance chains beyond the two layers in D1.

### D14: The sandbox has its own `$HOME`, and it is infrastructure

Inside the sandbox `HOME` is `/mysbx-home`, a fresh, empty, writable
tmpfs created with the other base mounts. The host home directory is
still **not** mounted, and the host's `HOME` *value* is never forwarded
(it is not in the forwarded list, plan.md "Environment").

"The host home is not mounted" is enforced, not merely claimed
(review-3 item 4): a mount source that IS the home (`path = "~/"`, or
a symlink resolving to it) or CONTAINS it (`path = "/home"`, or a
checkout root the home lives below) is refused in either layer, before
grant semantics apply — the merge compares canonicalized paths, so no
spelling slips past. Subdirectories (`~/.config/git`) stay the
supported shape. The NixOS assertion on the generated layer checks the
`dest` side of the same invariant at eval time, with lexical `..`
normalization; the runtime holds both.

Rationale, in the order the constraints bite:

- **Something must be there.** With `--clearenv` and no `HOME`, `cd ~`
  fails with `bash: cd: HOME not set`, and git, shells and editors that
  derive paths from `$HOME` fail or write to `/`. An unset `HOME` is not
  a confinement property, it is a broken sandbox.
- **Not the host home.** Mounting it would hand the payload `~/.ssh`,
  `~/.aws` and every agent credential in one bind — the exact thing the
  base table refuses. Exposing *parts* of the host home stays what it
  was: an explicit `[[mounts]]` entry of one of the two trusted layers
  (D6/D7). Such a mount may point its `dest` into
  `/mysbx-home` to seed dotfiles (`~/.gitconfig`); the tmpfs is created
  before the configured mounts, so they land on top of it. Only
  *below* it, though: a `dest` equal to `/mysbx-home` — or an ancestor
  of it, which on component boundaries is `/` alone — is refused
  (review-2 item 5), because such a mount replaces the tmpfs while
  `HOME` still names it, making the report's "tmpfs; the host home is
  not mounted" false. Unlike every other base path, the sandbox home
  is therefore protected in one direction only: its descendants are
  the seeding path, not an attack.
- **Not the repo root.** `HOME = <repo>` would make every tool that
  writes to `~` (shell history, caches, `.gitconfig` edits, agent state)
  pollute the checkout, and would make `~` and the work tree
  indistinguishable to the payload.
- **Not under `/home`.** A path such as `/home/<user>` inside the
  sandbox would mirror a host path that is deliberately absent; a payload
  (or a reviewer of `--dry-run`) could not tell the two apart. The
  literal invariant is worth keeping checkable, so the sandbox home is
  namespaced instead: `/mysbx-home`. It reads "no in-sandbox path under
  `/home/`" — mount *sources* are host paths and may of course live in
  the host home; what must not happen is a `dest` (or `HOME` itself)
  mirroring one. That is why the generated user layer gives its
  baseline mounts explicit destinations under `/mysbx-home` (review-2
  item 6) instead of letting them default to their host path. The check
  is a NixOS assertion on the generated layer; a hand-written config or
  sidecar can still write such a `dest`, and mysbx accepts it — the
  invariant is a property of what myconfig generates, not something the
  CLI enforces.
- **Ephemeral, except what is declared.** A tmpfs dies with the
  sandbox. Persisting the *whole* home stays refused (it would be a
  host home in disguise); the decided middle ground is `state-dirs`
  (D15): explicitly declared subdirectories are backed by the sidecar
  (D10), the rest of the home stays ephemeral.

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

### D15: `state-dirs` — persisted sandbox-home subdirectories, backed by the sidecar

`state-dirs` is a list of paths **relative to the sandbox home** whose
content should persist across runs:

```toml
state-dirs = [".local/share/opencode", ".local/state/opencode"]
```

For each entry mysbx synthesizes a **host backing store** under the
sidecar (`<repo>.mysbx/state/<entry>`), creates it before the backend
starts and binds it `rw` at `/mysbx-home/<entry>`, next to the repo and
git-metadata binds of D13. The result: a sandboxed agent keeps its
sessions, caches and auth-free state per repository — the state
survives the sandbox, the sandbox still never sees a host-home path.

Why a separate key instead of `[[mounts]]`:

- **The host path is not configuration.** A mount entry names a host
  path a trusted layer approves; a state entry names a *shape* below
  the sandbox home, and mysbx derives the host side from the sidecar
  (D2/D10). The sidecar is where per-repo disposable state lives by
  design — `state/` is exactly the "room for state" D10 reserved.
- **No host-home path can enter the sandbox through it.** The entry
  grammar is deliberately tiny: relative, no `/`, no `~/`, no `.`,
  no `..` (the parser rejects them at the schema edge). Mount paths get
  the D8 forms because they name host trees; a state entry must be
  joinable into TWO trees (the sidecar's `state/` on the host, the
  tmpfs home in the sandbox), and any ambiguous spelling would let one
  of the two joins escape its anchor.
- **The mount guards would fight it.** `check_symlinkable_dests`
  (review-2 item 2) refuses a `dest` below a writable bind; a state
  directory IS a writable bind at `/mysbx-home/<entry>`, so a mount
  dest below it is refused like a dest below the repo — correct, and
  exactly why the state binds are emitted as *implicit infrastructure*
  between the git binds and the configured mounts, not as mounts:
  a `[[mounts]]` entry may never cover or hide them
  (`check_hidden_mounts` treats them like the repo bind).

Layer semantics:

- Both trusted layers declare (`state-dirs` in the user config is the
  host-wide set — the agent state dirs of every sandbox of this user —
  and a sidecar adds per-repo ones). The lists concatenate, user layer
  first; duplicates are dropped (first occurrence wins), unlike mounts,
  because two binds of the same entry would target the same backing
  directory and the later could only "win" by pointing somewhere the
  schema forbids anyway.
- **Entries may not nest** (`.local/share` and `.local/share/opencode`
  together are refused, `bwrap.rs::check_state_dirs`): the inner bind
  would land on `<sidecar>/state/.local/share/opencode` — a path that
  exists only as the outer entry's own backing store — making the
  layout ambiguous. Declare only the narrowest entries.

Runtime behavior:

- The backing directories are created (idempotently) after the merge
  and before the backend starts — bubblewrap requires an existing
  bind source. Under `--dry-run` nothing is created; the argv shows
  the would-be sources.
- Creation walks the entry **one component at a time** and refuses any
  component that is not a real directory (`lib.rs::ensure_plain_dir`).
  A symlink is the case that matters: the state tree is the only part
  of the sidecar the payload can write, so it can plant one there
  between two runs, and a plain `create_dir_all` would follow it —
  creating directories outside the sidecar and binding them `rw` at
  `/mysbx-home/<entry>`. That is a host path (the host home included)
  re-entering the sandbox without any layer declaring it, i.e. exactly
  what D9/D14 forbid. The entry *spelling* being unambiguous is not
  enough: the spelling is only half of the path, the filesystem is the
  other half. Refusing fails the run with the offending path named;
  deleting the sidecar's `state/` tree recovers (the state is
  disposable, D10).
- Deleting the sidecar discards the state, on purpose (D10): the state
  is disposable, and the next `mysbx init` (a run after that recreates
  the `state/` tree empty; a run *without* a sidecar config is refused,
  cli.md D13) starts over.
- The report lists every entry with its backing store
  (`state dirs: …`, one `<entry> <-> <sidecar>/state/<entry>` line
  each), and the `home:` line says when part of the tmpfs home is
  sidecar-backed, so "the host home is not mounted" never becomes a
  lie by omission.

Trust: a state directory is writable host state the payload can plant
symlinks in, so it joins the writable sets of the argv guards like the
repo does — a `[[mounts]]` `dest` below a state directory is refused
(`DestBelowWritable`), and the symlink-planting is also why the backing
stores are created symlink-free rather than with `create_dir_all` (see
"Runtime behavior" above).

The same argument runs in the other direction, at config time: a
writable bind (an `rw` mount, the repo, a git dir) whose source is an
**ancestor** of a backing store is refused (`StateTreeWritable`). The
sidecar's `state/` directory holds no policy file, so the policy-file
rule of D7 does not catch it — but its LAYOUT decides where the next
run's state binds come from, which is the same "steers the next run"
property. The backing store itself stays mountable (the payload has it
rw already and cannot rewrite its own parent), and every `ro` view of
the tree stays allowed. The two checks are deliberate belt and braces:
this one names the offending configuration, `ensure_plain_dir` catches
a symlink whatever created it. It is NOT a policy file: the sidecar's
`config.toml` stays the only steered-next-run artifact; `state/` holds
payload data, trusted exactly as much as the work tree.

On myconfig hosts the NixOS module (`../../default.nix`) writes
`myconfig.ai.mysbx.config.stateDirs` into the generated user layer —
per-agent modules append the state directories of their tool (today
opencode's `~/.local/{share,state}/opencode`).

### D16: `workmux` — the interactive payload is a tmux session, on a socket inside the sandbox

**Superseded by D17.** The boolean `workmux` key is gone from the
schema; the multiplexer is now *chosen by name*
(`multiplexer = "workmux"`). Everything below still describes what the
`workmux` choice does and why its socket lives where it does — D17 only
generalizes the switch and lifts the socket rule to every multiplexer.
The key itself is rejected with a migration message naming its
replacement (`config.rs`), so an old hand-written sidecar fails loudly
instead of silently losing its session.

```toml
# what this section describes; today it is spelled
multiplexer = "workmux"
```

Selecting workmux makes the **interactive** payload a
[workmux](https://github.com/raine/workmux) tmux session instead of a
bare shell: mysbx execs the entry pinned by its wrapper
(`MYSBX_MUX_ENTRY_WORKMUX`), which boots a tmux server, bootstraps the
workmux sidebar + dashboard and attaches — the same bootstrap the
bubblewrap-jail tier does in
`../../myconfig.ai.workmux/jail.nix` (`agent-bubblewrap-workmux-tmux`),
expressed here as *configuration* instead of a Nix call site.
`mysbx run -- CMD` is unaffected (cli.md D11).

**The tmux socket lives inside the sandbox, and that is not
configurable.** It is always `/mysbx-home/.mysbx-tmux/socket`: a path
in the sandbox home tmpfs (D14), created by the entry inside the
sandbox, exported as `TMUX_TMPDIR` so a bare `tmux` in any pane finds
the same server, and gone when the run ends.

Why not the jail tier's repo-local socket (`<repo>__worktrees/.agent-bubblewrap/socket`)?
Because that is a HOST path, and under mysbx it would be reachable from
every other sandbox of the same repository (and from a tmux client
outside the sandbox, if the directory were ever bound elsewhere). A
tmux server is a command-execution service: whoever reaches its socket
runs processes in the session's namespace. Keeping the socket in the
tmpfs makes "one sandbox, one tmux server" a property of the
filesystem layout rather than of a naming convention.

That isolation is **enforced, not assumed** (`bwrap.rs::check_workmux_socket`):

- `TMUX_TMPDIR` is emitted after `[env]`, like `HOME` and `PATH`
  (D14), so no layer can repoint it at `/tmp/tmux-1000` or at a bound
  host directory — such an entry parses, shows up in `--dry-run` and
  never reaches the payload;
- no mount `dest` may be the socket directory or lie below it (a dest
  AT it would put host content under the socket, a dest below it would
  land inside the directory the tmux server owns); component
  look-alikes (`/mysbx-home/.mysbx-tmux2`) stay mountable, like
  `/usr/bin2` for the base paths;
- no `state-dirs` entry may back it with a sidecar directory (D15) —
  that would turn the socket into a host path shared by every sandbox
  of the repository, so the socket is deliberately not persistable;
- the base table binds no `/run` and gives `/tmp` a fresh tmpfs
  (plan.md), so the host's own default socket directory
  (`/tmp/tmux-<uid>`) does not exist inside the sandbox at all — which
  also covers workmux's *own* sidebar socket, which it derives from the
  tmux socket path and puts in `/tmp`
  (`/tmp/workmux-sidebar--mysbx-home-.mysbx-tmux-socket.sock`, observed
  in a real run): inside the tmpfs, i.e. per sandbox, like the tmux
  socket itself.

Layer semantics: see D17 — either layer may decide and the sidecar wins.

What the session can and cannot do is a consequence of the base, not of
this key: `workmux add` creates a git worktree in the
`<repo>__worktrees` sibling, which is **outside** the repo bind (D13),
so a sandbox that should create worktrees must declare that directory
`rw` in its sidecar `[[mounts]]`. Without it the dashboard, the sidebar
and every pane still work, and `workmux add` fails with a filesystem
error naming the path — the honest outcome, since no layer declared it.

On myconfig hosts none of this is hand-written: `myconfig.ai.mysbx.workmux`
(`../../default.nix`) generates `workmux = true`, the in-sandbox
`~/.config/workmux/config.yaml` (with the *plain* agent binaries, since
the sandbox is already the sandbox), the `tmux`/`workmux` entries of the
tool closure and the `MYSBX_WORKMUX_ENTRY` pin. The wiring that decides
*whether* a host gets it lives with the other workmux tiers
(`../../../myconfig.ai.workmux/mysbx.nix`), next to `jail.nix` and
`sandbox.nix`.

### D17: `multiplexer` — which terminal multiplexer the interactive payload is

```toml
multiplexer = "workmux"   # tmux | workmux | herdr | aoe | none
```

One top-level string key names the **interactive** payload of a
sandbox. It replaces the boolean `workmux` of D16, which could only
say "session" or "no session" while this repo runs several
agent-oriented multiplexers side by side.

| value | payload |
| --- | --- |
| `"tmux"` | plain tmux, one session per repo, on the in-sandbox socket |
| `"workmux"` | the workmux session of D16 (sidebar + dashboard) |
| `"herdr"` | [herdr](https://herdr.dev), the agent multiplexer (`../../../programs.herdr.nix`) |
| `"aoe"` | Agent of Empires (`../../../programs.agent-of-empires/`), a tmux-based agent session manager |
| `"none"` | a plain interactive shell — the pre-D16 behaviour |

**A string enum, not a table, and not a command.** A `[multiplexer]`
table would invite a `command = …` key, and configuration that names a
command is configuration that executes (D4). What a layer may say is
*which of the multiplexers this build carries* runs — the argument
vector belongs to the entry scripts in mysbx's own closure. The value
is validated strictly: anything outside the five names is a schema
error naming the file, the key and the accepted values (like `mode`,
D9/D11).

**Default: `none`.** An omitted key decides nothing (tri-state in a
layer, like `backend` and `network`), and with neither layer deciding
the payload is the plain shell. So a host that never mentions the key
keeps a byte-identical argv — the multiplexer is opt-in, and it is the
generated user layer that opts in (see below).

**Layering: either layer may decide, and the sidecar wins when both
do** — like `backend`, not like `network`. Inherited verbatim from D16,
and it is the trust model that allows it: both layers are trusted (D7,
the sidecar lives outside the repo, D2), and the key grants no host
access at all — it selects one of the payloads mysbx itself carries and
adds one environment variable. Neither the narrow-only rule of
`network` nor the `[env]` override refusal applies. The user config is
therefore the *default* ("what my sandboxes usually start") and the
sidecar the *decision* ("this repo wants herdr"), including
`multiplexer = "none"` for a repo that wants a bare shell.

**Availability is checked, never fallen back on.** Each choice needs an
entry from mysbx's own closure, pinned by the wrapper as
`MYSBX_MUX_ENTRY_TMUX`, `…_WORKMUX`, `…_HERDR`, `…_AOE`
(`../../nix/mysbx.nix`). A selection whose entry is not pinned — an
unwrapped build, a host that does not install that multiplexer — is a
**refused run** (exit 1, the message naming the value and the missing
variable), never a silent plain shell: the operator asked for a
session, and discovering the plain shell after the work happened in it
is the worse outcome (the D16 argument, kept). The refusal is a
configuration error, not an exec failure: it happens while the argv is
built, so `--dry-run` refuses it too and no `bwrap` is started.

**The socket/state isolation of D16 holds for every value.**
`/mysbx-home/.mysbx-tmux` (`bwrap.rs::MUX_SOCKET_DIR`) is exported as
`TMUX_TMPDIR` for every non-`none` choice — emitted after `[env]`, like
`HOME` and `PATH`, so no layer can repoint it — and the D16 guards
(`bwrap.rs::check_mux_socket`) apply unchanged: no mount `dest` at or
below it, no `state-dirs` entry backing it. tmux, workmux and aoe are
tmux servers and put their socket there; herdr is its own multiplexer
and runs monolithic (`herdr --no-session`), so its socket and state
live in the sandbox-home tmpfs, which is per-run and per-sandbox by
construction (D14). The variable is set for herdr too, uniformly: one
code path, and a pane running plain `tmux` inside a herdr session lands
on the same private socket rather than on `/tmp/tmux-<uid>`.

**`run -- CMD` is unaffected** (cli.md D11): a one-shot command is
never wrapped in a session, so its argv is byte-identical to a
`multiplexer = "none"` one — no payload swap, no `TMUX_TMPDIR`, none of
the socket guards.

**`mysbx init` writes the key.** The template records the value the
*user layer* currently names (`none` when it names none), with the
enum in a comment above it. Two properties this buys: the sidecar of a
fresh repo is self-documenting (the operator sees what to change and to
what), and writing it changes nothing — the sidecar wins over the user
config, so any other value would silently *downgrade* the host default
for every newly initialized repo. `init` copies the default; it does
not invent one.

The accepted consequence: a repository initialized while the host
default was `"workmux"` keeps workmux after the host switches to
something else — its sidecar now says so explicitly, and per-repo is
exactly the level at which this decision belongs. Changing it is
editing one line (`mysbx edit`); deleting the line hands the repo back
to the user layer. When the user layer cannot be read at all, the
template leaves the key commented out rather than guess a value that
would override it.

On myconfig hosts the value is generated:
`myconfig.ai.mysbx.config.multiplexer` (`../../default.nix`) writes it
into the user layer and defaults to `"workmux"` where the workmux integration is
wired (`myconfig.ai.mysbx.workmux.enable`, set by
`../../../myconfig.ai.workmux/mysbx.nix`) and to `"none"` otherwise —
the behaviour-preserving default. The module also pins the entries of
the multiplexers whose package the host has
(`myconfig.ai.mysbx.{workmux,herdr,aoe}.package`; tmux always) and adds
the selected one's binaries to the sandbox `PATH` via `extraTools`, so
the panes find the tool they are running in.
