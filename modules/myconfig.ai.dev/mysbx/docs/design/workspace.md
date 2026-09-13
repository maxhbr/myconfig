<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the workspace model — the live repo and clone sessions

Status: agreed. This is the decision record for plan.md phase 2e (bd
myconfig-o6z): whether `mysbx` grows a workspace mode beside the implicit
live repo of [config.md](./config.md) D13, what its configuration surface
is, and what the result handoff is. The gvisor tier
(`../../myconfig.ai.gvisor-agent-sandbox/docs/spec.md` §9) is the
precedent for the handoff mechanics; the microvm tier
(`../../myconfig.ai.dev/sandboxes/myconfig.ai.microvm/docs/workspace-layout.md`)
is the precedent for keeping clones beside the repo.

Vocabulary, on top of [`../../CONTEXT.md`](../../CONTEXT.md):

- **live run** — a run whose workspace is the repo itself: the implicit,
  always-rw bind of config.md D13. The default, unchanged.
- **host repo** — the checkout the sidecar belongs to, the one the
  operator stood in.
- **session** — a named clone run, `NAME`.
- **clone** — the session's standalone git repository at
  `<repo>.mysbx/clones/NAME`.
- **session branch** — `agent/mysbx/NAME`, the branch a session commits to.

## Decisions

### D1: Two modes — `live` is the default, `clone` is opt-in per run

| mode | workspace | how selected |
| --- | --- | --- |
| live (default) | the repo itself, the implicit rw bind of config.md D13 | nothing — omit `--session` |
| clone | `<repo>.mysbx/clones/NAME`, bound rw at the repo's own path | `mysbx --session NAME`, `mysbx run --session NAME -- CMD` |

`--session NAME` **implies** clone mode. There is no TOML surface at all:
no `workspace` key is accepted in either layer (an unknown key is a schema
error, config.md D11). The live default is not expressible and can
therefore be neither narrowed nor widened by a file, and a session name is
a per-run fact, not repository policy — the filesystem layout is the
registry (a clone directory exists ⇒ a session exists), like the sidecar
itself. A run without `--session` is byte-identical to today's argv: the
live mode is untouched by this decision.

**Worktree mode is rejected, not deferred.** A linked worktree shares its
gitdir with the host repo, so committing from inside the sandbox needs rw
access to the host repo's git state — refs, objects, config — which is
exactly the exposure the clone mode exists to avoid, and the opposite of
what the unattended runs (D8) need. Worktrees stay what they are today:
created on the host (`workmux add`, `git worktree add`) and entered with a
live run. The `<repo>__worktrees` bind of config.md D13 covers that
workflow; clone runs do not bind it (D3).

### D2: The clone lives in the sidecar; the first `--session` run creates it

```text
/path/to/the/repo                     ← host repo
/path/to/the/repo.mysbx/
    config.toml
    state/…                           ← state-dirs backing (live runs)
    clones/
        NAME/                         ← the session clone (standalone repo)
        NAME.json                     ← the session's result file (D5)
```

`NAME` matches `[A-Za-z0-9][A-Za-z0-9._-]{0,63}` — no slashes, no leading
dot, no empty string. Anything else is a usage error at parse time
(cli.md D8), like every schema edge.

There is exactly one `mysbx init` per repo (cli.md D12/config.md D12), and
it does not change: it creates the sidecar and `config.toml`, nothing
else. The **first** `--session NAME` run creates the clone:

1. `git clone --origin origin --no-hardlinks <repo> <clone>` — both flags
   required, per the gvisor precedent: `--no-hardlinks` because a local
   clone hardlinks object *files*, and a writable bind of the clone would
   then write through to the host repo's objects; `--origin origin` pins
   the remote name, so a user's `clone.defaultRemoteName` cannot move the
   exact-ref probes of the handoff verbs (D6) out from under them.
2. The starting point is the host repo's **current checked-out HEAD** —
   the committed state. A dirty host working tree is deliberately not
   carried over: the session is a commit-based handoff, not a working-tree
   sync. The operator who wants uncommitted work in a session commits it
   first.
3. The session branch, by exact-ref precedence:
   - `refs/remotes/origin/agent/mysbx/NAME` exists in the fresh clone —
     an earlier session of the same name left the branch in the host repo
     via `fetch` (D6) — the session **continues at that existing tip**:
     `git checkout --no-track -b agent/mysbx/NAME
     refs/remotes/origin/agent/mysbx/NAME`. Destroying and recreating a
     session must not silently fork its own branch.
   - otherwise the branch is new:
     `git checkout -b agent/mysbx/NAME <HEAD>`.

Refused at creation, with the offending fact named: an **empty host repo**
(no commits — there is nothing to clone), and a host branch literally
named `agent` — it collides with the `refs/heads/agent/mysbx/*` namespace
(ref-directory conflict), and refusing it up front beats failing inside a
handoff verb later. The `agent/mysbx/` prefix is thereby reserved for
sessions.

Auto-creation is the one deliberate exception to "a run creates nothing"
(cli.md D13) — the accepted rationale: D13 guards against surprise writes
from a command that reads like a read; `--session NAME` is an explicit
operator decision that names the thing to be created, like `init` names
the sidecar. It creates exactly one thing, at a derived path, reported
before the run starts.

**The clone is not disposable state.** The sidecar's disposal promise
(config.md D10) gains a carve-out: `clones/` can hold the only copy of
unmerged work. `rm -rf` on the sidecar discards it; the guarded removal
path is `session destroy` (D7).

### D3: A clone run binds the clone at the repo's own path — nothing else of the host repo

- The clone is bound **rw at the original host repo path**. Path identity
  is preserved: tools, configs and prompts keyed to the repo path work
  unchanged, and the payload cannot tell the bind from the real checkout.
- The host repo itself is **not mounted at all**. The `<repo>__worktrees`
  sibling is **not bound** in clone runs: it is operator state of the
  live checkout (config.md D13), and a session must not reach host
  worktrees. `git-dirs` approvals do not apply either: the clone carries
  its own `.git` *directory*, not a pointer, so there is nothing external
  to approve.
- **The clone's `origin` self-resolves inside the sandbox.** Its URL is
  the host repo's absolute path, and inside the sandbox that path IS the
  clone bind. `git fetch origin` / `git push origin` inside the sandbox
  therefore talk to the clone itself (a push to its own checked-out branch
  is refused by git's `receive.denyCurrentBranch`, a fetch is a local
  no-op) and reach neither host state nor a network remote. The payload
  would have to invent a new remote, and holds no credentials for one.
- **Policy-file adjacency.** The clone sits in the sidecar next to
  `config.toml`. The bind is exactly the clone directory, and the existing
  guards keep the rest of the sidecar out: the policy-file refusal
  (config.md D7) rejects any rw source covering `config.toml`, and no
  mount may bind the sidecar root or `clones/` itself.

### D4: In a clone run every mount is `ro`; `--rw` is refused; `state-dirs` are off

A clone run uses the **same merged configuration** — both layers plus
flags, cli.md D6, one sandbox definition per repo — but every `[[mounts]]`
entry and every `--ro` grant is **downgraded to read-only**. The clone is
the only writable bind of a clone run. Rationale: the configured mounts
carry context (tool configs, data to read); the clone carries the result.
A session must be able to run beside the host repo without being able to
write host state through a config entry.

`--rw` in a clone run is a **refused run** (exit `70`, cli.md D8) naming
the flag and the mode — never a silent downgrade: an operator who believes
a directory is writable while the payload meets `EROFS` is the worse
failure. `--ro` behaves as usual. `--verbose` marks every downgraded mount
(cli.md D10).

`state-dirs` are **not handled at all** in clone runs: no backing store
is created, nothing is bound. Per-session versus shared agent state is a
real question (parallel sessions sharing one opencode state dir is a
failure mode) and is deliberately deferred — a follow-up bead decides it
(see "Open follow-ups"). Scratch space in a clone run is the tmpfs home
and `/tmp`, as in any run.

### D5: `run --result` writes per session: `<repo>.mysbx/clones/NAME.json`

A `run --result --session NAME -- CMD` (cli.md D17) writes its outcome to
`<repo>.mysbx/clones/NAME.json` — same schema and contract as D17
(version, state, exitCode, repo, payload, timestamps, per-state fields),
one file per session so parallel sessions cannot overwrite each other's
results. A live `--result` run keeps `<repo>.mysbx/result.json`. The
pointer line goes to stderr in both cases (cli.md D9/D17).

### D6: The handoff is host-side: `fetch`, `merge`, `push`, `diff`

Four subcommands, all **host-side**: they do git plumbing between the
host repo and the clone, start no sandbox, and take the session as their
one positional argument. There is no `--repo` flag — the host repo is the
one the current directory resolves to (cli.md D1), like every other verb.
Their mechanics adopt the gvisor spec verbatim:

- **`mysbx fetch NAME`** — the fast-forward-only refspec:
  `git -C <repo> fetch --no-tags <clone>
  refs/heads/agent/mysbx/NAME:refs/heads/agent/mysbx/NAME`. Both sides
  fully qualified, no `+` force marker anywhere (a branch literally named
  `+agent/…` would otherwise smuggle the force flag). Git creates an
  absent destination ref, advances one that is an ancestor of the session
  tip, and **rejects a diverged or rewound one without touching it**:
  with standalone clones the host-local branch and the session branch can
  advance independently, and a forced fetch would silently discard
  host-only commits. Publishing rebased or amended session history stays
  an explicit operator action (`git branch -D` after inspection);
  destructive replacement is never automatic. The host repo learns
  nothing permanent about the clone — the fetch names the clone path
  once, no remote is configured.
- **`mysbx merge NAME [--no-ff|--ff|--squash] [-- GIT-MERGE-ARGS…]`** —
  refuse a detached HEAD and a dirty host working tree first (the merge
  lands in the currently checked-out branch); `--no-ff` is the default,
  `--ff`/`--squash`/explicit git-merge args pass through; then the
  `fetch` above; then `git merge <args> refs/heads/agent/mysbx/NAME` —
  the **exact fetched ref, never the bare name** (git's DWIM order checks
  `refs/tags/` first, so a same-named tag could win). On success the
  fetched host-local ref is deleted — it was the ferry copy, the work is
  in the merge; a recreated session then correctly starts at HEAD again.
  On conflict the ref stays, with the recovery hint (resolve, then
  `git branch -D agent/mysbx/NAME`).
- **`mysbx push NAME [REMOTE]`** — the implicit `fetch` first (the pushed
  ref is current; a diverged host-local branch fails the push before
  anything is published), then
  `git -C <repo> push <remote> refs/heads/agent/mysbx/NAME:refs/heads/agent/mysbx/NAME`
  — explicit, non-forced, fully qualified. The default REMOTE is the
  **host repo's own** `origin`: publishing goes through the host repo's
  configured remotes with host-side credentials. The clone never talks to
  a network remote.
- **`mysbx diff NAME`** — the implicit `fetch` (like `push`), then
  `git -C <repo> diff HEAD...refs/heads/agent/mysbx/NAME` — the
  **three-dot** form: the changes on the session branch since it diverged
  from the host's HEAD, not the host's own drift.

Failure wording follows the gvisor spec: a diverged or rewound
host-local branch, a missing clone, a dirty tree, a detached HEAD are
each named as such; git's own diagnostics pass through.

### D7: `mysbx session list` / `mysbx session destroy` — the one noun-group verb

[cli.md](./cli.md) D3 ("single verbs, no nested command trees") gains one
**closed** exception: the `session` group, two verbs, not an open tree —
a third verb is a decision, not a given.

- **`mysbx session list`** — `clones/` is the registry: one line per
  entry with the name, the session branch, and the ahead-count (commits
  in the session branch that the host repo does not have). An entry
  without `.git` is debris (an interrupted creation) and is marked as
  such, per the gvisor incomplete-inventory precedent.
- **`mysbx session destroy NAME [--force]`** — guarded removal, per the
  microvm precedent (intrinsic properties, not location trust): the
  resolved path must be strictly inside `<repo>.mysbx/clones/`, its
  basename must equal `NAME`, and it must contain `.git`; never the
  sidecar root, never `/`. It **refuses while the session branch holds
  commits the host repo does not have** (unmerged work, checked without
  mutating anything) unless `--force` is given. The clone is plain
  `rm -rf`'d — it is standalone, no worktree bookkeeping, the host repo
  is untouched. A host-local `agent/mysbx/NAME` branch left by a `fetch`
  is **not** deleted: it is the operator's imported copy, and deleting it
  silently would contradict the unmerged-work guard this verb exists to
  enforce.

### D8: Unattended runs force clone mode

The unattended/batch form (bd myconfig-dys, blocked on this decision)
**requires** `--session NAME`: an unattended run editing the live working
tree is the risky combination this workspace model exists to prevent
(plan.md 2e). Its result is the per-session `NAME.json` of D5 plus the
session branch, consumed through the verbs of D6. Recorded here so the
unattended design starts from this contract; nothing about it is
implemented by this decision.

### D9: The sidecar is not a source repo; workmux does not cover sessions

- **Running `mysbx` inside a clone is refused.** Repo resolution (the
  sidecar-ancestor walk of plan.md phase 1) treats `<repo>.mysbx/clones/*`
  as inside the sidecar: a run there is an error naming the owning repo
  and session — the sidecar is not a checkout, and a sandbox-of-the-clone
  would be a sandbox with no defined workspace.
- **Workmux dashboards do not cover mysbx sessions.** They watch
  `<repo>__worktrees`; the clones live in the sidecar and are invisible
  to them. Accepted — the session verbs of D6/D7 are the interface.
  Nothing to build.

## Open follow-ups

Filed as beads, discovered from this decision:

1. Implementation: `--session`, clone creation, the clone-run argv
   (forced-ro mounts, no `__worktrees` bind, no state binds, per-session
   result file).
2. Implementation: the handoff verbs `fetch` / `merge` / `push` / `diff`.
3. Implementation: `session list` / `session destroy` with the guards
   above.
4. Decision: `state-dirs` in clone runs — per-session backing
   (`<repo>.mysbx/clones/NAME`-adjacent or `state/NAME/`) versus the
   shared per-repo store, or none.

## Non-goals

- No TOML surface for sessions or modes, ever (D1).
- No worktree mode (D1) and no bridge to workmux dashboards (D9).
- No per-session agent state before the state follow-up decides it (D4).
- No `--repo` flag anywhere (cli.md D1): the verbs of D6/D7 belong to the
  repo the operator stands in.
