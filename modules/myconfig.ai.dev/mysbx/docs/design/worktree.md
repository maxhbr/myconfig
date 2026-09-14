<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the worktree noun group — inspecting the host's workmux worktrees

Status: agreed (bd myconfig-j9l). This is the decision record for the
`mysbx worktree …` verb group: read-only inspection of the HOST's
workmux-style linked worktrees — the checkouts in
`<repo>__worktrees/<handle>` — from wherever the operator stands.

Vocabulary, on top of [`../../CONTEXT.md`](../../CONTEXT.md):

- **host worktree** — a linked git worktree of the host repo, created
  by `workmux add` (or `git worktree add`), living in the
  `<repo>__worktrees` sibling of the repo root.
- **base branch** — the branch a worktree diverged from; workmux
  records it as `branch.<branch>.workmux-base` in the repository
  config when it creates the worktree.

## The problem

The workspace model's handoff verbs (`fetch`/`merge`/`push`/`diff`,
[workspace.md](./workspace.md) D6) and the session verbs (D7) are
host-side: they resolve the repo from the cwd and run git against it.
But from inside a mysbx sandbox the cwd is a *clone* (a live run binds
the repo itself; a clone run binds the session clone at the repo's
path), and an operator — or a sandboxed agent that drives the host
through the terminal it was given — has no verb that answers "which
worktrees exist on the host, and what does the work in this one look
like?" without leaving the myconfig tooling entirely (`workmux list`,
`workmux merge` do parts of it, but with tmux side effects and no
`--dry-run` contract).

## Decisions

### W1: A second closed noun group — `worktree list | diff | hunk`

[cli.md](./cli.md) D3's single-verb rule gains one more **closed**
exception, beside `session`: the `worktree` group, three verbs, not an
open tree — a fourth is a decision, not a given.

- **`mysbx worktree list`** — one line per directory entry of the
  `<repo>__worktrees` sibling: the handle, the checked-out branch, and
  the ahead-count — the commits in the worktree's branch that the base
  branch does not have (`-` when no honest number can be computed, the
  same honesty rule as `session list` of workspace.md D7).
- **`mysbx worktree diff NAME`** — the THREE-DOT diff of the worktree's
  branch against its base branch — the changes on the branch since it
  diverged, not the base's own drift (the same semantic as
  `mysbx diff`, workspace.md D6, applied to worktrees).
- **`mysbx worktree hunk NAME`** — run the interactive `hunk` diff
  viewer on that same three-dot diff, exec'd like `$EDITOR` is for
  `mysbx edit` (D12): mysbx hands the terminal over, the tool's own
  exit code propagates unchanged (D8).

The group is host-side like the handoff verbs: no sandbox is started,
the repo is the one the cwd resolves to (cli.md D1), `--dry-run`
prints the exact git commands (D9) and `--verbose` is refused — there
is no run to report on.

### W2: The worktrees directory is the registry — discovered, never created

`<repo>__worktrees` is operator state beside the repo, the same trust
decision as the sidecar's (config.md D13): a run never creates it, and
neither does this verb group. The resolution reuses
[`Repo::worktrees`](../../mysbx-rs/src/repo.rs) — the sibling spelling
`<parent>/<basename>__worktrees`, recorded when it exists. An absent
sibling is the empty registry: `worktree list` prints nothing and exits
`0` (a listing, not a verdict), the same as `session list` with an
empty `clones/`.

The per-worktree facts come from the layout, not from a directory
listing alone: a handle directory must contain a `.git` FILE (a linked
worktree's pointer) to be a worktree — an entry without one is debris
and marked as such, per the `session list` incomplete-inventory
precedent.

### W3: The base branch is `branch.<branch>.workmux-base` — master is the fallback

The three-dot diff needs the *other* end, and "the base branch" is
workmux's own concept: `workmux add` records it as
`branch.<branch>.workmux-base` in the repository config, and
`workmux merge` reads exactly that key (with a local `main` fallback we
do NOT adopt — see below). `mysbx worktree diff NAME` therefore:

1. resolves the worktree's checked-out branch
   (`git -C <worktree> symbolic-ref --short HEAD`),
2. reads the base from the REPO's config
   (`git -C <repo> config --get branch.<branch>.workmux-base`) — the
   host repo's config, where workmux wrote it, not the worktree's own
   view,
3. falls back to `master`, then `main`, then `HEAD` of the main
   checkout — the first that resolves — when no `workmux-base` key
   exists (a worktree added by hand, or by an older workmux).

The fallback chain names the base it used, so the operator can tell a
configured base from a guessed one. workmux's own `main` fallback is
not adopted because this repository's main branch is `master` — the
chain covers both spellings instead.

### W4: `hunk` is exec'd, its binary never invented

`mysbx worktree hunk NAME` builds the hunk invocation —
`hunk diff <base>...<branch>` with the working directory set to the
worktree — and `exec`s it, the same discipline as `mysbx edit`'s
`$EDITOR` (D12): the tool replaces this process, owns the terminal,
and its exit code propagates unchanged (D8). The binary is a plain
`hunk` on PATH — no `MYSBX_HUNK` pin, deliberately: unlike `bwrap` or
the multiplexer entries, hunk is not part of mysbx's own sandbox
closure and adds no trust decision (it runs host-side, with the
operator's own terminal, like `$EDITOR` does). A `hunk` that cannot be
exec'd is the plain runtime failure of the `exec` (D8).

`--dry-run` prints the exact invocation (the executable first, one
argument per line, D9) and runs nothing — the honest dry run of an
interactive tool.

### W5: Read-only, in scope and out

The group is deliberately **read-only**: `list`, `diff` and `hunk`
never write — no fetch, no merge, no ref deletion, no `workmux`
invocation. Creating, merging and removing worktrees stay with
`workmux add` / `workmux merge` / `workmux remove` and plain
`git worktree` — those are *mutations of host state with tmux side
effects*, the exact class of operation the host-side verbs of
workspace.md D6 wrap in guards (and `workmux` already owns them).
Agent status is likewise **out of scope**: `workmux status` needs the
live tmux server and its state files, a channel mysbx has no part of —
`workmux status` is one command away, and the verbs here name no
channel they cannot honor. This is the "read-only subset" the bead
names as the fallback: full integration with the workmux status
channel is a follow-up decision if it ever earns one.

## Non-goals

- No worktree creation, merge, rebase or removal (W5).
- No `--repo` flag anywhere (cli.md D1).
- No TOML surface (the registry is the filesystem layout, W2).
- No agent-status column (W5).
