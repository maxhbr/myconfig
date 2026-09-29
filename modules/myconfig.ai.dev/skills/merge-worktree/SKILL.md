---
name: merge-worktree
description: Commit, rebase and fast-forward-merge the current git worktree branch into its local base branch, then clean up the worktree (herdr-style `<repo>__worktrees/<name>` layout). Plain git plus the optional herdr CLI; does not need workmux. Use when the user says "merge-worktree", "merge this worktree" or "finish this branch" and workmux is not in use for it, or passes flags like --keep / --no-verify.
---

# Merge worktree

Finish work on the current branch of a linked git worktree: commit, rebase
onto the local base branch, fast-forward the base branch, clean up. Works
with the herdr/workmux layout (`<parent>/<repo>__worktrees/<name>` next to
the main checkout `<parent>/<repo>`) and with any other linked worktree.
Uses only `git` and, when available, `herdr`. Never calls `workmux`.

## Arguments

`$ARGUMENTS` may contain:

- `--keep`, `-k`: merge, but keep the worktree, its branch and its herdr
  workspace.
- `--no-verify`, `-n`: pass `--no-verify` to `git commit` and `git rebase`.
- one bare word: the base branch to merge into (overrides detection).

## Rules

- Local only: never `git fetch`, `git pull` or `git push`; never rebase onto
  `origin/<branch>` or any other remote-tracking ref.
- Never force: no `--force`, no `git branch -D`, no `reset --hard`, no
  `stash` in someone else's checkout. If a step refuses, stop and report.
- Never use `herdr ... --trust-repository` or add a `safe.directory` entry
  unless the user explicitly approves it.
- If anything is ambiguous (base branch, dirty main checkout that overlaps,
  complex conflicts), stop and ask.

## Step 0: Collect facts

```bash
branch=$(git branch --show-current)          # empty = detached HEAD: stop
wt=$(git rev-parse --show-toplevel)
main_wt=$(git worktree list --porcelain | awk '/^worktree /{sub(/^worktree /, ""); print; exit}')
```

Stop if `branch` is empty or `wt` equals `main_wt` (you are in the main
checkout, not in a linked worktree).

## Step 1: Resolve the base branch

Use the first that applies:

1. the bare word from `$ARGUMENTS`;
2. `git config --get "branch.$branch.workmux-base"` (set by workmux and
   `git branch-to-worktree`);
3. the branch checked out in the main worktree
   (`git -C "$main_wt" branch --show-current`);
4. `main`.

Then check it:

- `git show-ref --verify --quiet "refs/heads/$base"` must succeed. The base
  is always a LOCAL branch. Do not create it from a remote (for example from
  a stale remote `HEAD`).
- If rule 3 and rule 4 disagree (the main checkout is on something other
  than `main` and no explicit/configured base exists), ask the user.

## Step 2: Commit

If `git status --porcelain` is not empty: `git add -A`, review
`git diff --staged`, and commit with the repository's message style
(`git log --oneline -15`). Follow the `commit` skill if it is available.
Skip when the tree is clean.

## Step 3: Rebase onto the local base

```bash
git rebase [--no-verify] "$base"
```

On conflicts:

- Before resolving a file, read what the base changed in it:
  `git log -p -n 3 "$base" -- <file>`.
- Keep both sides' intent; stage the file; `git rebase --continue`.
- If a conflict is unclear, `git rebase --abort` is NOT automatic: ask the
  user whether to abort or how to resolve.

Afterwards verify `git merge-base --is-ancestor "$base" HEAD`.

## Step 4: Fast-forward the base branch

Find where the base is checked out:

```bash
base_wt=$(git worktree list --porcelain \
  | awk -v b="refs/heads/$base" '/^worktree /{p=substr($0, 10)} $0=="branch " b {print p}')
```

- **Base checked out in a worktree** (usually the main checkout):
  ```bash
  git -C "$base_wt" status --porcelain     # show the user what is there
  git -C "$base_wt" merge --ff-only "$branch"
  ```
  Uncommitted changes in that checkout are fine when the merge does not
  touch them; git refuses otherwise. On refusal, stop and report; do not
  stash, commit or reset the user's changes.
- **Base not checked out anywhere**: update the ref fast-forward-only
  (a local ref update, no network):
  ```bash
  git fetch . "$branch:$base"
  ```

Confirm: `git rev-parse "$base"` equals `git rev-parse "$branch"`.

## Step 5: Clean up (skip with `--keep`)

Removing the worktree deletes the directory the agent may be running in and
the herdr workspace/pane it may be running inside. So:

1. Find the herdr workspace of the worktree (only when `HERDR_ENV=1` and
   `herdr` exists):
   ```bash
   herdr worktree list --cwd "$main_wt"
   ```
   Pick the entry whose `path` equals `$wt`; note `open_workspace_id`.
2. **If the agent runs inside that worktree** (its cwd is under `$wt`, or
   `open_workspace_id` equals `$HERDR_WORKSPACE_ID`): do NOT remove it.
   Report the merge and give the user the cleanup commands to run from
   elsewhere:
   - with a herdr workspace:
     `herdr worktree remove --workspace <open_workspace_id>` then
     `git -C <main_wt> branch -d <branch>`;
   - without: `git -C <main_wt> worktree remove <wt>` then
     `git -C <main_wt> branch -d <branch>`.
3. **Otherwise** run those commands yourself. `git worktree remove` refuses
   when the worktree has untracked or modified files: report, do not add
   `--force`. `git branch -d` refuses unmerged branches: report, never `-D`.

## Step 6: Report

State: the base branch and its new commit, the merged commits
(`git log --oneline <old-base>..<base>`), what was cleaned up or which
cleanup commands the user still has to run, and anything that was skipped.

## Troubleshooting

- `detected dubious ownership` / libgit2 `not owned by current user`: the
  repository is owned by another uid (for example inside a sandbox VM).
  Report it; do not add trust yourself.
- `fatal: Needed a single revision` from `git rev-parse --short a b`:
  `--short` takes one revision; call it once per ref.
