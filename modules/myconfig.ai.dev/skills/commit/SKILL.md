---
name: commit
description: Commit the current working-tree changes with a well-formed message. Use when the user says "commit", "commit this", "do a commit", "stage and commit", or otherwise asks to commit the current changes.
---

# Commit

Commit the current state following best practices.

Commit the CURRENT STATE of the working tree. Run `git status` and `git diff`
to see what changed, and `git log --oneline -15` to match the repository's
existing message style. Write a clear conventional commit message that
describes the actual changes in the context of the current history — not
filtered by conversation relevance — stage the changes, and commit.

## Unrelated changes

Prefer one focused commit. If the working tree holds clearly unrelated
changes alongside the ones being committed, exclude them when that is
straightforward — stage by path (`git add <paths>`) or by hunk
(`git add -p`), leaving the rest for a separate commit. Do this only when
the boundary is obvious; do not agonise over it or split changes that belong
together. When the unrelated changes are entangled or the boundary is
unclear, just commit the whole current state.

## Do not validate

The commit is a snapshot of the current state, not a quality gate. The agent
must **not**:

- run builds, tests, linters, formatters, or type checks
- review the diff for correctness or judge whether the work is finished
- refuse to commit because something looks wrong, unfinished, or broken
- ask the user to confirm before committing

## Attribution trailer

End the commit message with a trailer line recording the model and agent
harness that produced it. Do not derive this from the conversation — run:

    compute-commit-attribution-trailer.sh

If the command succeeds it prints the exact trailer line (for example
`supported by <model> in <harness>`, with parts omitted when unknown);
append it to the commit message as-is. If it fails or prints nothing,
omit the trailer entirely.

## On failure

If `git commit` itself fails (for example, a repository pre-commit hook
rejects the commit), stop and surface the error output so the user can decide
how to proceed. Do **not** try to repair the working tree or amend the change
to satisfy the hook.
