---
name: commit
description: Commit the current working-tree changes with a well-formed message. Use when the user says "commit", "commit this", "do a commit", "stage and commit", or otherwise asks to commit the current changes.
---

# Commit

Commit the current state following best practices.

Commit the changes that are relevant to the current conversation. Run
`git status` and `git diff` to see what changed, write a clear conventional
commit message matching the repository's existing style (`git log --oneline
-15`), stage the relevant changes, and commit.

## On failure

Do **not** try to fix things. If a pre-commit hook or check fails, or if
something looks off (secrets, unrelated dirty files, failing builds), stop
and explain to the user why the commit cannot be made. Surface the relevant
error output so the user can decide how to proceed.
