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
harness that produced it, in this exact format:

    supported by <model> in <harness>

Detect both at commit time via the shell environment; do not guess from the
conversation.

### Detecting the harness

- pi sets `PI_CODING_AGENT=true` in its shell tool's environment -> harness
  is `pi`.
- opencode sets `OPENCODE=1` in its shell tool's environment -> harness is
  `opencode`.

### Detecting the model

- pi: use `$PI_MODEL`, prefixed with `$PI_PROVIDER/` only when the model id
  does not already contain a `/`. Both variables are resolved freshly for
  every shell command, so they always reflect the current selection.
- opencode: the active model is not exported to the shell. Read the most
  recently selected model from opencode's state file, guarding against a
  missing file, a missing `jq`, and incomplete entries:

  ```bash
  f="${XDG_STATE_HOME:-$HOME/.local/state}/opencode/model.json"
  command -v jq >/dev/null && [ -f "$f" ] && jq -r '
    (.recent[0] // empty) | select(.providerID and .modelID)
    | "\(.providerID)/\(.modelID)"' "$f"
  ```

  If that prints nothing (file missing/empty, or no valid entry), fall back
  to the configured default model, guarded the same way:
  `command -v jq >/dev/null && [ -f ~/.config/opencode/opencode.json ] && jq -r '.model // empty' ~/.config/opencode/opencode.json`.

### Fall back gracefully

Treat an empty result or a literal `null` (e.g. `null/null`) as "model
unknown". If only the model is known, the trailer is `supported by <model>`;
if only the harness is known, `supported in <harness>`; if neither can be
determined, omit the trailer line entirely. Never emit `supported by  in `,
`null`, or other empty/placeholder values.

## On failure

If `git commit` itself fails (for example, a repository pre-commit hook
rejects the commit), stop and surface the error output so the user can decide
how to proceed. Do **not** try to repair the working tree or amend the change
to satisfy the hook.
