---
description: Commit current working-tree changes with a well-formed message derived from the diff
---
Commit the current state following best practices.

Commit the changes that are relevant to the current conversation. Run
`git status` and `git diff` to see what changed, write a clear conventional
commit message matching the repository's existing style (`git log --oneline
-15`), stage the relevant changes, and commit.

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

Do **not** try to fix things. If a pre-commit hook or check fails, or if
something looks off (secrets, unrelated dirty files, failing builds), stop
and explain to the user why the commit cannot be made. Surface the relevant
error output so the user can decide how to proceed.
