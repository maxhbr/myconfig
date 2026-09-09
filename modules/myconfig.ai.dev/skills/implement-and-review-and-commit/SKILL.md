---
name: implement-and-review-and-commit
description: Run one full unit-of-work cycle for a task — implement the change, review it (reusing the code-review skill / a focused self-review), fix any issues found, then commit via the commit skill. Use when the user says "implement and review and commit", asks to do a task end-to-end, or wants a single implement→review→fix→commit pass. The task to perform is passed as the arguments.
---

# Implement → Review → Commit

A thin orchestrator that runs one complete unit-of-work cycle for the task
given in the arguments. It does **not** reimplement reviewing or committing —
it delegates to the existing `code-review` and `commit` skills.

The task/change to implement is whatever the user passed as arguments
(appended below as `User: <args>`).

## Process

### 1. Implement

Do the requested change. Read the relevant files first, follow the repo's
conventions (e.g. any `AGENTS.md` / `CONTRIBUTING.md` / coding standards
present), and make the smallest correct edit that satisfies the task. Keep the
change focused — this cycle should produce one coherent, reviewable increment.

Note the fixed point before you start editing so the review has a baseline:
capture `git rev-parse HEAD` (this is the "since" point for the review).

### 2. Review

Review the change you just made.

- **Preferred:** invoke the `code-review` skill by loading
  `/skill:code-review` with the fixed point from step 1 (the pre-edit
  `HEAD`). Let it run its two-axis (Standards + Spec) review against the diff.
  Pass the original task text as the spec when it asks for one.
- **Fallback:** if the `code-review` skill is unavailable or cannot run in
  this repo (e.g. it depends on tooling/issue-tracker that isn't set up), do a
  focused self-review of `git diff <fixed-point>...HEAD` against (a) the
  repo's documented coding standards and (b) the task's stated intent. Look
  for correctness bugs, missed requirements, scope creep, and obvious code
  smells.

### 3. Fix

Address every issue the review surfaced that is worth fixing (correctness,
standards violations, missed requirements). Re-review briefly if the fixes
were substantial. Do not expand scope beyond the original task.

If the review is clean, proceed directly.

### 4. Commit

Commit the result by loading and following the `commit` skill
(`/skill:commit`). This inherits the repository's commit-message conventions
and the model/harness attribution trailer. Do not hand-roll the commit — the
commit skill owns that logic.

## Guardrails

- One cycle = one focused change = ideally one commit.
- If implementation is blocked (ambiguous task, failing build you can't
  resolve, missing context), stop and explain rather than committing
  something broken.
- If the `commit` skill refuses (pre-commit hook failure, secrets, unrelated
  dirty files), surface its output and stop — do not force the commit.
