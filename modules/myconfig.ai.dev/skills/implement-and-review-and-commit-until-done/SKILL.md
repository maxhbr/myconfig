---
name: implement-and-review-and-commit-until-done
description: Iterative driver that repeatedly takes the next concrete step toward a high-level goal by calling the implement-and-review-and-commit cycle in a loop until the goal is met. Use when the user gives a larger goal and says "until done", "keep going until it's finished", or wants the work broken into small reviewed+committed increments automatically. The overall goal is passed as the arguments.
---

# Implement → Review → Commit, Until Done

An iterative driver toward the high-level goal given in the arguments. It does
**not** duplicate the implement/review/commit logic — each iteration reuses the
`implement-and-review-and-commit` skill for a single concrete step, producing
one small, reviewable commit per iteration.

The overall goal is whatever the user passed as arguments (appended below as
`User: <args>`).

## Process

Repeat the following loop:

### 1. Plan the next step

Assess the current state of the repo relative to the goal and identify the
**single next concrete step** that moves toward it. Keep it small enough to be
one coherent commit. If a rough plan helps, sketch the remaining steps on the
first iteration, but only commit to the next one.

### 2. Execute one cycle

Run the `implement-and-review-and-commit` skill (`/skill:implement-and-review-and-commit`)
for that single step, passing the step description as its arguments. This
performs implement → review → fix → commit for that increment. Do not
reimplement any of those phases here — delegate the whole cycle.

### 3. Re-evaluate

Decide whether the overall goal is now satisfied:

- **Goal met** → stop. Go to the summary.
- **Goal not met, and a useful next step exists** → loop back to step 1.
- **Goal not met, but no useful step can be identified** (blocked, ambiguous,
  needs a human decision) → stop and explain what's blocking.

### 4. Summarize

When the loop ends, summarize what was accomplished across the iterations:
list the commits produced (`git log --oneline <start>..HEAD`), what each one
did, whether the goal was fully reached, and any remaining/blocked work.

## Guardrails / stopping conditions

Stop the loop when **any** of these holds:

- The goal is met (primary success condition).
- No further useful step can be identified.
- A safety cap of **10 iterations** is reached — stop and report progress
  rather than looping forever. (Raise the cap only if the user explicitly asks
  to continue.)
- An iteration fails to produce a commit (implementation blocked, review found
  an unresolvable problem, or the `commit` skill refused). Stop and surface the
  reason; do not keep looping over a broken state.
- Two consecutive iterations make no meaningful progress toward the goal.

Each iteration must produce its own commit — keep increments small and
reviewable. Never batch multiple steps into one commit to "save time".
