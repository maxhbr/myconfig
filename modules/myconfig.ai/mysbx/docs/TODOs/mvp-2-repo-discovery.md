<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# MVP 2 — repo discovery, guard, implicit init

Part of [`../plan.md`](../plan.md), phase 1. Depends on item 1.

Everything here is pure path logic and belongs in one module
(`../../mysbx-rs/src/repo.rs`) with unit tests over a temporary directory
tree — no bubblewrap, no config.

## The resolution rule

From the current directory, in order:

1. Walk up; the first ancestor `<dir>` with an **existing** `<dir>.mysbx`
   directory wins.
2. Otherwise the **git work-tree root** (the nearest ancestor containing
   `.git`, file or directory, so worktrees and submodules resolve).
3. Otherwise the **current directory** itself.

The sidecar is always `<repo>.mysbx`; repo and sidecar determine each other.

## Do

- [ ] Implement the three-step resolution as a pure function over a starting
      path, plus a thin wrapper reading the real CWD.
- [ ] Hard error (exit `1`, `mysbx: ` prefix) when the resolved repo is
      `$HOME` or `/`. Not overridable in the MVP. This is the guard the
      existing wrappers call `rejectHomeCwd`
      (`../../../fns/bubblewrap-app.nix`); under implicit init it prevents a
      `rw` bind of the whole home under a policy nobody wrote.
- [ ] Make `init` use the resolved repo instead of the raw CWD, and keep it
      idempotent (`config.md` D12).
- [ ] Implicit init: the bare form creates a missing sidecar, reports it with
      the `## ` prefix, then continues.
- [ ] Tests: sidecar found in an ancestor; no sidecar but a `.git` two levels
      up; neither; `$HOME` rejected; `/` rejected; a `.git` *file* (worktree)
      resolves.

## Watch out

- Step 1 beats step 2 on purpose: an existing sidecar is a policy the user
  wrote, and it must not be shadowed by a `.git` closer to the CWD (nested
  repos, submodules).
- Canonicalize before comparing against `$HOME`, otherwise a symlinked home
  slips past the guard.

## Done when

- The resolution function has tests for all three branches and both guards.
- `mysbx init` in a subdirectory creates the sidecar next to the *git root*.
