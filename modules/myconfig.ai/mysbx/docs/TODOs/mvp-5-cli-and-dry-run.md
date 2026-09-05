<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# MVP 5 — the CLI surface

Part of [`../plan.md`](../plan.md), phase 1. Depends on item 4.

## The surface

```
mysbx [--dry-run]                 enter an interactive sandbox shell
mysbx run [--dry-run] -- CMD...   run one command in the sandbox
mysbx init                        create the sidecar (idempotent)
mysbx version | help
```

`--dry-run` is a **global flag**, valid for the bare form and for `run`. It
runs the whole pipeline — resolve the repo, load and merge both layers,
canonicalize, apply the narrow-not-widen rule — and stops immediately before
`exec`. A dry run that skipped validation would exercise the wrong function.

Output of `--dry-run`: the argv, **one argument per line, on stdout**, with no
`## ` prefix and no quoting. It is the result, not a diagnostic
(`cli.md` D9 keeps stdout for results), so `mysbx run --dry-run -- ls | wc -l`
is meaningful and the golden tests compare bytes.

## Do

- [ ] Extend the hand-rolled parser in `../../mysbx-rs/src/lib.rs`: the global
      `--dry-run`, the `run` verb, and the `--` split. Everything after `--`
      is verbatim, including things that look like flags (`cli.md` D4, D5).
- [ ] Update `../../mysbx-rs/src/usage.txt` in the same commit — there is no
      derive macro keeping them in sync (`cli.md` D5), and the test below is
      the only guard.
- [ ] Exit codes (`cli.md` D8): `0` success, `1` runtime failure (no git repo,
      `$HOME`/`/` guard, missing mount path, widening sidecar, backend failed
      to start), `2` usage error. The payload's own code is propagated
      unchanged.
- [ ] Diagnostics to stderr with the `mysbx: ` prefix; `init` progress to
      stdout with `## `.
- [ ] `run` without `--` and without a command is a usage error (`2`), not an
      empty sandbox.
- [ ] Tests: a `--dry-run` byte-comparison per payload form; every exit code
      reachable through `run(argv)`; a test asserting each accepted flag and
      verb appears in `usage.txt`.

## Watch out

- `mysbx --dry-run` (bare) and `mysbx run --dry-run` must produce the same
  argv apart from the payload — one code path, two entry points.
- Do not let `--dry-run` appear after `--`; there it is payload.

## Done when

- Every command in the surface above has a test asserting its exit code.
- `usage.txt` and the parser cannot drift without a red test.
