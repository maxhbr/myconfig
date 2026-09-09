<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# MVP 4 — the bubblewrap argv

Part of [`../plan.md`](../plan.md), phase 1. Depends on items 2 and 3.
**This item carries the security claim of the MVP** — review it against the
base table in `../plan.md`.

## Shape

One pure function, no I/O:

```rust
fn bwrap_argv(cfg: &Merged, repo: &Repo, payload: &Payload) -> Vec<String>
```

`Payload` is either an interactive shell or an explicit command vector
(`cli.md` D4: everything after `--` is passed verbatim and never parsed).
Nothing else may construct a `bwrap` invocation; item 5 prints this vector and
item 6 executes it.

## The argv, in order

1. `--clearenv`
2. `--unshare-all`, then re-share the network unless `network = false`
   (so a deny sandbox is recognisable by the *absence* of the share flag —
   assert both directions)
3. base binds: `/nix/store` ro, `/usr/bin` ro, `--proc /proc`, `--dev /dev`,
   `/etc/localtime` ro, tmpfs `/tmp`
4. the repo, `rw`, at its real host path
5. configured mounts, in declaration order, `--ro-bind` / `--bind`, with
   `dest` defaulting to the source path
6. environment: the forwarded host variables (`TERM COLORTERM LANG LC_ALL
   EDITOR VISUAL`, each only when set), then `[env]`, then `PATH` pointing at
   the MVP's dev-tool closure
7. `--chdir` into the repo
8. `--` and the payload

Not present, deliberately: `/run`, `~/tmp`, a host-backed `/tmp/<name>`, and
any automatic `OPENAI_API_KEY` forward. See the base table in `../plan.md` for
the reasoning behind each.

## Do

- [ ] Implement `bwrap_argv` and the `Payload` type.
- [ ] Golden tests, one file per case under `tests/assets/argv/`, compared
      byte for byte: minimal config; one `ro` mount; one `rw` mount; a mount
      with an explicit `dest`; `network = false`; an `[env]` entry; a sidecar
      that narrows the user config; the interactive payload; a `--` payload
      containing flag-looking arguments (`-x`, `--help`).
- [ ] A test asserting the *absence* of `/run` and of any `$HOME` bind beyond
      what a mount declares.

## Watch out

- Host paths in the golden files make the tests machine-dependent. Build the
  argv against a synthetic root (a `tempdir`) and normalise it in the
  comparison, or keep the fixtures parameterised by that root.
- Bubblewrap flag order is semantic for overlapping binds — do not let a
  refactor reorder sections 3–5.

## Done when

- Every base-table row has a corresponding assertion.
- Changing any base decision breaks exactly one visible golden diff.
