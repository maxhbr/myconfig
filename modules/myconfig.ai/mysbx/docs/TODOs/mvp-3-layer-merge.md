<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# MVP 3 — configuration layers and the narrow-not-widen rule

Part of [`../plan.md`](../plan.md), phase 1. Depends on item 1.

Produces the *merged config* that item 4 turns into an argv. Pure functions,
no process execution.

## The rule (`config.md` D6, D7)

Precedence, lowest first: built-in defaults → user config
(`$XDG_CONFIG_HOME/mysbx/config.toml`) → sidecar `config.toml` → flags.

The sidecar may **narrow**, never **widen**:

- **Mounts**: a sidecar mount is accepted only if its path is *at or below* a
  path the user config grants (prefix containment on canonicalized paths, at
  a path-component boundary — `/a/bc` is not below `/a/b`). Its mode may
  equal the granted mode or downgrade `rw` → `ro`, never upgrade.
- **Env**: the sidecar **may** introduce variables the user config never
  mentions, but **may not override** a variable the user config sets. The
  asymmetry is deliberate: an invented variable is a value the repo already
  controls, while overriding a user-set one is how a repo would redirect a
  tool at something the user did not choose.
- **Backend**: the sidecar may name a backend; it cannot be silently absent —
  see `cli.md` D7 (never auto-detected).
- **Network**: the sidecar may set `false`; it may not set `true` when the
  user config set `false`.

A violation is a hard error naming the offending key, the sidecar path and
the granting (or missing) user-config entry. Never a warning.

## Do

- [ ] `Config::load` for both layers, with "file absent" = empty layer for the
      user config and for the sidecar (item 2 guarantees the sidecar exists).
- [ ] Canonicalize every path eagerly and fail on a missing one
      (`config.md` D8) — before the merge, so error messages point at the
      layer that wrote the path.
- [ ] Implement the merge as `fn merge(user: Config, sidecar: Config) ->
      Result<Merged, Error>` returning a *distinct* `Merged` type, so no later
      code can accidentally consume an unmerged layer.
- [ ] Tests: containment accepted; sibling path rejected; component-boundary
      case (`/a/bc` vs `/a/b`); `ro` → `rw` upgrade rejected; `rw` → `ro`
      downgrade accepted; sidecar-only env accepted; env override rejected;
      `network` upgrade rejected.

## Watch out

- Canonicalization can turn a *granted* path into something outside the grant
  (symlinks). Compare canonicalized paths on both sides, and say so in the
  error.
- Do not sort or deduplicate mounts silently: mount order is argv order, and a
  later `rw` bind nested inside an earlier `ro` bind is a real pattern
  (`../../../fns/bubblewrap-app.nix` relies on it).

## Done when

- The merge is total: every rejection has a test and a message naming a file.
- `Merged` is the only input item 4 accepts.
