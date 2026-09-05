<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# MVP 1 — schema change and design docs

Part of [`../plan.md`](../plan.md), phase 1. **No behaviour change**: this item
only makes the schema and the design docs describe the tool that items 2–6
build. Do it first, atomically, so no later item has to argue against a stale
doc.

## Do

- [ ] `../../mysbx-rs/src/config.rs`: remove the `Repo` struct and the
      `Config::repo` field. A `[repo]` table is now an unknown key, i.e. a
      schema error (the strict-parsing rule of `config.md` D11 already covers
      it). The repo is implicit — see item 2.
- [ ] `../../mysbx-rs/src/config.rs`: change the default of `network` from
      `false` to `true`. Keep the key: `network = false` is the deny switch.
- [ ] `../../mysbx-rs/src/lib.rs`: `init` writes a sidecar `config.toml`
      without a `[repo]` table — a comment header plus commented-out examples
      of `[[mounts]]` and `[env]`.
- [ ] `../../mysbx-rs/tests/assets/valid/{minimal,full,user-config}.toml`:
      drop `[repo]`; `minimal.toml` becomes what the new `init` writes.
- [ ] `../../mysbx-rs/tests/assets/invalid/`: add `schema-repo-table.toml`
      (a `[repo]` table must now be rejected) and keep the existing cases
      passing.
- [ ] `../design/config.md`: rewrite **D9**. It currently claims default deny
      including "no network"; the MVP shares the network by default and has a
      permissive base. State the real claim (strong accident barrier, moderate
      malice barrier) and keep the part that survives: nothing from the host
      filesystem is available unless declared.
- [ ] `../design/config.md`: adjust **D5** (the sidecar no longer decides
      "which repos are mounted") and add a new decision **D13: the repo is
      implicit** — it is the sidecar's repo, always mounted `rw` at its real
      path, not expressible in configuration. Rationale: a config that could
      name a different repo creates a contradiction class between the sidecar
      location and its content.
- [ ] `../design/cli.md`: close the **D2** open question — bare `mysbx`
      creates the sidecar implicitly when it is missing.
- [ ] `../../README.md`: update the transcript if the `init` output changed.
- [ ] `../feature-comparison.md`: update the `mysbx` column cells this item
      invalidates (config schema, network default, workspace row).

## Done when

- `cargo test` passes with the new assets.
- No design doc still mentions `[repo]` or a default-deny network.
- `git grep -n 'repo' ../../mysbx-rs/src` finds only comments about the
  *implicit* repo.
