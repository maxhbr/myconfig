# Update stale path comments in moved `myconfig.ai.dev` files

This file tracks stale **relative-path references inside comments and
docs** that pile up as directories move from `modules/myconfig.ai/` into
`modules/myconfig.ai.dev/` (bd: myconfig-e4j). Each pure-file-move commit
must keep the moved files byte-identical to preserve snapshot-verified
`outPath` equality, so comment fixes are deferred here.

## Affected files (comments still reference pre-move relative paths)

- `modules/myconfig.ai.dev/fns/seed-agent-config.nix`
  - lines ~9, 56, 73, 136, 547: comments say `../myconfig.ai.microvm/...`
    and `../programs.herdr.nix`; correct paths are now
    `../myconfig.ai/myconfig.ai.microvm/...` and
    `../programs/programs.herdr.nix`.
  - This file is interpolated into
    `modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/default.nix:37`
    (`seedAgentConfig = ${../../myconfig.ai.dev/fns/seed-agent-config.nix};`)
    and copied into the store verbatim, so its content hash feeds directly
    into the `agent-qemu-pi` / `agent-qemu-herdr` runner derivation paths.
- `modules/myconfig.ai.dev/fns/bubblewrap-app.nix`
  - line ~35: comment says `../myconfig.ai.jail.nix`; correct path is
    `../myconfig.ai/myconfig.ai.jail.nix`.
  - This file is imported (not interpolated) by `myconfig.ai.jail.nix`,
    `myconfig.ai.workmux/jail.nix`, and `myconfig.ai.nono-agent-sandbox.nix`,
    so the comment is harmless at eval time, but it was left stale to keep
    the moved file byte-identical to its pre-move original.

- `modules/myconfig.ai.dev/programs/programs.herdr.nix` and
  `programs.herdr.README.md`: comments say `./myconfig.ai.workmux/jail.nix`;
  the workmux tree now lives at `../myconfig.ai.workmux/jail.nix` (moved by
  myconfig-e4j.4). Comments only; safe to fix in any commit that touches
  these files anyway.
- `modules/myconfig.ai.dev/mysbx/` (moved to here from
  `modules/myconfig.ai/mysbx/` by myconfig-e4j.4):
  - `default.nix` lines ~20, ~50-54, ~144, ~211, ~287, ~402, ~431, ~709,
    ~726: comments still say `../myconfig.ai.workmux/…`,
    `../../shell.…`, `../../programs.fish`, `./../programs.pi-coding-agent/`,
    `../programs.pi-coding-agent`, `../myconfig.ai.dev/fns/…`; correct paths
    are now `../myconfig.ai.workmux/…`, `../../../shell.…`,
    `../../../programs.fish`, `../programs/programs.pi-coding-agent/`,
    `../fns/…`.
    NOTE: `default.nix` is only imported (never interpolated verbatim), but
    the wrapper derivations it builds embed comment-free strings only —
    still, fix the comments together with the files above to keep one
    "stale comments" cleanup commit.
  - `nix/mysbx.nix`, `nix/mux-entry-lib.nix`, `nix/workmux-entry.nix`:
    comments still say `../../myconfig.ai.workmux/jail.nix`,
    `../../programs.pi-coding-agent`, `../../myconfig.ai.dev/fns/…`.
  - `README.md`, `docs/plan.md`, `docs/feature-comparison.md`,
    `docs/design/config.md`, `docs/TODOs/mvp-*.md`: relative doc links
    like `../myconfig.ai.workmux/mysbx.nix`,
    `../../myconfig.ai.dev/fns/…`, `../programs.pi-coding-agent/…`,
    `../../myconfig.ai.jail.nix`.
- `modules/myconfig.ai.dev/myconfig.ai.workmux/` (moved here from
  `modules/myconfig.ai/myconfig.ai.workmux/` by myconfig-e4j.4):
  - `mysbx.nix` line ~66: comment says `../mysbx/docs/design/config.md`;
    correct is `../mysbx/docs/design/config.md` — now actually CORRECT
    again after the move (mysbx moved too); verify and drop from this list
    in the cleanup commit if confirmed.
  - `sandbox.nix` line ~67: references option values only — fine.
- `modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix` line
  ~27 and `lib/detached-gui-launcher.nix` line ~17: comments reference
  `modules/myconfig.ai/myconfig.ai.workmux/…`, which is now
  `modules/myconfig.ai.dev/myconfig.ai.workmux/…` (moved by
  myconfig-e4j.4). `builders.nix` is embedded in a derivation, so fix it
  together with the files above in the same cleanup commit.
- Host comment references (`hosts/host.f13/ai.f13.nix`,
  `hosts/host.p14/ai.p14.nix`, `hosts/host.thing/default.nix`): comments
  say `modules/myconfig.ai/mysbx/README.md`; correct is now
  `modules/myconfig.ai.dev/mysbx/README.md`. Comments only.

## What to do

1. Fix the comment paths listed above (any commit that touches these files
   anyway can fold this in; no behavioral change is needed — comments only).
2. Expect the `agent-qemu-*` wrapper store paths to change in that commit;
   that is fine — it is a deliberate content change, not a regression.
3. Verify with the snapshot method from
   `AGENTS.md` (compare option slices before/after, excluding
   `pkgOutPaths` for `agent-qemu-*`).

## Related

- `modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix`
  intentionally still references `modules/myconfig.ai/fns/seed-agent-config.nix`
  in comments: that file lives in a directory copied wholesale into the store
  (`import ${./.}/runner.nix` in `default.nix`), so even comment edits there
  change the runner derivation output path. Update those comments together
  with the files above.
