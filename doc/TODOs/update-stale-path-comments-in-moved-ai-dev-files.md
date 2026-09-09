# Update stale path comments in moved `myconfig.ai.dev` files

The move of `modules/myconfig.ai/{fns,skills,hermes-agent}/` to
`modules/myconfig.ai.dev/` (commit that introduced this file's parent change)
could not update the **relative-path references inside comments** of files
that are embedded in derivations, because any content change alters the
store hash and breaks snapshot-verified outPath equality.

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
