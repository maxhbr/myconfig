# Drop the workmux `nativeCheckInputs` tmux override

The workmux package (flake input `workmux`, pinned at `eb2867f` when this
note was written; the pin has since moved — see Triage below — without any
change to the packaging) is consumed via two places that both add tmux to
the package's check phase:

- `modules/myconfig.ai.dev/myconfig.ai.workmux/default.nix` —
  `workmuxPkg` (feeds `myconfig.ai.dev.workmux.package` and thereby all
  jail / mysbx / microvm / home.packages consumers)
- `modules/myconfig.ai.dev/sandboxes/myconfig.ai.qemu-agent-sandbox/default.nix`
  — the standalone `runnerExpression` embeds the package in a `writeText`,
  so it must pin the same (patched) store path; the override is duplicated
  there on purpose.

## Why it exists

Upstream's `flake.nix` builds workmux with `doCheck = true` but
`nativeBuildInputs = [ installShellFiles pkgs.git ]` — no tmux. Since
upstream commit `eb2867fd` ("fix dashboard removal across projects (#272)",
pulled in by the 2026-09-13 flake.lock update) the test
`workflow::remove::tests::remove_uses_context_repository_not_process_cwd`
(`src/workflow/remove.rs`) execs `tmux has-session` via
`TmuxBackend::is_running()`. Without tmux on the build sandbox's PATH the
exec fails with ENOENT and the whole `cargo test` run fails —
deterministically on every remote builder / CI (`ssh-ng://nixBuild@builder.workstation`),
and only accidentally passing on interactive dev machines where tmux is in
the ambient PATH. Upstream's CONTRIBUTING.md documents "tmux (required for
tests)" but the nix packaging does not provide it.

Introduced by commit 79bdd1d43b on branch `f13-build-failure` (upstream issue
tracked as bd myconfig-wrd).

## What to do

Remove the `overrideAttrs` blocks (keep the plain
`inputs.workmux.packages.${system}.default`) once the locked workmux
revision builds its package with tmux available in the check phase — e.g.
upstream adds `pkgs.tmux` to `nativeBuildInputs`/`nativeCheckInputs`, or
sets `doCheck = false`, or the failing test is made hermetic / skipped
without tmux.

## How to verify

- `nix build .#nixosConfigurations.test-f13.config.system.build.toplevel
  --dry-run` must succeed.
- Direct check: build the workmux derivation alone, e.g.

  ```bash
  nix build --impure --expr '(builtins.getFlake "git+file://'"$PWD"'").inputs.workmux.packages.x86_64-linux.default'
  ```

  must complete including its `cargo test` run (check the log for
  `remove_uses_context_repository_not_process_cwd ... ok`).

## Flaky test skip (`checkFlags`)

Both overrides also pass
`--skip=multiplexer::tmux::tests::run_shell_preserves_literal_tmux_formats`
(`src/multiplexer/tmux.rs`). That test returns early without tmux, so it
only runs because of the tmux override, and it races: it stops polling as
soon as `format-output` exists, but the shell's `>` creates the file before
`printf` writes to it (failed on `builder.workstation` with workmux
`761ad3b`; the same test passed at `1d5e7da`). Drop the skip together with
the tmux override, or earlier once upstream waits for the file's content
instead of its existence.

## Triage 2026-10-31

Pin moved again — checked the drop condition at the new locked rev and at
upstream `main`; NOT met.

- flake.lock `workmux` rev: `23c09d88a1bce1b63c2d894439bb79915dd71901`
  (bumped `3cef212` → `23c09d8` in commit 9853c47162, 2026-10-25).
- Upstream `flake.nix` at 23c09d8 and at `main` (raw.githubusercontent.com):
  still `nativeBuildInputs = [ pkgs.installShellFiles pkgs.git ]`, no tmux
  in build/check inputs, no `doCheck = false`.
- `src/workflow/remove.rs` at 23c09d8: `remove_uses_context_repository_not_process_cwd`
  still drives `BackendType::Tmux` → `TmuxBackend::is_running()` →
  `tmux has-session`.
- `src/multiplexer/tmux.rs` at 23c09d8: `run_shell_preserves_literal_tmux_formats`
  still polls `output.exists()` — the race (read between `>` and `printf`)
  is not fixed, so the `checkFlags` skip must stay too.
- Override verified only by store presence (workmux-23c09d8 built via a
  home.packages consumer); `nix eval`/`nix build` are not runnable in this
  sandbox (read-only /nix/store, no daemon socket). Full build verification
  only applies once the drop condition is met.
- This note remains the permanent tracking doc per the close reason of bead
  myconfig-wrd (won't file upstream; keep the override).
- Next check: next workmux pin bump.
