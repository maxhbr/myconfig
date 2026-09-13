# Drop the workmux `nativeCheckInputs` tmux override

The workmux package (flake input `workmux`, pinned at `eb2867f` at the time
of writing) is consumed via two places that both add tmux to the package's
check phase:

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
