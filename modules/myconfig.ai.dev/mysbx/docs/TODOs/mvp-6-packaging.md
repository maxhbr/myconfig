<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# MVP 6 — Nix packaging and checks

Part of [`../plan.md`](../plan.md), phase 1. Depends on item 5. Last item, so
the package only ever wraps a finished CLI.

## Do

- [x] `../../nix/mysbx.nix`: wrap the binary and set `MYSBX_BWRAP` to the
      absolute store path of `pkgs.bubblewrap`
      (`/nix/store/...-bubblewrap-<ver>/bin/bwrap`). The Rust side reads that
      variable and falls back to a `PATH` lookup, so `cargo run` and
      `cargo test` work in a dev shell without Nix wrapping.
- [x] Bake the dev-tool closure: build the `PATH` value the argv uses
      (item 4, section 6) from an explicit package list in the Nix
      expression and pass it as `MYSBX_TOOLS_PATH`. The list lives as the
      `toolsEnv` buildEnv in `../../nix/mysbx.nix` (bash, coreutils,
      findutils, gnugrep, gnused, gawk, which, less, procps, hostname,
      ripgrep, fd, jq, git, nix, python3, curl); plan.md leaves the exact
      contents to this item, and joining the shared
      `myconfig.ai.sandboxTools` option is phase 2d there.
- [x] Provide `bash` from that closure as the interactive payload
      (`MYSBX_SHELL` = the wrapper's `pkgs.bash` — bashInteractive), so the
      bare form never depends on the host `$SHELL`.
- [x] Wire `cargo test` into `nix flake check` for `x86_64-linux`, following
      `../../../myconfig.ai.gvisor-agent-sandbox/nix/checks.nix`: the check
      `mysbx-tests` (`../../nix/checks.nix`) is the crate with
      `doCheck = true`, imported from `flake.nix` like the gvisor tier's
      check set. No bwrap on the test PATH — the real-execution tests in
      `cargo test` skip without one, and executed bubblewrap stays out of
      CI on purpose.
- [x] Rebuild the host that enables the module and confirm the wrapper
      works: `./build-pkg-for-host.sh mysbx-0.1.0 f13`, plus
      `result/bin/mysbx --help`, `mysbx run --dry-run -- ls /` from a real
      checkout (argv[0] is the wrapped `bwrap` store path) and a real
      `mysbx run -- /usr/bin/env true` in a throwaway git repo under /tmp.

## Explicitly not in this item

- No home-manager-generated user config (phase 2a in `../plan.md`).
- No executed bubblewrap in `nix flake check`. Running bwrap inside a nix
  check needs nested user namespaces and is environment-dependent; the argv
  golden tests are the CI gate, real execution is the operator's manual
  acceptance step.
- No change to any module outside `mysbx/`.

## Done when

- [x] `nix flake check` covers the cargo tests (the `mysbx-tests` check,
      x86_64-linux only, like the microvm and gvisor check sets).
- [x] `mysbx run --dry-run -- ls /` from a real checkout shows the wrapped
      `bwrap` store path as argv[0].
- [x] `git diff --stat` touches nothing outside `modules/myconfig.ai.dev/mysbx/`.
      Exception, blessed by the reviewer: one six-line hunk in `flake.nix`
      imports this item's check set — the exact pattern the gvisor tier's
      checks.nix commit already established.
