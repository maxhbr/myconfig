<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# MVP 6 — Nix packaging and checks

Part of [`../plan.md`](../plan.md), phase 1. Depends on item 5. Last item, so
the package only ever wraps a finished CLI.

## Do

- [ ] `../../nix/mysbx.nix`: wrap the binary (`makeWrapper`) and set
      `MYSBX_BWRAP` to the absolute store path of `pkgs.bubblewrap`. The Rust
      side reads that variable and falls back to a `PATH` lookup, so
      `cargo run` and `cargo test` work in a dev shell without Nix wrapping.
- [ ] Bake the dev-tool closure: build the `PATH` value the argv uses
      (item 4, section 6) from an explicit package list in the Nix expression
      and pass it as `MYSBX_TOOLS_PATH`. Keep the list next to the base table
      in `../plan.md`; it is a security-relevant list, not packaging detail.
- [ ] Provide `bash` from that closure as the interactive payload
      (`MYSBX_SHELL`), so the bare form never depends on the host `$SHELL`.
- [ ] Wire `cargo test` into `nix flake check` for `x86_64-linux`, following
      `../../../myconfig.ai.gvisor-agent-sandbox/nix/checks.nix`.
- [ ] Rebuild the host that enables the module and confirm the wrapper works:
      `./build-pkg-for-host.sh mysbx-0.1.0 f13`.

## Explicitly not in this item

- No home-manager-generated user config (phase 2a in `../plan.md`).
- No executed bubblewrap in `nix flake check`. Running bwrap inside a nix
  check needs nested user namespaces and is environment-dependent; the argv
  golden tests are the CI gate, real execution is the operator's manual
  acceptance step.
- No change to any module outside `mysbx/`.

## Done when

- `nix flake check` covers the cargo tests.
- `mysbx run --dry-run -- ls /` from a real checkout shows the wrapped
  `bwrap` store path as argv[0].
- `git diff --stat` touches nothing outside `modules/myconfig.ai/mysbx/`.
