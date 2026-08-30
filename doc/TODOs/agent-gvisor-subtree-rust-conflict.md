# agent-gvisor: Rust rewrite collides with the vendored git subtree

## Context

`modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/` is vendored from
<https://github.com/maxhbr/gvisor-agent-sandbox> via `git subtree` (see the
module `README.md`, "Origin"). The Rust rewrite of the CLI added
repo-local files INSIDE that subtree directory:

- `rust/` — the `agent-gvisor` crate (sources + cargo tests),
- `docs/spec.md` — the CLI contract the crate implements,
- `tests/agent-gvisor-cli-harness.sh` — the executed stub harness,
- `nix/checks.nix` — the flake-check derivations running the above,
- `nix/agent-gvisor.nix` — rewritten from `writeShellApplication` (bash
  text from `bin/agent-gvisor`) to `rustPlatform.buildRustPackage`
  wrapping `../rust`,
- `bin/agent-gvisor` (the bash CLI) was DELETED at the end of the rewrite
  (after stub-parity against the bash CLI was proven) — upstream still
  ships it, so a subtree pull re-adds it.

None of these exist upstream, so a future

```bash
git subtree pull --prefix=modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox \
    gvisor-agent-sandbox main
```

will conflict on `nix/agent-gvisor.nix` and `README.md` (both heavily
modified locally) and re-introduce upstream versions of `bin/agent-gvisor`,
`nix/agent-session.nix` and friends as new/modified files.

## What to do when pulling upstream

- Do NOT let the pull overwrite `rust/`, `docs/spec.md`,
  `tests/agent-gvisor-cli-harness.sh` or `nix/checks.nix` — these are
  repo-local; if `git subtree` proposes deleting them, that is a merge
  artifact, not an upstream change.
- Re-resolve `nix/agent-gvisor.nix` by hand: upstream changes to the bash
  script must be translated into the Rust crate (`modules/.../rust/src/`)
  and `docs/spec.md` first, then mirrored in the tests. The bash script
  itself stays deleted.
- After resolving, validate with
  `nix build .#checks.x86_64-linux.agent-gvisor-cargo-tests` and
  `nix build .#checks.x86_64-linux.agent-gvisor-cli-harness`
  (plus `./build-pkg-for-host.sh agent-gvisor-configured f13` on f13).

## Long-term fix

Move the Rust crate (and its tests) OUT of the vendored subtree directory —
e.g. to `pkgs/agent-gvisor/` or `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox-rust/`
— so `git subtree pull` only touches genuinely upstream files. Introducing
commit: the M1 commit of the `agent/gvisor/improve-agents` branch ("feat(ai.gvisor-agent-sandbox):
CLI specification for the Rust rewrite", i.e. the commit adding
`docs/spec.md` and this file).
