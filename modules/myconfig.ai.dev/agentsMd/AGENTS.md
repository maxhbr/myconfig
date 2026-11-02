# Nix

Repository-independent rules for working with Nix. A project's own `AGENTS.md`
carries the more specific rules and takes precedence over this file.

## Feedback loop

- `nix flake check` evaluates and builds every flake output for every system.
  That is far too slow as an edit/build loop: build only the attribute you
  touched (one host, one package, one test) and leave the full check to CI.
  Use `nix build --dry-run` first when the build would be large.
- Inspect a failed build with `nix log /nix/store/<drv> | grep <keyword>`
  instead of re-running it with more output.
- Look attributes up with `nix eval` rather than `nix flake show`, e.g.
  `nix eval .#foo --apply 'x: builtins.attrNames x'`.
- `nix run` runs software that is not installed; `nix-locate bin/ip` finds the
  package that provides a file.

## Editing flakes and expressions

- Flakes evaluate the git tree: `git add` every created file before evaluating
  or building. An untracked file does not exist for Nix.
- To pick up a change in a package, change the expression. `--rebuild` re-runs
  the cached derivation and does not see edited sources.
- Format Nix code with the project's formatter (`nix fmt` when the flake
  provides one, otherwise nixfmt-rfc-style).
- Prefer nix-provided language dependencies (e.g. `python3.withPackages`) over
  pip, venv or `npm install` inside a derivation.
- Inside `nix develop`/`nix-shell`, locate headers, libraries and tools through
  the environment (`env | rg /nix/store`, `$NIX_CFLAGS_COMPILE`,
  `$PKG_CONFIG_PATH`, `$buildInputs`) instead of guessing system paths.

## Foreign systems and remote builders

- Build another platform through its attribute, e.g.
  `nix build .#packages.aarch64-linux.hello`; for non-flake expressions use
  `nix-build --eval-system aarch64-linux`.
- aarch64-linux is reachable from x86_64 hosts through remote builders and
  binfmt emulation, so a foreign-architecture build (or a NixOS test for
  another platform) needs no locally installed cross toolchain.

## Patch files

To carry a fix for an upstream package: clone the source, apply the change,
produce the patch with `git format-patch`, and reference that patch file from
the package expression instead of vendoring a fork.
