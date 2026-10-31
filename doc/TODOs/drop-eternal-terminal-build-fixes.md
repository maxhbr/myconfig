# Drop the eternal-terminal build fixes in flake.pkgs_overrides.nix

Override in `flake.pkgs_overrides.nix` (the herdr part of the former
combined note is done — see "Triage 2026-10-31" below):

```nix
(_self: prev: {
  eternal-terminal = prev.eternal-terminal.overrideAttrs (_old: {
    doCheck = false;
    enableParallelBuilding = false;
  });
})
```

The earlier `postPatch` bumping `set(CMAKE_CXX_STANDARD 17)` → `20` was
dropped when nixpkgs started shipping `pkgs/by-name/et/eternal-terminal/
cmake-cxx-version.patch` (still present at the pinned rev, together with
`thread-pool-c++20-result-of.patch`).

`doCheck = false` and `enableParallelBuilding = false` are needed in the
mysbx build sandbox (`/run/mysbx-nix/state/builds`):

- the Catch2 integration tests spawn an et daemon that `chown`s its socket
  path, which fails with `Error: (22): Invalid argument` there
  (`FATAL ... UserTerminalRouter.cpp:12`);
- at `-j24` the C++20 PCH makes parallel `cc1plus` runs OOM the sandbox
  (`g++: fatal error: Killed signal terminated program cc1plus`).

## What to do

Remove the override once the build sandbox supports the chown-based
integration tests and the C++20 PCH memory footprint, i.e. drop
`doCheck = false`/`enableParallelBuilding = false` first and confirm
`nix build .#nixosConfigurations.test-f13.pkgs.eternal-terminal` passes
its tests there. If a future nixpkgs bump removes or renames the upstream
patches again, the override may shrink to nothing.

## Triage 2026-10-31

- herdr (`herdr = self.master.herdr`): REMOVED. The `nixpkgs` input was
  bumped past the upstream fix; pinned nixpkgs (1c89083e) and `master`
  both evaluate herdr to the same derivation
  (`3jwjwdimj6q5xz7q43nnlhcvhkndqvf1-herdr-0.9.5.drv`, output
  `qrzi3nn4g3pwhhd3zgxxg7n2jcz561kg-herdr-0.9.5` present on
  cache.nixos.org), so the override was a no-op. Split off into its own
  commit; this note now tracks only eternal-terminal.
- eternal-terminal: still BLOCKED. Dropping the override requires a live
  build inside the real mysbx build sandbox
  (`/run/mysbx-nix/state/builds` on f13). This worktree agent runs in a
  sandbox VM without that build environment (no writable /nix/store,
  no /run/mysbx-nix), so the test build cannot be reproduced here.
  Upstream state re-verified at pinned rev 1c89083e:
  `cmake-cxx-version.patch` and `thread-pool-c++20-result-of.patch`
  still applied, upstream `doCheck = true`.
- Next check: on f13 hardware, drop both flags temporarily and run
  `nix build .#nixosConfigurations.test-f13.pkgs.eternal-terminal`
  inside the mysbx build sandbox.

## How to verify

```bash
nix build .#nixosConfigurations.test-f13.pkgs.eternal-terminal
nix build --dry-run .#nixosConfigurations.test-f13.config.system.build.toplevel
```
