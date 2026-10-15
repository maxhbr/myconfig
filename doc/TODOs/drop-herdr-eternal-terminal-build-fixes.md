# Drop the herdr and eternal-terminal build fixes in flake.pkgs_overrides.nix

Two overlays in `flake.pkgs_overrides.nix` work around build failures of
nixpkgs packages at the locked `nixpkgs` revision
(9ae611a455b90cf061d8f332b977e387bda8e1ca) on host f13 (and any other host
pulling these packages):

## herdr

Override: `herdr = self.master.herdr;`

At the locked rev the package fails at link time:

```
binutils-2.46/bin/ld.bfd: .eh_frame_hdr refers to overlapping FDEs
binutils-2.46/bin/ld.bfd: final link failed: bad value
```

when linking the auditable-build object against the static `libghostty-vt`
produced by the vendored zig build. nixpkgs master (and `nixos-unstable-small`)
fixed this with a `postPatch` in `pkgs/by-name/he/herdr/package.nix` that sets
`lib.bundle_compiler_rt = false;` and `lib.bundle_ubsan_rt = false;` in
`vendor/libghostty-vt/src/build/GhosttyLibVt.zig`. The override takes the whole
package from the `master` channel (`pkgs.master`, exposed by the
`mkSubPkgsOverlay`s in `flake.nix`); that derivation
(`/nix/store/wsaf3skg…-herdr-0.9.1.drv`) is binary-cached on
cache.nixos.org, so nothing is built locally.

**What to do:** remove the override once the `nixpkgs` input is bumped past the
commit adding that `postPatch` (herdr rev that contains it: check
`nix eval --raw .#nixosConfigurations.test-f13.pkgs.herdr.drvPath` against
`nix eval --raw github:nixos/nixpkgs#herdr.drvPath` — once both print the same
drv, the override is a no-op and can be deleted).

## eternal-terminal

Override (as of the nixpkgs bump that ships
`pkgs/by-name/et/eternal-terminal/cmake-cxx-version.patch`): only
`doCheck = false; enableParallelBuilding = false;` remain. The earlier
`postPatch` bumping `set(CMAKE_CXX_STANDARD 17)` → `20` had to be dropped:
upstream now patches exactly that line, so `substituteInPlace
--replace-fail "set(CMAKE_CXX_STANDARD 17)" ...` found nothing to replace
and aborted `patchPhase` with
`pattern set\(CMAKE_CXX_STANDARD\ 17\) doesn't match anything`.

`doCheck = false` and `enableParallelBuilding = false` are needed in the
mysbx build sandbox (`/run/mysbx-nix/state/builds`):

- the Catch2 integration tests spawn an et daemon that `chown`s its socket
  path, which fails with `Error: (22): Invalid argument` there
  (`FATAL ... UserTerminalRouter.cpp:12`);
- at `-j24` the C++20 PCH makes parallel `cc1plus` runs OOM the sandbox
  (`g++: fatal error: Killed signal terminated program cc1plus`).

**What to do:** remove the override once the build sandbox supports the
chown-based integration tests and the C++20 PCH memory footprint, i.e. drop
`doCheck = false`/`enableParallelBuilding = false` first and confirm
`nix build .#nixosConfigurations.test-f13.pkgs.eternal-terminal` passes its
tests there. If a future nixpkgs bump removes or renames the upstream patches
again, the override may shrink to nothing.

## How to verify

```bash
# both packages alone
nix build .#nixosConfigurations.test-f13.pkgs.herdr
nix build .#nixosConfigurations.test-f13.pkgs.eternal-terminal
# whole host, must no longer list the herdr/eternal-terminal drv failures
nix build --dry-run .#nixosConfigurations.test-f13.config.system.build.toplevel
```
