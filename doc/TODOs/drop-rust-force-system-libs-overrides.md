# Drop the Rust "force system libs" buildInputs overrides

## What is overridden

`flake.pkgs_overrides.nix` appends missing native library `buildInputs` to
six Rust packages:

| Package        | Added `buildInputs`                    |
| -------------- | -------------------------------------- |
| `codex`        | `aws-lc`, `oniguruma`, `sqlite`, `zstd` |
| `wprs`         | `zstd`                                  |
| `ccusage`      | `zstd`                                  |
| `aichat`       | `oniguruma`                             |
| `nono`         | `aws-lc`                                |
| `voxtype-onnx` | `oniguruma`, `sqlite`                   |

## Why (upstream bug)

nixpkgs commit `44cddf92c432`
("rustPlatform.cargoSetupHook: Force system libs",
<https://github.com/NixOS/nixpkgs/commit/44cddf92c432>) made
`cargoSetupHook` export, among others:

- `ZSTD_SYS_USE_PKG_CONFIG=1`
- `RUSTONIG_SYSTEM_LIBONIG=1`
- `AWS_LC_SYS_USE_SYSTEM=1`
- `LIBSQLITE3_SYS_USE_PKG_CONFIG=1`

so the `zstd-sys`, `onig_sys` and `aws-lc-sys` crate build scripts no longer
compile their vendored C code and instead demand the system library through
pkg-config. Any Rust package that does not list the lib in `buildInputs`
(its `.pc` file must land on `PKG_CONFIG_PATH`) fails with a build-script
panic such as:

```
codex> thread 'main' panicked at .../zstd-sys-2.0.16+zstd.1.5.7/build.rs:60:10:
codex> Can't probe for zstd in pkg-config: pkg-config exited with status code 1
nono> AWS_LC_SYS_USE_SYSTEM=1 requires a system-installed AWS-LC, but none was found.
aichat> Unable to find oniguruma in pkg-config, and RUSTONIG_SYSTEM_LIBONIG is set
```

First seen with the `nixos-unstable` input at `f4a03c0` (2026-11-07); every
other failure in the affected build
(`../_logs/2026-11-08-myconfig-thing.log`) is a cascading
`Reason: N dependencies failed` error (fish-completions, agent-bubblewrap
wrappers, home-manager, toplevel).

## What to do

Each package is fixed independently upstream; drop its entry as soon as the
pinned `nixos-unstable` branch contains the corresponding commit:

1. **codex** — fixed by `16b463698a5b` "codex: fix build with sys libs"
   (adds `aws-lc`, `oniguruma`, `sqlite`, `zstd`; mirrored 1:1 here).
2. **aichat** — fixed on master (`oniguruma` in `buildInputs`).
3. **wprs**, **ccusage**, **nono**, **voxtype-onnx** — no fix merged on any
   branch yet (as of 2026-11-09); file/track the upstream PRs, then wait
   for them to reach `nixos-unstable`.

Check and clean up per package:

```bash
# does the pinned branch contain the fix?
nix flake metadata github:NixOS/nixpkgs/nixos-unstable 2>&1 | grep Revision
# then, against the pinned rev, verify the package expression:
nix eval --raw github:NixOS/nixpkgs/nixos-unstable#legacyPackages.x86_64-linux.<pkg>.meta.position
```

Once a package's expression carries the missing `buildInputs` upstream,
delete its entry from the overlay block in `flake.pkgs_overrides.nix`
(search for "force system libs") and its rows in the header table, then:

```bash
./build-pkg-for-host.sh <pkg> thing
nix build --dry-run .#nixosConfigurations.test-thing.config.system.build.toplevel
```

Appending via `overrideAttrs` is idempotent once upstream ships the fix, but
it leaves duplicate `buildInputs` entries and a needless rebuild of the
package — do not keep entries past their upstream fix.
