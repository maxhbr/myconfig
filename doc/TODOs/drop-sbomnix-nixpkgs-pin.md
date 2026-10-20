# Drop the sbomnix nixpkgs pin

`sbomnix` is taken from a dedicated, frozen nixpkgs input instead of the
`nixpkgs` input:

- `flake.nix`: input `nixpkgs-sbomnix`
  (`github:nixos/nixpkgs/fe13242281a67641c0102cdb1087f0ebe586c7df`, the
  `nixpkgs` rev before the lock update `4c98baffc8`) and its
  `mkSubPkgsOverlay "nixpkgs-sbomnix"`.
- `flake.pkgs_overrides.nix`: overlay `sbomnix = self.nixpkgs-sbomnix.sbomnix;`
  and its row in the header table.

At `nixpkgs` 09063bd1f2b36fd9fd793c36dfade7bd40a93dfc (and at `master`
7db7fcbc / `nixos-unstable-small` f43dcf4b at the time) two dependencies of
sbomnix fail:

- `python3.14-reuse-6.2.0`: `failed to produce output path for output 'man'`
  — `pkgs/development/interpreters/python/hooks/sphinx-hook.sh` evaluates
  `sphinxBuilders` as a scalar, which under `__structuredAttrs` yields only
  `html`.
- `python3.14-sqliteschema-2.0.1`: `typepy<2,>=1.1.4 not satisfied by version
  2.0.0` (chain sbomnix → dfdiskcache → SimpleSQLite → sqliteschema).

## What to do

1. Check whether the current `nixpkgs` input builds sbomnix again (ideally it
   is binary-cached):

   ```bash
   p=$(nix eval --raw .#nixosConfigurations.test-f13.pkgs.nixos-unstable.sbomnix.outPath)
   nix path-info --store https://cache.nixos.org "$p"
   ```

2. Once it does, delete the overlay, its table row, the `mkSubPkgsOverlay`
   line and the `nixpkgs-sbomnix` input, then run
   `nix flake lock` so the input disappears from `flake.lock`.
3. Also re-check the `dfdiskcache` override in `flake.pkgs_overrides.nix`:
   it only existed for sbomnix and is a no-op for it while the pin is active.
4. Verify:

   ```bash
   nix build .#nixosConfigurations.test-f13.pkgs.sbomnix
   ./build-pkg-for-host.sh sbomnix f13
   ```
