# Drop the cudaPackages nixpkgs-unstable pin

`cuda_gdb` and `cuda_sanitizer_api` (and with them `cuda-merged-13.4` and
the nvidia hosts' toplevel) could not be built at `nixpkgs` 09063bd1f2b3
(head of `nixos-unstable`, 2026-10-17): patchelf 0.19.2 aborts in
`shiftFile` (Assertion `splitIndex == -1' failed) when the
`addDriverRunpath` hook re-patches some CUDA 13.4 binaries, so both
builders die with exit code 134.

Fixed upstream by nixpkgs commit
`e59d8331a8728c061160dc7a087dce3dfd564cfb`
("cudaPackages: use patched patchelf to fix build failures"), which pins
patchelf 0.15.2 in `pkgs/development/cuda-modules/buildRedist/default.nix`.
The commit is on `nixpkgs-unstable` (and `master`, `staging`) but was not
yet on `nixos-unstable` when this pin was added.

## What to do

1. Check whether the `nixpkgs` input includes commit `e59d8331a872`:

   ```bash
   git -C ~/myconfig/nixos/nixpkgs branch -r --contains e59d8331a872
   ```

2. Once it does, remove:
   - `flake.nix`: input `nixpkgs-unstable`
     (`github:nixos/nixpkgs?ref=nixpkgs-unstable`) and its
     `mkSubPkgsOverlay "nixpkgs-unstable"` line.
   - `flake.pkgs_overrides.nix`: the `cudaPackages` `overrideScope` overlay
     that takes `cuda_gdb` / `cuda_sanitizer_api` from
     `pkgs.nixpkgs-unstable.cudaPackages_13_4`, plus its row in the header
     table.

3. Verify the two redists build (or are cached) from the main input:

   ```bash
   nix build --dry-run --impure -f - <<'EOF' >/dev/null
   let
     flake = builtins.getFlake "path:/home/mhuber/myconfig/myconfig";
     pkgs = flake.nixosConfigurations.test-thing.pkgs;
   in pkgs.cudaPackages.cudatoolkit
   EOF
   ```

The upstream FIXME (drop once patchelf PR #672 is in the default
patchelf) may land first; either way the override can go once the pinned
`nixos-unstable` rev contains `e59d8331a872`.
