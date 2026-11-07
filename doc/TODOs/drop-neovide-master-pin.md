# Drop the neovide master pin

## What is pinned

`flake.pkgs_overrides.nix` overrides `pkgs.neovide` to the build from the
`master` flake input:

```nix
(_final: prev: {
  neovide = prev.master.neovide;
})
```

## Why (upstream bug)

The neovide 0.17.1 bump in nixpkgs (`1aeb44c24e`, merged 2026-11-04)
clobbered **both** `src.hash` and `cargoHash` with the hash of the *skia*
source tree (`sha256-ahqSduR4sCkcEmoo++PpxfcH0SZH4o6k/n8ZZmB1aA0=` — the
correct value for the `rust-skia/skia` fetch used by the same package, but
wrong for the neovide fetch).

Since fixed-output fetches are keyed by their hash, the "neovide" src fetch
and the skia fetch collapse onto the **same store path**, which contains
skia's tree. The build then unpacks skia instead of neovide and fails at the
vendor-staging step:

```
neovide> FileNotFoundError: [Errno 2] No such file or directory: 'Cargo.lock'
```

which cascades into dozens of `Cannot build … Reason: N dependencies failed`
errors for every neovide-dependent closure (home-manager user environments,
`system-units`, toplevel). Observed in
`../_logs/2026-11-07-myconfig-f13.log`.

Upstream fix: `2cc8df2292` ("neovide: fix source and cargo hashes",
2026-11-05, <https://github.com/NixOS/nixpkgs/commit/2cc8df2292>). It is
merged to master but the `nixos-unstable` branch head at lock time
(`f9a75f41be`, 2026-11-05 10:58 UTC) predates it, so
`nix flake update nixos-unstable` was a no-op.

## What to do

1. Check whether the `nixos-unstable` branch contains the fix:

   ```bash
   nix flake metadata github:NixOS/nixpkgs/nixos-unstable 2>&1 |
     grep Revision
   curl -s https://api.github.com/repos/NixOS/nixpkgs/compare/2cc8df2292...nixos-unstable |
     python3 -c 'import json,sys; print(json.load(sys.stdin)["status"])'
   # "identical" or "behind" => branch contains the fix
   ```

2. Once the branch has advanced past `2cc8df2292`, bump the input:

   ```bash
   nix flake update nixos-unstable
   ```

3. Delete the `neovide = prev.master.neovide;` overlay block in
   `flake.pkgs_overrides.nix` (search for "neovide:") and its row in the
   header table. Note the `master` input must stay pinned past
   `2cc8df2292` until step 2 is done — the override reads
   `pkgs.master.neovide`.

4. Verify:

   ```bash
   ./build-pkg-for-host.sh neovide f13
   nix build --dry-run .#nixosConfigurations.test-f13.config.system.build.toplevel
   ```

If nixpkgs later bumps neovide beyond 0.17.1, keeping the override would
pin the package to the `master` input's version instead of the main
nixpkgs one — do not keep it around past the fix.
