---
name: apply-pr-to-flake
description: Apply a nixpkgs PR override to the flake.nix configuration
license: MIT
metadata:
  audience: nixos-admins
  workflow: pr-application
---

## What I do

Apply a nixpkgs PR to flake.nix by creating an input and overlay to override a specific package version. This follows the documented pattern in flake.nix lines 120-137.

## When to use me

Use this when you want to apply a PR from the main nixpkgs repository to override a package version in your flake without merging it upstream yet.

## Questions I'll ask

1. **PR number** - The pull request number (e.g., 500995)
2. **Repository** - Which nixpkgs repository to use (default: NixOS/nixpkgs)
3. **Package name** - Which nixpkgs package to override
4. **Input name** - Descriptive name for the input (e.g., pr500995, pr471984)

## Implementation steps

1. Get the PR commit hash by fetching PR details
2. Create the input in the `inputs` section after other inputs:
   ```
   {input}.url = "github:{owner}/{repo}/{commit-hash}";
   ```
3. Add the overlay in the PR patch section using the pattern:
   ```
   nixpkgs.overlays = map
     ({ input, pkg }:
     (_: _:
       { "${pkg}" =
         (import inputs."${input}" {
           inherit (pkgs) config system;
         })."${pkg}";
       })
     )
     [
       { input = "{input}"; pkg = "{package}"; }
     ];
   ```
4. Run `./nixfmtall.sh` to format all Nix files
5. Run `nix flake check` to validate the configuration

## Notes

- Follow the pattern exactly by placing the overlay module after the core system module and before other overlays
- Use descriptive input names: `pr<PR-number>` is preferred (e.g., pr500995)
- Comment out the URL in the template section for easy reference
- Always run `nix flake check` after making changes to ensure validity
- The overlay will only override the package; other packages remain from the main nixpkgs
- The entry should be placed near the other PR-related comments in the PR template section