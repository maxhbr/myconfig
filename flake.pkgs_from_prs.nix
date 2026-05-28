# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# ============================================================================
# Packages From Upstream PRs
# ============================================================================
#
# This repository can use packages from pending NixOS/nixpkgs pull requests
# before they are merged into `nixpkgs-unstable`. This is useful when a newer
# version of a package is needed but has not yet made it into the current
# nixpkgs channel.
#
# The mechanism uses a dedicated flake input pointing at a PR's head commit,
# plus a nixpkgs overlay that swaps the package out.
#
# ---
#
# ## Current Usage
#
# | Flake input | Package     | PR                                                 | Status   |
# | ----------- | ----------- | -------------------------------------------------- | -------- |
# | `pr523912`  | `llama-cpp` | <https://github.com/NixOS/nixpkgs/pull/523912>     | active   |
#
# (Commented-out example: `pr275479` → `freeplane` — see `flake.nix` for the
# template.)
#
# ---
#
# ## How It Works (Blueprint)
#
# The setup lives in `flake.nix` and has two parts: an **input declaration**
# and an **overlay module**.
#
# ### 1. Declare the PR as a Flake Input
#
# In the `inputs` block of `flake.nix`, add an entry whose name matches the PR
# number and whose URL targets the PR's head commit:
#
# ```nix
#   inputs = {
#     # ...
#
#     pr523912.url = "github:NixOS/nixpkgs/pull/523912/head";
#   };
# ```
#
# The URL format is always:
#
# ```
# github:NixOS/nixpkgs/pull/<PR_NUMBER>/head
# ```
#
# This pins the flake to the exact commit at the tip of the PR branch. The
# commit hash and narHash are recorded in `flake.lock`.
#
# ### 2. Add an Overlay That Imports the PR's Package Set
#
# In the nixosModules/homeModules configuration, add a special module that
# generates overlays for each PR-based package:
#
# ```nix
# (
#   { pkgs, config, ... }:
#   {
#     nixpkgs.overlays =
#       map
#         (
#           {
#             input,          # name of the flake input (e.g. "pr523912")
#             pkg,            # package attribute name (e.g. "llama-cpp")
#             maxVersion ? null,  # see "version guard" below
#           }:
#           let
#             prPkg  = import inputs."${input}" {
#               inherit (pkgs) config system;
#             };
#             prVersion = prPkg."${pkg}";
#           in
#           (_: super: {
#             "${pkg}" =
#               if maxVersion == null then
#                 prVersion                                     # always use PR
#               else if pkgs.lib.versionOlder (pkgs.lib.getVersion prVersion) maxVersion then
#                 prVersion                                     # PR version is newer
#               else
#                 super."${pkg}" or prVersion;                  # main nixpkgs caught up
#           })
#         )
#         [
#           { input = "pr523912"; pkg = "llama-cpp"; maxVersion = null; }
#         ];
#   }
# )
# ```
#
# Key points:
#
# - `import inputs."${input}" {...}` evaluates the PR's nixpkgs tree as a full
#   package set (using the same `system` and `config` as the host).
# - `prPkg."${pkg}"` extracts the single package from that tree.
# - The overlay replaces the package in the normal nixpkgs set with the PR
#   version.
#
# ### 3. Lock the PR Commit
#
# After adding a new input, run:
#
# ```bash
# nix flake update pr523912
# ```
#
# This fetches the PR head and records its `rev` and `narHash` in `flake.lock`.
#
# ---
#
# ## Version Guard (`maxVersion`)
#
# The `maxVersion` parameter controls when to stop using the PR version:
#
# | Value           | Behavior                                                         |
# | --------------- | ---------------------------------------------------------------- |
# | `null`          | **Always** use the PR version, even if main nixpkgs catches up.  |
# | `"x.y.z"`       | Use PR version **only** if it is newer than `maxVersion`.        |
# |                 | Once main nixpkgs reaches `>= maxVersion`, fall back to `super`. |
#
# Setting a version guard means the overlay will **automatically** stop using
# the PR once the upstream nixpkgs branch is at or past the target version.
# This is the recommended approach so the overlay becomes self-removing.
#
# Example:
#
# ```nix
# { input = "pr523912"; pkg = "llama-cpp"; maxVersion = "0.3.1"; }
# ```
#
# This says: "use the PR's llama-cpp as long as it's newer than 0.3.1; once
# main nixpkgs ships 0.3.1 or later, switch back to the main version."
#
# ---
#
# ## Adding a New PR-Based Package
#
# Follow these steps:
#
# 1. **Find the PR** on <https://github.com/NixOS/nixpkgs/pulls>.
# 2. **Add the input** in `flake.nix` inputs block:
#    ```nix
#    pr<NUMBER>.url = "github:NixOS/nixpkgs/pull/<NUMBER>/head";
#    ```
# 3. **Add an entry** to the overlay list in `flake.nix`:
#    ```nix
#    { input = "pr<NUMBER>"; pkg = "<package-name>"; maxVersion = null; }
#    ```
# 4. **Lock it**:
#    ```bash
#    nix flake update pr<NUMBER>
#    ```
# 5. **Test**:
#    ```bash
#    nix flake check
#    # or build for a specific host:
#    nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel
#    ```
#
# ---
#
# ## Checking If a PR Has Been Merged
#
# The PR input targets the `nixpkgs-unstable` branch (which is the default
# branch for NixOS/nixpkgs). Once the PR is merged, its changes are on that
# branch and the overlay becomes unnecessary.
#
# ### Method 1: GitHub UI
#
# Open the PR page: <https://github.com/NixOS/nixpkgs/pull/<NUMBER>>
#
# - If the page shows a green **"Merged"** badge, the PR is merged.
# - Check the **"Merged into NixOS:nixpkgs-unstable"** line to confirm the
#   target branch.
#
# ### Method 2: GitHub CLI
#
# ```bash
# gh pr view 523912 --json state,mergedAt,mergedCommit --jq '{state, mergedAt, sha: .mergedCommit.oid}'
# ```
#
# - `"state": "MERGED"` → PR is merged.
# - `"state": "OPEN"` → PR is still pending, keep using the overlay.
#
# ### Method 3: Compare the PR Package Version Against Main nixpkgs
#
# Evaluate the package version from both the PR input and the main nixpkgs
# input:
#
# ```bash
# # Version from the PR input
# nix eval --impure --raw --expr '
#   let prNixpkgs = import (builtins.getFlake "/home/mhuber/myconfig/myconfig#pr523912") { system = "x86_64-linux"; config = { allowUnfree = true; }; };
#   in builtins.getVersion prNixpkgs."llama-cpp"
# '
#
# # Version from the main nixpkgs input
# nix eval --impure --raw --expr '
#   let mainNixpkgs = import (builtins.getFlake "/home/mhuber/myconfig/myconfig#nixpkgs") { system = "x86_64-linux"; config = { allowUnfree = true; }; };
#   in builtins.getVersion (mainNixpkgs."llama-cpp" or null)
# '
# ```
#
# If both versions match (or the main version is newer), the PR has been
# merged and caught up. Set `maxVersion` to that version number to enable
# the overlay's auto-fallback.
#
# ### Cleanup After Merge
#
# Once a PR is merged and your `nixpkgs` input is updated past the merge point:
#
# 1. **Remove** the overlay entry from the list (or set `maxVersion` to the
#    merged version so it auto-fallbacks).
# 2. **Remove** the input from the `inputs` block.
# 3. Run `nix flake update` to clean up `flake.lock`.
#
# ---
#
# ## Notes
#
# - The PR input is evaluated as a full nixpkgs tree — it uses the same `system`
#   and `config` (e.g., `allowUnfree`) as the host nixpkgs.
# - Only packages available on the PR branch and compatible with your `system`
#   will work.
# - Keep `maxVersion = null` only temporarily; set a proper version guard when
#   you know the target version to enable self-cleanup.

{ inputs }: # inputs from flake scope; pkgs/config from module system
{
  pkgs,
  config,
  ...
}:

{
  nixpkgs.overlays =
    map
      (
        {
          input, # name of the flake input (e.g. "pr523912")
          pkg, # package attribute name (e.g. "llama-cpp")
          maxVersion ? null, # see "version guard" in README.pkgs_from_prs.md
        }:
        let
          prPkg = import inputs."${input}" {
            inherit (pkgs) config system;
          };
          prVersion = prPkg."${pkg}";
        in
        (_: super: {
          "${pkg}" =
            if maxVersion == null then
              prVersion
            else if pkgs.lib.versionOlder (pkgs.lib.getVersion prVersion) maxVersion then
              prVersion
            else
              super."${pkg}" or prVersion;
        })
      )
      [
        # { input = "pr275479"; pkg = "freeplane"; maxVersion = null; }
        {
          input = "pr523912";
          pkg = "llama-cpp";
          maxVersion = null;
        }
      ];
}
