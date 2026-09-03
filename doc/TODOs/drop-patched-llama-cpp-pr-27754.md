# Drop the patched llama-cpp PR-27754 package once merged upstream

## Context

GLM-5.3-Flash uses the `glm5next` model architecture, which is not yet
in an upstream llama.cpp release. Until it lands, the host carries a
pinned source overlay that fetches the PR head commit directly:

- **PR**: <https://github.com/ggml-org/llama.cpp/pull/27754>
  ("model: add GLM-5-Next (GLM-5.3-Flash)",
  `unslothai:glm5next/upstream`). The overlay pins head commit
  `f30bed88717059d8a4728864c88f8abad8d329a0`; the PR head has since
  moved on (checked 2026-09-03: `949f7efb097eb20ef36fecdb1afaebff9a4ae7ed`),
  so `rev`/`hash` in the overlay no longer track the tip of the branch.
- **Overlay**: `hosts/host.thing/nixpkgs.overlays.llama-cpp-pr-27754.nix`
  — exposes `pkgs.llama-cpp-pr-27754` (same source fetch shape as the
  pinned `llama-cpp` overlay, but pointing at the PR commit instead of a
  release tag).
- **Consumer**: `hosts/host.thing/myconfig.ai.llama-cpp/default.nix` —
  builds `patched-llama-cpp-pr-27754-pkg` (ROCm + Vulkan, no CUDA) and
  passes it per-model to the GLM-5.3-Flash model definitions via the
  `serverPackage` option added to the llama-cpp model submodule
  (`modules/myconfig.ai/myconfig.ai.llama-cpp/options.nix`).
- **Model files**: `hosts/host.thing/myconfig.ai.llama-cpp/GLM-5.3-Flash.nix`
  — sets `serverPackage = patched-llama-cpp-pr-27754-pkg` on the
  UD-Q2_K_XL quantisation and its `:mmproj` variant.

Models with a non-null `serverPackage` are excluded from the router
(the `llama-server` service backend and the `llama-server_<Device>`
INI-preset wrappers) because the router uses a single
`services.llama-cpp.package` binary for every section and cannot mix
per-model packages. They remain available via llama-swap and the
per-(model, device) script wrappers.

## When to act

Once PR #27754 is merged into llama.cpp's `master` and a release tag
(e.g. `b10xxx` or `v0.3.x`) includes the `glm5next` architecture, the
pinned overlay and the per-model `serverPackage` overrides are no
longer needed. The stock nixpkgs `llama-cpp` (pinned by
`hosts/host.thing/nixpkgs.overlays.llama-cpp.nix` to the release tag)
will then load GLM-5.3-Flash GGUFs directly.

## Steps

1.  **Verify the merge**: confirm the `glm5next` architecture is
    present in the llama.cpp release tag that the host's
    `nixpkgs.overlays.llama-cpp.nix` pins (or will pin after the next
    bump):
    ```bash
    # in a checkout of the tag:
    grep -r "glm5next\|GLM5NEXT" src/llama-arch.cpp src/models/
    ```

2.  **Remove the overlay**:
    - Delete `hosts/host.thing/nixpkgs.overlays.llama-cpp-pr-27754.nix`.
    - Remove its import from `hosts/host.thing/default.nix` (the line
      `./nixpkgs.overlays.llama-cpp-pr-27754.nix`).

3.  **Remove the patched package and per-model override** in
    `hosts/host.thing/myconfig.ai.llama-cpp/default.nix`:
    - Delete the `patched-llama-cpp-pr-27754-pkg` binding (and its
      GPU-flag comment block).
    - Stop passing `serverPackage =
      patched-llama-cpp-pr-27754-pkg` to the `GLM-5.3-Flash.nix`
      import (revert to
      `import ./GLM-5.3-Flash.nix { inherit modelsPullDir; }`).

4.  **Remove the `serverPackage` parameter** from
    `hosts/host.thing/myconfig.ai.llama-cpp/GLM-5.3-Flash.nix`:
    - Revert the function header to `{ modelsPullDir }:`.
    - Remove the `inherit serverPackage;` line from the model entry.

5.  **Optionally revert the `serverPackage` option** in
    `modules/myconfig.ai/myconfig.ai.llama-cpp/options.nix` and the
    `model.serverPackage != null` checks in `lib/scripts.nix` and
    `router.nix` — or keep them, since they are harmless when no model
    sets `serverPackage`. Keeping the option means future patched
    builds can use the same mechanism without re-implementing it.

6.  **Bump the pinned release tag** in
    `hosts/host.thing/nixpkgs.overlays.llama-cpp.nix` to a tag that
    includes the merged PR, and update the `hash` / `npmDepsHash`
    accordingly.

7.  **Verify**: build and switch the host, then load a GLM-5.3-Flash
    model:
    ```bash
    llama-server_Vulkan1_GLM-5.3-Flash-UD-Q2_K_XL
    ```
    The log should no longer show
    `unknown model architecture: 'glm5next'`.

## Triage 2026-09-03

Not actionable yet. Verified via the GitHub API and raw.githubusercontent:

- PR #27754 is still **open** (created 2026-08-26, last updated
  2026-09-01, 38 commits, mergeable_state `unstable`, zero review
  comments).
- `glm5next` is **absent** from `master` and from every checked tag:
  the host pin `b10549`, newer prereleases `b10646`/`b10700`/`b10710`/
  `b10720`/`b10730`, and the stable release `v0.3.0` (2026-08-25).
  (`grep -c glm5next src/llama-arch.cpp` = 0 on all of them; the PR
  head commit returns 4 matches, as expected.)
- All code references in this note still match the tree: overlay
  `hosts/host.thing/nixpkgs.overlays.llama-cpp-pr-27754.nix`, the
  import line in `hosts/host.thing/default.nix`, the
  `patched-llama-cpp-pr-27754-pkg` binding and
  `glm53_flash` import in
  `hosts/host.thing/myconfig.ai.llama-cpp/default.nix`, the
  `serverPackage` parameter in
  `hosts/host.thing/myconfig.ai.llama-cpp/GLM-5.3-Flash.nix`, and the
  `serverPackage` option plus the `model.serverPackage != null`
  checks in `modules/myconfig.ai/myconfig.ai.llama-cpp/{options.nix,
  lib/scripts.nix, router.nix}`. Introduced by commit
  `c001e0044b`.
- The pinned PR head (`f30bed88`) is stale relative to the branch tip
  (`949f7efb`) — harmless while the pin works, but a fresh bump must
  re-fetch `rev`/`hash`.

Related but separate: sibling PR #27742 (qwen4exp,
 `drop-patched-llama-cpp-pr-27742.md`) **was merged** into master on
 2026-08-27 (commit `6c84c7d5d`); that note should be triaged on its
  own — a release bump for #27742 may also change the base the #27754
  overlay builds on.
