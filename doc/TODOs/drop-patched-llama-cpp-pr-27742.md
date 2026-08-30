# Drop the patched llama-cpp PR-27742 package once merged upstream

## Context

Qwen3.8-Flash-Next uses the `qwen4exp` model architecture, which is not
yet in an upstream llama.cpp release. Until it lands, the host carries a
pinned source overlay that fetches the PR head commit directly:

- **PR**: <https://github.com/ggml-org/llama.cpp/pull/27742>
  (`unslothai:qwen4exp/qwen3.8-flash-next`, head commit
  `ef6876693f058169161143dc8e301ac104b45373`)
- **Overlay**: `hosts/host.thing/nixpkgs.overlays.llama-cpp-pr-27742.nix`
  — exposes `pkgs.llama-cpp-pr-27742` (same source fetch shape as the
  pinned `llama-cpp` overlay, but pointing at the PR commit instead of a
  release tag).
- **Consumer**: `hosts/host.thing/myconfig.ai.llama-cpp/default.nix` —
  builds `patched-llama-cpp-pkg` (ROCm + Vulkan, no CUDA) and passes it
  per-model to the Flash-Next model definitions via the `serverPackage`
  option of the llama-cpp model submodule
  (`modules/myconfig.ai/myconfig.ai.llama-cpp/options.nix`).
- **Model files**: `hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.8-Flash-Next.nix`
  — sets `serverPackage = patched-llama-cpp-pkg` on both quantisations and
  their `:mmproj` variants.

Models with a non-null `serverPackage` are excluded from the router (the
`llama-server` service backend and the `llama-server_<Device>` INI-preset
wrappers) because the router uses a single `services.llama-cpp.package`
binary for every section and cannot mix per-model packages. They remain
available via llama-swap and the per-(model, device) script wrappers.

## When to act

Once PR #27742 is merged into llama.cpp's `master` and a release tag
(e.g. `b10xxx`) includes the `qwen4exp` architecture, the pinned overlay
and the per-model `serverPackage` overrides are no longer needed. The stock
nixpkgs `llama-cpp` (pinned by `hosts/host.thing/nixpkgs.overlays.llama-cpp.nix`
to the release tag) will then load Flash-Next GGUFs directly.

## Steps

1.  **Verify the merge**: confirm the `qwen4exp` architecture is present
    in the llama.cpp release tag that the host's
    `nixpkgs.overlays.llama-cpp.nix` pins (or will pin after the next
    bump):
    ```bash
    # in a checkout of the tag:
    grep -r "qwen4exp\|QWEN4EXP" src/llama-arch.cpp src/models/qwen4exp.cpp
    ```

2.  **Remove the overlay**:
    - Delete `hosts/host.thing/nixpkgs.overlays.llama-cpp-pr-27742.nix`.
    - Remove its import from `hosts/host.thing/default.nix` (the line
      `./nixpkgs.overlays.llama-cpp-pr-27742.nix`).

3.  **Remove the patched package and per-model override** in
    `hosts/host.thing/myconfig.ai.llama-cpp/default.nix`:
    - Delete the `patched-llama-cpp-pkg` binding (and its GPU-flag
      comment block).
    - Stop passing `serverPackage = patched-llama-cpp-pkg` to the
      `Qwen3.8-Flash-Next.nix` import (revert to
      `import ./Qwen3.8-Flash-Next.nix { inherit modelsPullDir; }`).

4.  **Remove the `serverPackage` parameter** from
    `hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.8-Flash-Next.nix`:
    - Revert the function header to `{ modelsPullDir }:`.
    - Remove the `inherit serverPackage;` lines from each model entry.

5.  **Optionally revert the `serverPackage` option** in
    `modules/myconfig.ai/myconfig.ai.llama-cpp/options.nix` and the
    `model.serverPackage != null` checks in `lib/scripts.nix` and `router.nix`
    — or keep them, since they are harmless when no model sets `serverPackage`.
    Keeping the option means future patched builds can use the same
    mechanism without re-implementing it.

6.  **Bump the pinned release tag** in
    `hosts/host.thing/nixpkgs.overlays.llama-cpp.nix` to a tag that
    includes the merged PR, and update the `hash` / `npmDepsHash`
    accordingly.

7.  **Verify**: build and switch the host, then load a Flash-Next model:
    ```bash
    llama-server_Vulkan1_Qwen3.8-Flash-Next-UD-IQ4_XS
    ```
    The log should no longer show
    `unknown model architecture: 'qwen4exp'`.
