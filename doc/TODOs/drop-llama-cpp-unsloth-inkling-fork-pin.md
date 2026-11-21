# Drop the llama.cpp pin for Inkling-2-Mini

Introduced by the commit adding
`hosts/host.thing/nixpkgs.overlays.llama-cpp-inkling.nix` (search git log
for "Inkling").

## Context

Inkling-2-Mini (`inkling2_loop` looped-transformer layout) merged into
mainline llama.cpp only after the v1.0.0 release that the pinned nixpkgs
builds:

- ggml-org/llama.cpp#32694 — "Inkling 2 Mini" (arch `inkling2_loop` +
  `inkling2`), merged 2026-11-20
- ggml-org/llama.cpp#32704 — "inkling2_loop share the loop slots after
  the scale pass" (fixes the loop-slot KV sharing), merged 2026-11-21

The overlay pins ALL upstream llama.cpp backends (base, `-vulkan`,
`-rocm` — they are plain `.override`s of `pkgs.llama-cpp`, so a single
`overrideAttrs` on the base repins the whole family; the
`myconfig.ai.llmops.llama-cpp.backendVersions` assertion enforces the sync)
to the #32704 merge commit
(`690b87d88151dbd775bb6eff5581663688785928`). This also moves the rtx INI
router binary and every `llama-server_<Device>_<Name>` wrapper to the
pinned build — that is intended and required for the Inkling entries to be
router-eligible.

## What to remove

1. `hosts/host.thing/nixpkgs.overlays.llama-cpp-inkling.nix` — the overlay
   re-pinning `pkgs.llama-cpp` (also carries the `npmRoot =
   "ui/apps/chat"` + `npmDepsHash` rework for the post-v1.0.0 UI
   restructure, which upstream packaging will need anyway once nixpkgs
   moves past v1.0.0, and the `cuda_nvml_dev` buildInputs addition — see
   the nvml note below).
2. Its import in `hosts/host.thing/default.nix`.
3. In `hosts/host.thing/myconfig.ai.llmops.llama-cpp/default.nix` and
   `Inkling-2-Mini.nix`: the header comments requiring the pin. The
   entries themselves stay — they are ordinary models (no `serverPackage`,
   no forced `devices`), served by the device-default builds on
   Vulkan0/CUDA0 (INI router + ad-hoc wrappers) and Vulkan0/ROCm0
   (llama-swap), exactly like the Nanbeige shared-name models.

## NVML header workaround

After v1.0.0, `ggml-cuda.cu` includes `<nvml.h>` unconditionally
(SKU-aware FP16/BF16 accumulation gating, ggml-org/llama.cpp#30672);
the NVML library is dlopen'd at runtime, only the header is needed at
build time. nixpkgs' `cudaBuildInputs` does not ship it, so the overlay
appends `cudaPackages.cuda_nvml_dev` to buildInputs unconditionally:
`cudaSupport` is a callPackage argument and not visible to overrideAttrs,
so a CUDA-only addition would not reach the `pkgs.llama-cpp.override {
  cudaSupport = true; }` build that `lib/devices.nix` constructs. The
package is header-only and does not drag the CUDA toolkit into the
non-CUDA backends' closures (verified: zero cuda* paths in the vulkan
build's closure). Drop that addition when nixpkgs' llama-cpp package
starts providing it (or when the pin is dropped and the stock package
builds past #30672).

## Removal condition

A nixpkgs llama.cpp release built from a commit at or past
`690b87d88151dbd775bb6eff5581663688785928` (i.e. containing both #32694
and #32704). Verify with:

    nix eval --raw .#nixosConfigurations.test-thing.config.myconfig.ai.llmops.llama-cpp.backendVersions.base.srcTag

(or check `pkgs.llama-cpp.version` against v1.0.0) and a load test of
`Inkling-2-Mini-Q8_0.gguf`.

## Why the unsloth root-layout files are NOT served

The `inkling2`-arch GGUFs in the root of
<https://huggingface.co/unsloth/Inkling-2-Mini-GGUF> (24 physical layer
blocks + loop-control tensors, `block_count = 48` in KV) are loadable
only by Unsloth's prebuilt binaries: their official build applies the PR
pinset (pr-set.json) at CI build time, so **no git tree exists** whose
build reads that layout — the `b12588-mix-c0c3de8` tag commit builds
without inkling support (verified: no `inkling` arch in the built
libllama.so; loading fails on `blk.24.attn_q.weight not found`).
Serve the ggml-org quants instead (this is also what Unsloth's own
`Mainline_Support/` README recommends for mainline builds).

## DSpark drafter (optional, currently off)

`--spec-type draft-dspark` exists in the pinned build, but the only
published drafter (`unsloth/Inkling-2-Mini-GGUF/dspark-Inkling-2-Mini-Q8_0.gguf`,
arch `dflash`) fails to load on mainline:

    check_tensor_dims: tensor 'markov_w1.weight' has wrong shape;
    expected 256, 200058, got 256, 201024

(the drafter stores the *padded* vocab, mainline expects *unpadded*).
Re-enable by flipping the `dspark` argument in `Inkling-2-Mini.nix` to
`true` once either:

- unsloth publishes a drafter compatible with mainline (or ggml-org
  publishes one), or
- upstream mainline accepts the padded vocab for the dspark markov head
  (check ggml-org/llama.cpp#32218 and follow-ups).

## Verification

- `nix eval --raw .#nixosConfigurations.test-thing.config.system.build.toplevel.drvPath`
  still evaluates after the removal.
- The Inkling entries show up on the rtx5090 provider (port 33656, INI
  router) and the gfx1151 llama-swap config, and the
  `llama-server_{Vulkan0,Vulkan1,CUDA0,ROCm0}_Inkling-2-Mini-*` ad-hoc
  wrappers exist.
- CPU smoke test (done for the pinned build, 2026-11-21):
  `Inkling-2-Mini-IQ4_NL.gguf` + `mmproj-Inkling-2-Mini-Q8_0.gguf`
  served fine with `--jinja`, 2K ctx, image input ingested.
