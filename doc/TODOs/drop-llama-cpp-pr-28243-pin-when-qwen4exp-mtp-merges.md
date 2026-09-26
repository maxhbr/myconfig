# Drop the `llama-cpp-pr-28243` pin once upstream merges qwen4exp MTP

Upstream PR <https://github.com/ggml-org/llama.cpp/pull/28243> ("models:
Qwen3.8-Flash-Next MTP") adds the MTP graph for the `qwen4exp`
architecture, cross-model tensor borrowing (for the `shared-` draft
heads), and the `--spec-type draft-mtp` wiring for it. It is still
UNMERGED (still open as of 2026-09-25; the pinned nixpkgs `llama-cpp`
0.5.0 has no MTP code in `src/models/qwen4exp.cpp` and no
`borrow_shared_tensor`).

## What to remove once llama.cpp mainline ships qwen4exp MTP

- `hosts/host.thing/nixpkgs.overlays.llama-cpp-pr-28243.nix` — the whole
  overlay file (rev `a9e9c3c5fed8a0bb5cc617532d0d16b8f59c13e0`,
  unslothai `mtp/qwen4exp-nextn`, i.e. upstream b10786 + the PR).
- Its import in `hosts/host.thing/default.nix`.
- `patched-llama-cpp-pr-28243-pkg` in
  `hosts/host.thing/myconfig.ai.llama-cpp/default.nix` and the
  `mtpServerPackage` argument threading into
  `hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.8-Flash-Next.nix`
  (`mk_mtp_model` + the two `Qwen3.8-Flash-Next-UD-*-MTP` entries).
  At that point the MTP entries can drop `serverPackage` entirely
  and run on the stock nixpkgs `llama-cpp` — check that the nixpkgs
  version contains the merge (if it does not yet, bump the nixpkgs
  input or add a host overlay pinning a llama.cpp release that does).

## Verification that mainline is ready

- `src/models/qwen4exp.cpp` on the tag contains `graph_mtp` /
  `LLM_GRAPH_TYPE_DECODER_MTP` and the `mtp_flags = !ml.load_mtp`
  tensor gating (they are absent from the stock llama.cpp 0.5.0
  build, which is exactly why this pin exists).
- `src/llama-model-loader.cpp` contains `borrow_shared_tensor` (needed
  by the `mtp-Qwen3.8-Flash-Next-shared-*` heads).
- Serve `Qwen3.8-Flash-Next-UD-Q4_K_XL-MTP` and check the log for the
  `draft acceptance = …` line (per the unsloth MTP README, its absence
  means the build silently serves without speculation).

## Reference

Introduced by the "provision new models on hosts/host.thing" task
(thing-models worktree); hardware-verified in bead `myconfig-80a`.
This is the only llama.cpp build on host.thing that is not a stock
nixpkgs `llama-cpp` / `llama-cpp-vulkan` / `llama-cpp-rocm` /
CUDA-override build.
