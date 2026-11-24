# Re-enable Kolibri-1.1 once llama.cpp PR #32891 lands

## What to do

Re-enable the Kolibri-1.1 model entries disabled in
`hosts/host.thing/myconfig.ai.llmops.llama-cpp/Kolibri-1.1.nix`
(`amdModels` is empty; the entry is kept commented). Steps:

1. Drop the pin: mainline llama.cpp must contain the Kolibri-1.1
   loader (PR ggml-org/llama.cpp#32891, "model : add Kolibri-1.1
   support", hirschsn, open 2026-11-23). Then the host-wide overlay
   pin `hosts/host.thing/nixpkgs.overlays.llama-cpp-inkling.nix`
   (rev `690b87d`) needs rebasing past that merge anyway — check
   whether it can be dropped back to the stock nixpkgs package at the
   same time (its other reason, Inkling-2-Mini, is merged since
   #32694/#32704).
2. Uncomment the entry in `Kolibri-1.1.nix` and re-add
   `++ kolibri1_1.amdModels` to `amdModels` in
   `hosts/host.thing/myconfig.ai.llmops.llama-cpp/default.nix`.
3. Validate the load, not just the arch name:
   `llama-server_Vulkan0_Kolibri-1.1-Q8_0` must reach
   "model loaded" (the original deploy validated only that the
   `kolibri1` arch is known — Kolibri-**1** — and shipped a model
   whose tensors no mainline build loads).

## Preconditions

- PR #32891 (or successor) merged into ggml-org/llama.cpp master.
- The **Vulkan** backend implements `GGML_OP_ROUND_NVFP4`
  (`ggml_round_nvfp4`). As of the PR, kernels exist for CPU, CUDA
  and Metal only; without a Vulkan kernel (`vulkan-shaders/`) the
  gfx1151 Vulkan0/Vulkan1 serving path cannot run the 1.1
  sparse-indexer graph. Alternative: serve via `ROCm0` with a
  `serverPackage` pin of the merged fork (the HIP backend compiles
  the CUDA sources, so `round-nvfp4.cu`'s fallback path builds;
  runtime on gfx1151 unverified).

## Affected code

- `hosts/host.thing/myconfig.ai.llmops.llama-cpp/Kolibri-1.1.nix`
  — disabled entry + pull spec
  (`Aleph-Alpha/Kolibri-1.1-GGUF/Kolibri-1.1-Q8_0.gguf`)
- `hosts/host.thing/myconfig.ai.llmops.llama-cpp/default.nix`
  — `kolibri1_1` import and the `amdModels` wiring
- introduced by commit 789282477a ("thing: serve Kolibri-1.1-Q8_0 on
  the AMD llama-cpp backend"); disabled after the load failure
  `tensor 'blk.0.ffn_gate_inp.weight' not found`
  (llama.cpp `load_arch_tensors` for `kolibri1` assumes every layer
  is MoE — `src/models/kolibri1.cpp` — while 1.1 has a 2-block
  dense-FFN prefix, `kolibri1.leading_dense_block_count = 2`, plus a
  sparse attention indexer and inter-layer K/V sharing).

Tracked in bd as `myconfig-fssp`.
