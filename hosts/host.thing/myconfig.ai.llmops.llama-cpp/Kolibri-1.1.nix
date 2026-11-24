# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Aleph-Alpha Kolibri 1.1 Q8_0 GGUF:
# https://huggingface.co/Aleph-Alpha/Kolibri-1.1-GGUF
#
# Mainline's `kolibri1` arch support (#30673, in the pinned 690b87d
# inkling build AND upstream master) implements Kolibri-1 ONLY. 1.1 has
# a 2-block dense-FFN prefix (leading_dense_block_count = 2), an
# attention indexer (kolibri1.attention.indexer.*, kv_source_map) and
# inter-layer K/V sharing; loading it on any mainline build fails with
# "tensor 'blk.0.ffn_gate_inp.weight' not found".
#
# Served via the fork pin ../nixpkgs.overlays.llama-cpp-kolibri.nix
# (hirschsn/llama.cpp, PR ggml-org/llama.cpp#32891) as `serverPackage`,
# ROCm0 only:
# - the PR's GGML_OP_ROUND_NVFP4 ships CPU/CUDA/Metal kernels only — no
#   Vulkan shader — so Vulkan0 serving cannot run the sparse-indexer
#   graph; the entry must not appear on Vulkan devices;
# - the ROCm/HIP build compiles (the round-nvfp4 fallback kernel is
#   plain CUDA C, HIP-guarded only on the native-cvt fast path; the
#   lightning-indexer kernels even dispatch on amd_wmma_available,
#   RDNA3.5/4) — runtime on gfx1151 still pending validation;
# - serverPackage excludes the model from the rtx INI router (which
#   runs one build per device), correct here: the RTX 5090's 32 GB
#   cannot hold the weights anyway.
#
# Sizing (verified from the GGUF header): 78B-A3.5B MoE, 50 layers, 384
# routed experts x 512 (6 active) + 1 shared, GQA 48/4 @ 128, native
# 262144 context, hybrid SWA (40 SWA layers, window 513 + 10 sparse
# full-attention layers). Q8_0 weights 74.4 GiB; the full-context q8_0
# KV cache is only ~3 GiB (the 10 full-attention layers are the sole
# whole-context consumers), which fits gfx1151's 124 GiB GTT pool.
#
# See doc/TODOs/serve-kolibri-1.1-llama-cpp-pr-32891.md.
{
  llama-cpp-kolibri,
  modelsPullDir,
}:
{
  amdModels = [
    {
      name = "Kolibri-1.1-Q8_0";
      capabilities = [ "thinking" ];
      path = "/models/Aleph-Alpha-Kolibri-1.1-GGUF/Kolibri-1.1-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "Aleph-Alpha/Kolibri-1.1-GGUF/Kolibri-1.1-Q8_0.gguf" ];
      };
      # LFS oid via
      # /api/Aleph-Alpha/Kolibri-1.1-GGUF/tree/main?blobs=true
      sha256 = "2125b48a86fddb2963aece33355bf30e5a7086d8b9273ed5c92573b28e4472ad";
      devices = [ "ROCm0" ];
      serverPackage = llama-cpp-kolibri;
      ctxSize = 262144;
      cacheType = "q8_0";
      parallel = 1;
      ttl = 3600;
      group = "Kolibri";
      tags = [
        "kolibri1.1"
        "rocm"
        "Q8_0"
      ];
      variants = {
        nothink = {
          params = [
            "--chat-template-kwargs"
            "{\"enable_thinking\":false}"
          ];
        };
      };
    }
  ];
}
