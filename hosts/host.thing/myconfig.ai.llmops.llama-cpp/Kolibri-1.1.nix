# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Aleph-Alpha Kolibri 1.1 Q8_0 GGUF:
# https://huggingface.co/Aleph-Alpha/Kolibri-1.1-GGUF
#
# DISABLED: no llama.cpp build that this repo can produce loads the
# Kolibri-1.1 layout. Mainline's `kolibri1` arch support (#30673, in
# the pinned 690b87d inkling build AND upstream master) implements
# Kolibri-1 ONLY — it unconditionally requires
# `blk.<i>.ffn_gate_inp.weight` for every layer, while 1.1 has a
# 2-block dense-FFN prefix (leading_dense_block_count = 2), an
# attention indexer (kolibri1.attention.indexer.*, kv_source_map)
# and inter-layer K/V sharing. Every (model, device) entry fails at
# load with "tensor 'blk.0.ffn_gate_inp.weight' not found".
#
# Upstream support: PR ggml-org/llama.cpp#32891 ("model : add
# Kolibri-1.1 support", open, unmerged as of 2026-11-24). Besides the
# model loader it adds GGML_OP_ROUND_NVFP4 with CPU/CUDA/Metal
# kernels only — there is NO Vulkan kernel, so even with the PR
# applied the Vulkan0/Vulkan1 serving on gfx1151 cannot run the
# sparse-indexer graph (the op has no Vulkan implementation; the
# scheduler would have to place it on CPU per forward pass).
#
# Re-enable once #32891 (or a successor) is merged AND the Vulkan
# backend implements round-nvfp4, or by serving via a ROCm0
# serverPackage pin of the fork. Re-add `++ kolibri1_1.amdModels`
# in default.nix — the pull spec below then fetches the file again.
# See doc/TODOs/serve-kolibri-1.1-llama-cpp-pr-32891.md.
#
# Sizing for then (verified from the GGUF header): 78B-A3.5B MoE, 50
# layers, 384 routed experts x 512 (6 active) + 1 shared, GQA 48/4 @
# 128, native 262144 context, hybrid SWA (40 SWA layers, window 513 +
# 10 sparse full-attention layers). Q8_0 weights 74.4 GiB; the
# full-context q8_0 KV cache is only ~3 GiB (the 10 full-attention
# layers are the sole whole-context consumers), which fits gfx1151's
# 124 GiB GTT pool. AMD-only: the RTX 5090's 32 GB cannot hold the
# weights.
{ modelsPullDir }:
{
  amdModels = [ ];
  # amdModels = [
  #   {
  #     name = "Kolibri-1.1-Q8_0";
  #     capabilities = [ "thinking" ];
  #     path = "/models/Aleph-Alpha-Kolibri-1.1-GGUF/Kolibri-1.1-Q8_0.gguf";
  #     pull-models = {
  #       target_directory = modelsPullDir;
  #       hf_spec = [ "Aleph-Alpha/Kolibri-1.1-GGUF/Kolibri-1.1-Q8_0.gguf" ];
  #     };
  #     # LFS oid via
  #     # /api/Aleph-Alpha/Kolibri-1.1-GGUF/tree/main?blobs=true
  #     sha256 = "2125b48a86fddb2963aece33355bf30e5a7086d8b9273ed5c92573b28e4472ad";
  #     ctxSize = 262144;
  #     cacheType = "q8_0";
  #     parallel = 1;
  #     ttl = 3600;
  #     variants = {
  #       nothink = {
  #         params = [
  #           "--chat-template-kwargs"
  #           "{\"enable_thinking\":false}"
  #         ];
  #       };
  #     };
  #   }
  # ];
}
