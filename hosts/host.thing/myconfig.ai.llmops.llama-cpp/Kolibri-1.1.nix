# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Aleph-Alpha Kolibri 1.1 Q8_0 GGUF:
# https://huggingface.co/Aleph-Alpha/Kolibri-1.1-GGUF
#
# 78B-A3.5B MoE, "kolibri1" architecture (supported by the pinned
# llama.cpp v1.0.0), 50 layers, 384 routed experts x 512 (6 active) +
# 1 shared expert, 2 leading dense blocks, GQA 48 heads / 4 KV heads @
# 128, native 262144-token context. Hybrid sliding-window attention:
# 40 SWA layers (window 513) + 10 full-attention layers, so a full
# 262144-token KV cache is small (only the 10 full-attention layers
# address the whole context).
#
# Sizing (gfx1151, Strix Halo, 124 GiB GTT pool): Q8_0 weights are
# 74.4 GiB; the 262144-token q8_0 KV cache is ~3 GiB. Total ~77 GiB —
# fits with headroom. AMD-only: the RTX 5090's 32 GB cannot hold 74 GiB
# of weights.
#
# The SHA-256 is the HuggingFace LFS oid (via
# /api/.../tree/main?blobs=true, 2026-11). Logged in the startup banner
# for provenance but NOT verified at runtime.
{ modelsPullDir }:
{
  amdModels = [
    {
      name = "Kolibri-1.1-Q8_0";
      capabilities = [ "thinking" ];
      path = "/models/Aleph-Alpha-Kolibri-1.1-GGUF/Kolibri-1.1-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "Aleph-Alpha/Kolibri-1.1-GGUF/Kolibri-1.1-Q8_0.gguf"
        ];
      };
      sha256 = "2125b48a86fddb2963aece33355bf30e5a7086d8b9273ed5c92573b28e4472ad";
      ctxSize = 262144;
      cacheType = "q8_0";
      parallel = 1;
      ttl = 3600;
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
