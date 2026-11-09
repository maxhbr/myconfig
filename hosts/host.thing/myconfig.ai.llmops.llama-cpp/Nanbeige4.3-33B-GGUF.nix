# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Nanbeige4.3-33B GGUFs (bartowski imatrix quantisation of
# Nanbeige/Nanbeige4.3-33B):
# https://huggingface.co/bartowski/Nanbeige_Nanbeige4.3-33B-GGUF
#
# Dense LlamaForCausalLM text model (no mmproj in the repo, "Input
# support: text" per the upstream README), 33B params, 52
# FULL-attention layers, 8 KV heads @ head_dim 128 (GQA), native
# 524288-token context. The embedded chat template has a
# tool-definition section (upstream README "Prompt format with tool
# definitions"), hence `tools`.
#
# Sizing: llama-server falls back to the GGUF's n_ctx_train (524288)
# when no explicit `--ctx-size` is given, so every entry pins an
# explicit ctxSize/cacheType pair sized for its backend.
#
# KV cache costs ~212 KB/token (f16), ~113 KB/token (q8_0), ~60
# KB/token (q4_0) at this layer/head geometry.
#   - rtx5090 (32 GB): the weights dominate; small context + q4_0 KV.
#     Q6_K_S is the tightest entry (27.75 GB weights) — halve its
#     ctxSize if it fails to load.
#   - gfx1151 (Strix Halo, 124 GiB GTT pool): 262144 ctx fits with
#     headroom; the KV quality per entry is tuned to the total budget.
#
# Q4_K_M (20.79 GB), Q5_K_M (25.07 GB) and Q6_K_S (27.75 GB) are
# served on both backends; Q8_0 (35.2 GB) is AMD-only — same split
# rationale as
# Qwen3.8-27B-Q8_0 in Qwen3.8-27B.nix (the RTX 5090's 32 GB cannot hold
# 35.2 GB of weights). The rtx and amd entries share names, so the
# gfx1151 config (amdModels ++ fromRtxModels in default.nix) serves
# each name once — via the amd entry, which additionally gets the
# host-level `--no-mmap` AMD tuning.
#
# Model SHA-256 values are the HuggingFace LFS oids (verified via the
# /api/.../tree/main?blobs=true endpoint, 2026-11). They are logged in
# the startup banner for provenance but NOT verified at runtime.
{ modelsPullDir }:
let
  # Shared base attributes. The pull directory is
  # `bartowski-Nanbeige_Nanbeige4.3-33B-GGUF` (the helper maps
  # "org/repo" to "<target>/<org>-<repo>"); a single-file `hf_spec`
  # keeps the other 20+ repo quants out of /home/mhuber/models.
  baseModel = quant: {
    name = "Nanbeige4.3-33B-${quant}";
    capabilities = [ "tools" ];
    path = "/models/bartowski-Nanbeige_Nanbeige4.3-33B-GGUF/Nanbeige_Nanbeige4.3-33B-${quant}.gguf";
    pull-models = {
      target_directory = modelsPullDir;
      hf_spec = [
        "bartowski/Nanbeige_Nanbeige4.3-33B-GGUF/Nanbeige_Nanbeige4.3-33B-${quant}.gguf"
      ];
    };
    ttl = 1800;
  };

  sha256_Q4_K_M = "2cef79855ca47ee6571e166db32a07089e34f3cf78e0397b262d124ddb814c37";
  sha256_Q5_K_M = "2b10206e3ba0a6a8f88b8c23fdbc49e9efccb3331564e711e4fc2901b18288cc";
  sha256_Q6_K_S = "6f536627ea0f78961a1ac5c6d456ea57a3ce9f97155ceaedaeafb6fa9b3b4359";
  sha256_Q8_0 = "8523452a975fa67d84a926c9a1bbdbb32b78b2a516dbf14d9360271fc53cd7f1";
in
{
  rtxModels = [
    # 20.79 GB weights + ~3.9 GB KV @ 65536 ctx (q4_0) ≈ 24.7 GB — the
    # roomiest RTX entry.
    (
      baseModel "Q4_K_M"
      // {
        sha256 = sha256_Q4_K_M;
        ctxSize = 65536;
        cacheType = "q4_0";
        parallel = 1;
      }
    )
    # 25.07 GB weights + ~3.9 GB KV @ 65536 ctx (q4_0) ≈ 29 GB
    (
      baseModel "Q5_K_M"
      // {
        sha256 = sha256_Q5_K_M;
        ctxSize = 65536;
        cacheType = "q4_0";
        parallel = 1;
      }
    )
    # 27.75 GB weights + ~2.0 GB KV @ 32768 ctx (q4_0) ≈ 29.7 GB — the
    # tightest RTX entry.
    (
      baseModel "Q6_K_S"
      // {
        sha256 = sha256_Q6_K_S;
        ctxSize = 32768;
        cacheType = "q4_0";
        parallel = 1;
      }
    )
  ];

  amdModels = [
    # 20.79 GB weights + ~28.4 GB KV @ 262144 ctx (q8_0) ≈ 49 GB
    (
      baseModel "Q4_K_M"
      // {
        sha256 = sha256_Q4_K_M;
        ctxSize = 262144;
        cacheType = "q8_0";
        parallel = 1;
      }
    )
    # 25.07 GB weights + ~28.4 GB KV @ 262144 ctx (q8_0) ≈ 53 GB
    (
      baseModel "Q5_K_M"
      // {
        sha256 = sha256_Q5_K_M;
        ctxSize = 262144;
        cacheType = "q8_0";
        parallel = 1;
      }
    )
    # 27.75 GB weights + ~15.4 GB KV @ 262144 ctx (q4_0) ≈ 43 GB
    (
      baseModel "Q6_K_S"
      // {
        sha256 = sha256_Q6_K_S;
        ctxSize = 262144;
        cacheType = "q4_0";
        parallel = 1;
      }
    )
    # 35.2 GB weights + ~15.4 GB KV @ 262144 ctx (q4_0) ≈ 51 GB
    (
      baseModel "Q8_0"
      // {
        sha256 = sha256_Q8_0;
        ctxSize = 262144;
        cacheType = "q4_0";
        parallel = 1;
      }
    )
  ];
}
