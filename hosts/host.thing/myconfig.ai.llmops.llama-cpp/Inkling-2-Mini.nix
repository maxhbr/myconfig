# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Thinking Machines Lab "Inkling 2 Mini" (looped-transformer MoE,
# 5.4B active / 26B total), ggml-org GGUFs:
#   https://huggingface.co/ggml-org/Inkling-2-Mini-GGUF
#   https://unsloth.ai/docs/models/inkling-2-mini
#
# The ggml-org files use the `inkling2_loop` layout, which mainline llama.cpp
# reads only after #32694/#32704 (both merged after the v1.0.0 tag the pinned
# nixpkgs builds), so the host overlay re-pins ALL llama.cpp backends to a
# commit containing them (see ../nixpkgs.overlays.llama-cpp-inkling.nix).
# No `serverPackage` needed: the device-default builds read the layout, so
# the entries participate in the routers/llama-swap like any other model
# (rtx INI router on port 33656 included).
#
# Sampling follows the Unsloth guide (temp 1.0 / top-p 1.0 / top-k 0 /
# min-p 0.0, --jinja with the embedded chat template, native 1M context).
# Vision via the Q8_0 mmproj projector on the :mmproj variants (validated
# against the pinned build: `inkling2v` projector loads and ingests images).
#
# Sizing: the KV cache costs ~32 KiB/token (f16 — measured: the 1M-ctx
# f16 cache tried to allocate 32 GiB and OOM'd gfx1151 when another
# model was resident), ~16 KiB/token (q8_0), ~8 KiB/token (q4_0).
# Weights: Q8_0 ≈ 27.6 GB, IQ4_NL ≈ 14.6 GB. Every entry pins an
# explicit ctxSize/cacheType pair sized for its backend:
#   - gfx1151 (124 GiB GTT pool): 524288 ctx + q8_0 KV (Q8_0 ≈ 36 GB
#     total). This is double the 262144 ROCm deep-context cap in
#     doc/qwen38-gfx1151/profiles.md — that cap marks the *tested*
#     configuration, not a hard limit, and the 8 GiB KV is nowhere
#     near the pool.
#   - rtx5090 (32 GB VRAM): IQ4_NL at 262144 ctx + q8_0 KV ≈ 19 GB;
#     Q8_0 at 32768 ctx + q4_0 KV ≈ 28 GB — the tightest entry,
#     mirroring Nanbeige4.3-33B-Q6_K_S; halve its ctxSize if it fails
#     to load.
#
# Loop guardrails: the model card warns the model "sometimes became stuck in
# long reasoning loops" and recommends output-token limits. The reasoning
# budget is the direct fix — it counts tokens inside the
# <|content_thinking|> block and, when exhausted, force-feeds the
# end-of-thinking tag (with a wrap-up note) so the answer still completes.
# It is a server-side default: clients may override it per request via
# `reasoning_budget_tokens`. --n-predict is only a fallback cap for clients
# that send no max_tokens (LiteLLM always sends one); reasoning-budget then
# binds the thinking part of the same budget.
#
# DSpark speculative decoding is WIRED BUT OFF: the only published drafter
# (unsloth dspark-Inkling-2-Mini-Q8_0.gguf, arch `dflash`) fails to load on
# mainline with a vocab-padding shape mismatch (markov_w1 expects the
# unpadded 200058 vocab, the file stores 201024). Flip `dspark` to true to
# re-try once a compatible drafter or upstream fix exists; the hf_spec
# entry and params are generated conditionally.
{
  lib,
  modelsPullDir,
}:
let
  modelsDir = "/models/ggml-org-Inkling-2-Mini-GGUF";

  # LFS oids from
  # https://huggingface.co/api/models/ggml-org/Inkling-2-Mini-GGUF/tree/main?blobs=true
  sha256s = {
    Q8_0 = "d57542f8aea5d7639beba5fc4ae7f584754916f63ac5d5a69328c27f652f45da";
    IQ4_NL = "cbdf3fa9dc50a4124977980f37da4f1fa764b0cfe800d275cd43d26ce2a81af4";
    "mmproj-Q8_0" = "233287272ec5ac39b4168677aba54a4c024f202410f5629fac1830e1059f8abb";
    dspark = "2ec78510da837e8fc3be54a14757a0c5fbd2d8d9e85a0d92853fafdf674f3773";
  };

  mkInkling =
    quant:
    # `dspark` mirrors the guide's optional drafter; currently OFF — see the
    # header comment. When false, the entry serves the base model without
    # speculative decoding.
    dspark: ctxSize: cacheType:
    let
      mmproj = "mmproj-Inkling-2-Mini-${quant}.gguf";
    in
    {
      name = "Inkling-2-Mini-${quant}";
      capabilities = [
        "thinking"
        "tools"
        "agentic"
      ];
      path = "${modelsDir}/Inkling-2-Mini-${quant}.gguf";
      sha256 = sha256s.${quant};
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "ggml-org/Inkling-2-Mini-GGUF/Inkling-2-Mini-${quant}.gguf"
          "ggml-org/Inkling-2-Mini-GGUF/${mmproj}"
        ]
        ++ lib.optionals dspark [ "unsloth/Inkling-2-Mini-GGUF/dspark-Inkling-2-Mini-Q8_0.gguf" ];
      };
      inherit
        ctxSize
        cacheType
        ;
      parallel = 1;
      ttl = 1800;
      params = [
        # Pin the slot count to 1: omitting --parallel selects
        # llama.cpp's auto mode (4 slots, unified KV), which would
        # quarter the per-request context at the same --ctx-size.
        "--parallel"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "1.0"
        "--top-k"
        "0"
        "--min-p"
        "0.0"
        # Cap runaway thinking: once 6144 tokens are spent inside the
        # thinking block, the sampler injects the wrap-up message and the
        # end tag, so the model must produce its final answer (it keeps
        # generating within the remaining response budget). LiteLLM caps
        # total output at contextWindow/4 = 262144, well above the
        # 16384 --n-predict cap and 6144 thinking budget; thinking + answer
        # still easily fit.
        "--reasoning-budget"
        "6144"
        "--reasoning-budget-message"
        "Reasoning budget exhausted. Summarize what you established and give the final answer now."
        # Server-side output cap for clients that send no max_tokens;
        # per-request max_tokens/n_predict still overrides it.
        "--n-predict"
        "16384"
      ]
      ++ lib.optionals dspark [
        "--spec-type"
        "draft-dspark"
        "--model-draft"
        "/models/unsloth-Inkling-2-Mini-GGUF/dspark-Inkling-2-Mini-Q8_0.gguf"
        "--spec-draft-n-max"
        "3"
      ];
      variants = {
        # Vision variant: mmproj projector per the guide's server command
        # (the projector is optional; the base entries stay text-only).
        mmproj = {
          mmproj = "${modelsDir}/${mmproj}";
          capabilities = [ "vision" ];
        };
      };
    };
in
{
  # Same names on both hosts (so the shared-name dedup machinery in
  # default.nix — scriptOnlyVulkan1Names, fromRtxModels, pull-models —
  # treats these like any other shared model), but backend-sized
  # ctxSize/cacheType per the header: the rtx config serves its entries
  # on Vulkan0/CUDA0 (INI router + ad-hoc wrappers), the gfx container
  # serves the amd entries on Vulkan0/ROCm0 (llama-swap).
  rtxModels = [
    (mkInkling "Q8_0" false 32768 "q4_0")
    # q4_0 KV (not q8_0): the 262144-ctx q8_0 allocation OOM'd CUDA0 at
    # load (weights load to 100%, then llama-server exits 1 — the
    # 32768/q4_0 Q8_0 entry loads fine on the same GPU).
    (mkInkling "IQ4_NL" false 262144 "q4_0")
  ];

  amdModels = [
    (mkInkling "Q8_0" false 524288 "q8_0")
    (mkInkling "IQ4_NL" false 524288 "q8_0")
  ];
}
