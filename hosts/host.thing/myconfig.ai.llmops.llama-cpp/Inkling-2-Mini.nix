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
# min-p 0.0, --jinja with the embedded chat template, 32K context). Vision
# via the Q8_0 mmproj projector on the :mmproj variants (validated against
# the pinned build: `inkling2v` projector loads and ingests images).
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
    dspark:
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
      ctxSize = 32768;
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
  # Same entry on both hosts: the rtx config serves it on Vulkan0/CUDA0
  # (INI router + ad-hoc wrappers) and the gfx container on
  # Vulkan0/ROCm0 (llama-swap); the shared-name dedup machinery in
  # default.nix (scriptOnlyVulkan1Names, fromRtxModels, pull-models)
  # treats it like any other shared model.
  rtxModels = [
    (mkInkling "Q8_0" false)
    (mkInkling "IQ4_NL" false)
  ];

  amdModels = [
    (mkInkling "Q8_0" false)
    (mkInkling "IQ4_NL" false)
  ];
}
