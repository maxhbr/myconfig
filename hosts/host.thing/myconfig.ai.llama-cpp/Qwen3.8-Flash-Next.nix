# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Qwen3.8-Flash-Next GGUFs (unsloth). Both quantisations are multi-part
# GGUFs: `*-00001-of-NNNNN.gguf` … `*-NNNNN-of-NNNNN.gguf`.
# schema-wise `path` is just shard 1 — llama.cpp chains the remaining
# shards from the same directory when opening it, so a multi-part model
# needs no extra option. `pull-models.hf_spec` therefore uses the
# subdirectory form of `hf_spec` (`"org/repo/subdir"` -> subdir/*), which
# downloads all shards — same pattern as `qwen3.5-122B-A10B-Q5_K_M` in
# default.nix and `Qwen3.8-27B-BF16-split` in Qwen3.8-27B.nix.
# https://huggingface.co/unsloth/Qwen3.8-Flash-Next-GGUF
{
  modelsPullDir,
  serverPackage,
}:
let
  # `sequential` variant — serve a single request at a time.
  #
  # Why this exists: with more than one slot, concurrent Flash-Next
  # requests share the gfx1151 GPU and the KV pool, each request gets
  # slower, and serving degrades until the 600 s `--timeout` that
  # `lib/scripts.nix` passes to llama-server fires and the request times
  # out instead of finishing. One slot answers fewer requests per second
  # but every request finishes, which is what interactive use needs.
  #
  # Why the flag goes through `params`: `parallel = 1` alone does NOT
  # serialise anything. `lib/scripts.nix` emits `--parallel` only when
  # `parallel > 1`, so `parallel = 1` leaves the flag out and llama.cpp
  # falls back to its auto mode of 4 slots with a unified KV cache
  # (`tools/server/server.cpp`: `if (params.n_parallel < 0) { n_parallel
  # = 4; kv_unified = true; }`, see the `parallel` option in
  # modules/myconfig.ai/myconfig.ai.llama-cpp/options.nix). The explicit
  # `--parallel 1` in `params` is what pins the slot count to 1; the same
  # pattern as `Qwen3.8-27B-MTP-ngram-Q4_K_XL` in Qwen3.8-27B.nix.
  sequential_variant = {
    parallel = 1;
    params = [
      "--parallel"
      "1"
    ];
  };
in
{
  # Served by the `llama-cpp-33657` container (llama-swap on
  # Vulkan0/ROCm0) on the PR-27742-patched llama-cpp
  # (see nixpkgs.overlays.llama-cpp-pr-27742.nix). The per-model
  # `serverPackage` option also routes the ad-hoc host Vulkan1 script
  # wrappers from `scriptOnlyModels` (and the :mmproj variants) to the
  # patched build — everything that serves THIS model runs PR-27742,
  # while the rest of the host keeps the stock build.
  #
  # Multimodal: the repo ships `mmproj-F16.gguf` / `mmproj-BF16.gguf`
  # sidecars (the VLM projector). Both are 16-bit, so the download size
  # is identical; F16 keeps the full 10-bit mantissa while BF16 trades
  # mantissa precision for fp32-style exponent range — irrelevant for
  # normalised pre-trained vision-tower weights, so F16 is the faithful
  # choice. It is also the native matrix dtype of the gfx1151 RDNA3.5
  # backend, whereas BF16 runs software-converted; this matches the
  # mmproj pick this host already makes for gemma-4. Each entry gets a
  # `:mmproj` variant (auto-generated, see lib/variants.nix) serving
  # the same shards plus `--mmproj <file>`. No deliberate `ctxSize` /
  # `cacheType` retuning yet: leave the GGUF defaults, retune for
  # gfx1151 headroom after the first serving test.
  #
  # Each quantisation additionally gets a `sequential` variant (model
  # name suffix `-sequential`) that serves one request at a time; see
  # `sequential_variant` above.
  # Variant expansion is flat — every variant derives from the base
  # entry, never from another variant — so the `mmproj` variant keeps
  # the llama.cpp default (auto, 4 slots). Add
  # `"mmproj-sequential" = sequential_variant // { mmproj = "<same
  # mmproj-F16 path>"; };` to both `variants` sets if image requests
  # must be serialised too.
  amdModels = [
    {
      name = "Qwen3.8-Flash-Next-UD-IQ4_XS";
      inherit serverPackage;
      path = "/models/unsloth-Qwen3.8-Flash-Next-GGUF/UD-IQ4_XS/Qwen3.8-Flash-Next-UD-IQ4_XS-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/Qwen3.8-Flash-Next-GGUF/UD-IQ4_XS" # all 3 LLM shards
          "unsloth/Qwen3.8-Flash-Next-GGUF/mmproj-F16.gguf"
          # BF16 sibling: same contents, software-converted on RDNA3.5.
          "unsloth/Qwen3.8-Flash-Next-GGUF/mmproj-BF16.gguf"
        ];
      };
      variants = {
        # Multimodal serving of the same shards; the :mmproj variant
        # publishes the alias `Qwen3.8-Flash-Next-UD-IQ4_XS-mmproj`.
        mmproj = {
          mmproj = "/models/unsloth-Qwen3.8-Flash-Next-GGUF/mmproj-F16.gguf";
        };
        sequential = sequential_variant;
      };
      ttl = 1800;
    }
    {
      name = "Qwen3.8-Flash-Next-UD-Q4_K_XL";
      inherit serverPackage;
      path = "/models/unsloth-Qwen3.8-Flash-Next-GGUF/UD-Q4_K_XL/Qwen3.8-Flash-Next-UD-Q4_K_XL-00001-of-00004.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/Qwen3.8-Flash-Next-GGUF/UD-Q4_K_XL" # all 4 LLM shards
          "unsloth/Qwen3.8-Flash-Next-GGUF/mmproj-F16.gguf"
          "unsloth/Qwen3.8-Flash-Next-GGUF/mmproj-BF16.gguf"
        ];
      };
      variants = {
        mmproj = {
          mmproj = "/models/unsloth-Qwen3.8-Flash-Next-GGUF/mmproj-F16.gguf";
        };
        sequential = sequential_variant;
      };
      ttl = 1800;
    }
  ];
}
