# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# GLM-5.3-Flash GGUFs (unsloth). The UD-Q2_K_XL quantisation is a
# multi-part GGUF: `*-00001-of-00004.gguf` … `*-00004-of-00004.gguf`.
# Schema-wise `path` is just shard 1 — llama.cpp chains the remaining
# shards from the same directory when opening it, so a multi-part model
# needs no extra option. `pull-models.hf_spec` therefore uses the
# subdirectory form of `hf_spec` (`"org/repo/subdir"` -> subdir/*), which
# downloads all shards — same pattern as `qwen3.5-122B-A10B-Q5_K_M` in
# default.nix and `Qwen3.8-Flash-Next` in Qwen3.8-Flash-Next.nix.
# https://huggingface.co/unsloth/GLM-5.3-Flash-GGUF
{
  modelsPullDir,
  serverPackage,
}:
{
  # Served by the `llama-cpp-33657` container (llama-swap on
  # Vulkan0/ROCm0) on the PR-27754-patched llama-cpp
  # (see nixpkgs.overlays.llama-cpp-pr-27754.nix). The per-model
  # `serverPackage` option also routes the ad-hoc host Vulkan1 script
  # wrappers from `scriptOnlyModels` (and the :mmproj variants) to the
  # patched build — everything that serves THIS model runs PR-27754,
  # while the rest of the host keeps the stock build.
  #
  # Multimodal: the repo ships `mmproj-F16.gguf` / `mmproj-BF16.gguf`
  # sidecars (the VLM projector). Both are 16-bit, so the download size
  # is nearly identical; F16 keeps the full mantissa while BF16 trades
  # mantissa precision for fp32-style exponent range — irrelevant for
  # normalised pre-trained vision-tower weights, so F16 is the faithful
  # choice. It is also the native matrix dtype of the gfx1151 RDNA3.5
  # backend, whereas BF16 runs software-converted; this matches the
  # mmproj pick this host already makes for gemma-4 and
  # Qwen3.8-Flash-Next. Each entry gets a `:mmproj` variant
  # (auto-generated, see lib/variants.nix) serving the same shards plus
  # `--mmproj <file>`. No deliberate `ctxSize` / `cacheType` retuning
  # yet: leave the GGUF defaults, retune for gfx1151 headroom after the
  # first serving test.
  amdModels = [
    {
      name = "GLM-5.3-Flash-UD-Q2_K_XL";
      inherit serverPackage;
      path = "/models/unsloth-GLM-5.3-Flash-GGUF/UD-Q2_K_XL/GLM-5.3-Flash-UD-Q2_K_XL-00001-of-00004.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/GLM-5.3-Flash-GGUF/UD-Q2_K_XL" # all 4 LLM shards
          "unsloth/GLM-5.3-Flash-GGUF/mmproj-F16.gguf"
          # BF16 sibling: same contents, software-converted on RDNA3.5.
          "unsloth/GLM-5.3-Flash-GGUF/mmproj-BF16.gguf"
        ];
      };
      variants = {
        # Multimodal serving of the same shards; the :mmproj variant
        # publishes the alias `GLM-5.3-Flash-UD-Q2_K_XL-mmproj`.
        mmproj = {
          mmproj = "/models/unsloth-GLM-5.3-Flash-GGUF/mmproj-F16.gguf";
        };
      };
      ttl = 1800;
    }
  ];
}
