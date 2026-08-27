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
{ modelsPullDir }:
{
  # Served by the `llama-cpp-33657` container (llama-swap on
  # Vulkan0/ROCm0) on the PR-27742-patched llama-cpp
  # (see nixpkgs.overlays.llama-cpp-pr-27742.nix). The ad-hoc host
  # Vulkan1 script wrappers from `scriptOnlyModels` keep the unpinned
  # build — acceptable, the vulkan-serving instance is the container.
  #
  # No deliberate `ctxSize`/`cacheType`/`variants` yet: leave the GGUF
  # defaults, retune for gfx1151 headroom after the first serving test.
  amdModels = [
    {
      name = "Qwen3.8-Flash-Next-UD-IQ4_XS";
      path = "/models/unsloth-Qwen3.8-Flash-Next-GGUF/UD-IQ4_XS/Qwen3.8-Flash-Next-UD-IQ4_XS-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.8-Flash-Next-GGUF/UD-IQ4_XS" ]; # all 3 shards
      };
      ttl = 1800;
    }
    {
      name = "Qwen3.8-Flash-Next-UD-Q4_K_XL";
      path = "/models/unsloth-Qwen3.8-Flash-Next-GGUF/UD-Q4_K_XL/Qwen3.8-Flash-Next-UD-Q4_K_XL-00001-of-00004.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.8-Flash-Next-GGUF/UD-Q4_K_XL" ]; # all 4 shards
      };
      ttl = 1800;
    }
  ];
}
