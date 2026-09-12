# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Nex-N2.5-mini GGUFs (abenzerps quantisation of nex-agi/Nex-N2.5-mini):
# https://huggingface.co/abenzerps/Nex-N2.5-mini-GGUF
#
# Nex-N2.5-mini is a long-context agentic model for coding, tool use,
# computer use and multimodal workloads (qwen3_5_moe architecture —
# the `qwen35`/`qwen35moe` llama.cpp arch, supported by the pinned
# b10549 stock build; no `serverPackage` override needed). 125B total
# with 6B activated, 256 experts (8 active), hybrid linear-attention /
# full-attention layers (interval 4), native 262144-token context.
#
# The GGUF repo ships an optional `mmproj-Nex-N2.5-mini-F16.gguf`
# vision projector (899 MB, F16) and embeds the upstream chat template;
# `chat_template.jinja` is provided for runtimes that need a separate
# template file. Only the three K-quant files requested for this host
# are pulled (the repo also ships IQ/TQ ladders and Q8_0).
#
# Recommended sampling parameters (model card): temperature 0.7,
# top_p 0.95, top_k 40.
#
# Model SHA-256 values are the HuggingFace LFS oids (verified via the
# /api/.../tree/main?blobs=true endpoint). They are logged in the
# startup banner for provenance but NOT verified at runtime.
#
# Served on gfx1151 (Vulkan0/ROCm0, container llama-cpp). The three
# quants are single-file GGUFs: Q4_K_M 21.2 GB, Q5_K_M 24.7 GB,
# Q6_K 28.5 GB — all fit the 124 GiB GTT/TTM pool alongside the KV
# cache even at full 262k context. No deliberate `cacheType` retuning:
# leave the GGUF defaults, retune for gfx1151 headroom after the first
# serving test.
{ modelsPullDir }:
{
  amdModels = [
    {
      name = "Nex-N2.5-mini-Q4_K_M";
      path = "/models/abenzerps-Nex-N2.5-mini-GGUF/Nex-N2.5-mini-Q4_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "abenzerps/Nex-N2.5-mini-GGUF/Nex-N2.5-mini-Q4_K_M.gguf" ];
      };
      sha256 = "dd296f683c798a3e4058fb1ef8c462e6a4cc8b89cd196742a8e5a3da4057fbb3";
      ttl = 1800;
    }
    {
      name = "Nex-N2.5-mini-Q5_K_M";
      path = "/models/abenzerps-Nex-N2.5-mini-GGUF/Nex-N2.5-mini-Q5_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "abenzerps/Nex-N2.5-mini-GGUF/Nex-N2.5-mini-Q5_K_M.gguf" ];
      };
      sha256 = "f60514ff680b9561318129f22dddd9da59878bda211c178662f3d50938f0c9d4";
      ttl = 1800;
    }
    {
      name = "Nex-N2.5-mini-Q6_K";
      path = "/models/abenzerps-Nex-N2.5-mini-GGUF/Nex-N2.5-mini-Q6_K.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "abenzerps/Nex-N2.5-mini-GGUF/Nex-N2.5-mini-Q6_K.gguf" ];
      };
      sha256 = "9d21c304e8fcb78c204e1e95ce12ac2bee8b9c09bbcff5a229d5b638c1bda72e";
      ttl = 1800;
    }
  ];
}
