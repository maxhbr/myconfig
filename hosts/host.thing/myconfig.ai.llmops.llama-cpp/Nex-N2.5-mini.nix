# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Nex-N2.5-mini GGUFs (bartowski quantisation of nex-agi/Nex-N2.5-mini):
# https://huggingface.co/bartowski/nex-agi_Nex-N2.5-mini-GGUF
#
# Nex-N2.5-mini is a long-context agentic model for coding, tool use,
# computer use and multimodal workloads (qwen3_5_moe architecture —
# the `qwen35`/`qwen35moe` llama.cpp arch, supported by the stock
# nixpkgs build; no `serverPackage` override needed). 125B total
# with 6B activated, 256 experts (8 active), hybrid linear-attention /
# full-attention layers (interval 4), native 262144-token context.
#
# The GGUF repo ships optional `mmproj-nex-agi_Nex-N2.5-mini-f16.gguf`
# and `mmproj-nex-agi_Nex-N2.5-mini-bf16.gguf` vision projectors and
# embeds the upstream chat template. Three quantised files are pulled
# for this host (Q4_K_L and Q5_K_M on both backends, Q6_K_L on AMD
# only); the repository also ships lower-precision and Q8 variants.
# The bf16 projector is pulled alongside every quantisation and each
# entry gets a `:mmproj` variant (auto-generated, see lib/variants.nix)
# serving the same GGUF plus
# `--mmproj mmproj-nex-agi_Nex-N2.5-mini-bf16.gguf`.
#
# Recommended sampling parameters (model card): temperature 0.7,
# top_p 0.95, top_k 40.
#
# Model SHA-256 values are the HuggingFace LFS oids (verified via the
# /api/.../tree/main?blobs=true endpoint). They are logged in the
# startup banner for provenance but NOT verified at runtime.
#
# Served on gfx1151 (Vulkan0/ROCm0, container llama-cpp) and on the RTX
# backend (CUDA0/Vulkan0). Q4_K_L and Q5_K_M are available on both
# backends; Q6_K_L is AMD-only. No deliberate `cacheType`
# retuning: leave the GGUF defaults and retune for the target GPU after
# the first serving test.
{ modelsPullDir }:
let
  # Shared `:mmproj` variant (auto-generated, see lib/variants.nix):
  # serves the base GGUF plus the bf16 vision projector. The projector
  # is quantisation-independent, so all three entries share this one
  # file; each entry lists it in `hf_spec` so `pull-models` fetches it
  # wherever the base model is pulled.
  mmprojVariant = {
    mmproj = "/models/bartowski-nex-agi_Nex-N2.5-mini-GGUF/mmproj-nex-agi_Nex-N2.5-mini-bf16.gguf";
  };
  q4 = {
    name = "Nex-N2.5-mini-Q4_K_L";
    path = "/models/bartowski-nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q4_K_L.gguf";
    pull-models = {
      target_directory = modelsPullDir;
      hf_spec = [
        "bartowski/nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q4_K_L.gguf"
        "bartowski/nex-agi_Nex-N2.5-mini-GGUF/mmproj-nex-agi_Nex-N2.5-mini-bf16.gguf"
      ];
    };
    sha256 = "dda88fc54a5814fa54aa9603461f55f90f7e5718";
    variants = {
      mmproj = mmprojVariant;
    };
    ttl = 1800;
  };
  q5 = {
    name = "Nex-N2.5-mini-Q5_K_M";
    path = "/models/bartowski-nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q5_K_M.gguf";
    pull-models = {
      target_directory = modelsPullDir;
      hf_spec = [
        "bartowski/nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q5_K_M.gguf"
        "bartowski/nex-agi_Nex-N2.5-mini-GGUF/mmproj-nex-agi_Nex-N2.5-mini-bf16.gguf"
      ];
    };
    sha256 = "2c1d8f13ceeb010cce801b733138559c4964475b";
    variants = {
      mmproj = mmprojVariant;
    };
    ttl = 1800;
  };
  # q6 = {
  #    name = "Nex-N2.5-mini-Q6_K";
  #    path = "/models/bartowski-nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q6_K.gguf";
  #    pull-models = {
  #      target_directory = modelsPullDir;
  #      hf_spec = [ "bartowski/nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q6_K.gguf" ];
  #    };
  #    sha256 = "e58bfb38aba55f156dd91db82b025997f244cf5a";
  #    ttl = 1800;
  #  };
  q6 = {
    name = "Nex-N2.5-mini-Q6_K_L";
    path = "/models/bartowski-nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q6_K_L.gguf";
    pull-models = {
      target_directory = modelsPullDir;
      hf_spec = [
        "bartowski/nex-agi_Nex-N2.5-mini-GGUF/nex-agi_Nex-N2.5-mini-Q6_K_L.gguf"
        "bartowski/nex-agi_Nex-N2.5-mini-GGUF/mmproj-nex-agi_Nex-N2.5-mini-bf16.gguf"
      ];
    };
    sha256 = "11d19f0faf9a9d07ecc7d5caf773d4675e1e47aa";
    variants = {
      mmproj = mmprojVariant;
    };
    ttl = 1800;
  };
in
{
  amdModels = [ q6 ];
  rtxModels = [
    q4
    q5
  ];
}
