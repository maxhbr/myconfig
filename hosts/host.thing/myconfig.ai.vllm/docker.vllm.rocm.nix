# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# ROCm (AMD gfx1151) vLLM Docker configurations

{
  config,
  pkgs,
  lib,
  ...
}:

let
  common = import ./common.nix { inherit pkgs; };
  mkVllm = common.mkRocmVllmDockerized;

  # --- Variant 6: Qwen3.8-27B-FP8 (Qwen official, ROCm / AMD) ---
  vllmQwen38_27B_FP8_ROCm = mkVllm {
    modelHostPath = "/models/Qwen-Qwen3.8-27B-FP8";
    modelHfRepo = "Qwen/Qwen3.8-27B-FP8";
    servedModelName = "Qwen3.8-27B-FP8";
    containerName = "vllm-dockerized-Qwen3.8-27B-FP8-ROCm";
    port = 22549;
    maxModelLen = 131072;
    dtype = "auto";
    gpuMemoryUtilization = 0.75;
    maxNumSeqs = 3;
    reasoningParser = "qwen3";
    rocmOverrideGfxVersion = "11.5.1";
    extraConfig = {
      aliases = [
        "vllm:fp8"
        "vllm:rocm"
      ];
    };
  };

  # --- Variant 8: Nex-N2.5-mini (nex-agi, ROCm / AMD) ---
  # https://huggingface.co/nex-agi/Nex-N2.5-mini
  # 125B MoE (6B activated, 256 experts / 8 active) with hybrid
  # linear-attention / full-attention layers (interval 4) and a 1-layer
  # MTP head (`mtp_num_hidden_layers` in config.json; not enabled here —
  # the pinned vLLM image predates Nex MTP wiring, the upstream model
  # card ships an sglang fork for that). BF16 weights are ~70 GB, which
  # does NOT fit the RTX 5090's 32 GB — hence the ROCm variant on the
  # gfx1151 host, whose 124 GiB GTT/TTM pool holds the weights plus KV
  # at 0.85 utilization. Native context 262144; 131072 matches the
  # Qwen3.8-27B-FP8 ROCm precedent. Sampling per the model card:
  # temperature 0.7, top_p 0.95, top_k 40 (client-side). Reasoning
  # parser qwen3 per the model card ("Nex-N2.5-mini and Nex-N2.5-Pro:
  # --reasoning-parser qwen3"); tool calling would use qwen3_coder on
  # sglang — the launcher's TOOL_CALL_PARSER default (qwen3_xml) stays
  # overridable per launch, consistent with the other variants.
  vllmNex_N25_mini_ROCm = mkVllm {
    modelHostPath = "/models/nex-agi-Nex-N2.5-mini";
    modelHfRepo = "nex-agi/Nex-N2.5-mini";
    servedModelName = "Nex-N2.5-mini";
    containerName = "vllm-dockerized-Nex-N2.5-mini-ROCm";
    port = 22549;
    maxModelLen = 131072;
    dtype = "auto";
    gpuMemoryUtilization = 0.85;
    maxNumSeqs = 2;
    reasoningParser = "qwen3";
    rocmOverrideGfxVersion = "11.5.1";
    extraConfig = {
      aliases = [
        "vllm:nex"
      ];
    };
  };
in
{
  imports = [
    {
      # Podman start dependencies of the llama-swap unit are declared in
      # myconfig.ai.vllm/default.nix so they are not duplicated.
      virtualisation.podman.enable = lib.mkDefault true;
      virtualisation.podman.dockerCompat = lib.mkDefault true;
    }
  ];
  config = {
    environment.systemPackages = [
      vllmQwen38_27B_FP8_ROCm.vllmPkg
      vllmNex_N25_mini_ROCm.vllmPkg
    ];
    services.llama-swap.settings.models =
      vllmQwen38_27B_FP8_ROCm.modelConfig // vllmNex_N25_mini_ROCm.modelConfig;
  };
}
