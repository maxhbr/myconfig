# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# CUDA (NVIDIA RTX 5090) vLLM configurations
# Container runtime: Podman (NVIDIA GPU access via CDI; the
# nvidia-container-toolkit cdi-generator service provides the specs).

{
  config,
  pkgs,
  lib,
  ...
}:

let
  common = import ./common.nix { inherit pkgs; };
  mkVllm = common.mkCudaVllmDockerized;

  # --- Variant 1: Qwen3.8-27B-NVFP4 (base) ---
  vllmQwen38_27B_NVFP4 = mkVllm {
    modelHostPath = "/models/unsloth-Qwen3.8-27B-NVFP4";
    modelHfRepo = "unsloth/Qwen3.8-27B-NVFP4";
    servedModelName = "Qwen3.8-27B-NVFP4";
    containerName = "vllm-dockerized-Qwen3.8-27B-NVFP4";
    port = 22545;
    maxModelLen = 185024;
    extraConfig = { };
  };

  # --- Variant 2: Qwen3.8-27B-MTP-NVFP4 (multi-token prediction) ---
  # NOTE: upstream renamed the repo suffix order from
  # `Text-NVFP4-MTP` (3.6) to `MTP-NVFP4` (3.8); functionally
  # equivalent (NVFP4 weights + MTP draft head).
  # https://huggingface.co/sakamakismile/Qwen3.8-27B-MTP-NVFP4
  vllmQwen38_27B_MTP_NVFP4 = mkVllm {
    modelHostPath = "/models/sakamakismile-Qwen3.8-27B-MTP-NVFP4";
    modelHfRepo = "sakamakismile/Qwen3.8-27B-MTP-NVFP4";
    servedModelName = "Qwen3.8-27B-MTP-NVFP4";
    containerName = "vllm-dockerized-Qwen3.8-27B-MTP-NVFP4";
    port = 22545;
    maxModelLen = 185024;
    extraConfig = {
      aliases = [
        "vllm:mtp"
      ];
    };
  };

  # --- Variant 3: Qwen3.6-27B-int4-AutoRound (Intel AutoRound int4) ---
  # NOTE: kept on 3.6 — Intel has not published a Qwen3.8-27B
  # AutoRound int4 checkpoint as of this migration (checked via
  # `curl https://huggingface.co/api/models?author=Intel`, 2026-08).
  vllmQwen36_27B_int4_AutoRound = mkVllm {
    modelHostPath = "/models/Intel-Qwen3.6-27B-int4-AutoRound";
    modelHfRepo = "Intel/Qwen3.6-27B-int4-AutoRound";
    servedModelName = "Qwen3.6-27B-int4-AutoRound";
    containerName = "vllm-dockerized-Qwen3.6-27B-int4-AutoRound";
    port = 22545;
    maxModelLen = 185024;
    extraConfig = {
      aliases = [
        "vllm:autoround"
      ];
    };
  };

  # --- Variant 4: Qwen3.6-27B-int4-AutoRound (Lorbus, MTP + reasoning) ---
  # NOTE: kept on 3.6 — Lorbus has not published a Qwen3.8-27B
  # AutoRound checkpoint as of this migration.
  vllmQwen36_27B_int4_AutoRound_Lorbus = mkVllm {
    modelHostPath = "/models/Lorbus-Qwen3.6-27B-int4-AutoRound";
    modelHfRepo = "Lorbus/Qwen3.6-27B-int4-AutoRound";
    servedModelName = "Qwen3.6-27B-int4-AutoRound-Lorbus";
    containerName = "vllm-dockerized-Qwen3.6-27B-int4-AutoRound-Lorbus";
    port = 22545;
    maxModelLen = 262144;
    dtype = "half";
    gpuMemoryUtilization = 0.85;
    maxNumSeqs = 3;
    kvCacheDtype = "tq-t4nc";
    reasoningParser = "qwen3";
    compilationConfig = "none";
    speculativeConfig = "{\"method\": \"mtp\", \"num_speculative_tokens\": 1}";
    enableEnforceEager = false;
    extraConfig = {
      aliases = [
        "vllm:lorbus"
      ];
    };
  };

  # --- Variant 7: Qwen3.8-27B-NVFP4-RTX5090 (gittensor modelopt NVFP4) ---
  # Flag set mirrors the ad-hoc `podman run docker.io/vllm/vllm-openai:v0.27.1`
  # server used on the RTX 5090. Deviations from that ad-hoc command, per the
  # repo conventions baked into the factory:
  # - model is pulled to the host (/models/gittensor-model-hub-...) and
  #   mounted read-only instead of the container-internal HF cache bind mount
  #   (`-v ~/.cache/huggingface:/root/.cache/huggingface`; MAX_JOBS=2 omitted
  #   likewise, only needed for in-container compilation),
  # - host port publish instead of --network=host/--host/--port.
  vllmQwen38_27B_NVFP4_Cuda = mkVllm {
    modelHostPath = "/models/gittensor-model-hub-Qwen3.8-27B-NVFP4-RTX5090";
    modelHfRepo = "gittensor-model-hub/Qwen3.8-27B-NVFP4-RTX5090";
    servedModelName = "Qwen3.8-27B-NVFP4-RTX5090";
    containerName = "vllm-dockerized-Qwen3.8-27B-NVFP4-CUDA";
    port = 22545;
    dockerImage = "docker.io/vllm/vllm-openai:v0.27.1";
    maxModelLen = 131072;
    dtype = "auto";
    gpuMemoryUtilization = 0.92;
    maxNumSeqs = 4;
    kvCacheDtype = "fp8";
    reasoningParser = "qwen3";
    quantization = "modelopt";
    enablePrefixCaching = true;
    generationConfig = "vllm";
    # NOTE: the dedicated container-tools layer this image expects is not
    # exposed here. Once its pool subdirectory is curated under
    # /home/mhuber/models/pkgs/docker-layers/ (layer name derives from the
    # local package that produced the layers, e.g. `vram`), opt it in:
    # runtimeLayers = [ "<pool-layer-name>" ];
    extraConfig = { };
  };
in
{
  imports = [
    {
      # Self-contained defaults for this module (consistent with the ROCm
      # sibling); the llama-swap unit's Podman and NVIDIA CDI start
      # dependencies are declared in myconfig.ai.vllm/default.nix so
      # they are not duplicated.
      virtualisation.podman.enable = lib.mkDefault true;
      # NVIDIA CDI specs come from the same generator that served Docker.
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;
    }
  ];
  config = {
    environment.systemPackages = [
      vllmQwen38_27B_NVFP4.vllmPkg
      vllmQwen38_27B_MTP_NVFP4.vllmPkg
      # vllmQwen36_27B_int4_AutoRound.vllmPkg
      # vllmQwen36_27B_int4_AutoRound_Lorbus.vllmPkg
      vllmQwen38_27B_NVFP4_Cuda.vllmPkg
    ];
    services.llama-swap.settings.models =
      vllmQwen38_27B_NVFP4.modelConfig
      // vllmQwen38_27B_MTP_NVFP4.modelConfig
      # // vllmQwen36_27B_int4_AutoRound.modelConfig
      # // vllmQwen36_27B_int4_AutoRound_Lorbus.modelConfig
      // vllmQwen38_27B_NVFP4_Cuda.modelConfig;
    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [
          {
            type = "openai-compatible";
            name = "vllm";
            # Matches the port all CUDA vllm variants consolidated onto with
            # the Podman/CDI migration.
            api_base = "http://localhost:22545/v1";
            models = [
              { name = "Qwen3.8-27B-NVFP4"; }
              { name = "Qwen3.8-27B-MTP-NVFP4"; }
              # { name = "Qwen3.6-27B-int4-AutoRound"; }
              # { name = "Qwen3.6-27B-int4-AutoRound-Lorbus"; }
              # FP8 is the ROCm variant, served on its own port (22549); add
              # a second client pointing at 22549 to reach it from aichat
              # { name = "Qwen3.8-27B-FP8"; }
              { name = "Qwen3.8-27B-NVFP4-RTX5090"; }
            ];
          }
        ];
      }
    ];
  };
}
