# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# CUDA (NVIDIA RTX 5090) vLLM Docker configurations

{
  config,
  pkgs,
  lib,
  ...
}:

let
  common = import ./common.nix { inherit pkgs; };
  mkVllm = common.mkCudaVllmDockerized;

  # --- Variant 1: Qwen3.6-27B-NVFP4 (base) ---
  vllmQwen36_27B_NVFP4 = mkVllm {
    modelHostPath = "/models/unsloth-Qwen3.6-27B-NVFP4";
    modelHfRepo = "unsloth/Qwen3.6-27B-NVFP4";
    servedModelName = "Qwen3.6-27B-NVFP4";
    containerName = "vllm-dockerized-Qwen3.6-27B-NVFP4";
    port = 22548;
    maxModelLen = 185024;
    extraConfig = { };
  };

  # --- Variant 2: Qwen3.6-27B-Text-NVFP4-MTP (multi-token prediction) ---
  vllmQwen36_27B_Text_NVFP4_MTP = mkVllm {
    modelHostPath = "/models/sakamakismile-Qwen3.6-27B-Text-NVFP4-MTP";
    modelHfRepo = "sakamakismile/Qwen3.6-27B-Text-NVFP4-MTP";
    servedModelName = "Qwen3.6-27B-Text-NVFP4-MTP";
    containerName = "vllm-dockerized-Qwen3.6-27B-Text-NVFP4-MTP";
    port = 22548;
    maxModelLen = 185024;
    extraConfig = {
      aliases = [
        "vllm:mtp"
      ];
    };
  };

  # --- Variant 3: Qwen3.6-27B-int4-AutoRound (Intel AutoRound int4) ---
  vllmQwen36_27B_int4_AutoRound = mkVllm {
    modelHostPath = "/models/Intel-Qwen3.6-27B-int4-AutoRound";
    modelHfRepo = "Intel/Qwen3.6-27B-int4-AutoRound";
    servedModelName = "Qwen3.6-27B-int4-AutoRound";
    containerName = "vllm-dockerized-Qwen3.6-27B-int4-AutoRound";
    port = 22548;
    maxModelLen = 185024;
    extraConfig = {
      aliases = [
        "vllm:autoround"
      ];
    };
  };

  # --- Variant 5: Qwen-AgentWorld-35B-A3B (Qwen official, language world model) ---
  vllmQwenAgentWorld35B_A3B = mkVllm {
    modelHostPath = "/models/Qwen-AgentWorld-35B-A3B";
    modelHfRepo = "Qwen/Qwen-AgentWorld-35B-A3B";
    servedModelName = "Qwen-AgentWorld-35B-A3B";
    containerName = "vllm-dockerized-Qwen-AgentWorld-35B-A3B";
    port = 22548;
    maxModelLen = 131072;
    dtype = "bfloat16";
    gpuMemoryUtilization = 0.93;
    maxNumSeqs = 3;
    reasoningParser = "qwen3";
    extraConfig = {
      aliases = [
        "vllm:agentworld"
      ];
    };
  };

  # --- Variant 4: Qwen3.6-27B-int4-AutoRound (Lorbus, MTP + reasoning) ---
  vllmQwen36_27B_int4_AutoRound_Lorbus = mkVllm {
    modelHostPath = "/models/Lorbus-Qwen3.6-27B-int4-AutoRound";
    modelHfRepo = "Lorbus/Qwen3.6-27B-int4-AutoRound";
    servedModelName = "Qwen3.6-27B-int4-AutoRound-Lorbus";
    containerName = "vllm-dockerized-Qwen3.6-27B-int4-AutoRound-Lorbus";
    port = 22548;
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
in
{
  imports = [
    {
      virtualisation.docker.enable = lib.mkDefault true;
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;
      systemd.services.llama-swap = {
        wants = [
          "docker.service"
          "docker.socket"
          "nvidia-container-toolkit-cdi-generator.service"
        ];
        after = [
          "docker.service"
          "docker.socket"
          "nvidia-container-toolkit-cdi-generator.service"
        ];

        serviceConfig = {
          SupplementaryGroups = [ "docker" ];
          # Only needed if the service uses a dedicated non-root user:
          # User = "llama-swap";
        };

        environment = {
          DOCKER_HOST = "unix:///var/run/docker.sock";
        };
      };
    }
  ];
  config = {
    environment.systemPackages = [
      vllmQwen36_27B_NVFP4.vllmPkg
      vllmQwen36_27B_Text_NVFP4_MTP.vllmPkg
      vllmQwen36_27B_int4_AutoRound.vllmPkg
      vllmQwen36_27B_int4_AutoRound_Lorbus.vllmPkg
      vllmQwenAgentWorld35B_A3B.vllmPkg
    ];
    services.llama-swap.settings.models =
      vllmQwen36_27B_NVFP4.modelConfig
      // vllmQwen36_27B_Text_NVFP4_MTP.modelConfig
      // vllmQwen36_27B_int4_AutoRound.modelConfig
      // vllmQwen36_27B_int4_AutoRound_Lorbus.modelConfig
      // vllmQwenAgentWorld35B_A3B.modelConfig;
    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [
          {
            type = "openai-compatible";
            name = "vllm";
            api_base = "http://localhost:22548/v1";
            models = [
              { name = "Qwen3.6-27B-NVFP4"; }
              { name = "Qwen3.6-27B-Text-NVFP4-MTP"; }
              { name = "Qwen3.6-27B-int4-AutoRound"; }
              { name = "Qwen3.6-27B-int4-AutoRound-Lorbus"; }
              { name = "Qwen-AgentWorld-35B-A3B"; }
              { name = "Qwen3.6-27B-FP8"; }
            ];
          }
        ];
      }
    ];
  };
}
