# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# CUDA (NVIDIA RTX 5090) NInfer Podman configurations.
#
# Modelled on ../myconfig.ai.vllm/docker.vllm.cuda.nix's
# `vllmQwen38_27B_NVFP4` variant, but using NInfer
# (https://github.com/Neroued/ninfer) instead of vLLM, and podman instead of
# docker as the container runtime.

{
  config,
  pkgs,
  lib,
  ...
}:

let
  common = import ./common.nix { inherit pkgs; };
  mkNinfer = common.mkNinferDockerized;

  # --- Qwen3.8-27B NVFP4 (NInfer artifact) ---
  # https://huggingface.co/neroued/Qwen3.8-27B-nvfp4-NInfer
  ninferQwen38_27B_NVFP4 = mkNinfer {
    modelHostDir = "/models/neroued-Qwen3.8-27B-nvfp4-NInfer";
    modelFilename = "qwen3_8_27b_nvfp4.ninfer";
    modelHfRepo = "neroued/Qwen3.8-27B-nvfp4-NInfer";
    servedModelName = "qwen3.8-27b";
    containerName = "ninfer-dockerized-Qwen3.8-27B-NVFP4";
    port = 22545;
    extraConfig = {
      aliases = [
        "ninfer:qwen3.8-27b-nvfp4"
      ];
    };
  };
in
{
  imports = [
    {
      virtualisation.podman.enable = lib.mkDefault true;
      virtualisation.podman.dockerCompat = lib.mkDefault true;
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;

      systemd.services.llama-swap = {
        wants = [
          "podman.service"
          "podman.socket"
          "nvidia-container-toolkit-cdi-generator.service"
        ];
        after = [
          "podman.service"
          "podman.socket"
          "nvidia-container-toolkit-cdi-generator.service"
        ];
      };
    }
  ];
  config = {
    environment.systemPackages = [
      ninferQwen38_27B_NVFP4.ninferPkg
    ];
    services.llama-swap.settings.models = ninferQwen38_27B_NVFP4.modelConfig;
    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [
          {
            type = "openai-compatible";
            name = "ninfer";
            api_base = "http://localhost:22545/v1";
            models = [
              { name = "qwen3.8-27b"; }
            ];
          }
        ];
      }
    ];
  };
}
