# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# CUDA (NVIDIA RTX 5090) NInfer Podman configurations.
#
# Modelled on ../myconfig.ai.vllm/docker.vllm.cuda.nix's
# `vllmQwen38_27B_NVFP4` variant, but using NInfer
# (https://github.com/Neroued/ninfer) instead of vLLM. Serves on the same
# host port (22545) as the CUDA vLLM variants so llama-swap can swap the
# engine in and out on the same GPU.

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
    # Published SHA-256 (model card artifact table).
    modelSha256 = "bb3360522a06e136e0367f5703414d26272b7285c8a6ab6194135c17dbd81b32";
    modelHfRepo = "neroued/Qwen3.8-27B-nvfp4-NInfer";
    servedModelName = "Qwen3.8-27B-NVFP4";
    modelId = "qwen3.8-27b";
    containerName = "ninfer-dockerized-Qwen3.8-27B-NVFP4";
    port = 22545;
    # Pinned engine source (master @ 2026-08-19); newer than the
    # artifact's minimum_revision (5d2c1f55, see artifact-manifest.json).
    ninferGitRef = "feaf4dd0983fdaeb2ba4c06eec6da350e644fb3a";
    extraConfig = { };
  };
in
{
  imports = [
    {
      # Self-contained defaults (consistent with the vLLM CUDA sibling);
      # the llama-swap unit's Podman and NVIDIA CDI start dependencies
      # are declared in ./default.nix so they are not duplicated.
      virtualisation.podman.enable = lib.mkDefault true;
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;
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
