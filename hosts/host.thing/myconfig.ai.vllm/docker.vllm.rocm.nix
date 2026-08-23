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
    ];
    services.llama-swap.settings.models = vllmQwen38_27B_FP8_ROCm.modelConfig;
  };
}
