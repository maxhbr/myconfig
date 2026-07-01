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

  # --- Variant 6: Qwen3.6-27B-FP8 (Qwen official, ROCm / AMD) ---
  vllmQwen36_27B_FP8_ROCm = mkVllm {
    modelHostPath = "/models/Qwen-Qwen3.6-27B-FP8";
    modelHfRepo = "Qwen/Qwen3.6-27B-FP8";
    servedModelName = "Qwen3.6-27B-FP8";
    containerName = "vllm-dockerized-Qwen3.6-27B-FP8-ROCm";
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
      virtualisation.podman.enable = lib.mkDefault true;
      virtualisation.podman.dockerCompat = lib.mkDefault true;

      systemd.services.llama-swap = {
        wants = [
          "podman.service"
          "podman.socket"
        ];
        after = [
          "podman.service"
          "podman.socket"
        ];
      };
    }
  ];
  config = {
    environment.systemPackages = [
      vllmQwen36_27B_FP8_ROCm.vllmPkg
    ];
    services.llama-swap.settings.models = vllmQwen36_27B_FP8_ROCm.modelConfig;
  };
}
