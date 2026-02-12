# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    ./vulkan.nix
  ];
  config = {
    boot.kernelParams = [
      # "amd_iommu=off"
      "iommu=pt"            # Use pass-through for better performance
      "amd_iommu=on"        # Explicitly turn it on
      "amdgpu.gttsize=131072"
      "ttm.pages_limit=33554432"
    ];
    services.ollama = {
      environmentVariables = {
        HSA_OVERRIDE_GFX_VERSION = "11.5.1";
      };
    };
  };
}
