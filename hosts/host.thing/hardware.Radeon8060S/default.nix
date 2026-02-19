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
    # ./rocm.nix
  ];
  config = {
    boot.kernelParams = [
      # "amd_iommu=off"
      "iommu=pt" # Use pass-through for better performance
      "amd_iommu=on" # Explicitly turn it on
      "amdgpu.gttsize=131072"
      "ttm.pages_limit=33554432"
      "amdttm.pages_limit=33554432"
    ];
    environment.sessionVariables = rec {
      HSA_OVERRIDE_GFX_VERSION = "11.5.1";
    };
    services.ollama = {
      environmentVariables = {
        HSA_OVERRIDE_GFX_VERSION = "11.5.1";
        # Ensure it picks the Strix Halo iGPU if you have an eGPU plugged in
        # OLLAMA_VULKAN_DEVICE = "0";

        # This tells the RADV driver to ignore its internal 1/8th RAM limit
        # and allow allocations up to the full heap size.
        RADV_THREAD_SUBMISSION = "1";
      };
      rocmOverrideGfx = "11.5.1";
    };
  };
}
