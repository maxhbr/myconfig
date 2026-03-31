# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    (
      # vulkan
      {
        lib,
        config,
        pkgs,
        ...
      }:
      {
        config = lib.mkIf (config.specialisation != { }) {
          myconfig = {
            hardware.gpu.variant = [ "amd-no-rocm" ];
          };
        };
      }
    )
    # rocm
    (
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        specialisation = {
          rocm = {
            inheritParentConfig = true;
            configuration = {
              myconfig = {
                hardware.gpu.variant = [ "amd" ];
              };
            };
          };
        };
      }
    )
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
      # Use the internal Strix Halo iGPU for the Wayland compositor, not the eGPU
      WLR_DRM_DEVICES = "/dev/dri/by-path/pci-0000:c2:00.0-card";
      # # other options:
      # GGML_HIP_VISIBLE_DEVICES = 0;
      # HSA_ENABLE_SDMA = 0;
      # HIP_FORCE_DEV_KERNARG = 1;
    };
    services.ollama = {
      environmentVariables = {
        HSA_OVERRIDE_GFX_VERSION = "11.5.1";
        RADV_THREAD_SUBMISSION = "1";
      };
      rocmOverrideGfx = "11.5.1";
    };
  };
}
