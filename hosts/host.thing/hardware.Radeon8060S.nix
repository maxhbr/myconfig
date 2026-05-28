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
            hardware.gpu.variant = [ "amd" ];
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
                hardware.gpu.variant = [ "amd-no-rocm" ];
              };
            };
          };
        };
      }
    )
  ];
  config = {
    boot.kernelParams = [
      # IOMMU off for ~6% better memory bandwidth (no VFIO/passthrough in use).
      # See https://github.com/kyuz0/amd-strix-halo-toolboxes/issues/66
      # "amd_iommu=off"
      # Switch back to `iommu=pt` + `amd_iommu=on` if enabling rtx-vm/ passthrough.
      "iommu=pt" # Use pass-through for better performance
      "amd_iommu=on" # Explicitly turn it on
      # 124 GiB for GTT/TTM, leaving 4 GiB headroom for the system (128 GiB total).
      "amdgpu.gttsize=126976" # 124 * 1024
      "ttm.pages_limit=32505856" # 124 * 1024 * 1024 / 4
      "amdttm.pages_limit=32505856"
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
