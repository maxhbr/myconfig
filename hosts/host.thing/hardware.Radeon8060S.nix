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
    ./myconfig.observability.amd-smi-exporter.nix
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
      WLR_DRM_DEVICES = "/dev/dri/by-path/pci-0000:c3:00.0-card";
      # Niri (non-WLR) needs its own env var to pick the correct DRM device.
      NIRI_DRM_DEVICES = "/dev/dri/by-path/pci-0000:c3:00.0-card";
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

    # Tell niri to only use the iGPU for rendering, not the RTX 5090 eGPU.
    myconfig = {
      hardware.gpu.variant = [ "amd" ];
      desktop.wayland.niri.additionalConfigKdl = ''
        debug {
          render-drm-device "/dev/dri/by-path/pci-0000:c3:00.0-card"
          ignore-drm-device "/dev/dri/renderD129"
        }
      '';
    };

    # Opt-in specialisation for Vulkan deep-context (>128k) testing.
    #
    # `amdgpu.lockup_timeout=-1` disables the kernel's GPU lockup
    # watchdog, which can falsely trigger during long Vulkan fills on
    # gfx1151 / Strix Halo (the GPU appears "locked" while processing a
    # very large batch, then recovers). With the watchdog off, those
    # false positives no longer reset the GPU — but a REAL lockup also
    # no longer recovers, so the system can hang instead of resetting.
    #
    # This is NOT the default. Boot into this specialisation only for
    # deep-context Vulkan benchmarking; boot the default config for all
    # other workloads (ROCm 262k is unaffected — keep it on the default).
    #
    # Recovery / rollback: if the GPU hangs under this specialisation,
    # a soft reboot may not complete (the hung GPU can block the
    # shutdown path). Use a hard reset (physical power button) and
    # select the default boot entry. The specialisation only adds the
    # kernel param; it does not change the root filesystem, so no
    # rollback of store paths is needed.
    #
    # GPU canary: after booting into this specialisation, verify the GPU
    # is responsive before starting a deep-context run:
    #   rocm-smi            # device visible + utilisation
    #   vulkaninfo --summary  # Vulkan instance + physical device
    # If either fails, do NOT start the benchmark — the kernel param
    # may have interacted badly with the driver; hard-reset and use the
    # default config instead.
    specialisation.amdgpu-no-lockup-timeout = {
      configuration = {
        boot.kernelParams = [ "amdgpu.lockup_timeout=-1" ];
      };
    };
  };
}
