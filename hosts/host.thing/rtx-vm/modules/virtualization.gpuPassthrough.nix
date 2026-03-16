# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.virtualisation.gpuPassthrough;
in
{
  options.virtualisation.gpuPassthrough = {
    enable = lib.mkEnableOption "GPU passthrough via VFIO";

    gpus = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
      description = "List of GPU configurations for passthrough";
    };

    iommuType = lib.mkOption {
      type = lib.types.enum [ "intel" "amd" ];
      default = if config.boot.kernelModules == [ "kvm-intel" ] then "intel" else "amd";
      description = "IOMMU type based on CPU architecture";
    };
  };

  config = lib.mkIf cfg.enable {
    # IOMMU kernel parameters
    boot.kernelParams =
      [
        "iommu=pt"  # Pass-through mode for performance
        "iommufd=1" # Required for modern VFIO
      ]
      ++ lib.optional (cfg.iommuType == "amd") "amd_iommu=on"
      ++ lib.optional (cfg.iommuType == "intel") "intel_iommu=on";

    # VFIO kernel modules
    boot.kernelModules = [
      "vfio"
      "vfio_iommu_type1"
      "vfio_pci"
      "vfio_virqfd"
    ];

    # Bind NVIDIA GPUs to vfio-pci
    boot.extraModprobeConfig =
      let
        # Build vfio-pci ids parameter from gpu configurations
        nvidiaIds =
          lib.concatMapStringsSep "," (gpu: "${gpu.vendorId}:${builtins.concatStringsSep "," gpu.deviceIds}")
            (lib.filter (gpu: gpu.vendorId == "10de") cfg.gpus);
      in
      ''
        # NVIDIA GPU passthrough to VFIO
        options vfio-pci ids=${nvidiaIds}
        # Blacklist NVIDIA drivers for passthrough devices
        blacklist nouveau
        blacklist nvidia
        blacklist nvidia_drm
        blacklist nvidia_modeset
      '';

    # Ensure NVIDIA drivers are loaded for host when not passthrough
    hardware.nvidia = {
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };

    # Libvirt user permissions for VFIO access
    users.groups.libvirt = { };
    users.users.mhuber = lib.mkIf (config.users.users.mhuber.exists) {
      extraGroups = [ "libvirt" ];
    };

    # System packages for VM management
    environment.systemPackages = with pkgs; [
      qemu_kvm
      libvirt
      virtinst  # Provides virt-install
      virt-viewer
      spice
      spice-protocol
      edk2-ovmf
    ];

    # Thunderbolt auto-authorization for GPU
    services.hardware.bolt.enable = true;
  };
}
