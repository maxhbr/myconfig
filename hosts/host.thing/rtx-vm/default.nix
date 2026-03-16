# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# RTX 5090 GPU Passthrough VM Configuration Module
# High-level wrapper that sets up gpuPassthrough and libvirtDomains modules

{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.virtualisation.rtxVm;
in
{
  options.virtualisation.rtxVm = {
    enable = lib.mkEnableOption "RTX 5090 GPU passthrough VM";

    memory = lib.mkOption {
      type = lib.types.int;
      default = 16384;
      description = "VM memory in MB";
    };

    vcpus = lib.mkOption {
      type = lib.types.int;
      default = 6;
      description = "Number of vCPUs";
    };

    diskSize = lib.mkOption {
      type = lib.types.str;
      default = "100G";
      description = "Virtual disk size";
    };

    ubuntuVersion = lib.mkOption {
      type = lib.types.str;
      default = "24.04";
      description = "Ubuntu version";
    };

    networkType = lib.mkOption {
      type = lib.types.enum [
        "nat"
        "bridge"
      ];
      default = "nat";
      description = "Network type";
    };

    customStoragePath = lib.mkOption {
      type = lib.types.str;
      default = "/home/mhuber/disk/virtlibvirt/images";
      description = "Storage path for VM disks";
    };
  };

  config = lib.mkIf cfg.enable {
    # Enable libvirt daemon
    virtualisation.libvirtd.enable = true;

    # GPU passthrough configuration
    virtualisation.gpuPassthrough.enable = true;
    virtualisation.gpuPassthrough.iommuType = "amd";
    virtualisation.gpuPassthrough.gpus = [
      {
        vendorId = "10de";
        deviceIds = [
          "2b85"
          "22e8"
        ];
        pciAddresses = [
          "62:00.0"
          "62:00.1"
        ];
      }
    ];

    # Ubuntu AI VM definition
    virtualisation.libvirtDomains.ai-gpu-vm.enable = true;
    virtualisation.libvirtDomains.ai-gpu-vm.memory = cfg.memory;
    virtualisation.libvirtDomains.ai-gpu-vm.vcpus = cfg.vcpus;
    virtualisation.libvirtDomains.ai-gpu-vm.diskSize = cfg.diskSize;
    virtualisation.libvirtDomains.ai-gpu-vm.ubuntuVersion = cfg.ubuntuVersion;
    virtualisation.libvirtDomains.ai-gpu-vm.networkType = cfg.networkType;
    virtualisation.libvirtDomains.ai-gpu-vm.gpuPassthrough = true;
    virtualisation.libvirtDomains.ai-gpu-vm.customStoragePath = cfg.customStoragePath;

    # User permissions
    users.users.mhuber.extraGroups = [ "libvirt" ];
  };
}
