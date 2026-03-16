# Thunderbolt RTX 5090 GPU Passthrough to Ubuntu VM

This directory contains the plan and implementation for passing through the Thunderbolt-connected RTX 5090 eGPU to a Linux VM for CUDA/AI workloads (image generation, LLM inference, etc.).

## Overview

**Host:** `host.thing` (Framework laptop with AMD Strix Halo)  
**GPU:** NVIDIA GeForce RTX 5090 (Thunderbolt 4 eGPU)  
**VM OS:** Ubuntu 24.04 LTS  
**Management:** CLI-only via `virsh`  
**Framework:** NixOS-managed libvirt/QEMU configuration

## Current System State

### Hardware Configuration

| Component | Status | Details |
|-----------|--------|---------|
| Thunderbolt | ✅ Authorized | `services.hardware.bolt.enable = true` |
| IOMMU | ✅ Enabled | `amd_iommu=on`, `iommu=pt` |
| KVM | ✅ Available | `kvm-amd` module loaded |
| GPU IOMMU Group | 17 | Shared with audio device |
| VFIO | ✅ Available | Modules present in kernel |

### GPU Details

```
PCI Address: 62:00.0 (VGA) + 62:00.1 (Audio)
Vendor ID: 10de (NVIDIA)
Device IDs: 2b85 (GPU), 22e8 (Audio)
IOMMU Group: 17
Current Driver: NVIDIA (on host)
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    host.thing (NixOS)                        │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  VFIO Configuration                                  │   │
│  │  - IOMMU kernel params                               │   │
│  │  - VFIO kernel modules                               │   │
│  │  - NVIDIA GPU binding to vfio-pci                    │   │
│  └──────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Libvirt/QEMU                                        │   │
│  │  - libvirtd + QEMU/KVM                               │   │
│  │  - OVMF UEFI support                                 │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                             │
│  Thunderbolt 4 Port                                         │
│       │                                                     │
│       ▼                                                     │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              ai-gpu-vm (Ubuntu 24.04)                │   │
│  │  - RTX 5090 + Audio (PCI passthrough)                │   │
│  │  - CUDA Toolkit for AI workloads                     │   │
│  │  - Managed via virsh (CLI-only)                      │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Implementation Plan

### Module Structure

```
rtx-vm/
├── README.md              # This documentation
├── Makefile               # Quick start commands
├── default.nix            # Main module (import this)
├── modules/
│   ├── virtualization.gpuPassthrough.nix   # VFIO infrastructure
│   └── virtualization.libvirtDomains.nix   # VM definition
└── scripts/
    ├── bind-gpu-to-vfio.sh      # Bind GPU for VM
    ├── unbind-gpu-from-vfio.sh  # Return GPU to host
    └── create-ubuntu-vm.sh      # Create VM definition
```

### Phase 1: Core Infrastructure Modules

#### Module 1: `modules/virtualization.gpuPassthrough.nix`

**Purpose:** Enable VFIO infrastructure for GPU passthrough

**Configuration:**
```nix
{
  # IOMMU kernel parameters
  boot.kernelParams = [
    "amd_iommu=on"
    "iommu=pt"
    "iommufd=1"
  ];

  # VFIO kernel modules
  boot.kernelModules = [
    "vfio"
    "vfio_iommu_type1"
    "vfio_pci"
    "vfio_virqfd"
  ];

  # Bind NVIDIA GPU vendor ID to vfio-pci
  boot.extraModprobeConfig = ''
    options vfio-pci ids=10de:2b85,10de:22e8
    blacklist nouveau
  '';

  # Libvirt user permissions for VFIO
  users.groups.libvirt = { };
  users.users.mhuber.extraGroups = [ "libvirt" ];
}
```

**Features:**
- ✅ IOMMU passthrough mode enabled
- ✅ VFIO modules loaded at boot
- ✅ NVIDIA GPU bound to vfio-pci (not NVIDIA driver)
- ✅ Newlib driver blacklisted to prevent conflicts

#### Module 2: `modules/virtualization.libvirtDomains.nix`

**Purpose:** Configure libvirt/QEMU with GPU passthrough support

**Configuration:**
```nix
{
  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore";
    onShutdown = "shutdown";

    qemu = {
      runAsRoot = false;
      ovmf = {
        enable = true;
        packages = [ pkgs.OVMFFull.fd ];
      };
    };
  };

  # Required for GPU passthrough
  environment.systemPackages = with pkgs; [
    qemu_kvm
    libvirt
    virtinst  # For virt-install command
    edk2-ovmf
  ];
}
```

**Features:**
- ✅ QEMU/KVM with libvirt daemon
- ✅ OVMF UEFI firmware (required for GPU passthrough)
- ✅ CLI tools installed (`virsh`, `virt-install`)

### Phase 2: VM Definition

#### Module 3: `hosts/host.thing/vms/ai-gpu-vm.nix`

**Purpose:** Define the Ubuntu VM with GPU passthrough

**Configuration:**
```nix
{
  virtualisation.libvirtd.domains.ai-gpu-vm = {
    enable = true;
    memory = 16384;  # 16 GB
    vcpus = 6;

    # GPU passthrough configuration
    devices = {
      gpu = {
        enable = true;
        pciAddress = "62:00.0";
        audioAddress = "62:00.1";
        romBar = true;
      };
    };

    # Disk configuration
    disk = {
      size = "100G";
      type = "qcow2";
      storagePool = "default";
    };

    # Network configuration
    network = {
      type = "nat";  # Alternative: "bridge"
      interface = "default";
    };

    # VM settings
    os = {
      type = "hvm";
      architecture = "x86_64";
      firmware = "ovmf";
    };
  };
}
```

**Features:**
- ✅ Q35 machine type (required for PCIe passthrough)
- ✅ GPU + Audio from IOMMU Group 17 passed through
- ✅ 16GB RAM, 6 vCPUs (configurable)
- ✅ 100GB virtio-blk disk (configurable)
- ✅ NAT networking (bridge option available)
- ✅ OVMF UEFI firmware

### Phase 3: Management Scripts

#### Scripts: `scripts/`

**`bind-gpu-to-vfio.sh`** (run from `hosts/host.thing/rtx-vm/`)
```bash
#!/usr/bin/env bash
# Stop VM, unbind GPU from NVIDIA, bind to vfio-pci
set -euo pipefail

# Stop libvirt VMs
virsh shutdown ai-gpu-vm || true
virsh list --all | grep ai-gpu-vm && virsh destroy ai-gpu-vm

# Unbind from NVIDIA driver
echo -n "0000:62:00.0" > /sys/bus/pci/drivers/nvidia/unbind 2>/dev/null || true
echo -n "0000:62:00.1" > /sys/bus/pci/drivers/nvidia_hcd/unbind 2>/dev/null || true

# Bind to vfio-pci
echo -n "0000:62:00.0" > /sys/bus/pci/drivers/vfio-pci/bind
echo -n "0000:62:00.1" > /sys/bus/pci/drivers/vfio-pci/bind

echo "GPU bound to vfio-pci. Ready to start VM."
```

**`unbind-gpu-from-vfio.sh`**
```bash
#!/usr/bin/env bash
# Stop VM, unbind GPU from vfio-pci, bind to NVIDIA
set -euo pipefail

# Stop VM
virsh shutdown ai-gpu-vm || true
virsh list --all | grep ai-gpu-vm && virsh destroy ai-gpu-vm

# Unbind from vfio-pci
echo -n "0000:62:00.0" > /sys/bus/pci/drivers/vfio-pci/unbind
echo -n "0000:62:00.1" > /sys/bus/pci/drivers/vfio-pci/unbind

# Bind to NVIDIA driver
echo -n "0000:62:00.0" > /sys/bus/pci/drivers/nvidia/bind
echo -n "0000:62:00.1" > /sys/bus/pci/drivers/nvidia_hcd/bind

echo "GPU bound to NVIDIA driver. Ready for host use."
```

**`define-ubuntu-vm.sh`**
```bash
#!/usr/bin/env bash
# Create Ubuntu VM definition with GPU passthrough
set -euo pipefail

ISO_PATH="${1:-/path/to/ubuntu-24.04-desktop-amd64.iso}"
VM_NAME="ai-gpu-vm"

virt-install \
  --name "${VM_NAME}" \
  --ram 16384 \
  --vcpus 6 \
  --disk size=100,format=qcow2 \
  --cdrom "${ISO_PATH}" \
  --os-variant ubuntu24.04 \
  --machine q35 \
  --boot uefi \
  --graphics spice \
  --network network=default \
  --hostdev 62:00.0 \
  --hostdev 62:00.1 \
  --rombar 1 \
  --noautoconsole
```

**`install-ubuntu-iso.sh`**
```bash
#!/usr/bin/env bash
# Download Ubuntu 24.04 LTS ISO
set -euo pipefail

ISO_URL="https://releases.ubuntu.com/24.04/ubuntu-24.04-desktop-amd64.iso"
ISO_PATH="${1:-~/Downloads/ubuntu-24.04-desktop-amd64.iso}"

echo "Downloading Ubuntu 24.04 LTS..."
curl -L -o "${ISO_PATH}" "${ISO_URL}"
echo "ISO downloaded to ${ISO_PATH}"
```

### Phase 4: Host Configuration

#### Modified: `hosts/host.thing/default.nix`

```nix
{
  imports = [
    # ... existing imports ...
    ./rtx-vm/modules/virtualization.gpuPassthrough.nix
    ./rtx-vm/modules/virtualization.libvirtDomains.nix
  ];

  config = {
    # Enable virtualization
    virtualisation.libvirtd.enable = true;

    # Enable GPU passthrough
    virtualisation.gpuPassthrough = {
      enable = true;
      iommuType = "amd";  # Framework laptop has AMD CPU
      gpus = [
        {
          vendorId = "10de";
          deviceIds = [ "2b85" "22e8" ];
          pciAddresses = [ "62:00.0" "62:00.1" ];
        }
      ];
    };

    # Enable VM definition
    virtualisation.libvirtDomains.ai-gpu-vm = {
      enable = true;
      memory = 16384;
      vcpus = 6;
      diskSize = "100G";
      customStoragePath = "/home/mhuber/disk/virtlibvirt/images";
      networkType = "nat";
      gpuPassthrough = true;
      ubuntuVersion = "24.04";
    };

    # User permissions
    users.users.mhuber.extraGroups = [ "libvirt" ];
  };
}
```

### Quick Start

```bash
# From hosts/host.thing/rtx-vm/ directory

# 1. Check GPU status
make check

# 2. Bind GPU to vfio-pci
make bind

# 3. Create Ubuntu VM (will download ISO if needed)
make create

# 4. Start and install Ubuntu
make start
virt-viewer ai-gpu-vm

# 5. After Ubuntu install, install NVIDIA drivers and CUDA
# See "Ubuntu VM Setup" section below
```

**Alternative (manual commands):**

```bash
# Bind GPU
./scripts/bind-gpu-to-vfio.sh

# Create VM
./scripts/create-ubuntu-vm.sh /path/to/ubuntu-24.04-desktop-amd64.iso

# Start VM
virsh start ai-gpu-vm
virt-viewer ai-gpu-vm
```

## Usage

### Quick Start with Make

```bash
cd hosts/host.thing/rtx-vm/

# Check GPU status
make check

# Bind GPU to vfio-pci (for VM)
make bind

# Start VM
make start

# Connect to VM display
virt-viewer ai-gpu-vm

# Shutdown VM
make stop

# Unbind GPU (return to host)
make unbind
```

### Starting the VM with GPU

```bash
# 1. Ensure GPU is bound to vfio-pci
./scripts/bind-gpu-to-vfio.sh
# or: make bind

# 2. Start the VM
virsh start ai-gpu-vm

# 3. Connect to console (optional)
virt-viewer ai-gpu-vm
# or
virsh console ai-gpu-vm
```

### Stopping the VM and Returning GPU to Host

```bash
# 1. Shutdown VM gracefully
virsh shutdown ai-gpu-vm
# or: make stop

# 2. Wait for VM to stop, then unbind GPU
./scripts/unbind-gpu-from-vfio.sh
# or: make unbind

# 3. Verify GPU is back on host
nvidia-smi  # Should show GPU status
```

### VM Management Commands

```bash
# List all VMs
virsh list --all

# Get VM details
virsh dominfo ai-gpu-vm

# View VM console
virsh console ai-gpu-vm

# Pause/Resume VM
virsh suspend ai-gpu-vm
virsh resume ai-gpu-vm

# Shutdown VM
virsh shutdown ai-gpu-vm

# Force stop VM (if unresponsive)
virsh destroy ai-gpu-vm

# View VM logs
journalctl -u libvirtd -f
```

## Ubuntu VM Setup

### Post-Installation: Install NVIDIA Drivers and CUDA

```bash
# Inside Ubuntu VM

# Add NVIDIA PPA
sudo add-apt-repository ppa:graphics-drivers/ppa
sudo apt update

# Install NVIDIA driver (match host driver version)
sudo apt install nvidia-driver-550

# Install CUDA toolkit
sudo apt install cuda-toolkit-12-6

# Verify installation
nvidia-smi
nvcc --version
```

### Verify GPU Passthrough

```bash
# Inside Ubuntu VM

# Check GPU is present
lspci | grep -i nvidia

# Check driver is loaded
nvidia-smi

# Test CUDA
cuda-memcheck --version
```

## Known Issues & Limitations

### Thunderbolt-Specific Issues

| Issue | Impact | Workaround |
|-------|--------|------------|
| GPU may not auto-authorize on boot | Thunderbolt device blocked | Check `boltctl` and authorize manually |
| Suspend/hibernate with VM running | System may hang | Stop VM before suspending host |
| Thunderbolt re-enumeration | GPU may disconnect | Reboot after re-enumeration |

### NVIDIA-Specific Issues

| Issue | Impact | Workaround |
|-------|--------|------------|
| Driver version mismatch | VM fails to start | Match VM driver to host driver |
| PCI reset failures | GPU may not release | Reboot host to recover GPU |
| ROM bar issues | GPU may not initialize | Use `--rombar 1` in VM config |

### IOMMU Group Issues

| Issue | Impact | Workaround |
|-------|--------|------------|
| Group contains other devices | Cannot isolate GPU | Pass through entire group (GPU + Audio) |
| ACS override needed | IOMMU grouping too coarse | Add `pcie_acs_override=downstream` kernel param |

## Troubleshooting

### GPU Not Binding to vfio-pci

```bash
# Check current driver binding
lspci -k -s 62:00.0

# Check vfio-pci is loaded
lsmod | grep vfio

# Check IOMMU is enabled
dmesg | grep -i iommu

# Force rebind
echo -n "0000:62:00.0" > /sys/bus/pci/drivers/nvidia/unbind
echo -n "0000:62:00.0" > /sys/bus/pci/drivers/vfio-pci/bind
```

### VM Fails to Start

```bash
# Check libvirt logs
journalctl -u libvirtd -f

# Check VM configuration
virsh dominfo ai-gpu-vm

# Check OVMF firmware
ls -la /usr/share/OVMF/

# Verify GPU is bound to vfio-pci
lspci -k -s 62:00.0 | grep -A2 Kernel driver
```

### CUDA Not Working in VM

```bash
# Check driver version
nvidia-smi

# Check CUDA version
nvcc --version

# Verify GPU is visible
lspci | grep -i nvidia

# Check kernel modules
lsmod | grep nvidia
```

## Alternative: Container-Based Approach

If full VM isolation is not required, consider using Docker/Podman with NVIDIA Container Toolkit:

```nix
# Simpler setup, GPU available to both host and containers
services.docker.enable = true;
hardware.nvidia-container-toolkit.enable = true;
```

**Advantages:**
- ✅ GPU available to host and containers simultaneously
- ✅ Simpler setup (no VM management)
- ✅ Near-zero overhead
- ✅ No suspend/hibernate issues

**Disantages:**
- ❌ No hardware isolation
- ❌ Container escapes could affect host GPU
- ❌ Less suitable for development/testing different OSes

**When to use containers:**
- Running Stable Diffusion, LLM inference, etc.
- No need for Windows/Linux VM isolation
- Want GPU available to host desktop

**When to use VM:**
- Need full OS isolation
- Testing different Linux distributions
- Running Windows-specific CUDA applications
- Development environment isolation

## Future Enhancements

- [ ] Add USB controller passthrough for Thunderbolt peripherals
- [ ] Configure SR-IOV for network passthrough
- [ ] Add VM snapshots support
- [ ] Integrate with Home Manager for VM configuration management
- [ ] Add monitoring (GPU temp, utilization) via libvirt hooks
- [ ] Configure GPU hotplug support (experimental)

## References

### Documentation
- [NixOS Wiki: VFIO](https://nixos.wiki/wiki/VFIO)
- [NixOS Wiki: Libvirt](https://nixos.wiki/wiki/Libvirt)
- [NixOS Wiki: GPU Passthrough](https://nixos.wiki/wiki/GPU_Passthrough)
- [Linux GPU Passthrough Guide](https://github.com/gnif/linux-gpu-passthrough)

### NVIDIA Specific
- [NVIDIA VFIO Guide](https://docs.nvidia.com/datacenter/tesla/vgpu-software/index.html)
- [NVIDIA Container Toolkit](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/)

### Thunderbolt
- [Linux Thunderbolt Documentation](https://www.kernel.org/doc/html/latest/thunderbolt/thunderbolt.html)
- [bolt(8) man page](https://www.mankier.com/8/bolt)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2026-03-16 | Initial plan and documentation |

---

**Status:** 📝 Planning Phase  
**Next Steps:** Implement modules and test GPU passthrough
