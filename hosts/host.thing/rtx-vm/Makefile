# Makefile for RTX 5090 GPU Passthrough VM

.PHONY: help bind unbind create define start stop install-cuda check status cleanup

VM_NAME = ai-gpu-vm
GPU_PCI = 62:00.0
AUDIO_PCI = 62:00.1

help:
	@echo "RTX 5090 GPU Passthrough VM Management"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  bind       - Bind GPU to vfio-pci for VM passthrough"
	@echo "  unbind     - Unbind GPU from vfio-pci, return to NVIDIA driver"
	@echo "  create     - Create Ubuntu VM definition"
	@echo "  define     - Define VM from XML (after create)"
	@echo "  start      - Start the VM"
	@echo "  stop       - Shutdown the VM gracefully"
	@echo "  destroy    - Force stop the VM"
	@echo "  install-cuda - Install CUDA toolkit in VM (requires VM with GUI)"
	@echo "  check      - Check GPU binding status"
	@echo "  status     - Show VM status"
	@echo "  cleanup    - Remove VM and disk"
	@echo ""

bind:
	@echo "=== Binding GPU to vfio-pci ==="
	@./scripts/bind-gpu-to-vfio.sh

unbind:
	@echo "=== Unbinding GPU from vfio-pci ==="
	@./scripts/unbind-gpu-from-vfio.sh

create:
	@echo "=== Creating Ubuntu VM ==="
	@./scripts/create-ubuntu-vm.sh

define:
	@echo "=== Defining VM from XML ==="
	virsh define /tmp/$(VM_NAME).xml

start:
	@echo "=== Starting VM ==="
	virsh start $(VM_NAME)

stop:
	@echo "=== Shutting down VM ==="
	virsh shutdown $(VM_NAME)

destroy:
	@echo "=== Force stopping VM ==="
	virsh destroy $(VM_NAME)

install-cuda:
	@echo "=== CUDA Installation Instructions ==="
	@echo "After Ubuntu installation, run these commands in the VM:"
	@echo ""
	@echo "  # Add NVIDIA PPA"
	@echo "  sudo add-apt-repository ppa:graphics-drivers/ppa"
	@echo "  sudo apt update"
	@echo ""
	@echo "  # Install NVIDIA driver"
	@echo "  sudo apt install nvidia-driver-550"
	@echo ""
	@echo "  # Install CUDA toolkit"
	@echo "  sudo apt install cuda-toolkit-12-6"
	@echo ""
	@echo "  # Verify installation"
	@echo "  nvidia-smi"
	@echo "  nvcc --version"

check:
	@echo "=== GPU Binding Status ==="
	@echo ""
	@echo "GPU ($(GPU_PCI)):"
	@lspci -k -s $(GPU_PCI) | grep -A2 "Kernel driver" || echo "  Not found"
	@echo ""
	@echo "Audio ($(AUDIO_PCI)):"
	@lspci -k -s $(AUDIO_PCI) | grep -A2 "Kernel driver" || echo "  Not found"
	@echo ""
	@echo "IOMMU Group 17:"
	@find /sys/kernel/iommu_groups/17/devices -type l -exec basename {} \; 2>/dev/null || echo "  Not found"

status:
	@echo "=== VM Status ==="
	virsh list --all | grep $(VM_NAME) || echo "VM not defined or not running"

cleanup:
	@echo "=== Cleaning up VM ==="
	@virsh destroy $(VM_NAME) 2>/dev/null || true
	@virsh undefine $(VM_NAME) --nvram 2>/dev/null || true
	@rm -f /home/mhuber/disk/virtlibvirt/images/$(VM_NAME).qcow2
	@rm -f /tmp/$(VM_NAME).xml
	@echo "VM and disk removed"
