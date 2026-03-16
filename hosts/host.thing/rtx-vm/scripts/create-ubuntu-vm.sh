#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Define and create the Ubuntu AI VM with GPU passthrough
# This script creates the VM definition using virt-install

set -euo pipefail

VM_NAME="ai-gpu-vm"
VM_MEMORY=16384
VM_VCPUS=6
VM_DISK_SIZE="100G"
VM_DISK_PATH="/home/mhuber/disk/virtlibvirt/images/${VM_NAME}.qcow2"
ISO_PATH="${1:-}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if VM already exists
if virsh dominfo "${VM_NAME}" >/dev/null 2>&1; then
    log_warn "VM ${VM_NAME} already exists"
    read -p "Do you want to destroy and recreate? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        log_info "Destroying existing VM..."
        virsh destroy "${VM_NAME}" 2>/dev/null || true
        virsh undefine "${VM_NAME}" --nvram
        log_info "Removing existing disk..."
        rm -f "${VM_DISK_PATH}"
    else
        log_info "Aborted. Exiting."
        exit 0
    fi
fi

# Download Ubuntu ISO if not provided
if [ -z "${ISO_PATH}" ]; then
    ISO_PATH="${HOME}/Downloads/ubuntu-24.04-desktop-amd64.iso"
    if [ ! -f "${ISO_PATH}" ]; then
        log_info "Ubuntu ISO not found at ${ISO_PATH}"
        read -p "Download Ubuntu 24.04 ISO? (y/N) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            log_info "Downloading Ubuntu 24.04 LTS..."
            mkdir -p "$(dirname "${ISO_PATH}")"
            curl -L -o "${ISO_PATH}" "https://releases.ubuntu.com/24.04/ubuntu-24.04-desktop-amd64.iso"
        else
            log_error "ISO path required. Provide as argument or download manually."
            echo "Usage: $0 [/path/to/ubuntu-24.04-desktop-amd64.iso]"
            exit 1
        fi
    fi
else
    if [ ! -f "${ISO_PATH}" ]; then
        log_error "ISO file not found: ${ISO_PATH}"
        exit 1
    fi
fi

log_info "Using ISO: ${ISO_PATH}"

# Create disk image
log_info "Creating disk image at ${VM_DISK_PATH}..."
mkdir -p "$(dirname "${VM_DISK_PATH}")"
qemu-img create -f qcow2 "${VM_DISK_PATH}" "${VM_DISK_SIZE}"

# Check if GPU is bound to vfio-pci
log_info "Checking GPU binding..."
GPU_DRIVER=$(lspci -k -s "62:00.0" 2>/dev/null | grep "Kernel driver" | awk '{print $NF}' || echo "none")
if [ "${GPU_DRIVER}" != "vfio-pci" ]; then
    log_warn "GPU is not bound to vfio-pci (current: ${GPU_DRIVER})"
    log_warn "Run ./scripts/gpu-passthrough/bind-gpu-to-vfio.sh first"
    read -p "Continue anyway? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        log_info "Aborted"
        exit 1
    fi
fi

# Create VM using virt-install
log_info "Creating VM ${VM_NAME}..."

virt-install \
    --name "${VM_NAME}" \
    --memory "${VM_MEMORY}" \
    --vcpus "${VM_VCPUS}" \
    --disk "path=${VM_DISK_PATH},format=qcow2,bus=virtio" \
    --cdrom "${ISO_PATH}" \
    --os-variant ubuntu24.04 \
    --machine q35 \
    --boot uefi \
    --graphics spice \
    --network network=default \
    --hostdev 0000:62:00.0 \
    --hostdev 0000:62:00.1 \
    --rombar 1 \
    --noautoconsole \
    --noreboot \
    --print-xml > "/tmp/${VM_NAME}.xml"

# The XML is printed but we need to define it properly
# virt-install --import would be needed, but for now just define manually
log_info "VM definition created. Please review and adjust as needed."
log_info "VM XML saved to: /tmp/${VM_NAME}.xml"

# Define the VM from XML
virsh define "/tmp/${VM_NAME}.xml"

log_info "VM ${VM_NAME} defined successfully"
log_info ""
log_info "Next steps:"
log_info "1. Install Ubuntu: virsh start ${VM_NAME} && virt-viewer ${VM_NAME}"
log_info "2. After install, remove GPU passthrough temporarily to install NVIDIA drivers"
log_info "3. Add GPU passthrough back and install CUDA toolkit"
log_info ""
log_info "Manage VM with:"
log_info "  virsh start ${VM_NAME}"
log_info "  virsh shutdown ${VM_NAME}"
log_info "  virt-viewer ${VM_NAME}"
