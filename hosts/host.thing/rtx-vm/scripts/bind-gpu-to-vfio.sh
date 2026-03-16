#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Bind GPU to vfio-pci for VM passthrough
# This script detaches the GPU from the host NVIDIA driver and binds it to vfio-pci

set -euo pipefail

VM_NAME="ai-gpu-vm"
GPU_PCI="62:00.0"
AUDIO_PCI="62:00.1"

echo "=== GPU Passthrough: Binding GPU to vfio-pci ==="

# Step 1: Stop the VM if running
echo "Stopping VM ${VM_NAME}..."
if virsh list --all | grep -q "${VM_NAME}"; then
    if virsh list | grep -q "${VM_NAME}"; then
        echo "VM is running, shutting down..."
        virsh shutdown "${VM_NAME}"
        sleep 5
    fi
    if virsh list --all | grep "${VM_NAME}" | grep -q "running"; then
        echo "VM still running, forcing stop..."
        virsh destroy "${VM_NAME}"
    fi
fi

# Step 2: Unbind GPU from NVIDIA driver
echo "Unbinding GPU from NVIDIA driver..."
if [ -f /sys/bus/pci/devices/0000:${GPU_PCI}/driver ]; then
    DRIVER=$(readlink /sys/bus/pci/devices/0000:${GPU_PCI}/driver | xargs basename)
    echo "Current driver: ${DRIVER}"
    
    if [ "${DRIVER}" = "nvidia" ]; then
        echo -n "0000:${GPU_PCI}" > /sys/bus/pci/drivers/nvidia/unbind 2>/dev/null || \
            echo "GPU already unbound from nvidia"
    elif [ "${DRIVER}" = "vfio-pci" ]; then
        echo "GPU already bound to vfio-pci"
    else
        echo "Unexpected driver: ${DRIVER}"
    fi
fi

echo "Unbinding audio device from NVIDIA..."
if [ -f /sys/bus/pci/devices/0000:${AUDIO_PCI}/driver ]; then
    DRIVER=$(readlink /sys/bus/pci/devices/0000:${AUDIO_PCI}/driver | xargs basename)
    echo "Current audio driver: ${DRIVER}"
    
    if [[ "${DRIVER}" == nvidia* ]]; then
        echo -n "0000:${AUDIO_PCI}" > /sys/bus/pci/drivers/${DRIVER}/unbind 2>/dev/null || \
            echo "Audio already unbound"
    elif [ "${DRIVER}" = "vfio-pci" ]; then
        echo "Audio already bound to vfio-pci"
    fi
fi

# Step 3: Bind to vfio-pci
echo "Binding GPU to vfio-pci..."
echo -n "0000:${GPU_PCI}" > /sys/bus/pci/drivers/vfio-pci/bind
echo -n "0000:${AUDIO_PCI}" > /sys/bus/pci/drivers/vfio-pci/bind

# Step 4: Verify
echo ""
echo "=== Verification ==="
lspci -k -s "${GPU_PCI}" | grep -A2 "Kernel driver" || true
lspci -k -s "${AUDIO_PCI}" | grep -A2 "Kernel driver" || true

echo ""
echo "GPU successfully bound to vfio-pci"
echo "You can now start the VM: virsh start ${VM_NAME}"
