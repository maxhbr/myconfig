#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Unbind GPU from vfio-pci and return to NVIDIA driver
# This script detaches the GPU from vfio-pci and binds it back to the NVIDIA driver

set -euo pipefail

VM_NAME="ai-gpu-vm"
GPU_PCI="62:00.0"
AUDIO_PCI="62:00.1"

echo "=== GPU Passthrough: Unbinding GPU from vfio-pci ==="

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

# Step 2: Unbind from vfio-pci
echo "Unbinding GPU from vfio-pci..."
if [ -f /sys/bus/pci/devices/0000:${GPU_PCI}/driver ]; then
    DRIVER=$(readlink /sys/bus/pci/devices/0000:${GPU_PCI}/driver | xargs basename)
    echo "Current driver: ${DRIVER}"
    
    if [ "${DRIVER}" = "vfio-pci" ]; then
        echo -n "0000:${GPU_PCI}" > /sys/bus/pci/drivers/vfio-pci/unbind
        echo "GPU unbound from vfio-pci"
    else
        echo "GPU not bound to vfio-pci (driver: ${DRIVER})"
    fi
else
    echo "GPU device not found or has no driver"
fi

echo "Unbinding audio device from vfio-pci..."
if [ -f /sys/bus/pci/devices/0000:${AUDIO_PCI}/driver ]; then
    DRIVER=$(readlink /sys/bus/pci/devices/0000:${AUDIO_PCI}/driver | xargs basename)
    echo "Current audio driver: ${DRIVER}"
    
    if [ "${DRIVER}" = "vfio-pci" ]; then
        echo -n "0000:${AUDIO_PCI}" > /sys/bus/pci/drivers/vfio-pci/unbind
        echo "Audio unbound from vfio-pci"
    else
        echo "Audio not bound to vfio-pci (driver: ${DRIVER})"
    fi
else
    echo "Audio device not found or has no driver"
fi

# Step 3: Bind to NVIDIA driver
echo ""
echo "Binding GPU to NVIDIA driver..."
echo -n "0000:${GPU_PCI}" > /sys/bus/pci/drivers/nvidia/bind
echo "GPU bound to nvidia"

echo "Binding audio device to NVIDIA driver..."
echo -n "0000:${AUDIO_PCI}" > /sys/bus/pci/drivers/nvidia_hdc/bind 2>/dev/null || \
    echo "Audio binding to nvidia_hdc skipped (may be bound separately)"

# Step 4: Verify
echo ""
echo "=== Verification ==="
lspci -k -s "${GPU_PCI}" | grep -A2 "Kernel driver" || true

echo ""
echo "GPU successfully returned to NVIDIA driver"
echo "You can now use the GPU on the host: nvidia-smi"
