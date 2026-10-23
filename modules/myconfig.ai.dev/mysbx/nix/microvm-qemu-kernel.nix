# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The microvm-qemu backend's guest kernel (backends.md D9, bd
# myconfig-2l9b): the one input libkrun bundles (libkrunfw) and a
# stock qemu needs handed over as a bzImage.
#
# The guest is the krun rootfs's static-busybox init — NO initrd, no
# module loader: every driver the boot needs must be BUILT IN. The
# nixpkgs default kernel builds the virtio stack as MODULES
# (CONFIG_VIRTIO_FS=m, VIRTIO_PCI=m, EXT4_FS=m, OVERLAY_FS=m), so this
# pin takes the kernel package as a PARAMETER and rebuilds it with the
# microvm set compiled in:
#
#   virtio_pci / virtio_fs / virtio_blk / virtio_net  the devices
#   ext4 + overlayfs                                  the scratch + store overlay
#   8250 console                                      already =y, the serial console
#   tmpfs, proc, sysfs                                 already =y, the init's mounts
#
# A parameter, not a `pkgs.` reference (the same convention as every
# input of ./mysbx.nix), so a host pins exactly the kernel version its
# runs need; the module options pass `linuxKernel.packages.linux_XY`.
#
{
  lib,
  # The kernel PACKAGE (e.g. pkgs.linuxKernel.packages.linux_6_12) whose
  # `.kernel` is rebuilt with the microvm config. Passed as the package
  # set so the version choice stays the caller's.
  kernelPackages ? null,
}:
assert lib.assertMsg (kernelPackages != null) "microvm-qemu-kernel.nix needs kernelPackages";
let
  inherit (lib.kernel) yes no module;
in
kernelPackages.kernel.override {
  structuredExtraConfig = with lib.kernel; {
    # The virtio device stack — the VM's only hardware.
    VIRTIO = yes;
    VIRTIO_PCI = yes;
    VIRTIO_PCI_LIB = yes;
    VIRTIO_PCI_LIB_LEGACY = yes;
    VIRTIO_PCI_LEGACY = yes;
    VIRTIO_FS = yes;
    VIRTIO_BLK = yes;
    VIRTIO_NET = yes;
    VIRTIO_CONSOLE = yes;
    # The scratch (bd myconfig-dak.7) and the store overlay.
    EXT4_FS = yes;
    OVERLAY_FS = yes;
    # The exit channel (D9): the virtio-serial port the init reports
    # the payload's exit code over.
    VIRTIO_MENU = yes;
    # Headless: no firmware/ACPI dependency in the boot path beyond
    # what -M microvm provides (acpi=on in the qemu machine opts).
  };
  # Faster build, smaller bzImage: no modules to sign or strip.
  buildDTBs = false;
}
