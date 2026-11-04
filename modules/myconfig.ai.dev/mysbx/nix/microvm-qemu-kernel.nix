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
#   erofs                                             the guest-own store disk
#                                                     (docs/design/guest-store.md)
#   kvm (+ kvm_intel / kvm_amd)                        the nested-virt opt-in
#                                                     (docs/design/config.md D27): a
#                                                     `nested-virt = true` run boots
#                                                     -cpu host, so the guest gets
#                                                     VMX/SVM and its own /dev/kvm —
#                                                     BUILT IN because the static
#                                                     init loads no modules. They are
#                                                     INERT without VMX/SVM (the
#                                                     accel gate keeps a plain run on
#                                                     kvm:tcg), so ONE kernel serves
#                                                     both modes.
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
    # The virtio device stack — the VM's only hardware. The *_LIB
    # options stay at their defaults (VIRTIO_FS selects VIRTIO; an
    # explicit VIRTIO=yes makes the config generator re-ask — its
    # repeated-question refusal).
    VIRTIO_FS = yes;
    # The PCI transport the qemu microVM's devices ride on
    # (vhost-user-fs-PCI, virtio-blk-PCI) — without it built in, the
    # static init finds no devices at all. VIRTIO_PCI selects VIRTIO;
    # do not set VIRTIO itself (the generator's repeated-question
    # refusal).
    VIRTIO_PCI = yes;
    VIRTIO_BLK = yes;
    VIRTIO_NET = yes;
    VIRTIO_CONSOLE = yes;
    # The scratch (bd myconfig-dak.7) and the store overlay.
    EXT4_FS = yes;
    OVERLAY_FS = yes;
    # The guest-own nix store disk (docs/design/guest-store.md): the
    # erofs image the wrapper bakes mounts at /nix, read-only — and
    # the scratch overlay takes that mount as its lowerdir, so the
    # format must be built in like ext4 is (libkrunfw's own config
    # already carries it; the stock nixpkgs default does not).
    EROFS_FS = yes;
    # VIRTIO_FS=y selects FUSE — the default kernel ships it as a
    # module, and the config generator re-asks VIRTIO_FS when its
    # dependency stays m (the repeated-question refusal).
    FUSE_FS = yes;
    # The nested-virt opt-in (docs/design/config.md D27): the KVM
    # core and both vendor modules BUILT IN — the static init loads
    # no modules, so a nested-virt guest finds its /dev/kvm at boot.
    # Inert without VMX/SVM (no /dev/kvm node appears on a plain run:
    # the CPU carries no VMX for KVM to expose), so the same kernel
    # serves both modes with zero flags.
    # CRYPTO_DEV_CCP_DD comes with them because the nixpkgs default
    # config requires KVM_AMD_SEV=y (common-config.nix), whose
    # dependency `!(KVM_AMD=y && CRYPTO_DEV_CCP_DD=m)` would otherwise
    # leave the required answer UNUSED — a hard config error (the
    # generator's unused-option refusal). Building the ccp device
    # driver in keeps SEV satisfiable; both are inert without AMD
    # hardware.
    # NOTE on the generator's repeated-question refusal: KVM selects
    # VIRTUALIZATION; like VIRTIO above, the DEPENDENCY is left at
    # its default and only the leaf options are set here.
    KVM = yes;
    KVM_INTEL = yes;
    KVM_AMD = yes;
    CRYPTO_DEV_CCP_DD = yes;
  };
  # Faster build, smaller bzImage: no modules to sign or strip.
  buildDTBs = false;
}
