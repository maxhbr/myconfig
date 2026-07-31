# `t6`: FriendlyElec NanoPC-T6

This directory documents the planned NixOS installation for the FriendlyElec
NanoPC-T6 (`aarch64-linux`).

> [!WARNING]
> This is an installation plan, not yet a record of a verified working
> deployment.  Keep the vendor installation on eMMC intact until NixOS has
> booted successfully and the required hardware has been tested.

## Hardware

The NanoPC-T6 is a Rockchip RK3588 single-board computer intended for
networking, storage, multimedia, edge-computing, and industrial-control
applications.

| Component | Specification |
| --- | --- |
| SoC | Rockchip RK3588 |
| CPU | 6 × ARM Cortex-A73 + 2 × ARM Cortex-A53, ARMv8.2-A, up to 2.4 GHz |
| Architecture | AArch64 / `aarch64-linux` |
| GPU | ARM Mali-G610 MP4 |
| NPU | 6 TOPS (RKNN NPU) |
| VPU | H.265/H.264/VP9 decoding up to 4K60; H.265/H.264 encoding |
| Memory | 8, 16, 32, or 64 GB LPDDR5 |
| Onboard storage | 16, 32, 64, or 128 GB eMMC (depending on variant) |
| Removable storage | microSD/TF card |
| High-speed storage | 2 × M.2 PCIe 3.0 (NVMe SSD, 2242/2280) |
| Ethernet | 1 × 2.5 GbE (Realtek) + 1 × GbE (RK3588 GMAC) |
| Display | HDMI 2.1 up to 4K60, 2 × MIPI-DSI (up to 4K60) |
| USB | 1 × USB 3.0, 1 × USB 2.0, 1 × USB-C 3.0 |
| GPIO / expansion | UART, I²C, SPI, PWM, ADC, CAN, I²S, SPDIF |
| Power | 12 V DC input; nominal 12 V, 2 A |
| Dimensions | 120 × 100 mm |

## Upstream and vendor references

- [FriendlyElec NanoPC-T6 wiki](https://wiki.friendlyelec.com/wiki/index.php/NanoPC-T6)
- [EDK2 firmware for RK3588 boards](https://github.com/edk2-porting/edk2-rk3588)
- [EDK2 releases](https://github.com/edk2-porting/edk2-rk3588/releases)
- [NanoPC-T6 is a Platinum platform](https://github.com/edk2-porting/edk2-rk3588#platinum) (best support tier)
- [NixOS on ARM](https://wiki.nixos.org/wiki/NixOS_on_ARM)
- [NixOS ARM UEFI installation](https://wiki.nixos.org/wiki/NixOS_on_ARM/UEFI)
- [NixOS downloads](https://nixos.org/download/#nixos-iso)
- [NixOS U-Boot package](https://search.nixos.org/packages?query=uboot) (for the legacy U-Boot path)

## Installation strategy

Use the board-specific EDK2/UEFI image from edk2-porting/edk2-rk3588 to
initialize the hardware, then boot the standard NixOS AArch64 ISO through UEFI
and install to NVMe.

The initial layout should be:

| Device | Purpose |
| --- | --- |
| microSD | NanoPC-T6 EDK2 firmware |
| USB stick | NixOS AArch64 installer ISO |
| NVMe SSD | Final NixOS root filesystem |
| eMMC | Leave the vendor installation untouched during bring-up |

Keeping firmware, installer, and target system on separate devices makes
recovery easier and avoids destroying the working vendor image before the
NixOS hardware status is known.

> [!NOTE]
> The NanoPC-T6 cannot boot directly from NVMe/USB without firmware on SD or
> eMMC.  EDK2 on microSD provides that firmware layer, so NVMe becomes a
> viable NixOS target after the initial boot.

### Alternative: U-Boot / SD-image path (removed)

The previous approach used U-Boot and built a complete NixOS SD image via
`sdImage`.  This path has been **removed** in favour of EDK2/UEFI.  See
[install-nixos-nanopc-t6.md](./install-nixos-nanopc-t6.md) for recovery
instructions from git history if the U-Boot path is needed again.

## Prerequisites

- NanoPC-T6 and suitable power supply (12 V / 2 A)
- microSD card for EDK2
- USB stick for the NixOS installer
- NVMe SSD or eMMC as the NixOS target
- Wired Ethernet
- HDMI display and keyboard, or a serial UART adapter (3.3 V TTL)
- An existing Linux machine for writing images

A serial console is strongly recommended during initial bring-up.  The RK3588
debug UART is **ttyS2 at 1500000 baud** — faster than the standard 115200.
Use a 3.3 V TTL adapter, not an RS-232 voltage-level adapter.  Do not connect
the adapter's 5 V pin.

## 1. Download the EDK2 firmware

Download a release containing:

```text
nanopc-t6_UEFI_Release_v1.1.img
```

from the
[`edk2-porting/edk2-rk3588` releases](https://github.com/edk2-porting/edk2-rk3588/releases).

The NanoPC-T6 is a **Platinum** platform (best support tier) with full
mainline-compatible device tree support.  Its SD-card method writes the image
directly to the entire card, and the card remains installed because it supplies
the board firmware on every boot.

## 2. Write EDK2 to microSD

Identify the target device carefully:

```console
$ lsblk -o NAME,SIZE,MODEL,TYPE,TRAN
```

Write the image to the whole microSD device:

```console
$ sudo dd \
    if=nanopc-t6_UEFI_Release_v1.1.img \
    of=/dev/sdX \
    bs=4M \
    conv=fsync \
    status=progress
```

Replace `/dev/sdX` with the correct whole-disk device.  This operation destroys
all existing data on that device.

Alternatively use the helper script, which prompts for confirmation and
unmounts any mounted partitions first:

```console
$ ./flash-edk2-sd-card.sh nanopc-t6_UEFI_Release_v1.1.img /dev/sdX
```

If you omit the image path, the script downloads the firmware automatically
before flashing:

```console
$ ./flash-edk2-sd-card.sh /dev/sdX
```

By default it fetches `nanopc-t6_UEFI_Release_v1.1.img` from the
[`edk2-porting/edk2-rk3588` releases](https://github.com/edk2-porting/edk2-rk3588/releases).
Override the source with the `EDK2_URL` environment variable, or point at a
pre-downloaded image with `EDK2_IMG`.

Leave the EDK2 microSD installed during boot.

### EDK2 firmware configuration

After flashing and booting, you may want to adjust UEFI settings via the
firmware menu (press `Esc` at boot):

- **Device Tree mode**: `Device Manager` → `Rockchip Platform Configuration`
  → `ACPI / Device Tree` → `Config Table Mode` → **Device Tree** → **Mainline**
  (required for kernels ≥ 6.10).
- **Display**: if using kernels < 6.15, enable `Force UEFI GOP Display` to
  work around missing mainline DRM initialization.

## 3. Build and write the NixOS installer

Build the myconfig AArch64 installer ISO with the repo's ISO builder:

```console
$ ./build-iso-image.sh iso aarch64-linux
```

This produces an ISO under `../iso.aarch64-linux/` and, next to it, a generated
`dd.sh` flasher wired to that exact image.  `dd.sh` refuses anything that is not
a whole disk, prints the target with `lsblk`, prompts for confirmation,
unmounts any mounted partitions, then writes with `dd ... conv=fsync
status=progress`:

```console
$ ../iso.aarch64-linux/dd.sh /dev/sdY
```

Replace `/dev/sdY` with the whole USB-stick device.

Alternatively, write any AArch64 ISO manually:

```console
$ sudo dd \
    if=<some>-aarch64-linux.iso \
    of=/dev/sdY \
    bs=4M \
    conv=fsync \
    status=progress
```

The upstream minimal AArch64 ISO from <https://nixos.org/download/#nixos-iso>
(name similar to `nixos-minimal-<release>-aarch64-linux.iso`) also works as a
fallback.

Use an ISO rather than the generic ARM SD image: EDK2 should expose the board
as a UEFI AArch64 system, allowing the regular NixOS installer to boot
`EFI/BOOT/BOOTAA64.EFI`.

## 4. Connect the debug console

The RK3588 debug UART (UART2) is used by both EDK2 and the Linux kernel.
Settings:

```text
1500000 baud, 8 data bits, no parity, 1 stop bit
```

Example with `picocom`:

```console
$ picocom -b 1500000 /dev/ttyUSB0
```

> [!NOTE]
> The baud rate is **1500000** (not the common 115200).  The UART adapter
> and software must support this rate.  CH340 and CP2104 adapters generally
> handle it well.

## 5. Boot the installer

1. Insert the EDK2 microSD.
2. Insert the NixOS USB stick.
3. Connect Ethernet.
4. Connect serial or HDMI and a keyboard.
5. Power on the board.
6. Enter the UEFI boot menu if the USB installer is not selected automatically.
7. Select the USB device or `BOOTAA64.EFI`.

After the installer starts, inspect the environment:

```console
$ uname -m
aarch64

$ lsblk -o NAME,SIZE,MODEL,TYPE,TRAN
$ ip link
$ lspci -nn
$ dmesg
```

Do not assume which `/dev/mmcblkN` device is the microSD card and which is eMMC.
Determine this from size, model, boot media, and `dmesg`.

## 6. Select the installation target

NVMe is the preferred initial target because it allows the eMMC vendor system
to remain untouched.

For NVMe:

```console
$ export DISK=/dev/nvme0n1
```

Possible alternatives include:

```text
/dev/mmcblk0     (eMMC, typically the larger one)
/dev/mmcblk1     (eMMC or microSD, check sizes)
/dev/sda         (USB/SATA disk)
```

Verify the selected disk before continuing:

```console
$ lsblk -o NAME,SIZE,MODEL,SERIAL,TYPE,TRAN,MOUNTPOINTS
```

## 7. Partition the target

The `format-target-disk.sh` helper performs the whole partition/format/mount
sequence below (with a confirmation prompt and automatic `pN`/`N` suffix
detection), producing the `EFI`/`nixos` filesystem labels expected by
`hardware-configuration.nix`:

```console
$ sudo ./format-target-disk.sh /dev/nvme0n1
```

To do it manually instead, create a GPT with a 512 MiB EFI System Partition
and an ext4 root partition:

```console
$ sudo wipefs -a "$DISK"

$ sudo parted "$DISK" --script \
    mklabel gpt \
    mkpart ESP fat32 1MiB 513MiB \
    set 1 esp on \
    mkpart nixos ext4 513MiB 100%
```

NVMe and eMMC devices use `pN` partition suffixes:

```console
$ export ESP="${DISK}p1"
$ export ROOT="${DISK}p2"
```

SATA and USB disks such as `/dev/sda` normally use:

```console
$ export ESP="${DISK}1"
$ export ROOT="${DISK}2"
```

Format and mount:

```console
$ sudo mkfs.fat -F 32 -n EFI "$ESP"
$ sudo mkfs.ext4 -L nixos "$ROOT"

$ sudo mount "$ROOT" /mnt
$ sudo mkdir -p /mnt/boot
$ sudo mount "$ESP" /mnt/boot
```

## 8. Generate the NixOS configuration

```console
$ sudo nixos-generate-config --root /mnt
```

The generated `hardware-configuration.nix` must be retained and committed only
after checking that its filesystem UUIDs and detected modules correspond to
the intended target disk.

A minimal bring-up configuration can start with:

```nix
{ config, lib, pkgs, ... }:

{
  nixpkgs.hostPlatform = "aarch64-linux";

  boot.loader.systemd-boot.enable = true;

  # Embedded UEFI implementations do not always provide reliable persistent
  # EFI variables.  Install the removable-media fallback loader instead.
  boot.loader.efi.canTouchEfiVariables = false;

  # Prefer a recent kernel while RK3588 mainline support is being evaluated.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # TODO: enable and pin the board device tree only after confirming the
  # DTB exists in the selected kernel package (see below):
  #   hardware.deviceTree = {
  #     enable = true;
  #     name = "rockchip/rk3588-nanopc-t6.dtb";
  #   };

  # RK3588 debug UART is ttyS2 at 1500000 baud.
  boot.kernelParams = [
    "console=ttyS2,1500000n8"
    "console=tty0"
  ];

  networking.hostName = "t6";
  networking.useDHCP = lib.mkDefault true;

  services.openssh.enable = true;

  system.stateVersion = "25.11";
}
```

Integrate these settings into this repository's normal host/module structure
instead of treating the example as the final host configuration.

## 9. Install

Review the generated files and then run:

```console
$ sudo nixos-install --flake /path/to/myconfig#t6
```

Set any required passwords or install SSH authorized keys through the host
configuration.

Reboot:

```console
$ sudo reboot
```

Remove the NixOS USB stick but keep the EDK2 microSD inserted.

## 10. Validate the first boot

Confirm at least:

```console
$ uname -a
$ systemctl --failed
$ journalctl -b -p warning
$ ip -brief link
$ ethtool <ethernet-interface>
$ lsblk
$ lspci -nn
$ lsusb
$ dmesg
```

Test both Ethernet ports separately.  Also verify that the machine can complete
a rebuild from the repository:

```console
$ sudo nixos-rebuild test --flake .#t6
```

Use the repository's own wrapper commands instead when they are required by
the flake structure.

## Moving EDK2 to eMMC or SPI NOR

Once NixOS is proven stable on NVMe, EDK2 can optionally be moved from microSD
to eMMC or SPI NOR flash (if the board variant has one).

The edk2-rk3588 project documents flashing to SPI NOR via `RKDevTool` from
Linux booted on the device, or through Maskrom mode from another computer.
See <https://docs.radxa.com/en/rock5/lowlevel-development/bootloader_spi_flash>
for the general RK3588 procedure.

This operation is destructive and removes the vendor eMMC installation when
targeting eMMC.  Retain a recovery image and verify Loader/MaskRom access
before attempting it.

The simplest maintainable layout may remain:

- EDK2 on a small microSD card
- NixOS and persistent data on NVMe
- eMMC retained as a recovery or vendor-test system

## Recovery

The board provides Rockchip Loader and MaskRom recovery modes.  Before
changing eMMC firmware:

1. Download and retain the vendor recovery tools and images.
2. Confirm that the host detects the board over USB-C in Loader or MaskRom mode.
3. Record the required button sequence and USB port (MASK button + power on).
4. Back up any useful eMMC contents.
5. Keep the EDK2 microSD and NixOS installer USB available.

## Expected hardware-support status

The following is a planning estimate, not a verified compatibility matrix.

| Component | Initial expectation | Notes |
| --- | --- | --- |
| CPU and RAM | Likely | Standard RK3588/AArch64 support |
| microSD and eMMC | Likely | Device naming must be checked carefully |
| USB | Likely | Verify all ports and USB-C behavior |
| NVMe (both slots) | Likely | Preferred NixOS target; check M.2 slot PCIe/SATA mux |
| HDMI | Plausible | Basic display is more likely than full multimedia acceleration |
| 2.5 GbE (Realtek) | Likely | Verify the actual MAC/PHY combination |
| GbE (GMAC) | Requires testing | Board device-tree details matter |
| DisplayPort (USB-C) | Requires testing | May have hot-plug and orientation quirks |
| Wi-Fi/Bluetooth | Requires testing | May require firmware blobs and board-specific configuration |
| Mali GPU | Partial/uncertain | Mainline Panfrost may provide acceleration; verify in practice |
| Hardware video codecs | Partial/uncertain | Vendor BSP support is usually broader than mainline |
| NPU (RKNN) | Unlikely by default | Requires Rockchip's RKNN software stack |
| MIPI-DSI | Uncertain | Board-specific display pipeline |
| GPIO/I²C/SPI/UART/CAN | Requires testing | Pin control and device-tree configuration are significant |
| SPI NOR flash | Varies by variant | Some T6 revisions include it, others do not |
| Fan control | Requires testing | PWM fan header available on the GPIO header |

## Device tree and kernel fallback

EDK2 supplies the platform firmware and a board description, but successful
UEFI boot does not guarantee that every board peripheral is supported by the
NixOS kernel.

The vendor's board device-tree name is:

```text
rockchip/rk3588-nanopc-t6.dtb
```

For the NanoPC-T6 **LTS** variant, try:

```text
rockchip/rk3588-nanopc-t6-lts.dtb
```

First check whether the selected NixOS kernel already contains it:

```console
$ find /run/current-system/kernel-modules/lib/modules \
    -path '*dtbs*' \
    -name '*rk3588*nanopc*t6*.dtb'
```

A NixOS device-tree override may look like:

```nix
{
  hardware.deviceTree = {
    enable = true;
    name = "rockchip/rk3588-nanopc-t6.dtb";
  };
}
```

Only enable this after confirming that the DTB exists in the selected kernel
package and is appropriate for that kernel version.

Do not take a DTB from an old vendor kernel and assume it is ABI-compatible
with a current mainline kernel.  If essential hardware is missing, use this
order of investigation:

1. Confirm that EDK2 is using the correct NanoPC-T6 platform image.
2. Verify EDK2 device tree mode is set to **Mainline** (not Vendor).
3. Test the latest NixOS kernel.
4. Inspect the DTB selected by firmware and the DTBs shipped by the kernel.
5. Test a current mainline board DTB or a narrowly scoped device-tree patch.
6. Package a custom mainline kernel if required.
7. Use FriendlyElec's downstream kernel only as a last-resort compatibility bridge.

## Bring-up checklist

- [ ] Record the exact board revision and RAM/eMMC variant.
- [ ] Record the EDK2 release and image checksum.
- [ ] Boot the NixOS AArch64 installer through EDK2.
- [ ] Capture the complete serial boot log.
- [ ] Confirm CPU, RAM, and thermal sensors.
- [ ] Identify microSD, eMMC, NVMe, and SATA device names.
- [ ] Install NixOS to NVMe without modifying eMMC.
- [ ] Confirm reboot through `EFI/BOOT/BOOTAA64.EFI`.
- [ ] Test both Ethernet ports (2.5 GbE + GbE).
- [ ] Test all USB ports and USB-C.
- [ ] Test HDMI output.
- [ ] Check Wi-Fi and Bluetooth firmware/driver status.
- [ ] Check NVMe stability under sustained I/O (both slots).
- [ ] Check thermals under sustained CPU and storage load.
- [ ] Decide whether EDK2 should remain on microSD or move to SPI NOR/eMMC.
- [ ] Replace the planning estimates above with measured results.
