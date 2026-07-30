# `roc`: Firefly ROC-RK3568-PC

This directory documents the planned NixOS installation for the Firefly
ROC-RK3568-PC (`aarch64-linux`).

> [!WARNING]
> This is an installation plan, not yet a record of a verified working
> deployment. Keep the vendor installation on eMMC intact until NixOS has
> booted successfully and the required hardware has been tested.

## Hardware

The ROC-RK3568-PC is a Rockchip RK3568 single-board computer intended for
networking, storage, multimedia, edge-computing, and industrial-control
applications.

| Component | Specification |
| --- | --- |
| SoC | Rockchip RK3568 |
| CPU | 4 × ARM Cortex-A55, ARMv8.2-A, up to 2.0 GHz |
| Architecture | AArch64 / `aarch64-linux` |
| GPU | ARM Mali-G52 2EE |
| NPU | RKNN NPU, rated at 0.8 TOPS INT8 |
| VPU | H.265/H.264/VP9 decoding up to 4K60; H.265/H.264 encoding up to 1080p60 |
| Memory | 2, 4, or 8 GB LPDDR4 |
| Onboard storage | 32, 64, or 128 GB eMMC, depending on variant |
| Removable storage | microSD/TF card |
| High-speed storage | M.2 PCIe 3.0 NVMe, sizes 2242 and 2280 |
| SATA | SATA 3.0 for a 2.5-inch SSD or HDD |
| Ethernet | 2 × Gigabit Ethernet |
| Wireless | Optional/onboard Wi-Fi 6 and Bluetooth 5.0, depending on variant |
| Display | HDMI 2.0 up to 4K60, 2 × MIPI-DSI, eDP 1.3 |
| Camera | 2 × MIPI-CSI |
| USB | 1 × USB 3.0, 2 × USB 2.0, 1 × USB-C OTG |
| Industrial I/O | 1 × RS-485 and 2 × RS-232 through the RJ45 control port |
| Expansion | GPIO, UART, I²C, SPI, PWM, ADC, CAN, I²S, SPDIF |
| Power | 9–24 V DC input; nominal 12 V, 5.5 × 2.1 mm barrel connector |
| Dimensions | 138.0 × 77.5 × 19.9 mm |

The vendor specification lists approximately 0.3 W idle, 4.2 W normal, and
7.8 W maximum board power consumption. Actual consumption will depend on
storage, peripherals, network activity, and CPU load.

## Upstream and vendor references

- [Firefly product page](https://en.t-firefly.com/product/industry/rocrk3568pc.html)
- [ROC-RK3568-PC specification PDF](https://download.t-firefly.com/%E4%BA%A7%E5%93%81%E8%A7%84%E6%A0%BC%E6%96%87%E6%A1%A3/%E5%BC%80%E6%BA%90%E4%B8%BB%E6%9D%BF/ROC-RK3568-PC%20Specification.pdf)
- [Firefly ROC-RK3568-PC manual](https://wiki.t-firefly.com/en/ROC-RK3568-PC/)
- [Firefly firmware downloads](https://en.t-firefly.com/doc/download/107.html)
- [EDK2 firmware for RK356x boards](https://github.com/S199pWa1k9r/edk2-rk356x)
- [ROC-RK3568-PC EDK2 installation notes](https://github.com/S199pWa1k9r/edk2-rk356x/blob/main/docs/firefly-ROC-RK356x-PC.md)
- [EDK2 releases](https://github.com/S199pWa1k9r/edk2-rk356x/releases)
- [NixOS on ARM](https://wiki.nixos.org/wiki/NixOS_on_ARM)
- [NixOS ARM UEFI installation](https://wiki.nixos.org/wiki/NixOS_on_ARM/UEFI)
- [NixOS downloads](https://nixos.org/download/#nixos-iso)

## Installation strategy

Use a board-specific EDK2/UEFI image to initialize the hardware, then boot the
standard NixOS AArch64 ISO through UEFI.

The initial layout should be:

| Device | Purpose |
| --- | --- |
| microSD | ROC-RK3568-PC EDK2 firmware |
| USB stick | NixOS AArch64 installer ISO |
| NVMe SSD | Final NixOS root filesystem |
| eMMC | Leave the vendor installation untouched during bring-up |

Keeping firmware, installer, and target system on separate devices makes
recovery easier and avoids destroying the working vendor image before the
NixOS hardware status is known.

## Prerequisites

- ROC-RK3568-PC and suitable power supply
- microSD card for EDK2
- USB stick for the NixOS installer
- NVMe SSD, SATA disk, or eMMC as the NixOS target
- Wired Ethernet
- HDMI display and keyboard, or a 3.3 V TTL serial adapter
- An existing Linux machine for writing images

A serial console is strongly recommended during initial bring-up. Use a
3.3 V TTL adapter, not an RS-232 voltage-level adapter. Do not connect the
adapter's 5 V pin.

## 1. Download the EDK2 firmware

Download a release containing:

```text
ROC-RK3568-PC_EFI.img
```

from the
[`edk2-rk356x` releases](https://github.com/S199pWa1k9r/edk2-rk356x/releases).

The project documents the ROC-RK3568-PC explicitly. Its SD-card method writes
the image directly to the entire card, and the card remains installed because
it supplies the board firmware on every boot.

## 2. Write EDK2 to microSD

Identify the target device carefully:

```console
$ lsblk -o NAME,SIZE,MODEL,TYPE,TRAN
```

Write the image to the whole microSD device:

```console
$ sudo dd \
    if=ROC-RK3568-PC_EFI.img \
    of=/dev/sdX \
    bs=4M \
    conv=fsync \
    status=progress
```

Replace `/dev/sdX` with the correct whole-disk device. This operation destroys
all existing data on that device.

Alternatively use the helper script, which prompts for confirmation and
unmounts any mounted partitions first:

```console
$ ./flash-edk2-sd-card.sh ROC-RK3568-PC_EFI.img /dev/sdX
```

If you omit the image path, the script downloads (and decompresses) a known
firmware image automatically before flashing:

```console
$ ./flash-edk2-sd-card.sh /dev/sdX
```

By default it fetches `ROC-RK3568-PC_EFI.img.gz` from the
[`jaredmcneill/quartz64_uefi` releases](https://github.com/jaredmcneill/quartz64_uefi/releases).
Override the source with the `EDK2_URL` environment variable, or point at a
pre-downloaded image with `EDK2_IMG`.

Leave the EDK2 microSD installed during boot.

## 3. Build and write the NixOS installer

Build the myconfig AArch64 installer ISO with the repo's ISO builder:

```console
$ ./build-iso-image.sh iso aarch64-linux
```

This produces an ISO under `../iso.aarch64-linux/` and, next to it, a generated
`dd.sh` flasher wired to that exact image. `dd.sh` refuses anything that is not
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

The EDK2 documentation identifies UART2 as the firmware debug console and uses:

```text
115200 baud, 8 data bits, no parity, 1 stop bit
```

Example with `picocom`:

```console
$ picocom -b 115200 /dev/ttyUSB0
```

The Linux kernel may later expose a Rockchip serial console with a different
device name or baud rate. Do not assume that the firmware and kernel console
settings are identical.

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
/dev/mmcblk0
/dev/mmcblk1
/dev/sda
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
{ lib, pkgs, ... }:

{
  nixpkgs.hostPlatform = "aarch64-linux";

  boot.loader.systemd-boot.enable = true;

  # Embedded UEFI implementations do not always provide reliable persistent
  # EFI variables. Install the removable-media fallback loader instead.
  boot.loader.efi.canTouchEfiVariables = false;

  # Prefer a recent kernel while RK3568 mainline support is being evaluated.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "roc";
  networking.useDHCP = lib.mkDefault true;

  services.openssh.enable = true;

  environment.systemPackages = with pkgs; [
    ethtool
    git
    pciutils
    usbutils
    vim
  ];

  system.stateVersion = "26.05";
}
```

Integrate these settings into this repository's normal host/module structure
instead of treating the example as the final host configuration.

Do not add speculative serial `boot.kernelParams` until the working Linux
console device and baud rate have been confirmed from the boot log.

## 9. Install

Review the generated files and then run:

```console
$ sudo nixos-install
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

Test both Ethernet ports separately. Also verify that the machine can complete
a rebuild from the repository:

```console
$ sudo nixos-rebuild test --flake .#roc
```

Use the repository's own wrapper commands instead when they are required by
the flake structure.

## Expected hardware-support status

The following is a planning estimate, not a verified compatibility matrix.

| Component | Initial expectation | Notes |
| --- | --- | --- |
| CPU and RAM | Likely | Standard RK3568/AArch64 support |
| microSD and eMMC | Likely | Device naming must be checked carefully |
| USB | Likely | Verify all ports and OTG behavior |
| NVMe | Likely | Preferred NixOS target |
| HDMI | Plausible | Basic display is more likely than full multimedia acceleration |
| One Ethernet port | Likely | Verify the actual MAC/PHY combination |
| Both Ethernet ports | Requires testing | Board device-tree details matter |
| SATA | Requires testing | Depends on controller, power, and device-tree support |
| Wi-Fi/Bluetooth | Requires testing | May require firmware blobs and board-specific configuration |
| Mali GPU | Partial/uncertain | Mainline Panfrost may provide acceleration; verify in practice |
| Hardware video codecs | Partial/uncertain | Vendor BSP support is usually broader than mainline |
| NPU | Unlikely by default | Requires Rockchip's RKNN software stack rather than normal NixOS support |
| MIPI-DSI/eDP/CSI | Uncertain | Board-specific display and camera pipelines |
| RS-232/RS-485/CAN/GPIO | Requires testing | Pin control and device-tree configuration are significant |

## Device tree and kernel fallback

EDK2 supplies the platform firmware and a board description, but successful
UEFI boot does not guarantee that every board peripheral is supported by the
NixOS kernel.

The vendor's board device-tree name is commonly associated with:

```text
rockchip/rk3568-firefly-roc-pc.dtb
```

First check whether the selected NixOS kernel already contains it:

```console
$ find /run/current-system/kernel-modules/lib/modules \
    -path '*dtbs*' \
    -name '*rk3568*roc*pc*.dtb'
```

A NixOS device-tree override may eventually look like:

```nix
{
  hardware.deviceTree = {
    enable = true;
    name = "rockchip/rk3568-firefly-roc-pc.dtb";
  };
}
```

Only enable this after confirming that the DTB exists in the selected kernel
package and is appropriate for that kernel version.

Do not take a DTB from an old vendor kernel and assume it is ABI-compatible
with a current mainline kernel. If essential hardware is missing, use this
order of investigation:

1. Confirm that EDK2 is using the correct ROC-RK3568-PC platform image.
2. Test the latest NixOS kernel.
3. Inspect the DTB selected by firmware and the DTBs shipped by the kernel.
4. Test a current mainline board DTB or a narrowly scoped device-tree patch.
5. Package a custom mainline kernel if required.
6. Use Firefly's downstream kernel only as a last-resort compatibility bridge.

## Moving EDK2 to eMMC

Once NixOS is proven stable on NVMe, EDK2 can optionally be moved from microSD
to eMMC. The upstream EDK2 documentation describes using Rockchip's
`upgrade_tool` in Loader or MaskRom mode and writing:

```text
ROC-RK3568-PC_EFI.img
```

to eMMC.

This operation is destructive and removes the vendor eMMC installation.
Retain a recovery image and verify Loader/MaskRom access before attempting it.

The simplest maintainable layout may remain:

- EDK2 on a small microSD card
- NixOS and persistent data on NVMe
- eMMC retained as a recovery or vendor-test system

## Recovery

The board provides Rockchip Loader and MaskRom recovery modes. Before changing
eMMC firmware:

1. Download and retain the vendor recovery tools and images.
2. Confirm that the host detects the board over USB in Loader or MaskRom mode.
3. Record the required button sequence and USB port.
4. Back up any useful eMMC contents.
5. Keep the EDK2 microSD and NixOS installer USB available.

## Bring-up checklist

- [ ] Record the exact board revision and RAM/eMMC variant.
- [ ] Record the EDK2 release and image checksum.
- [ ] Boot the NixOS AArch64 installer through EDK2.
- [ ] Capture the complete serial boot log.
- [ ] Confirm CPU, RAM, and thermal sensors.
- [ ] Identify microSD, eMMC, NVMe, and SATA device names.
- [ ] Install NixOS to NVMe without modifying eMMC.
- [ ] Confirm reboot through `EFI/BOOT/BOOTAA64.EFI`.
- [ ] Test both Gigabit Ethernet ports.
- [ ] Test all USB ports.
- [ ] Test HDMI output.
- [ ] Check Wi-Fi and Bluetooth firmware/driver status.
- [ ] Check NVMe stability under sustained I/O.
- [ ] Check thermals under sustained CPU and storage load.
- [ ] Decide whether EDK2 should remain on microSD or move to eMMC.
- [ ] Replace the planning estimates above with measured results.
