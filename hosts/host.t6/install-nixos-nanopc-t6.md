# Installing NixOS on the FriendlyElec NanoPC-T6

This guide describes a practical way to install NixOS on the FriendlyElec NanoPC-T6 / NanoPC-T6 LTS.

The NanoPC-T6 is a Rockchip RK3588-based ARM board. Treat this as an unsupported or semi-supported ARM SBC installation rather than a standard x86 NixOS install.

## Important boot limitation

The NanoPC-T6 cannot boot directly from M.2/NVMe or USB storage. The bootloader and boot partition must live on eMMC or TF/microSD.

You can still place the root filesystem on NVMe or USB, but the board must start booting from eMMC or microSD first.

Recommended installation order:

1. Boot NixOS from microSD.
2. Confirm kernel, device tree, Ethernet, and storage detection.
3. Install NixOS to eMMC.
4. Optionally move the root filesystem to NVMe while keeping `/boot` on eMMC or microSD.

## Requirements

- NanoPC-T6 or NanoPC-T6 LTS
- microSD card
- Linux or NixOS host machine
- 12V/2A power supply
- Ethernet connection
- Optional but strongly recommended: serial UART adapter
- Nix installed on the host machine

## 1. Create a NixOS AArch64 SD image

Create a file named `nanopc-t6-sd-image.nix`:

```nix
{ config, lib, pkgs, ... }:

{
  imports = [
    "${pkgs.path}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
  ];

  nixpkgs.hostPlatform = "aarch64-linux";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # NanoPC-T6
  hardware.deviceTree.name = "rockchip/rk3588-nanopc-t6.dtb";

  # For NanoPC-T6 LTS, try this instead if your kernel provides it:
  # hardware.deviceTree.name = "rockchip/rk3588-nanopc-t6-lts.dtb";

  services.openssh.enable = true;
  networking.useDHCP = lib.mkDefault true;

  users.users.root.initialPassword = "nixos";

  system.stateVersion = "25.11";
}
```

Build the image:

```bash
nix-build '<nixpkgs/nixos>' \
  -A config.system.build.sdImage \
  -I nixos-config=./nanopc-t6-sd-image.nix
```

The image will be created under:

```bash
result/sd-image/
```

Decompress it:

```bash
unzstd result/sd-image/*.img.zst
```

## 2. Flash the NixOS image to microSD

Find the target device:

```bash
lsblk
```

Flash the image.

Replace `/dev/sdX` with the whole microSD device, not a partition such as `/dev/sdX1`.

```bash
sudo dd if=result/sd-image/*.img of=/dev/sdX bs=4M status=progress conv=fsync
sync
```

## 3. Add NanoPC-T6 U-Boot to the microSD card

The generic NixOS ARM image normally does not include board-specific Rockchip U-Boot firmware.

Build the NanoPC-T6 U-Boot package:

```bash
nix build nixpkgs#uboot-nanopc-t6-rk3588_defconfig
find result -type f
```

If the build result contains `u-boot-rockchip.bin`, flash it at sector offset 64:

```bash
sudo dd if=result/u-boot-rockchip.bin of=/dev/sdX seek=64 conv=notrunc,fsync
sync
```

If the build result instead contains separate `idbloader.img` and `u-boot.itb` files, use the older two-file Rockchip layout:

```bash
sudo dd if=result/idbloader.img of=/dev/sdX seek=64 conv=notrunc,fsync
sudo dd if=result/u-boot.itb     of=/dev/sdX seek=16384 conv=notrunc,fsync
sync
```

## 4. Boot the NanoPC-T6 from microSD

1. Insert the microSD card.
2. Connect Ethernet.
3. Connect HDMI/keyboard or serial UART.
4. Apply power.

Log in with:

```text
user: root
password: nixos
```

Check the system:

```bash
uname -a
lsblk
ip addr
dmesg | less
```

If networking works, rebuild or customize from the running system:

```bash
nano /etc/nixos/configuration.nix
nixos-rebuild switch
```

## 5. Install NixOS to eMMC

Once the microSD boot is working, install to eMMC from the running NixOS system.

Identify devices carefully:

```bash
lsblk
```

Common naming may be:

```text
/dev/mmcblk0   eMMC
/dev/mmcblk1   microSD
```

Do not assume this blindly. Verify the device sizes before continuing.

Partition the eMMC:

```bash
sudo parted /dev/mmcblk0 -- mklabel gpt
sudo parted /dev/mmcblk0 -- mkpart primary ext4 32MiB 100%
sudo parted /dev/mmcblk0 -- set 1 boot on
sudo mkfs.ext4 -L nixos /dev/mmcblk0p1
```

Mount the target system:

```bash
sudo mount /dev/disk/by-label/nixos /mnt
```

Generate the initial configuration:

```bash
sudo nixos-generate-config --root /mnt
```

Edit the generated config:

```bash
sudo nano /mnt/etc/nixos/configuration.nix
```

Ensure it contains at least:

```nix
{ config, lib, pkgs, ... }:

{
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  hardware.deviceTree.name = "rockchip/rk3588-nanopc-t6.dtb";

  services.openssh.enable = true;
  networking.useDHCP = lib.mkDefault true;

  system.stateVersion = "25.11";
}
```

Install NixOS:

```bash
sudo nixos-install
```

Flash U-Boot to eMMC as well.

If using `u-boot-rockchip.bin`:

```bash
sudo dd if=result/u-boot-rockchip.bin of=/dev/mmcblk0 seek=64 conv=notrunc,fsync
sync
```

If using separate Rockchip U-Boot files:

```bash
sudo dd if=result/idbloader.img of=/dev/mmcblk0 seek=64 conv=notrunc,fsync
sudo dd if=result/u-boot.itb     of=/dev/mmcblk0 seek=16384 conv=notrunc,fsync
sync
```

Power off:

```bash
sudo poweroff
```

Remove the microSD card and power the board back on.

## 6. Optional: Use NVMe for the root filesystem

The board cannot boot directly from NVMe, but NixOS can use NVMe as `/` after U-Boot and `/boot` are loaded from eMMC or microSD.

A typical layout is:

- eMMC or microSD: U-Boot and `/boot`
- NVMe: `/`

Example `/etc/nixos/hardware-configuration.nix` structure:

```nix
{
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BOOT_UUID";
    fsType = "ext4";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/NVME_ROOT_UUID";
    fsType = "ext4";
  };
}
```

Get UUIDs with:

```bash
lsblk -f
```

After changing filesystem layout, rebuild:

```bash
sudo nixos-rebuild switch
```

## Troubleshooting

### Board does not boot

Use serial UART if possible. HDMI may not initialize early enough to show bootloader or kernel messages.

Check:

- Was U-Boot written to the correct whole device?
- Was the correct device tree selected?
- Is the power supply adequate?
- Is the SD card known-good?
- Is the board trying to boot from eMMC before microSD?

### No Ethernet

Check kernel logs:

```bash
dmesg | grep -i eth
dmesg | grep -i r8169
dmesg | grep -i stmmac
```

Also check interfaces:

```bash
ip link
```

### Device tree not found

Inspect the available kernel device trees:

```bash
find /run/current-system/kernel-modules/lib/firmware /run/current-system -name '*nanopc*t6*.dtb' 2>/dev/null
find /boot -name '*.dtb'
```

You may need a newer kernel or a different device tree name.

### Need to recover the board

FriendlyElec documents Maskrom recovery for RK3588 boards. In general:

1. Hold the MASK button.
2. Power the board.
3. Connect USB-C to a host computer.
4. Use Rockchip tooling to erase or reflash eMMC/SPI.

Refer to the FriendlyElec NanoPC-T6 wiki for board-specific recovery details.

## Notes

Hardware support may vary depending on the kernel version and firmware availability.

Features that may require extra work include:

- HDMI
- GPU acceleration
- VPU/video acceleration
- Wi-Fi and Bluetooth modules
- fan control
- PCIe/NVMe quirks
- NPU/vendor acceleration

Start with a minimal headless Ethernet setup first. Add hardware-specific features incrementally after the base system boots reliably.

## References

- FriendlyElec NanoPC-T6 wiki: <https://wiki.friendlyelec.com/wiki/index.php/NanoPC-T6>
- NixOS on ARM: <https://nixos.wiki/wiki/NixOS_on_ARM>
- NixOS package search for U-Boot: <https://search.nixos.org/packages?query=uboot>
- U-Boot Rockchip documentation: <https://docs.u-boot.org/en/latest/board/rockchip/rockchip.html>
