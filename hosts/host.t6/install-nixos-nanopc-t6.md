# U-Boot-based installation (legacy path)

> [!NOTE]
> The primary installation path now uses **EDK2/UEFI** firmware (see
> [README.md](./README.md)).  This file documents the legacy U-Boot approach
> which is still available if EDK2 does not work for your use case.

The legacy path builds a complete NixOS SD image with U-Boot baked in and
flashes it directly to microSD or eMMC.

## Key differences from the EDK2 path

- Uses **U-Boot** instead of EDK2/UEFI as the board firmware layer
- Builds a full NixOS SD image (with extlinux config and device tree) via
  `sdImage` in `hardware-configuration.nix`
- The image is directly bootable from microSD — no separate installer step
- U-Boot is fused into the image at sector offset 64 (32 KiB) per Rockchip
  boot ROM layout
- The built image already has U-Boot included — no separate flashing step

## Quick start

From the flake root:

```bash
# Build and flash the SD image in one step:
./hosts/host.t6/flash-sd-card.sh /dev/sdX

# Or build only (print image path):
./hosts/host.t6/flash-sd-card.sh --build-only
```

The script builds the `t6` SD image (with U-Boot fused in), then flashes it
to the specified device with `dd conv=fsync`.

## Manual build (without the helper)

1. Build the SD image:

```bash
nix build .#nixosConfigurations.test-t6.config.system.build.sdImage
```

2. Flash the image (U-Boot is already fused in):

```bash
sudo dd if=result/sd-image/*.img of=/dev/sdX bs=4M status=progress conv=fsync
sync
```

3. Insert into the NanoPC-T6 and power on.

## Installing to eMMC from the SD boot

Once the SD image is working:

1. Boot from microSD.
2. Identify the eMMC device (usually `/dev/mmcblk0`):

```bash
lsblk
```

3. Partition and format eMMC:

```bash
sudo parted /dev/mmcblk0 -- mklabel gpt
sudo parted /dev/mmcblk0 -- mkpart primary ext4 32MiB 100%
sudo parted /dev/mmcblk0 -- set 1 boot on
sudo mkfs.ext4 -L nixos /dev/mmcblk0p1
```

4. Mount and generate config:

```bash
sudo mount /dev/disk/by-label/nixos /mnt
sudo nixos-generate-config --root /mnt
```

5. Install:

```bash
sudo nixos-install
```

6. Flash U-Boot to eMMC as well:

```bash
sudo dd if=$(nix build --print-out-paths nixpkgs#uboot-nanopc-t6-rk3588_defconfig)/u-boot-rockchip.bin \
    of=/dev/mmcblk0 seek=64 conv=notrunc,fsync
sync
```

7. Power off, remove microSD, power on.

## Troubleshooting

### Board does not boot

Use serial UART if possible.  The RK3588 debug UART is **ttyS2 at 1500000 baud**.

Check:

- Was U-Boot written to the correct whole device?
- Was the correct device tree selected (`rockchip/rk3588-nanopc-t6.dtb`)?
- Is the power supply adequate (12 V / 2 A)?
- Is the SD card known-good?

### No Ethernet

```bash
dmesg | grep -i eth
dmesg | grep -i r8169
dmesg | grep -i stmmac
ip link
```

### Need to recover the board

FriendlyElec documents Maskrom recovery for RK3588 boards:

1. Hold the MASK button.
2. Power the board.
3. Connect USB-C to a host computer.
4. Use Rockchip tooling to erase or reflash eMMC/SPI.

## References

- FriendlyElec NanoPC-T6 wiki: <https://wiki.friendlyelec.com/wiki/index.php/NanoPC-T6>
- NixOS on ARM: <https://nixos.wiki/wiki/NixOS_on_ARM>
- U-Boot Rockchip documentation: <https://docs.u-boot.org/en/latest/board/rockchip/rockchip.html>
