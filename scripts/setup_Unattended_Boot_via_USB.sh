#!/usr/bin/env nix-shell
#! nix-shell -i bash -p cryptsetup util-linux coreutils pass

# Setup Unattended Boot via USB (Option 1: Write key onto the start of the stick)
# See: https://nixos.wiki/wiki/Full_Disk_Encryption#Option_1:_Write_key_onto_the_start_of_the_stick
#
# Usage: setup_Unattended_Boot_via_USB.sh <luks-device> <usb-device>
#   <luks-device>  e.g. /dev/sda2  - the LUKS-encrypted partition
#   <usb-device>   e.g. /dev/sdb   - the USB stick to write the key onto

set -euo pipefail

LUKS_DEVICE="${1:-}"
USB_DEVICE="${2:-}"
KEYFILE="hdd.key"

usage() {
    echo "Usage: $0 <luks-device> <usb-device>"
    echo ""
    echo "  <luks-device>  The LUKS-encrypted partition (e.g. /dev/sda2)"
    echo "  <usb-device>   The USB stick to write the key onto (e.g. /dev/sdb)"
    echo ""
    echo "WARNING: The USB stick will be overwritten at its start and will no"
    echo "         longer be usable as a regular storage device."
    exit 1
}

if [[ -z $LUKS_DEVICE || -z $USB_DEVICE ]]; then
    usage
fi

if [[ ! -b $LUKS_DEVICE ]]; then
    echo "ERROR: '$LUKS_DEVICE' is not a block device." >&2
    exit 1
fi

if ! sudo cryptsetup isLuks "$LUKS_DEVICE"; then
    echo "ERROR: '$LUKS_DEVICE' is not a LUKS device." >&2
    exit 1
fi

if [[ ! -b $USB_DEVICE ]]; then
    echo "ERROR: '$USB_DEVICE' is not a block device." >&2
    exit 1
fi

echo "### Generating key file: $KEYFILE"
dd if=/dev/random of="$KEYFILE" bs=4096 count=1
chmod 600 "$KEYFILE"

echo "### Adding key to LUKS device: $LUKS_DEVICE"
sudo cryptsetup luksAddKey "$LUKS_DEVICE" "$KEYFILE"

echo "### Writing key to USB device: $USB_DEVICE"
echo "WARNING: This will overwrite the beginning of $USB_DEVICE!"
sudo dd if="$KEYFILE" of="$USB_DEVICE"

if [[ -d "$HOME/.password-store" ]]; then
    echo "### Copying key to ~/.password-store"
    STORE_KEY_NAME="luks/$(basename "$LUKS_DEVICE")-usb-key"
    pass insert --force --multiline "$STORE_KEY_NAME" <"$KEYFILE"
    echo "Key stored in password-store as: $STORE_KEY_NAME"
fi

echo "### Retrieving disk info for NixOS config"
USB_BY_ID="$(ls -la /dev/disk/by-id/ | grep "$(basename "$USB_DEVICE")$" | awk '{print $9}' | head -n1 || true)"
LUKS_UUID="$(sudo blkid -s UUID -o value "$LUKS_DEVICE")"

echo ""
echo "======================================================================="
echo "Add the following to your NixOS configuration:"
echo "======================================================================="
echo ""
echo "  # Needed to find the USB device during initrd stage"
echo '  boot.initrd.kernelModules = [ "usb_storage" ];'
echo ""
echo "  boot.initrd.luks.devices = {"
echo "    luksroot = {"
if [[ -n $LUKS_UUID ]]; then
    echo "      device = \"/dev/disk/by-uuid/${LUKS_UUID}\";"
else
    echo "      device = \"${LUKS_DEVICE}\";"
fi
echo "      allowDiscards = true;"
echo "      keyFileSize = 4096;"
if [[ -n $USB_BY_ID ]]; then
    echo "      # pinning to /dev/disk/by-id/usbkey works"
    echo "      keyFile = \"/dev/disk/by-id/${USB_BY_ID}\";"
else
    echo "      keyFile = \"${USB_DEVICE}\";"
    echo "      # NOTE: Replace '${USB_DEVICE}' with the stable /dev/disk/by-id/... path"
    echo "      # Run: ls -la /dev/disk/by-id/ | grep $(basename "$USB_DEVICE")"
fi
echo "    };"
echo "  };"
echo ""
echo "======================================================================="
echo ""
echo "IMPORTANT: Remove $KEYFILE from the current directory after verifying your setup!"
