#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/
#
# Build a myconfig installer ISO.
#
# Usage:
#   ./build-iso-image.sh [<iso>] [<system>]
#
#   <iso>      ISO variant to build (flake package myconfig-<iso>). Default: iso
#   <system>   Nix system to build for. Default: the current system.
#              Use "aarch64-linux" to build an AArch64 ISO (e.g. for the
#              ROC-RK3568-PC and other ARM boots via UEFI).
#
# Environment overrides:
#   SYSTEM     same as the <system> positional argument.
#
# Building an aarch64 ISO on an x86_64 host requires either an aarch64 remote
# builder or local binfmt/qemu emulation:
#   boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

set -exuo pipefail

writeScripts() {
    local outDir="$1"
    local outFile="$2"
    local system="$3"

    cat <<'EOF' | tee "$outDir/dd.sh"
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p parted gptfdisk systemdMinimal e2fsprogs util-linux
set -euo pipefail
EOF
    echo "iso=$outFile" | tee -a "$outDir/dd.sh"
    cat <<'EOF' | tee -a "$outDir/dd.sh"
sdX="${1:-}"
die() {
    echo "error: $*" >&2
    exit 1
}
[[ -n $sdX ]] || die "usage: ./dd.sh <device>   (e.g. /dev/sdX)"
[[ -b $sdX ]] || die "$sdX is not a block device"
# Refuse partitions and other non-disk nodes: we overwrite the partition table.
[[ "$(lsblk -no TYPE "$sdX")" == "disk"* ]] || die "$sdX is not a whole disk"

lsblk "$sdX" >&2 || true
read -r -p "About to OVERWRITE $sdX with ${iso##*/}. Type 'yes' to continue: " ans
[[ $ans == "yes" ]] || die "aborted."

echo "==> Unmounting any mounted partitions of $sdX ..." >&2
for part in "$sdX"?*; do
    [[ -b $part ]] && sudo umount "$part" 2>/dev/null || true
done

echo "==> Writing $iso to $sdX ..." >&2
sudo dd if="$iso" of="$sdX" bs=4M conv=fsync status=progress
sync
echo "==> Done." >&2
EOF
    chmod +x "$outDir/dd.sh"

    if [[ $system == aarch64-* ]]; then
        # Emulated AArch64 UEFI guest: qemu ships the edk2 firmware under
        # $qemu/share/qemu/edk2-aarch64-code.fd.
        cat <<EOF | tee "$outDir/run-qemu.sh"
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p qemu
set -ex
fw="\$(dirname "\$(command -v qemu-system-aarch64)")/../share/qemu/edk2-aarch64-code.fd"
qemu-system-aarch64 \\
    -machine virt \\
    -cpu cortex-a72 \\
    -m 4096 \\
    -smp 4 \\
    -bios "\$fw" \\
    -boot d \\
    -cdrom "$outFile" \\
    -nographic
EOF
    else
        cat <<EOF | tee "$outDir/run-qemu.sh"
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p qemu_kvm
set -ex
qemu-kvm -boot d -cdrom "$outFile" -m 32000 -cpu host -smp 6
EOF
    fi
    chmod +x "$outDir/run-qemu.sh"
}

getIsoFromOutLink() {
    local iso="$1"
    local outLink="$2"
    local outArr
    outArr=("$outLink/$iso/"*".iso")
    readlink -f "${outArr[-1]}"
}

build() {
    local iso="$1"
    local system="$2"

    # Keep the historical output location for native builds; suffix the
    # system for cross/emulated builds so the two do not clobber each other.
    local outDir flakeAttr
    if [[ $system == "$currentSystem" ]]; then
        outDir="../$iso"
        flakeAttr=".#myconfig-$iso"
    else
        outDir="../$iso.$system"
        flakeAttr=".#packages.$system.myconfig-$iso"
    fi
    local outLink="$outDir/result"

    time nix build --out-link "$outLink" --show-trace "$flakeAttr"

    local out
    out="$(getIsoFromOutLink "$iso" "$outLink")"
    du -h "$out"

    writeScripts "$outDir" "$out" "$system"
    date >"$outDir/date"
    git log -1 >"$outDir/lastCommit"
}

iso="${1:-iso}"
currentSystem="$(nix eval --impure --raw --expr 'builtins.currentSystem')"
system="${2:-${SYSTEM:-$currentSystem}}"

cd "$(dirname "${BASH_SOURCE[0]}")"
[[ -d "$HOME/myconfig/priv" ]] && cd "$HOME/myconfig/priv"
nix flake update
build "$iso" "$system"
times
