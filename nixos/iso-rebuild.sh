#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"
set -e
cd "$myconfigDir/nixos"
set -x
hostName=${1:-dev}
drv=$(nix-build \
          --show-trace \
          --no-out-link \
          iso.nix \
          --argstr hostName "$hostName")
set +x

out=$drv/iso/nixos.iso
du -h $out
cp $out "$myconfigDir/../nixos-${hostName}.iso"

times
cat <<EOF
run with:
$ qemu-kvm -boot d -cdrom ./nixos-dev.iso -m 32000 -cpu host -smp 6
EOF
