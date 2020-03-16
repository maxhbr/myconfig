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
out=("$drv/iso/nixos"*".iso")
du -h "$out"
install -m 644 "$out" "$myconfigDir/../nixos-${hostName}.iso"

set +x
times
cat <<EOF
run with:
$ qemu-kvm -boot d -cdrom ISO_FILE -m 32000 -cpu host -smp 6
EOF
