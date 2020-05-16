#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"
set -e
cd "$myconfigDir/iso"
set -x
hostConfig=${1:-roles/dev.nix}
NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$myconfigDir/hosts/minimal"
NIX_PATH=""
drv=$(nix-build iso.nix \
          $NIX_PATH_ARGS \
          -j0 \
          --show-trace \
          --no-out-link \
          --argstr hostConfig "$hostConfig")
out=("$drv/iso/nixos"*".iso")
finalOut="nixos-myconfig-$(basename ${hostConfig%.*}).iso"
du -h "$out"
mkdir -p "$HOME/Downloads/ISOs"
install -m 644 "$out" "$HOME/Downloads/ISOs/$finalOut"

./gc.sh || true

set +x
times
cat <<EOF
run with:
$ qemu-kvm -boot d -cdrom $finalOut -m 32000 -cpu host -smp 6
$ sudo dd if=$finalOut of=/dev/sdX bs=4M
EOF
