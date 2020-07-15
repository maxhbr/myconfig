#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

build() (
    local hostConfig="$1"

    NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$myconfigDir/hosts/minimal"
    NIX_PATH=""
    if nix ping-store --store ssh://192.168.178.90; then
        jobCountArgs="-j0"
    else
        jobCountArgs=""
    fi

    set -x
    time nix-build iso.nix \
         $NIX_PATH_ARGS \
         $jobCountArgs \
         --show-trace \
         --no-out-link \
         --argstr hostConfig "$hostConfig"
)

buildAndCopy() {
    local hostConfig="$1"

    drv=$(build "$hostConfig")
    out=("$drv/iso/nixos"*".iso")
    finalOut="nixos-myconfig-$(basename ${hostConfig%.*}).iso"
    du -h "$out"
    install -D -m 644 -v "$out" -t "$myconfigDir/__out/iso/"
    nix-store --delete "$drv"
}

buildAndCopy "${1:-roles/dev.nix}"

cat <<EOF
run with:
$ qemu-kvm -boot d -cdrom $finalOut -m 32000 -cpu host -smp 6
$ sudo dd if=$finalOut of=/dev/sdX bs=4M
EOF
