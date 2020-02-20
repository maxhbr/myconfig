#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

# $ $0 [--no-bc] [--ci [base]] [--check]

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"

set -e
args=""

if [[ "$1" == "--no-bc" ]]; then
    shift
    args="$args --option binary-caches ''"
fi

if [[ "$1" == "--ci" ]]; then
    shift
    if [[ ! -f "$myconfigDir/hostname" ]]; then
        cat <<EOF > "$myconfigDir/imports/dummy-hardware-configuration.nix"
{...}: {
  fileSystems."/" = { device = "/dev/sdXX"; fsType = "ext4"; };
  fileSystems."/boot" = { device = "/dev/sdXY"; fsType = "vfat"; };
  boot.loader.grub.devices = [ "/dev/sdXY" ];
}
EOF

        if [[ "$1" ]]; then
            if [[ -d "$myconfigDir/nixos/$1" ]]; then
                echo -n "$1" | tee "$myconfigDir/hostname"
                shift
            fi
        else
            echo -n "base" | tee "$myconfigDir/hostname"
        fi
    else
        echo "this system is already provisioned, can't run this test"
        exit 1
    fi
fi

if [[ "$1" == "--check" ]]; then
    shift
    args="$args --check"
fi

set -x

time \
    nix-build '<nixpkgs/nixos>' \
              $NIX_PATH_ARGS \
              $args \
              -A system \
              --no-out-link \
              --show-trace --keep-failed \
              --arg configuration "$nixosConfig"
