#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

help() {
    cat <<EOF
  $ $0 [--no-bc] [--ci] [--hostname hostname] [--check]
EOF
}

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"

set -e
ARGS=""
IN_CI=NO
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        --no-bc)
            ARGS="$args --option binary-caches ''"
            shift
            ;;
        --check)
            ARGS="$args --check"
            shift
            ;;
        --ci)
            IN_CI=YES
            shift
            ;;
        --hostname)
            nixosConfig="$myconfigDir/nixos/host-${2}"
            if [[ ! -d "$nixosConfig" ]]; then
                nixosConfig="$myconfigDir/nixos/${2}"
                if [[ ! -d "$nixosConfig" ]]; then
                    help
                    exit 2
                fi
            fi
            shift
            shift
            ;;
        *)
            help
            exit 1
            ;;
    esac
done
NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$nixosConfig"

if [[ "$IN_CI" == "YES" ]]; then
    tmpImports=$(mktemp -d)
    NIX_PATH_ARGS="$NIX_PATH_ARGS -I nixos-imports=$tmpImports"
    cat <<EOF > "$tmpImports/dummy-hardware-configuration.nix"
{...}: {
  fileSystems."/" = { device = "/dev/sdXX"; fsType = "ext4"; };
  fileSystems."/boot" = { device = "/dev/sdXY"; fsType = "vfat"; };
  boot.loader.grub.devices = [ "/dev/sdXY" ];
}
EOF
else
    NIX_PATH_ARGS="$NIX_PATH_ARGS -I nixos-imports=$myconfigImports"
fi

set -x

out=$(nix-build '../nixpkgs/nixos' \
                $NIX_PATH_ARGS \
                $ARGS \
                -A system \
                --no-out-link \
                --show-trace --keep-failed \
                --arg configuration "$nixosConfig")
nix path-info -hS $out
times
