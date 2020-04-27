#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

help() {
    cat <<EOF
  $ $0 [--dry-run] [--no-bc] [--hostname hostname] [--check]
EOF
}

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"

set -e
ARGS=""
DRYRUN=NO
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        --no-bc)
            ARGS="$ARGS --option binary-caches ''"
            shift
            ;;
        --check)
            ARGS="$ARGS --check"
            shift
            ;;
        --dry-run)
            ARGS="$ARGS --dry-run"
            DRYRUN=YES
            shift
            ;;
        --hostname)
            set -x
            nixosConfig="$myconfigDir/nixos/host-${2}"
            if [[ ! -d "$nixosConfig" ]]; then
                nixosProfile="$myconfigDir/nixos/${2}.nix"
                if [[ -f "$nixosProfile" ]]; then
                    nixosConfig=$(mktemp -d)
                    trap 'ret=$?; rm "'"$nixosConfig"'/default.nix"; rmdir "'"$nixosConfig"'" 2>/dev/null; exit $ret' 0
                    cat <<EOF | tee "$nixosConfig/default.nix"
{...}: {
  imports = [ $nixosProfile ];
  config = {
    fileSystems."/" = { device = "/dev/sdXX"; fsType = "ext4"; };
    fileSystems."/boot" = { device = "/dev/sdXY"; fsType = "vfat"; };
    boot.loader.grub.devices = [ "/dev/sdXY" ];
    networking.hostName = "adHoc";
    networking.hostId = "123456";
  };
}
EOF
                    cat <<EOF | tee "$nixosConfig/nixPath.nix"
{ nix.nixPath = [ ]; }
EOF
                else
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
NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$nixosConfig -I nix-path-file=$nixosConfig/nixPath.nix"
NIX_PATH=""

set -x

out=$(set -x;
    nix-build '../nixpkgs/nixos' \
              $NIX_PATH_ARGS \
              $ARGS \
              -A system \
              --no-out-link \
              --show-trace --keep-failed \
              --arg configuration "$nixosConfig")
if [[ "$DRYRUN" != "YES" ]]; then
    if [[ -z "$out" ]]; then
        exit 1
    fi

    nix path-info -hS $out
fi
times
