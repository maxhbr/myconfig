#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix cachix
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

help() {
    cat <<EOF
$@ [--push-to-cachix] path/to/config.nix [--dry-run] [arguments [..]]
EOF
}

getNamePrefixFromConfig() {
    local config=${1%.nix}
    if [[ "$config" == "" ]]; then
        return
    fi

    local configBN
    configBN="$(basename "$config")"
    if [[ "$configBN" != "default" ]]; then
        echo "-$configBN"
    else
        getNamePrefixFromConfig "$(dirname "$config")"
    fi
}

build() {
    local nixfile
    nixfile="$(readlink -f $1)"; shift

    if [[ ! -f "$nixfile" ]]; then
        echo "'$nixfile' not found"
        exit 1
    fi

    jobCountArgs=""
    if [[ -f "$myconfigDir/secrets/workstation/ip" ]]; then
        if nix ping-store --store ssh://"$(cat "$myconfigDir/secrets/workstation/ip")"; then
            # force build on remote host
            jobCountArgs="-j0"
        fi
    fi

    (cd $myconfigDir;
     export NIX_PATH=""
     set -x;
     time nix-build "$nixfile" \
          -I nixpkgs="$nixpkgs" -I nixos-config="$myconfigDir/misc/empty_nixos_config.nix" \
          $jobCountArgs \
          --show-trace \
          --no-out-link \
          "$@")
}

buildAndCopy() {
    local drv
    drv=$(build "$@")
    local nixfile
    nixfile="$(readlink -f $1)"
    local outArr
    outArr=("$drv/iso/nixos-myconfig"*".iso")
    local out="${outArr[-1]}"
    du -h "$out"
    local outDir
    outDir="$myconfigDir/__out/iso$(getNamePrefixFromConfig "${nixfile}")"
    install -D -m 644 -v "$out" -t "$outDir"
    nix-store --delete "$drv" || {
        sleep 20
        nix-store --delete "$drv" || echo "failed to\n\tnix-store --delete \"$drv\""
    }

    cat <<EOF | tee "$outDir/dd.sh"
#!/usr/bin/env bash
set -ex
sdX="\$1"
sudo dd if="$outDir/$(basename "$out")" of="\$sdX" bs=4M conv=sync
EOF
    chmod +x "$outDir/dd.sh"

    cat <<EOF | tee "$outDir/run-qemu.sh"
#!/usr/bin/env bash
set -ex
qemu-kvm -boot d -cdrom "$outDir/$(basename "$out")" -m 32000 -cpu host -smp 6
EOF
    chmod +x "$outDir/run-qemu.sh"
}

if [[ "$1" == "--help" ]]; then
    help
elif [[ "$1" == "--push-to-cachix" ]]; then
    shift
    build "$@" | cachix push maxhbr
elif [[ "$2" == "--dry-run" ]]; then
    build "$@"
else
    buildAndCopy "$@"
fi

