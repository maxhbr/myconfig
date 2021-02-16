#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix cachix
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

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

build() (
    local hostConfig="$1"

    NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$myconfigDir/misc/empty_nixos_config.nix"
    jobCountArgs=""
    if [[ -f "$myconfigDir/secrets/workstation/ip" ]]; then
        if nix ping-store --store ssh://"$(cat "$myconfigDir/secrets/workstation/ip")"; then
            jobCountArgs="-j0"
        fi
    fi

    (cd $myconfigDir/;
     export NIX_PATH=""
     set -x;
     time nix-build iso.nix \
          $NIX_PATH_ARGS \
          $jobCountArgs \
          --show-trace \
          --no-out-link \
          $([[ "$hostConfig" ]] && echo "--argstr hostConfig $hostConfig")
    )
)

buildAndCopy() {
    local hostConfig="$1"

    drv=$(build "$hostConfig")
    outArr=("$drv/iso/nixos-myconfig"*".iso")
    out="${outArr[-1]}"
    du -h "$out"
    local outDir
    outDir="$myconfigDir/__out/iso$(getNamePrefixFromConfig "${hostConfig}")"
    install -D -m 644 -v "$out" -t "$outDir"
    nix-store --delete "$drv"

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

if [[ "$1" == "--dry-run" ]]; then
    shift
    build "${1}" "--dry-run"
elif [[ "$1" == "--push-to-cachix" ]]; then
    shift
    build "${1}" | cachix push maxhbr
else
    buildAndCopy "${1}"
fi

