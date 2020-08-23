#!/usr/bin/env bash
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

    local configBN="$(basename "$config")"
    if [[ "$configBN" != "default" ]]; then
        echo "-$configBN"
    else
        getNamePrefixFromConfig "$(dirname "$config")"
    fi
}

build() (
    local hostConfig="$1"
    local secondaryHostConfig="$2"

    NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$myconfigDir/misc/empty_nixos_config.nix"
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
         --argstr hostConfig "$hostConfig" $([[ "$secondaryHostConfig" ]] && echo "--argstr secondaryHostConfig $secondaryHostConfig")
)

buildAndCopy() {
    local hostConfig="$1"
    local secondaryHostConfig="$2"

    drv=$(build "$hostConfig" "$secondaryHostConfig")
    out=("$drv/iso/nixos-myconfig"*".iso")
    du -h "$out"
    local outDir="$myconfigDir/__out/iso$(getNamePrefixFromConfig "${hostConfig}")$(getNamePrefixFromConfig "${secondaryHostConfig}")"
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

buildAndCopy "${1:-role.dev}" "$2"
