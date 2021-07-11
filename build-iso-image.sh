#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

set -e
cd "$(dirname "${BASH_SOURCE[0]}")"

writeScripts() {
    local outFile="$1"
    local outDir
    outDir="$(dirname "$outFile")"
    cat <<EOF | tee "$outDir/dd.sh"
#!/usr/bin/env bash
set -ex
sdX="\$1"
sudo dd if="$outFile" of="\$sdX" bs=4M conv=sync
EOF
    chmod +x "$outDir/dd.sh"

    cat <<EOF | tee "$outDir/run-qemu.sh"
#!/usr/bin/env bash
set -ex
qemu-kvm -boot d -cdrom "$outFile" -m 32000 -cpu host -smp 6
EOF
    chmod +x "$outDir/run-qemu.sh"
}

build() {
    time nix build --no-link --show-trace $@ .#myconfig-iso
}

getIsoFromDrv() {
    local drv="$1"
    local outArr
    outArr=("$drv/iso/nixos-myconfig"*".iso")
    echo "${outArr[-1]}"
}

buildAndCopy() {
    local drv
    drv=$(build "$@")
    if [[ "$*" == *"--dry-run"* ]]; then
        return 0
    fi

    local out
    out="$(getIsoFromDrv "$drv")"
    du -h "$out"


    local outDir="__out/iso"
    install -D -m 644 -v "$out" -t "$outDir"
    writeScripts "$outDir/$(basename "$out")"


    nix-store --delete "$drv" || {
        sleep 20
        nix-store --delete "$drv" || printf "failed to\n\tnix-store --delete \"$drv\"\n"
    }
}

buildAndCopy "$@"
times
