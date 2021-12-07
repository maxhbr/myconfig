#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

set -ex
cd "$(dirname "${BASH_SOURCE[0]}")"

writeScripts() {
    local outDir="$1"
    local outFile="$2"
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

getIsoFromOutLink() {
    local outLink="$1"
    local outArr
    outArr=("$outLink/iso/"*".iso")
    echo "$(readlink -f "${outArr[-1]}")"
}

build() {
    local outDir="../iso"
    local outLink="$outDir/result"

    time nix build --out-link "$outLink" --show-trace .#myconfig-iso

    local out
    out="$(getIsoFromOutLink "$outLink")"
    du -h "$out"

    writeScripts "$outDir" "$out"
}

build
times
