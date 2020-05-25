. ./common.sh

cleanup() {
    logH2 "cleanup" "nixos and nix-env"
    pcent=$(echo $(df -h /  --output=pcent | tail -1 | cut -d'%' -f1))
    if [[ "$pcen" -gt 90 || "$((RANDOM%100))" -gt 90 ]]; then
        if [[ "$pcen" -gt 95 ]]; then
            ./gc.sh 15d
        else
            ./gc.sh
        fi
    else
        echo "* $(tput bold)do not$(tput sgr0) gc ..."
    fi
}

export -f cleanup
