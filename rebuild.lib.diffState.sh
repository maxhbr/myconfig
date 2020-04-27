. ./common.sh

getOldFile() {
    newFile=$1
    echo "$newFile.old"
}

generateDiffWithOld() {
    local newFile=$1
    local defaultSdiffArgs="-bBWs"
    local oldFile=$(getOldFile "$newFile")
    if [[ -f "$oldFile" ]]; then
        (set +e; sdiff "${2:-$defaultSdiffArgs}" $oldFile $newFile |
             while read; do
                 local line="$REPLY"
                 case $line in
                     *'<'* ) echo "$(tput setaf 1)$line$(tput sgr0)" ;;
                     *'>'* ) echo "$(tput setaf 2)$line$(tput sgr0)" ;;
                     *'|'* ) echo "$(tput setaf 3)$line$(tput sgr0)" ;;
                     *)      echo "$line" ;;
                 esac
             done)
        rm "$oldFile"
    fi
    mv $newFile $oldFile
}

diffCurrentSystemDeps() {
    have nix-store || return 0
    [[ -e $1 ]] || return 0

    local profileRoot=$1
    local outFile="$logsDir/currentSystemDeps-$(hostname)-$(basename "$profileRoot")"

    nix-store -qR "$profileRoot" |
        sed 's/^[^-]*-//g' |
        # while read line ; do echo "$(sed 's/^[^-]*-//g' <<< $line) $line" ; done |
        sort -u > "$outFile"

    logH2 "diff dependencies of $profileRoot"
    generateDiffWithOld "$outFile"
}

diffGenerations() {
    local outFile="$logsDir/currentGenerations-$(hostname)"
    { sudo --preserve-env=NIX_PATH \
           nix-env \
           --list-generations --profile /nix/var/nix/profiles/system || return; }> "$outFile"
    logH2 "diff nixos generations"
    generateDiffWithOld "$outFile"
}

diffDiskUsage() {
    [[ -e /dev/dm-2 ]] || return 0

    local outFile="$logsDir/currentDiskUsage-$(hostname)"
    df -h --output="pcent,used" /dev/dm-2 > "$outFile"

    logH2 "diff disk usage"
    generateDiffWithOld "$outFile" "-bBW"
}


showStatDifferences() {
    logH1 "show" "stats"
    diffGenerations || logERR "failed: diffGenerations"
    diffCurrentSystemDeps /run/current-system/ || logERR "failed: diffCurrentSystemDeps /run/current-system/"
    diffCurrentSystemDeps ~/.nix-profile || logERR "failed: diffCurrentSystemDeps ~/.nix-profile"
    diffDiskUsage || logERR "failed: diffDiskUsage"
}

setupExitTrap() {
    local msg=$1
    local cmd="code=\$?; if [[ \"\$code\" -ne 0 ]]; then logERR \"at $msg\"; logERR \"error code is: \$code\"; fi; showStatDifferences; exit \$code"
    trap "$cmd" EXIT ERR INT TERM
}

export -f setupExitTrap
export -f showStatDifferences
